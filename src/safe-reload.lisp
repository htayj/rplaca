(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Safe in-place RPLACA reload
;;; --------------------------------------------------------------------------

(defparameter *safe-reload-preflight-timeout* 60
  "Default timeout, in seconds, for the isolated safe reload preflight worker.")

(defparameter *safe-reload-worker-marker* "RPLACA-SAFE-RELOAD-RESULT "
  "Marker prefix used to locate the isolated reload worker's Lisp data result.")

(defparameter *safe-reload-visible-summary-limit* 240
  "Maximum characters of result summary shown in visible reload notifications.")

(defvar *safe-reload-lock* (bt:make-lock "rplaca safe reload")
  "Process-local nonblocking lock guarding in-place RPLACA reloads.")

(defvar *safe-reload-condition*
  (bt:make-condition-variable :name "rplaca safe reload")
  "Condition variable for exact request/worker handoff under the reload lock.")

(defvar *safe-reload-running-p* nil
  "True while a synchronous or interactive safe reload request is active.")

(defvar *safe-reload-active-request* nil
  "Exact process-wide reload request currently owning the reload lifecycle.")

(defvar *safe-reload-worker-constructor* nil
  "Test override for the managed interactive preflight thread constructor.
NIL uses BT:MAKE-THREAD.  An override receives FUNCTION and NAME arguments.")

(defvar *safe-reload-completion-dispatch-function* nil
  "Test override for marshalling preflight completion to the frame process.
NIL queues a CLIM window-manager event to the request's top-level sheet.  An
override receives the SAFE-RELOAD-REQUEST and must preserve frame ownership.")

(defvar *safe-reload-preflight-function* nil
  "Function used by RPLACA-SAFE-RELOAD-PREFLIGHT.
The value is dynamically replaceable by tests.  NIL means use the built-in
isolated SBCL worker preflight implementation.")

(defvar *safe-reload-live-function* nil
  "Function used for the live in-place reload after preflight succeeds.
The value is dynamically replaceable by tests and is called with :BUFFER and
:SOURCE-ROOT.  NIL means use ASDF:LOAD-SYSTEM on the current image without
reinitializing RPLACA runtime state.")

(defstruct safe-reload-result
  "Result object returned by RPLACA safe reload entry points."
  status
  stage
  summary
  preflight
  live
  stdout
  stderr
  exit-code
  condition-type
  condition-message
  duration-seconds
  source-root)

(defstruct safe-reload-request
  "Exact ownership record for one synchronous or interactive reload."
  token
  mode
  buffer
  frame
  started-at
  timeout
  source-root
  (notify-p t :type boolean)
  (stage :claimed :type keyword)
  worker
  preflight)

(defclass rplaca-safe-reload-completion-event (clim:window-manager-event)
  ((request :initarg :request
            :reader safe-reload-completion-event-request)))

(define-condition runtime-admission-closed (error)
  ((operation :initarg :operation
              :reader runtime-admission-closed-operation))
  (:report
   (lambda (condition stream)
     (format stream "Cannot start ~A while safe reload owns runtime admission."
             (runtime-admission-closed-operation condition)))))

(defun call-with-runtime-admission (function &key (operation "runtime work"))
  "Call FUNCTION under the outer runtime-admission lock or signal.

FUNCTION must publish its observable buffer/registry reservation before it
returns.  The admission lock is deliberately outermost: callers may acquire
their existing buffer or registry lock inside FUNCTION, but must never call
this helper while already holding such an inner lock."
  (bt:with-lock-held (*safe-reload-lock*)
    (when *safe-reload-active-request*
      (error 'runtime-admission-closed :operation operation))
    (funcall function)))

(defun call-with-runtime-settlement-admission
    (function &key (operation "runtime settlement"))
  "Run final settlement under admission, waiting for an active reload.

Unlike a new start, already-owned cleanup must eventually publish settlement.
If an unexpected visibility gap allowed reload to claim first, wait on the
exact reload condition and run FUNCTION under the outer lock only after live
redefinition has ended.  FUNCTION may acquire the operation's inner lock."
  (declare (ignore operation))
  (bt:with-lock-held (*safe-reload-lock*)
    (loop :while *safe-reload-active-request*
          :do (bt:condition-wait *safe-reload-condition*
                                 *safe-reload-lock*
                                 ;; BT v1 has no portable broadcast.  Timed
                                 ;; rechecks keep the fallback notify safe for
                                 ;; multiple settlement waiters.
                                 :timeout 0.1))
    (funcall function)))

(defvar *message-help-runtime-lock*
  (bt:make-lock "rplaca message help runtime")
  "Lock guarding active independent auxiliary-frame reservations.")

(defvar *message-help-runtime-reservations* (make-hash-table :test #'eq)
  "Exact reservations for auxiliary-frame construction and top-level lifetimes.")

(defun message-help-active-count-snapshot ()
  "Return the number of constructing or running independent auxiliary frames."
  (bt:with-lock-held (*message-help-runtime-lock*)
    (hash-table-count *message-help-runtime-reservations*)))

(defun reserve-message-help-runtime ()
  "Atomically reserve one auxiliary-frame lifetime against live reload."
  (let ((token (cons :message-help (gensym "RUNTIME-"))))
    (call-with-runtime-admission
     (lambda ()
       (bt:with-lock-held (*message-help-runtime-lock*)
         (setf (gethash token *message-help-runtime-reservations*) t))
       token)
     :operation "an independent auxiliary frame")))

(defun release-message-help-runtime (token)
  "Release exact auxiliary-frame runtime TOKEN idempotently."
  (when token
    (bt:with-lock-held (*message-help-runtime-lock*)
      (remhash token *message-help-runtime-reservations*))))

(defun rplaca-reload-result-ok-p (result)
  "Return true when RESULT represents a completed safe reload."
  (and (typep result 'safe-reload-result)
       (eq :ok (safe-reload-result-status result))))

(defun rplaca-reload-result-summary (result)
  "Return the human-readable summary string for a safe reload RESULT."
  (if (typep result 'safe-reload-result)
      (or (safe-reload-result-summary result) "")
      (format nil "~A" result)))

(defun safe-reload-now-seconds ()
  "Return an internal real-time timestamp in fractional seconds."
  (/ (get-internal-real-time)
     (coerce internal-time-units-per-second 'double-float)))

(defun safe-reload-elapsed-seconds (started-at)
  "Return seconds elapsed since STARTED-AT."
  (- (safe-reload-now-seconds) started-at))

(defun safe-reload-normalize-timeout (timeout)
  "Return TIMEOUT as a positive integer number of seconds."
  (let ((value (cond
                 ((null timeout) *safe-reload-preflight-timeout*)
                 ((integerp timeout) timeout)
                 ((stringp timeout) (parse-integer timeout))
                 (t (error "Reload timeout must be an integer, got ~S" timeout)))))
    (max 1 value)))

(defun safe-reload-source-root (&optional source-root)
  "Return the RPLACA source root used by worker and live reloads."
  (uiop:ensure-directory-pathname
   (truename
    (or source-root
        (asdf:system-source-directory :rplaca)
        "."))))

(defun safe-reload-setup-candidates ()
  "Return candidate Lisp dependency setup files for reload workers."
  (remove nil
          (list (uiop:getenv "RPLACA_QUICKLISP_SETUP")
                (uiop:getenv "RPLACA_ULTRALISP_SETUP")
                (namestring (merge-pathnames #P"quicklisp/setup.lisp"
                                             (user-homedir-pathname))))))

(defun safe-reload-worker-setup-path ()
  "Return the first readable setup file for reload workers, or NIL."
  (dolist (candidate (safe-reload-setup-candidates))
    (unless (blank-string-p candidate)
      (let ((path (probe-file candidate)))
        (when path
          (return (namestring (truename path))))))))

(defun safe-reload-asd-path (source-root)
  "Return SOURCE-ROOT's rplaca.asd path when present."
  (let ((path (merge-pathnames #P"rplaca.asd"
                               (uiop:ensure-directory-pathname source-root))))
    (and (probe-file path) path)))

(defun safe-reload-load-asd (source-root)
  "Reload SOURCE-ROOT's rplaca.asd when present."
  (let ((asd-path (safe-reload-asd-path source-root)))
    (when asd-path
      (asdf:load-asd asd-path))))

(defun safe-reload-worker-form (source-root marker)
  "Return the Lisp form evaluated by the isolated reload preflight worker."
  (format nil
          "(let ((*print-case* :downcase)~%      (*print-circle* t)~%      (*print-escape* t)~%      (*print-pretty* t)~%      (*print-readably* nil)~%      (exit-code 0))~%  (handler-case~%      (let* ((source-root (truename ~S))~%             (asd-path (merge-pathnames #P\"rplaca.asd\" source-root)))~%        (pushnew source-root asdf:*central-registry* :test #'equal)~%        (when (probe-file asd-path)~%          (asdf:load-asd asd-path))~%        (let ((ql-package (find-package :ql)))~%          (when ql-package~%            (multiple-value-bind (quickload present-p)~%                (find-symbol \"QUICKLOAD\" ql-package)~%              (when (and present-p (fboundp quickload))~%                (funcall quickload :rplaca :silent t)))))~%        (asdf:load-system :rplaca :force t)~%        (format t \"~~&~A~~S~~%\"~%                (list :status :ok~%                      :summary \"Preflight reload succeeded.\")))~%    (error (condition)~%      (setf exit-code 1)~%      (format t \"~~&~A~~S~~%\"~%              (list :status :preflight-failed~%                    :summary (format nil \"Preflight reload failed: ~~A\" condition)~%                    :condition-type (format nil \"~~S\" (type-of condition))~%                    :condition-message (format nil \"~~A\" condition)))))~%  (uiop:quit exit-code))"
          (namestring source-root)
          marker
          marker))

(defun safe-reload-worker-command (source-root timeout)
  "Return the argv list for the isolated safe reload preflight worker."
  (let ((argv (list "timeout" (format nil "~D" timeout)
                    "sbcl" "--noinform" "--disable-debugger"
                    "--eval" "(require :asdf)"))
        (setup-path (safe-reload-worker-setup-path)))
    (when setup-path
      (setf argv (append argv (list "--load" setup-path))))
    (append argv
            (list "--eval" (safe-reload-worker-form
                             source-root
                             *safe-reload-worker-marker*)
                  "--eval" "(uiop:quit)"))))

(defun safe-reload-worker-payload (stdout)
  "Return the worker payload embedded in STDOUT, or NIL when absent."
  (let* ((marker *safe-reload-worker-marker*)
         (position (and stdout (search marker stdout :from-end t))))
    (when position
      (handler-case
          (lisp-data-read (subseq stdout (+ position (length marker))))
        (error () nil)))))

(defun safe-reload-payload-value (payload key)
  "Return KEY from worker PAYLOAD when it is a plist."
  (and (listp payload) (getf payload key)))

(defun safe-reload-worker-summary (payload stdout stderr exit-code)
  "Return a concise preflight summary for PAYLOAD and process outputs."
  (or (safe-reload-payload-value payload :summary)
      (safe-reload-payload-value payload :condition-message)
      (and (not (blank-string-p stderr))
           (format nil "Preflight reload failed: ~A"
                   (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (bounded-tool-execution-string stderr))))
      (and (not (blank-string-p stdout))
           (format nil "Preflight reload failed: ~A"
                   (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (bounded-tool-execution-string stdout))))
      (format nil "Preflight reload worker failed with exit code ~A."
              exit-code)))

(defun safe-reload-worker-result (payload stdout stderr exit-code duration source-root)
  "Convert worker process output to a SAFE-RELOAD-RESULT."
  (let* ((ok-p (and (eql exit-code 0)
                    (eq :ok (safe-reload-payload-value payload :status))))
         (status (if ok-p :ok :preflight-failed)))
    (make-safe-reload-result
     :status status
     :stage :preflight
     :summary (if ok-p
                  (or (safe-reload-payload-value payload :summary)
                      "Preflight reload succeeded.")
                  (safe-reload-worker-summary payload stdout stderr exit-code))
     :stdout (bounded-tool-execution-string stdout)
     :stderr (bounded-tool-execution-string stderr)
     :exit-code exit-code
     :condition-type (safe-reload-payload-value payload :condition-type)
     :condition-message (safe-reload-payload-value payload :condition-message)
     :duration-seconds duration
     :source-root source-root)))

(defun safe-reload-condition-result (status stage condition started-at
                                     &key preflight source-root)
  "Return a SAFE-RELOAD-RESULT describing CONDITION."
  (make-safe-reload-result
   :status status
   :stage stage
   :summary (format nil "~:(~A~) failed: ~A" stage condition)
   :preflight preflight
   :condition-type (condition-type-name condition)
   :condition-message (format nil "~A" condition)
   :duration-seconds (safe-reload-elapsed-seconds started-at)
   :source-root source-root))

(defun %safe-reload-run-preflight-worker (&key timeout source-root)
  "Run the isolated worker that proves RPLACA can be loaded from SOURCE-ROOT."
  (let* ((started-at (safe-reload-now-seconds))
         (root (safe-reload-source-root source-root))
         (seconds (safe-reload-normalize-timeout timeout))
         (argv (safe-reload-worker-command root seconds)))
    (handler-case
        (multiple-value-bind (stdout stderr exit-code)
            (uiop:run-program argv
                              :directory root
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (safe-reload-worker-result
           (safe-reload-worker-payload stdout)
           stdout
           stderr
           exit-code
           (safe-reload-elapsed-seconds started-at)
           root))
      (error (condition)
        (safe-reload-condition-result :preflight-failed
                                      :preflight
                                      condition
                                      started-at
                                      :source-root root)))))

(defun safe-reload-refresh-runtime-registrations (buffer)
  "Refresh non-destructive runtime registrations after a live source reload."
  ;; Do not call INITIALIZE-RPLACA-RUNTIME, RESET-INTERACTION-STATE, or
  ;; RPLACA-MAIN here.  These targeted refreshes make reloaded commands,
  ;; tools, keybindings, and package entrypoints visible while preserving the
  ;; user's buffers, sessions, frame, and compose draft.
  (when (fboundp 'init-tools)
    (funcall 'init-tools))
  (when (fboundp 'install-chat-frame-keybindings)
    (funcall 'install-chat-frame-keybindings))
  (when (fboundp 'reload-package-channels)
    (funcall 'reload-package-channels))
  (when (fboundp 'reload-active-packages)
    (let ((*package-runtime-maintenance-admitted-p* t))
      (funcall 'reload-active-packages :buffer buffer))))

(defun %safe-reload-live-reload (&key buffer source-root)
  "Reload RPLACA in the current image without resetting application state."
  (let ((started-at (safe-reload-now-seconds))
        (stdout (make-string-output-stream))
        (stderr (make-string-output-stream))
        (root (safe-reload-source-root source-root)))
    (handler-case
        (let ((*standard-output* stdout)
              (*trace-output* stderr)
              (*error-output* stderr))
          (safe-reload-load-asd root)
          (asdf:load-system :rplaca :force t)
          (safe-reload-refresh-runtime-registrations buffer)
          (make-safe-reload-result
           :status :ok
           :stage :live
           :summary "Live reload succeeded."
           :stdout (bounded-tool-execution-string
                    (get-output-stream-string stdout))
           :stderr (bounded-tool-execution-string
                    (get-output-stream-string stderr))
           :duration-seconds (safe-reload-elapsed-seconds started-at)
           :source-root root))
      (error (condition)
        (let ((result (safe-reload-condition-result :live-failed
                                                    :live
                                                    condition
                                                    started-at
                                                    :source-root root)))
          (setf (safe-reload-result-stdout result)
                (bounded-tool-execution-string
                 (get-output-stream-string stdout))
                (safe-reload-result-stderr result)
                (bounded-tool-execution-string
                 (get-output-stream-string stderr)))
          result)))))

(defun safe-reload-preflight-function ()
  "Return the effective preflight function."
  (or *safe-reload-preflight-function*
      #'%safe-reload-run-preflight-worker))

(defun safe-reload-live-function ()
  "Return the effective test override live reload function, if any."
  *safe-reload-live-function*)

(defun call-safe-reload-live-function (buffer source-root)
  "Run the effective live reload function for BUFFER and SOURCE-ROOT."
  (if *safe-reload-live-function*
      (funcall *safe-reload-live-function*
               :buffer buffer
               :source-root source-root)
      (%safe-reload-live-reload :buffer buffer :source-root source-root)))

(defun rplaca-safe-reload-preflight (&key timeout source-root)
  "Preflight a RPLACA reload in an isolated worker and return a result.
The current Lisp image is not mutated by this function."
  (let ((started-at (safe-reload-now-seconds)))
    (handler-case
        (funcall (safe-reload-preflight-function)
                 :timeout (safe-reload-normalize-timeout timeout)
                 :source-root source-root)
      (error (condition)
        (safe-reload-condition-result :preflight-failed
                                      :preflight
                                      condition
                                      started-at
                                      :source-root source-root)))))

(defun safe-reload-result-plist (result)
  "Return RESULT as Lisp data suitable for tools and debug summaries."
  (when (typep result 'safe-reload-result)
    (append
     (list :status (safe-reload-result-status result)
           :ok (rplaca-reload-result-ok-p result)
           :stage (safe-reload-result-stage result)
           :summary (safe-reload-result-summary result))
     (when (safe-reload-result-duration-seconds result)
       (list :duration-seconds (safe-reload-result-duration-seconds result)))
     (when (safe-reload-result-exit-code result)
       (list :exit-code (safe-reload-result-exit-code result)))
     (when (safe-reload-result-condition-type result)
       (list :condition-type (safe-reload-result-condition-type result)))
     (when (safe-reload-result-condition-message result)
       (list :condition-message (safe-reload-result-condition-message result)))
     (when (safe-reload-result-source-root result)
       (list :source-root (namestring (safe-reload-result-source-root result))))
     (when (and (safe-reload-result-stdout result)
                (not (blank-string-p (safe-reload-result-stdout result))))
       (list :stdout (safe-reload-result-stdout result)))
     (when (and (safe-reload-result-stderr result)
                (not (blank-string-p (safe-reload-result-stderr result))))
       (list :stderr (safe-reload-result-stderr result)))
     (when (safe-reload-result-preflight result)
       (list :preflight
             (safe-reload-result-plist
              (safe-reload-result-preflight result))))
     (when (safe-reload-result-live result)
       (list :live
             (safe-reload-result-plist
              (safe-reload-result-live result)))))))

(defun emit-safe-reload-result-event (result)
  "Emit the structured safe-reload-result debug event for RESULT."
  (ignore-errors
    (apply #'file-debug-event
           "safe-reload-result"
           (safe-reload-result-plist result))))

(defun safe-reload-start-notification-text ()
  "Return the visible notification shown before reload preflight starts."
  "[RPLACA safe reload started: preflighting updated source...]")

(defun notify-safe-reload-started (buffer)
  "Insert a visible start notification for BUFFER."
  (when buffer
    (buffer-insert-system-message buffer (safe-reload-start-notification-text))))

(defun safe-reload-current-chat-frame ()
  "Return the currently active chat frame, when running under McCLIM."
  (let ((class (find-class 'rplaca-chat-frame nil))
        (frame clim:*application-frame*))
    (and class (typep frame class) frame)))

(defun redisplay-safe-reload-status-now ()
  "Synchronously redisplay the current chat frame after a reload status change."
  (let ((frame (safe-reload-current-chat-frame)))
    (when (and frame (fboundp 'handle-chat-frame-redisplay))
      (ignore-errors
        (funcall 'handle-chat-frame-redisplay frame)))))

(defun safe-reload-one-line-summary (summary)
  "Return SUMMARY collapsed to one display line."
  (with-output-to-string (out)
    (let ((space-p nil))
      (loop :for char :across (or summary "")
            :do (cond
                  ((find char '(#\Space #\Tab #\Newline #\Return) :test #'char=)
                   (unless space-p
                     (write-char #\Space out)
                     (setf space-p t)))
                  (t
                   (write-char char out)
                   (setf space-p nil)))))))

(defun safe-reload-visible-summary (result)
  "Return RESULT summary text suitable for the transcript surface."
  (let* ((summary (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (safe-reload-one-line-summary
                                (rplaca-reload-result-summary result))))
         (limit *safe-reload-visible-summary-limit*))
    (cond
      ((<= (length summary) limit)
       summary)
      (t
       (format nil "~A... see tool result/debug log for details."
               (subseq summary 0 limit))))))

(defun safe-reload-notification-text (result)
  "Return the visible buffer notification for RESULT."
  (let ((summary (safe-reload-visible-summary result)))
    (case (and (typep result 'safe-reload-result)
               (safe-reload-result-status result))
      (:ok
       (format nil "[RPLACA safe reload succeeded: ~A]" summary))
      (:busy
       (format nil "[RPLACA safe reload busy: ~A]" summary))
      (:refused
       (format nil "[RPLACA safe reload refused: ~A]" summary))
      (:preflight-failed
       (format nil "[RPLACA safe reload failed during preflight: ~A]" summary))
      (:live-failed
       (format nil "[RPLACA safe reload failed during live reload: ~A]" summary))
      (t
       (format nil "[RPLACA safe reload failed: ~A]" summary)))))

(defun maybe-notify-safe-reload-buffer (buffer result notify-p)
  "Insert RESULT as a visible system message when requested."
  (when (and notify-p buffer)
    (buffer-insert-system-message buffer (safe-reload-notification-text result)))
  result)

(defun finalize-safe-reload-result (result buffer notify-p)
  "Emit debug and visible notifications for RESULT, then return it."
  (emit-safe-reload-result-event result)
  (maybe-notify-safe-reload-buffer buffer result notify-p))

(defun safe-reload-busy-result ()
  "Return a result for an overlapping reload request."
  (make-safe-reload-result
   :status :busy
   :stage :lock
   :summary "Another RPLACA safe reload is already running."))

(defun safe-reload-buffer-runtime-active-p (buffer)
  "Return true when BUFFER owns or awaits provider/tool/OAuth runtime work."
  (and buffer
       (bt:with-lock-held ((buffer-runtime-lock buffer))
         (not
          (null
           (or (buffer-pending-stream buffer)
               (buffer-streaming-message buffer)
               (buffer-pending-tool-execution buffer)
               (buffer-pending-interactive-operation buffer)
               (buffer-runtime-application buffer)
               (buffer-runtime-teardown buffer)
               (buffer-runtime-start-generation buffer)
               (buffer-runtime-start-owner buffer)
               (buffer-runtime-tool-cancellation-p buffer)
               (buffer-runtime-stopping-p buffer)
               (buffer-runtime-stopped-notification-p buffer)
               (buffer-disposing-p buffer)
               (buffer-pending-tool-calls buffer)
               (buffer-user-input-pending buffer)
               (eq :oauth (buffer-status buffer))))))))

(defun safe-reload-process-buffer-snapshot (buffer interop-threads)
  "Return buffers whose runtime ownership can make live reload unsafe.

The buffer ring is frame-process-owned and has no independent lock.  The
interactive command copies it on the frame process, then combines it with the
explicit BUFFER and buffers from the already-stable interop registry snapshot."
  (remove-duplicates
   (remove nil
           (append (list buffer)
                   (copy-list *buffer-ring*)
                   (mapcar #'interop-thread-buffer interop-threads)))
   :test #'eq))

(defun safe-reload-subagent-active-p (handle)
  "Return true until HANDLE has both terminal status and a settled worker."
  (let ((snapshot (subagent-snapshot handle)))
    (or (not (getf snapshot :done-p))
        (not (getf snapshot :worker-finished-p)))))

(defun safe-reload-interop-turn-unsettled-p (turn)
  "Return true until TURN has terminal status and its runner has exited."
  (bt:with-lock-held ((interop-turn-lock turn))
    (not (settled-interop-turn-p turn))))

(defun safe-reload-process-runtime-activity (&optional buffer)
  "Return the kind of process activity that prevents live redefinition.

This deliberately conservative process-wide check snapshots each registry
under its own established lock, releases that lock, and only then inspects
individual buffer, interop, and subagent objects under their respective locks.
No runtime lock is held while another registry or object lock is acquired.
When a snapshot cannot be verified, return :SNAPSHOT-ERROR so safety wins over
attempting a reload from incomplete state."
  (handler-case
      (let* ((help-frame-count (message-help-active-count-snapshot))
             (runtime-callback-count
               (runtime-callback-dispatch-pending-count))
             (oauth-flow (openai-oauth-pending-flow))
             (interop-threads (interop-thread-registry-snapshot))
             (interop-turns (interop-turn-registry-snapshot))
             (interop-runtime-operation-count
               (active-interop-runtime-operation-count))
             (synchronous-subagent-count
               (active-synchronous-subagent-run-count))
             (subagents (list-subagents))
             (buffers (safe-reload-process-buffer-snapshot
                       buffer interop-threads)))
        (cond
          ((plusp runtime-callback-count) :external-callback)
          ((package-lifecycle-operation-active-p) :package-lifecycle)
          ((plusp help-frame-count) :help-frame)
          (oauth-flow :oauth)
          ((some #'safe-reload-buffer-runtime-active-p buffers)
           :buffer-runtime)
          ((or (plusp interop-runtime-operation-count)
               (some #'interop-thread-execution-reserved-p interop-threads)
               (some #'safe-reload-interop-turn-unsettled-p interop-turns))
           :interop)
          ((or (plusp synchronous-subagent-count)
               (some #'safe-reload-subagent-active-p subagents))
           :subagent)
          (t nil)))
    (error () :snapshot-error)))

(defun safe-reload-refused-result (activity)
  "Return a refusal result for unsafe process ACTIVITY."
  (make-safe-reload-result
   :status :refused
   :stage :quiescence
   :summary
   (if (eq activity :snapshot-error)
       "Runtime quiescence could not be verified; reload was not started."
       "Runtime work is active; wait for external callbacks, close help frames, and stop provider/tool/OAuth, interop, and subagent work, then retry.")))

(defun safe-reload-complete-result (preflight live started-at)
  "Return the final success result combining PREFLIGHT and LIVE phases."
  (make-safe-reload-result
   :status :ok
   :stage :complete
   :summary "Preflight and live reload completed."
   :preflight preflight
   :live live
   :duration-seconds (safe-reload-elapsed-seconds started-at)
   :source-root (or (safe-reload-result-source-root live)
                    (safe-reload-result-source-root preflight))))

(defun safe-reload-live-failed-result (live preflight)
  "Attach PREFLIGHT to LIVE failure result and return it."
  (when (and (typep live 'safe-reload-result)
             (null (safe-reload-result-preflight live)))
    (setf (safe-reload-result-preflight live) preflight))
  live)

(defun run-safe-reload-under-lock (buffer timeout source-root notify-p started-at)
  "Run preflight and, if safe, the live reload while the reload lock is held."
  (let ((preflight (rplaca-safe-reload-preflight
                    :timeout timeout
                    :source-root source-root)))
    (if (rplaca-reload-result-ok-p preflight)
        (let ((live (handler-case
                        (call-safe-reload-live-function buffer source-root)
                      (error (condition)
                        (safe-reload-condition-result :live-failed
                                                      :live
                                                      condition
                                                      started-at
                                                      :preflight preflight
                                                      :source-root source-root)))))
          (finalize-safe-reload-result
           (if (rplaca-reload-result-ok-p live)
               (safe-reload-complete-result preflight live started-at)
               (safe-reload-live-failed-result live preflight))
           buffer
           notify-p))
        (finalize-safe-reload-result preflight buffer notify-p))))

(defun safe-reload-try-acquire-lock ()
  "Try to acquire the reload lock without blocking; return NIL on overlap."
  (handler-case
      (bt:acquire-lock *safe-reload-lock* nil)
    (error () nil)))

(defun safe-reload-try-claim-request (request)
  "Atomically verify quiescence, close admission, and claim REQUEST.

Return true and NIL on success.  Otherwise return NIL and either :BUSY or the
activity kind that made the process unsafe.  The outer admission lock remains
held throughout inner registry/buffer snapshots, so a start either publishes
before this check and is observed, or sees the installed request and refuses."
  (unless (safe-reload-try-acquire-lock)
    (return-from safe-reload-try-claim-request (values nil :busy)))
  (unwind-protect
       (cond
         (*safe-reload-active-request*
          (values nil :busy))
         (t
          (let ((activity
                  (safe-reload-process-runtime-activity
                   (safe-reload-request-buffer request))))
            (if activity
                (values nil activity)
                (progn
                  (setf *safe-reload-active-request* request
                        *safe-reload-running-p* t)
                  (values t nil))))))
    (bt:release-lock *safe-reload-lock*)))

(defun safe-reload-blocked-result (blocker)
  "Return the busy or quiescence result represented by BLOCKER."
  (if (eq blocker :busy)
      (safe-reload-busy-result)
      (safe-reload-refused-result blocker)))

(defun safe-reload-request-active-p (request)
  "Return true when REQUEST still owns the process-wide reload lifecycle."
  (bt:with-lock-held (*safe-reload-lock*)
    (eq request *safe-reload-active-request*)))

(defun finish-safe-reload-request (request)
  "Release exact REQUEST ownership and clear visible busy state."
  (bt:with-lock-held (*safe-reload-lock*)
    (when (eq request *safe-reload-active-request*)
      (setf *safe-reload-active-request* nil
            *safe-reload-running-p* nil)
      #+sbcl
      (sb-thread:condition-broadcast *safe-reload-condition*)
      #-sbcl
      (bt:condition-notify *safe-reload-condition*)
      t)))

(defun safe-reload-record-worker (request worker)
  "Record WORKER only while REQUEST still owns the reload lifecycle."
  (bt:with-lock-held (*safe-reload-lock*)
    (when (eq request *safe-reload-active-request*)
      (setf (safe-reload-request-worker request) worker)
      (bt:condition-notify *safe-reload-condition*)
      t)))

(defun safe-reload-await-recorded-worker (request)
  "Wait until REQUEST records the current worker, or loses exact ownership."
  (let ((current (bt:current-thread)))
    (bt:with-lock-held (*safe-reload-lock*)
      (loop :while (and (eq request *safe-reload-active-request*)
                        (null (safe-reload-request-worker request)))
            :do (bt:condition-wait *safe-reload-condition*
                                   *safe-reload-lock*))
      (and (eq request *safe-reload-active-request*)
           (eq current (safe-reload-request-worker request))))))

(defun safe-reload-publish-preflight (request preflight)
  "Publish PREFLIGHT completion for exact active REQUEST."
  (bt:with-lock-held (*safe-reload-lock*)
    (when (and (eq request *safe-reload-active-request*)
               (eq :preflight (safe-reload-request-stage request)))
      (setf (safe-reload-request-preflight request) preflight
            (safe-reload-request-stage request) :preflight-complete)
      t)))

(defun safe-reload-begin-completion (request)
  "Claim REQUEST's frame-process application and return its worker/preflight."
  (bt:with-lock-held (*safe-reload-lock*)
    (when (and (eq request *safe-reload-active-request*)
               (eq :preflight-complete
                   (safe-reload-request-stage request)))
      (setf (safe-reload-request-stage request) :applying)
      (values (safe-reload-request-preflight request)
              (safe-reload-request-worker request)
              t))))

(defun make-safe-reload-preflight-thread (function name)
  "Create one managed preflight worker through the testable constructor."
  (if *safe-reload-worker-constructor*
      (funcall *safe-reload-worker-constructor* function name)
      (bt:make-thread function :name name)))

(defun safe-reload-queue-completion-event (request)
  "Queue REQUEST completion to its grafted CLIM top-level sheet."
  (let* ((frame (safe-reload-request-frame request))
         (sheet (and frame
                     (ignore-errors (clim:frame-top-level-sheet frame)))))
    (unless (and sheet (ignore-errors (clim:sheet-grafted-p sheet)))
      (error "Safe reload frame is no longer grafted."))
    (clim:queue-event
     sheet
     (make-instance 'rplaca-safe-reload-completion-event
                    :sheet sheet
                    :request request))
    t))

(defun safe-reload-dispatch-completion (request)
  "Marshal REQUEST completion to the frame process without mutating UI state."
  (handler-case
      (funcall (or *safe-reload-completion-dispatch-function*
                   #'safe-reload-queue-completion-event)
               request)
    (error (condition)
      (ignore-errors
        (file-debug-event "safe-reload-completion-dispatch-error"
                          :condition (format nil "~A" condition)))
      (finish-safe-reload-request request)
      nil)))

(defun run-safe-reload-preflight-worker (request)
  "Run REQUEST's isolated preflight and publish only immutable completion."
  (when (safe-reload-await-recorded-worker request)
    (let ((preflight
            (rplaca-safe-reload-preflight
             :timeout (safe-reload-request-timeout request)
             :source-root (safe-reload-request-source-root request))))
      (when (safe-reload-publish-preflight request preflight)
        (safe-reload-dispatch-completion request))))
  request)

(defun safe-reload-started-result ()
  "Return the immediate result from an accepted asynchronous preflight."
  (make-safe-reload-result
   :status :started
   :stage :preflight
   :summary "Isolated reload preflight is running."))

(defun safe-reload-missing-frame-result ()
  "Return a refusal when an interactive reload has no application frame."
  (make-safe-reload-result
   :status :refused
   :stage :frame
   :summary "Interactive safe reload requires a running RPLACA frame."))

(defun safe-reload-live-result-after-preflight (request preflight)
  "Apply PREFLIGHT at the frame boundary and return the final reload result."
  (let* ((buffer (safe-reload-request-buffer request))
         (source-root (safe-reload-request-source-root request))
         (started-at (safe-reload-request-started-at request))
         (activity (and (rplaca-reload-result-ok-p preflight)
                        (safe-reload-process-runtime-activity buffer))))
    (cond
      ((not (rplaca-reload-result-ok-p preflight))
       preflight)
      (activity
       (let ((result (safe-reload-refused-result activity)))
         (setf (safe-reload-result-preflight result) preflight)
         result))
      (t
       (let ((live
               (handler-case
                   (call-safe-reload-live-function buffer source-root)
                 (error (condition)
                   (safe-reload-condition-result
                    :live-failed
                    :live
                    condition
                    started-at
                    :preflight preflight
                    :source-root source-root)))))
         (if (rplaca-reload-result-ok-p live)
             (safe-reload-complete-result preflight live started-at)
             (safe-reload-live-failed-result live preflight)))))))

(defun apply-safe-reload-preflight-completion (request)
  "Apply exact REQUEST completion on the frame process and finalize visibly."
  (multiple-value-bind (preflight worker claimed-p)
      (safe-reload-begin-completion request)
    (unless claimed-p
      (return-from apply-safe-reload-preflight-completion nil))
    (let ((result nil)
          (buffer (safe-reload-request-buffer request))
          (notify-p (safe-reload-request-notify-p request)))
      (unwind-protect
           (progn
             ;; The completion event may be dequeued before the worker lambda
             ;; has returned from QUEUE-EVENT.  Join that already-complete
             ;; worker before redefining any code it could still execute.
             (when (and worker (not (eq worker (bt:current-thread))))
               (bt:join-thread worker))
             (setf result
                   (handler-case
                       (safe-reload-live-result-after-preflight
                        request preflight)
                     (error (condition)
                       (safe-reload-condition-result
                        :live-failed
                        :application
                        condition
                        (safe-reload-request-started-at request)
                        :preflight preflight
                        :source-root
                        (safe-reload-request-source-root request)))))
             (finalize-safe-reload-result result buffer notify-p))
        (finish-safe-reload-request request))
      result)))

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event rplaca-safe-reload-completion-event))
  "Apply safe reload completion only at the CLIM application event boundary."
  (declare (ignore sheet))
  (let ((request (safe-reload-completion-event-request event)))
    (handler-case
        (apply-safe-reload-preflight-completion request)
      (error (condition)
        (finish-safe-reload-request request)
        (ignore-errors
          (file-debug-event "safe-reload-completion-application-error"
                            :condition (format nil "~A" condition)))
        nil))))

(defun run-synchronous-safe-reload-request
    (buffer timeout source-root notify-p)
  "Run a programmatic reload synchronously under exact request ownership."
  (let ((request
          (make-safe-reload-request
           :token (cons :safe-reload (gensym "REQUEST-"))
           :mode :synchronous
           :buffer buffer
           :started-at (safe-reload-now-seconds)
           :timeout timeout
           :source-root source-root
           :notify-p notify-p)))
    (multiple-value-bind (claimed-p blocker)
        (safe-reload-try-claim-request request)
      (if claimed-p
          (unwind-protect
               (run-safe-reload-under-lock
                buffer timeout source-root notify-p
                (safe-reload-request-started-at request))
            (finish-safe-reload-request request))
          (finalize-safe-reload-result
           (safe-reload-blocked-result blocker)
           buffer
           notify-p)))))

(defun start-interactive-safe-reload (buffer)
  "Start a managed preflight and return before it completes."
  (let* ((frame (safe-reload-current-chat-frame))
         (request
           (make-safe-reload-request
            :token (cons :safe-reload (gensym "REQUEST-"))
            :mode :interactive
            :buffer buffer
            :frame frame
            :started-at (safe-reload-now-seconds)
            :notify-p t
            :stage :preflight)))
    (unless (or frame *safe-reload-completion-dispatch-function*)
      (return-from start-interactive-safe-reload
        (finalize-safe-reload-result
         (safe-reload-missing-frame-result) buffer t)))
    (multiple-value-bind (claimed-p blocker)
        (safe-reload-try-claim-request request)
      (unless claimed-p
        (return-from start-interactive-safe-reload
          (finalize-safe-reload-result
           (safe-reload-blocked-result blocker) buffer t))))
    (notify-safe-reload-started buffer)
    (redisplay-safe-reload-status-now)
    (handler-case
        (let ((worker
                (make-safe-reload-preflight-thread
                 (lambda () (run-safe-reload-preflight-worker request))
                 "rplaca-safe-reload-preflight")))
          (unless worker
            (error "Safe reload worker constructor returned NIL."))
          (safe-reload-record-worker request worker)
          (safe-reload-started-result))
      (error (condition)
        (unwind-protect
             (finalize-safe-reload-result
              (safe-reload-condition-result
               :preflight-failed
               :worker-start
               condition
               (safe-reload-request-started-at request))
              buffer
              t)
          (finish-safe-reload-request request))))))

(defun rplaca-safe-reload (&key buffer timeout source-root (notify-p t))
  "Safely reload updated RPLACA source in-place.
The process must first be quiescent: no buffer runtime owner, provider/tool/OAuth
activity, interop turn, or subagent worker may be active.  An isolated worker
then proves that RPLACA can load from SOURCE-ROOT.  The live image is reloaded
only when that preflight returns :OK.  Accepted live reload is a synchronous,
explicit development operation and briefly blocks its caller.  Refusals and
all preflight, busy, and live-reload failures are returned as SAFE-RELOAD-RESULT
objects rather than signaled to the caller."
  (run-synchronous-safe-reload-request buffer timeout source-root notify-p))

(defun safe-reload-rplaca-command (buffer)
  "Start a managed safe reload from a user command when quiescent.

The isolated preflight runs off the CLIM command process.  Its completion is
marshalled back as an application event; only the accepted live ASDF reload is
synchronous and briefly blocks the UI at that quiescent frame boundary.  Unsafe
activity is visibly refused before the start notification or either phase."
  (start-interactive-safe-reload buffer))
(defcommand safe-reload-rplaca-command)
