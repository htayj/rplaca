(in-package :rplaca)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Load the contrib once, before any concurrent cancellation path can race
  ;; package initialization.
  (require :sb-posix))

(defvar *posix-kill-function*
  (let ((symbol (and (find-package "SB-POSIX")
                     (find-symbol "KILL" "SB-POSIX"))))
    (and symbol (fboundp symbol) (symbol-function symbol)))
  "Resolved process signal function used by managed subprocess teardown.")

(defvar *openai-oauth-pending* nil
  "The application-owned OpenAI OAuth flow awaiting frame-process handling.")

(defvar *openai-oauth-pending-lock*
  (bt:make-lock "openai-oauth-pending")
  "Serialize publication and exact-flow claims for `*openai-oauth-pending*'.")

(defvar *runtime-settlement-notify-function* nil
  "Test override for managed post-worker buffer wakeups.
NIL uses WAKE-BUFFER-DISPLAY-CHANGE.  A non-NIL function receives BUFFER and
REASON and is captured when the short settlement waiter is constructed.")

(defun call-runtime-settlement-notify-safely
    (notify buf reason error-event)
  "Call settlement NOTIFY without allowing extension errors to escape a worker."
  (handler-case
      (funcall notify buf reason)
    (error (condition)
      (file-debug-event
       error-event
       :buffer-name (and buf (buffer-name buf))
       :condition (format nil "~A" condition))
      nil)))

(defun openai-oauth-pending-flow ()
  "Return the currently published OAuth flow under the pending-flow lock."
  (bt:with-lock-held (*openai-oauth-pending-lock*)
    *openai-oauth-pending*))

(defun openai-oauth-login-pending-p (&optional buffer)
  "Return true when an OAuth flow is pending, optionally for BUFFER."
  (let ((flow (openai-oauth-pending-flow)))
    (not (null (and flow
                    (or (null buffer)
                        (eq buffer (openai-oauth-flow-buffer flow))))))))

(defun join-openai-oauth-flow-worker (flow)
  "Join FLOW's callback worker and detach its notification-only waiter."
  (let ((worker (openai-oauth-flow-thread-snapshot flow))
        (waiter nil))
    (when (and worker (not (eq worker (bt:current-thread))))
      (bt:join-thread worker))
    (setf waiter
          (bt:with-lock-held ((openai-oauth-flow-lock flow))
            (openai-oauth-flow-settlement-thread flow)))
    ;; The notifier owns no application state after the callback worker exits.
    ;; Reap it only when already dead; an internal wake hook must never hold a
    ;; caller, frame process, or teardown boundary hostage.
    (when (and waiter
               (not (eq waiter (bt:current-thread)))
               (not (bt:thread-alive-p waiter)))
      (bt:join-thread waiter))
    (when (or (null worker)
              (not (eq worker (bt:current-thread))))
      (call-with-runtime-settlement-admission
       (lambda ()
         (bt:with-lock-held ((openai-oauth-flow-lock flow))
           (when (or (null worker)
                     (eq worker (openai-oauth-flow-thread flow)))
             (setf (openai-oauth-flow-thread flow) nil
                   (openai-oauth-flow-settlement-thread flow) nil
                   (openai-oauth-flow-worker-settled-p flow) t))))
       :operation "OAuth callback worker settlement")))
  flow)

(defun ensure-openai-oauth-settlement-waiter (flow worker)
  "Ensure a managed post-WORKER-exit frame wakeup for FLOW."
  (let ((notify (or *runtime-settlement-notify-function*
                    #'wake-buffer-display-change)))
    (bt:with-lock-held ((openai-oauth-flow-lock flow))
      (unless (openai-oauth-flow-cancelled-p flow)
        (or (openai-oauth-flow-settlement-thread flow)
            (handler-case
                (progn
                  (setf (openai-oauth-flow-settlement-waiter-done-p flow)
                        nil)
                  (setf (openai-oauth-flow-settlement-thread flow)
                        (bt:make-thread
                         (lambda ()
                           (unwind-protect
                                (progn
                                  (ignore-errors (bt:join-thread worker))
                                  (call-runtime-settlement-notify-safely
                                   notify
                                   (openai-oauth-flow-buffer flow)
                                   :oauth-settled
                                   "runtime-oauth-settlement-waiter-error"))
                             (bt:with-lock-held
                                 ((openai-oauth-flow-lock flow))
                               (when
                                   (or
                                    (null
                                     (openai-oauth-flow-settlement-thread flow))
                                    (eq
                                     (bt:current-thread)
                                     (openai-oauth-flow-settlement-thread flow)))
                                 (setf
                                  (openai-oauth-flow-settlement-waiter-done-p
                                   flow)
                                  t)))))
                         :name "rplaca-oauth-settlement"
                         :initial-bindings
                         (acons '*suppress-chat-redisplay-requests*
                                nil
                                bt:*default-special-bindings*))))
              (error (condition)
                (file-debug-event
                 "runtime-oauth-settlement-waiter-error"
                 :condition (format nil "~A" condition))
                nil)))))))

(defun openai-oauth-flow-settlement-thread-snapshot (flow)
  "Return FLOW's settlement waiter under its ownership lock."
  (and flow
       (bt:with-lock-held ((openai-oauth-flow-lock flow))
         (openai-oauth-flow-settlement-thread flow))))

(defun openai-oauth-flow-worker-settled-p-safe (flow)
  "Return true after FLOW's callback worker exits.

The settlement waiter is notification-only.  Reap it when already dead, but
never join it live from the frame process."
  (let ((worker (openai-oauth-flow-thread-snapshot flow)))
    (cond
      ((null worker)
       (bt:with-lock-held ((openai-oauth-flow-lock flow))
         (setf (openai-oauth-flow-worker-settled-p flow) t))
       t)
      ((bt:thread-alive-p worker)
       (ensure-openai-oauth-settlement-waiter flow worker)
       nil)
      (t
       (bt:join-thread worker)
       (multiple-value-bind (waiter waiter-done-p)
           (bt:with-lock-held ((openai-oauth-flow-lock flow))
             (values (openai-oauth-flow-settlement-thread flow)
                     (openai-oauth-flow-settlement-waiter-done-p flow)))
         (declare (ignore waiter-done-p))
         (when (and waiter
                    (not (eq waiter (bt:current-thread)))
                    (not (bt:thread-alive-p waiter)))
           (bt:join-thread waiter)))
       (call-with-runtime-settlement-admission
        (lambda ()
          (bt:with-lock-held ((openai-oauth-flow-lock flow))
            (when (eq worker (openai-oauth-flow-thread flow))
              (setf (openai-oauth-flow-thread flow) nil
                    (openai-oauth-flow-settlement-thread flow) nil
                    (openai-oauth-flow-worker-settled-p flow) t))))
        :operation "OAuth frame settlement")
       t))))

(defun publish-openai-oauth-pending-flow (flow)
  "Publish FLOW only when no other OAuth flow is pending."
  (check-type flow openai-oauth-flow)
  (bt:with-lock-held (*openai-oauth-pending-lock*)
    (when *openai-oauth-pending*
      (return-from publish-openai-oauth-pending-flow nil))
    (setf *openai-oauth-pending* flow)
    t))

(defun publish-reserved-openai-oauth-pending-flow
    (buffer flow generation)
  "Publish FLOW only while BUFFER still owns its exact start reservation.

The buffer runtime lock is deliberately acquired before the OAuth registry
lock.  Teardown detaches buffer ownership first, drops that lock, and only then
claims the registry, so publication either becomes visible for that claim or
observes the invalidated generation and loses without publishing.  The start
reservation remains owned until the command has applied its visible status;
its unwind cleanup releases it afterward."
  (check-type buffer buffer)
  (check-type flow openai-oauth-flow)
  (bt:with-lock-held ((buffer-runtime-lock buffer))
    (when (and (eq buffer (openai-oauth-flow-buffer flow))
               (eql generation
                    (buffer-runtime-start-generation buffer))
               (= generation (buffer-runtime-generation buffer))
               (eq (bt:current-thread)
                   (buffer-runtime-start-owner buffer))
               (not (buffer-runtime-stopping-p buffer))
               (not (buffer-runtime-stopped-notification-p buffer))
               (not (buffer-disposing-p buffer))
               (not (buffer-disposed-p buffer)))
      (publish-openai-oauth-pending-flow flow))))

(defun claim-openai-oauth-pending-flow (flow)
  "Clear and claim FLOW only when it is still the exact pending object."
  (bt:with-lock-held (*openai-oauth-pending-lock*)
    (when (eq flow *openai-oauth-pending*)
      (setf *openai-oauth-pending* nil)
      flow)))

(defun claim-buffer-openai-oauth-pending-flow (buffer)
  "Clear and return the pending OAuth flow when it belongs to BUFFER."
  (bt:with-lock-held (*openai-oauth-pending-lock*)
    (let ((flow *openai-oauth-pending*))
      (when (and flow (eq buffer (openai-oauth-flow-buffer flow)))
        (setf *openai-oauth-pending* nil)
        flow))))

(defun take-openai-oauth-pending-flow ()
  "Clear and return the pending OAuth flow, if any."
  (bt:with-lock-held (*openai-oauth-pending-lock*)
    (prog1 *openai-oauth-pending*
      (setf *openai-oauth-pending* nil))))

;;; --------------------------------------------------------------------------
;;; Interactive Agent Runtime
;;; --------------------------------------------------------------------------

(defun insert-agent-message-from-content (buf content-blocks agent-kw)
  "Insert an agent message from API content blocks.
Shows text parts and tool call summaries. Stores raw-content for round-trip."
  (let* ((text-parts (content-text-blocks content-blocks))
          (tool-uses (content-tool-use-blocks content-blocks))
          (canonical-content (canonicalize-message-content "assistant" content-blocks))
          (display (with-output-to-string (s)
                     (when (plusp (length text-parts))
                       (write-string text-parts s))
                     (dolist (tu tool-uses)
                       (when (plusp (length text-parts))
                         (write-char #\Newline s))
                       (write-string (format-tool-call-display tu) s))))
          (agent-msg (buffer-insert-read-only-message
                      buf
                      agent-kw
                      (string-trim '(#\Space #\Tab #\Newline #\Return)
                                   display)
                      :raw-content canonical-content)))
    agent-msg))

(defun prepare-tool-calls (buf tool-use-blocks)
  "Persist the unresolved TOOL-USE-BLOCKS before any reentrant hook runs."
  ;; Stash current input
  (setf (buffer-stashed-input buf) (message-text (buffer-input-message buf)))
  ;; Queue all tool calls for processing
  (setf (buffer-pending-tool-calls buf) (copy-list tool-use-blocks))
  (setf (buffer-tool-call-results buf) nil)
  buf)

(defun begin-tool-calls (buf tool-use-blocks)
  "Start a tool-call sequence and dispatch its first operation.

The unresolved queue is made durable first, so teardown can synthesize a
protocol-complete result even if a hook cancels the buffer before dispatch."
  (prepare-tool-calls buf tool-use-blocks)
  (advance-tool-calls buf))

(defstruct interactive-tool-execution
  "Worker-owned result state for one interactive tool invocation."
  tool-name
  tool-input
  tool-id
  agent-keyword
  (execution :background :type keyword)
  (buffer-generation 0 :type integer)
  tool-buffer-snapshot
  execution-plan
  effects
  result-text
  error
  (done-p nil :type boolean)
  (cancel-requested-p nil :type boolean)
  cancel-function
  worker
  settlement-waiter
  (settlement-waiter-done-p nil :type boolean)
  (lock (bt:make-lock "interactive-tool-execution")))

(defstruct interactive-buffer-operation
  "Managed non-CLIM worker for one interactive shell/pipeline/compaction action.

The worker owns a detached BUFFER snapshot and may publish only immutable
effects plus RESULT.  APPLY-FUNCTION is invoked only after the CLIM frame
process claims this exact operation."
  kind
  payload
  (buffer-generation 0 :type integer)
  buffer-snapshot
  runner
  apply-function
  cancel-function
  result
  error
  effects
  (done-p nil :type boolean)
  (cancel-requested-p nil :type boolean)
  current-stream
  process
  worker
  settlement-waiter
  (settlement-waiter-done-p nil :type boolean)
  (lock (bt:make-lock "interactive-buffer-operation")))

(defvar *interactive-operation-cancel-requested-p* nil
  "Worker-local cancellation predicate for managed interactive operations.")

(defvar *interactive-operation-stream-state-callback* nil
  "Worker-local callback publishing the provider stream currently owned by an operation.")

(defvar *interactive-operation-process-callback* nil
  "Worker-local callback publishing the subprocess currently owned by an operation.")

(defvar *interactive-operation-worker-constructor* nil
  "Test override for managed interactive operation thread construction.")

(defstruct buffer-runtime-application
  "Unique ownership token for one terminal result application phase."
  token
  owner
  (generation 0 :type integer)
  kind
  subject
  auxiliary)

(defstruct buffer-runtime-teardown
  "Single-flight cancellation retained until every prior owner settles."
  token
  owner
  stream-pairs
  tool-states
  operation-states
  operation-cancellation-states
  oauth-flow
  workers
  (stop-p nil :type boolean)
  (initializing-p t :type boolean)
  (workers-settled-p nil :type boolean)
  (reaper-started-p nil :type boolean)
  (frame-delivery-p nil :type boolean)
  (finalizing-p nil :type boolean))

(defvar *buffer-runtime-application-context* nil
  "Dynamically bound terminal-application token for atomic continuations.")

(defun buffer-runtime-application-valid-p (buf context)
  "Return true while CONTEXT still owns BUF's current runtime generation."
  (and context
       (bt:with-lock-held ((buffer-runtime-lock buf))
         (and (eq context (buffer-runtime-application buf))
              (= (buffer-runtime-application-generation context)
                 (buffer-runtime-generation buf))
              (not (buffer-runtime-stopping-p buf))
              (not (buffer-disposing-p buf))
              (not (buffer-disposed-p buf))))))

(defun buffer-runtime-continuation-valid-p (buf)
  "Return true when the current dynamic application may start more work."
  (or (null *buffer-runtime-application-context*)
      (buffer-runtime-application-valid-p
       buf *buffer-runtime-application-context*)))

(defun interactive-buffer-operation-snapshot (operation)
  "Return an immutable terminal snapshot of OPERATION."
  (bt:with-lock-held ((interactive-buffer-operation-lock operation))
    (list :done-p (interactive-buffer-operation-done-p operation)
          :cancel-requested-p
          (interactive-buffer-operation-cancel-requested-p operation)
          :result (interactive-buffer-operation-result operation)
          :error (interactive-buffer-operation-error operation)
          :effects (copy-list
                    (interactive-buffer-operation-effects operation)))))

(defun interactive-buffer-operation-cancel-requested-p-safe (operation)
  "Return true when cancellation has been requested for OPERATION."
  (bt:with-lock-held ((interactive-buffer-operation-lock operation))
    (interactive-buffer-operation-cancel-requested-p operation)))

(defun publish-interactive-buffer-operation-stream (operation stream)
  "Publish STREAM as OPERATION's cancellable provider owner."
  (let ((cancel-p nil))
    (bt:with-lock-held ((interactive-buffer-operation-lock operation))
      (setf (interactive-buffer-operation-current-stream operation) stream
            cancel-p
            (interactive-buffer-operation-cancel-requested-p operation)))
    (when (and cancel-p stream)
      (cancel-stream-state stream :stop-reason "cancelled"))
    stream))

(defun clear-interactive-buffer-operation-stream (operation stream)
  "Clear STREAM only when it is still OPERATION's exact provider owner."
  (bt:with-lock-held ((interactive-buffer-operation-lock operation))
    (when (eq stream
              (interactive-buffer-operation-current-stream operation))
      (setf (interactive-buffer-operation-current-stream operation) nil)))
  stream)

(defun publish-interactive-buffer-operation-process (operation process)
  "Publish PROCESS as OPERATION's cancellable subprocess owner."
  (bt:with-lock-held ((interactive-buffer-operation-lock operation))
    (setf (interactive-buffer-operation-process operation) process))
  ;; Do not signal only the parent here when cancellation was already
  ;; requested.  The subprocess runner observes the flag on its next bounded
  ;; poll and snapshots/terminates the complete process tree.
  process)

(defun clear-interactive-buffer-operation-process (operation process)
  "Clear PROCESS only when it is still OPERATION's exact subprocess owner."
  (bt:with-lock-held ((interactive-buffer-operation-lock operation))
    (when (eq process (interactive-buffer-operation-process operation))
      (setf (interactive-buffer-operation-process operation) nil)))
  process)

(defun cancel-interactive-buffer-operation (operation)
  "Cooperatively cancel OPERATION and close its currently owned I/O."
  (when operation
    (let ((stream
            (bt:with-lock-held ((interactive-buffer-operation-lock operation))
              (setf (interactive-buffer-operation-cancel-requested-p operation)
                    t)
              (interactive-buffer-operation-current-stream operation))))
      (when stream
        (ignore-errors
          (cancel-stream-state stream :stop-reason "cancelled")))))
  operation)

(defun interactive-buffer-operation-publication-allowed-p (buf)
  "Validate BUF for a managed non-CLIM operation under its runtime lock."
  (let ((application *buffer-runtime-application-context*))
    (cond
      (application
       (unless (and (eq application (buffer-runtime-application buf))
                    (= (buffer-runtime-application-generation application)
                       (buffer-runtime-generation buf)))
         (return-from interactive-buffer-operation-publication-allowed-p nil)))
      ((buffer-runtime-application buf)
       (error "Buffer ~A is applying a terminal runtime result"
              (buffer-name buf)))))
  (when (or (buffer-disposed-p buf)
            (buffer-disposing-p buf)
            (buffer-runtime-stopping-p buf)
            (buffer-runtime-stopped-notification-p buf))
    (if *buffer-runtime-application-context*
        (return-from interactive-buffer-operation-publication-allowed-p nil)
        (error "Cannot start an interactive operation while buffer ~A is stopping"
               (buffer-name buf))))
  (when (or (buffer-pending-stream buf)
            (buffer-pending-tool-execution buf)
            (buffer-pending-interactive-operation buf)
            (buffer-runtime-start-generation buf))
    (error "Buffer ~A already has a runtime operation" (buffer-name buf)))
  t)

(defun make-interactive-buffer-operation-worker (function name)
  "Construct one managed operation worker behind a testable boundary."
  (if *interactive-operation-worker-constructor*
      (funcall *interactive-operation-worker-constructor* function name)
      (bt:make-thread
       function
       :name name
       :initial-bindings
       (acons '*suppress-chat-redisplay-requests*
              nil
              bt:*default-special-bindings*))))

(defparameter *interactive-subprocess-default-timeout* 30
  "Default wall-clock timeout for interactive shell and pipeline commands.")

(defparameter *interactive-subprocess-output-limit* 65536
  "Maximum characters retained independently from subprocess stdout/stderr.")

(defparameter *interactive-subprocess-poll-interval* 0.02
  "Polling interval for cancellable interactive subprocesses.")

(defvar *interactive-subprocess-drain-thread-constructor* nil
  "Optional test constructor for bounded subprocess drain workers.
When non-NIL it receives a function and thread name.")

(defun executable-pathname-on-path (name)
  "Return the first executable-shaped NAME found on PATH, or NIL."
  (loop :for directory
          :in (uiop:split-string (or (uiop:getenv "PATH") "")
                                 :separator '(#\:))
        :for candidate
          := (and (not (blank-string-p directory))
                  (merge-pathnames
                   name (uiop:ensure-directory-pathname directory)))
        :when (and candidate (probe-file candidate))
          :return (truename candidate)))

(defparameter *interactive-subprocess-setsid-path*
  (executable-pathname-on-path "setsid")
  "Path to util-linux SETSID used to isolate managed command process groups.")

(defstruct (bounded-subprocess-output-state
            (:constructor %make-bounded-subprocess-output-state))
  "One continuously drained, memory-bounded subprocess output channel."
  (limit 0 :type integer)
  (buffer "" :type string)
  (count 0 :type integer)
  (truncated-p nil :type boolean)
  error
  thread)

(defun make-bounded-subprocess-output-state (limit)
  "Create a subprocess output state retaining at most LIMIT characters."
  (let ((limit (max 0 limit)))
    (%make-bounded-subprocess-output-state
     :limit limit
     :buffer (make-string limit))))

(defun drain-bounded-subprocess-output (stream state)
  "Drain STREAM to EOF, retaining only STATE's fixed character limit."
  (unwind-protect
       (handler-case
           (let ((chunk (make-string 4096)))
             (loop :for amount := (read-sequence chunk stream)
                   :while (plusp amount)
                   :do (let* ((count
                                (bounded-subprocess-output-state-count state))
                              (remaining
                                (max 0
                                     (- (bounded-subprocess-output-state-limit
                                         state)
                                        count)))
                              (retained (min amount remaining)))
                         (when (plusp retained)
                           (replace
                            (bounded-subprocess-output-state-buffer state)
                            chunk
                            :start1 count
                            :end1 (+ count retained)
                            :end2 retained)
                           (incf
                            (bounded-subprocess-output-state-count state)
                            retained))
                         (when (> amount retained)
                           (setf
                            (bounded-subprocess-output-state-truncated-p state)
                            t)))))
         (error (condition)
           (setf (bounded-subprocess-output-state-error state)
                 (format nil "~A" condition)
                 (bounded-subprocess-output-state-truncated-p state) t)))
    (ignore-errors (close stream :abort t)))
  state)

(defun start-bounded-subprocess-output-drain (stream state name)
  "Start a drain worker for STREAM and publish it in STATE."
  (setf (bounded-subprocess-output-state-thread state)
        (if *interactive-subprocess-drain-thread-constructor*
            (funcall *interactive-subprocess-drain-thread-constructor*
                     (lambda ()
                       (drain-bounded-subprocess-output stream state))
                     name)
            (bt:make-thread
             (lambda () (drain-bounded-subprocess-output stream state))
             :name name)))
  state)

(defun settle-bounded-subprocess-output (state)
  "Join STATE's drain worker and return retained text and truncation status."
  (let ((thread (bounded-subprocess-output-state-thread state)))
    (when (and thread (not (eq thread (bt:current-thread))))
      (bt:join-thread thread)
      ;; JOIN-THREAD is a one-shot synchronization operation on some
      ;; Bordeaux Threads implementations.  Clear the published owner so the
      ;; UNWIND-PROTECT cleanup cannot attempt a second join after normal
      ;; settlement.
      (setf (bounded-subprocess-output-state-thread state) nil)))
  (values
   (subseq (bounded-subprocess-output-state-buffer state)
           0 (bounded-subprocess-output-state-count state))
   (bounded-subprocess-output-state-truncated-p state)))

(defun linux-process-child-pids (pid)
  "Return PID's direct Linux children from procfs, or NIL when unavailable."
  (let ((path (format nil "/proc/~D/task/~D/children" pid pid)))
    (handler-case
        (with-open-file (stream path :direction :input)
          (loop :for value := (read stream nil nil)
                :while value
                :when (integerp value)
                  :collect value))
      (error () nil))))

(defun linux-process-descendant-pids (pid)
  "Return PID's descendants deepest-first using a bounded procfs snapshot."
  (labels ((walk (current seen)
             (if (member current seen)
                 (values nil seen)
                 (let ((new-seen (cons current seen))
                       (result nil))
                   (dolist (child (linux-process-child-pids current))
                     (multiple-value-bind (descendants updated-seen)
                         (walk child new-seen)
                       (setf result (append result descendants (list child))
                             new-seen updated-seen)))
                   (values result new-seen)))))
    (nth-value 0 (walk pid nil))))

(defun linux-process-stat-state-and-group-from-path (path)
  "Return Linux task state and process-group ID read from procfs PATH."
  (handler-case
      (with-open-file (stream path :direction :input)
        (let* ((line (read-line stream nil ""))
               (comm-end (position #\) line :from-end t)))
          (if (and comm-end (< (+ comm-end 4) (length line)))
              (let ((state (char line (+ comm-end 2))))
                (with-input-from-string (tail (subseq line (+ comm-end 4)))
                  (let ((parent-pid (read tail nil nil))
                        (process-group-id (read tail nil nil)))
                    (declare (ignore parent-pid))
                    (values state process-group-id))))
              (values nil nil))))
    (error () (values nil nil))))

(defun linux-process-stat-state-and-group (pid)
  "Return Linux PID's task state and process-group ID, or NIL values."
  (linux-process-stat-state-and-group-from-path
   (format nil "/proc/~D/stat" pid)))

(defun linux-process-id-live-p (pid)
  "Return true when Linux PID exists and is not a zombie/dead task."
  (let ((state (nth-value 0 (linux-process-stat-state-and-group pid))))
    (and state
         (not (member state '(#\Z #\X) :test #'char=)))))

(defun linux-process-group-live-p (process-group-id)
  "Return true when PROCESS-GROUP-ID contains a non-zombie Linux task."
  (and process-group-id
       (handler-case
           (some
            (lambda (path)
              (multiple-value-bind (state group-id)
                  (linux-process-stat-state-and-group-from-path path)
                (and (eql group-id process-group-id)
                     state
                     (not (member state '(#\Z #\X) :test #'char=)))))
            (directory #P"/proc/*/stat"))
         (error () nil))))

(defun signal-process-id (pid signal)
  "Send SIGNAL to PID through SB-POSIX without a reader-time dependency."
  (handler-case
      (when *posix-kill-function*
        (funcall *posix-kill-function* pid signal))
    (error () nil)))

(defun signal-process-group (process-group-id signal)
  "Send SIGNAL to every member of PROCESS-GROUP-ID."
  (when process-group-id
    (signal-process-id (- process-group-id) signal)))

(defun signal-interactive-subprocess-tree (process signal)
  "Signal PROCESS descendants deepest-first, then the process itself."
  (let ((pid (ignore-errors (sb-ext:process-pid process))))
    (when pid
      (dolist (child (linux-process-descendant-pids pid))
        (signal-process-id child signal))
      (signal-process-id pid signal)))
  process)

(defun terminate-interactive-subprocess
    (process &key process-group-id)
  "Send TERM, then bounded KILL, to PROCESS's isolated group or tree."
  (when process
    (let* ((pid (ignore-errors (sb-ext:process-pid process)))
           (descendants (and pid (linux-process-descendant-pids pid))))
      ;; The negative PGID signal closes the spawn-after-snapshot escape: every
      ;; current or newly forked descendant remains in the isolated group.
      (when process-group-id
        (signal-process-group process-group-id 15))
      ;; Procfs descendants remain a portability/race fallback for systems
      ;; where SETSID could not establish the expected group.
      (dolist (child descendants)
        (signal-process-id child 15))
      (when pid
        (signal-process-id pid 15))
      (ignore-errors (sb-ext:process-kill process 15))
      (loop :repeat 20
            :while (or (eq :running (sb-ext:process-status process))
                       (some #'linux-process-id-live-p descendants))
            :do (sleep *interactive-subprocess-poll-interval*))
      ;; Retain the pre-TERM descendants even if the shell exits first, and
      ;; resnapshot any last children while procfs still exposes the parent.
      (setf descendants
            (remove-duplicates
             (append descendants
                     (and pid (linux-process-descendant-pids pid)))))
      (when process-group-id
        (signal-process-group process-group-id 9))
      (dolist (child descendants)
        (when (linux-process-id-live-p child)
          (signal-process-id child 9)))
      (when (eq :running (sb-ext:process-status process))
        (when pid
          (signal-process-id pid 9))
        (ignore-errors (sb-ext:process-kill process 9)))
      (loop :repeat 20
            :for group-live-p
              := (and process-group-id
                       (linux-process-group-live-p process-group-id))
            :while (or (eq :running (sb-ext:process-status process))
                       group-live-p
                       (some #'linux-process-id-live-p descendants))
            ;; Repeat KILL while any group member remains executable.  This
            ;; both waits for asynchronous delivery and catches a last fork by
            ;; a member that had not yet processed the preceding signal.
            :do (when group-live-p
                  (signal-process-group process-group-id 9))
                (sleep *interactive-subprocess-poll-interval*))))
  process)

(defun await-interactive-subprocess-process-group (process)
  "Return PROCESS's PID after SETSID establishes PID as its PGID, or NIL."
  (let ((pid (ignore-errors (sb-ext:process-pid process))))
    (when pid
      (loop :repeat 50
            :for group-id
              := (nth-value 1 (linux-process-stat-state-and-group pid))
            :when (eql group-id pid)
              :return pid
            :while (eq :running (sb-ext:process-status process))
            :do (sleep 0.001)))))

(defun run-interactive-subprocess
    (command &key directory
                  (timeout *interactive-subprocess-default-timeout*)
                  (output-limit *interactive-subprocess-output-limit*)
                  (cancel-requested-p
                    *interactive-operation-cancel-requested-p*)
                  (process-callback
                    *interactive-operation-process-callback*))
  "Run COMMAND with bounded output, wall time, and cooperative cancellation.

COMMAND is either a shell string or an argv list.  Returns a plist and always
clears the published process owner before returning."
  (let* ((working-directory
           (uiop:ensure-directory-pathname
            (or directory (truename "."))))
         (program (if (stringp command) "/bin/sh" (first command)))
         (arguments (if (stringp command)
                        (list "-lc" command)
                        (rest command)))
         (setsid-path *interactive-subprocess-setsid-path*)
         (launch-program (or setsid-path program))
         (launch-arguments
           (if setsid-path
               (append (list "--wait"
                             "/bin/sh"
                             "-c"
                             ;; SBCL starts RUN-PROGRAM children as process
                             ;; group leaders.  util-linux SETSID must fork in
                             ;; that case, so the SB-EXT process PID is not the
                             ;; managed session PGID.  Publish the exact new
                             ;; session leader on stdout before EXEC; this
                             ;; bounded control line is consumed separately
                             ;; from command output.
                             "printf '%s\\n' \"$$\"; exec \"$@\""
                             "rplaca-managed-command"
                             (if (pathnamep program)
                                 (namestring program)
                                 program))
                       arguments)
               arguments))
         (deadline
           (and timeout
                (+ (get-internal-real-time)
                   (round (* (max 0 timeout)
                             internal-time-units-per-second)))))
         (process nil)
         (process-group-id nil)
         (stdout-stream nil)
         (stderr-stream nil)
         (stdout-state (make-bounded-subprocess-output-state output-limit))
         (stderr-state (make-bounded-subprocess-output-state output-limit))
         (cleanup-completed-p nil)
         (owner-cleared-p nil)
         (timed-out-p nil)
         (cancelled-p nil))
    (unless (and program (or (stringp program) (pathnamep program)))
      (error "Interactive subprocess command must be a string or argv list"))
    (labels ((cleanup-process-group ()
               (unless cleanup-completed-p
                 ;; Always signal an explicitly isolated group.  A single
                 ;; procfs visibility check can miss the interval after a fast
                 ;; leader exits but before its background child is observed.
                 (when (and process
                            (or process-group-id
                                (eq :running
                                    (sb-ext:process-status process))))
                   (terminate-interactive-subprocess
                    process :process-group-id process-group-id))
                 (setf cleanup-completed-p t)))
             (clear-process-owner ()
               (unless owner-cleared-p
                 (when process-callback
                   (ignore-errors (funcall process-callback nil)))
                 (setf owner-cleared-p t))))
      (unwind-protect
           (progn
             (setf process
                   (sb-ext:run-program
                    launch-program launch-arguments
                    :wait nil
                    :search t
                    :directory working-directory
                    :output :stream
                   :error :stream)
                   stdout-stream (sb-ext:process-output process)
                   stderr-stream (sb-ext:process-error process))
             (when setsid-path
               (let ((session-line (read-line stdout-stream nil nil)))
                 (setf process-group-id
                       (and session-line
                            (parse-integer session-line :junk-allowed nil)))))
             ;; Drain both pipes immediately and continuously.  Retained
             ;; memory is fixed, while excess output is discarded so neither
             ;; child channel can deadlock on a full pipe or grow a disk file.
             (start-bounded-subprocess-output-drain
              stdout-stream stdout-state "rplaca-subprocess-stdout")
             (start-bounded-subprocess-output-drain
              stderr-stream stderr-state "rplaca-subprocess-stderr")
             (when process-callback
               (funcall process-callback process))
             (loop :while (eq :running (sb-ext:process-status process))
                   :do (cond
                         ((and cancel-requested-p
                               (funcall cancel-requested-p))
                          (setf cancelled-p t)
                          (cleanup-process-group))
                         ((and deadline
                               (>= (get-internal-real-time) deadline))
                          (setf timed-out-p t)
                          (cleanup-process-group))
                         (t
                          (sleep *interactive-subprocess-poll-interval*))))
             ;; A normal session leader may have left background work.  Empty
             ;; the exact group before waiting for pipe EOF or clearing owner.
             (cleanup-process-group)
             (clear-process-owner)
             (multiple-value-bind (stdout stdout-truncated-p)
                 (settle-bounded-subprocess-output stdout-state)
               (multiple-value-bind (stderr stderr-truncated-p)
                   (settle-bounded-subprocess-output stderr-state)
                 (list
                  :command (if (stringp command)
                               command
                               (format nil "~{~A~^ ~}" command))
                  :directory (namestring working-directory)
                  :process-group-id process-group-id
                  :exit-code (and (not timed-out-p)
                                  (not cancelled-p)
                                  (ignore-errors
                                    (sb-ext:process-exit-code process)))
                  :timed-out-p timed-out-p
                  :cancelled-p cancelled-p
                  :stdout stdout
                  :stderr stderr
                  :stdout-truncated-p stdout-truncated-p
                  :stderr-truncated-p stderr-truncated-p))))
        (cleanup-process-group)
        ;; Closing from this owner is only a last-resort wake on constructor or
        ;; decoding failures.  Normal drain workers close their own streams.
        (when (and stdout-stream
                   (null (bounded-subprocess-output-state-thread stdout-state)))
          (ignore-errors (close stdout-stream :abort t)))
        (when (and stderr-stream
                   (null (bounded-subprocess-output-state-thread stderr-state)))
          (ignore-errors (close stderr-stream :abort t)))
        (when (bounded-subprocess-output-state-thread stdout-state)
          (ignore-errors (settle-bounded-subprocess-output stdout-state)))
        (when (bounded-subprocess-output-state-thread stderr-state)
          (ignore-errors (settle-bounded-subprocess-output stderr-state)))
        (clear-process-owner)))))

(defun start-interactive-buffer-operation
    (buf kind runner apply-function
     &key cancel-function payload (status :working) snapshot)
  "Start RUNNER on a detached snapshot and return without blocking CLIM.

RUNNER receives SNAPSHOT and the operation.  APPLY-FUNCTION receives live BUF,
the operation, RESULT, and ERROR after exact frame-process claim."
  (let* ((operation
           (make-interactive-buffer-operation
            :kind kind
            :payload (copy-runtime-owned-data payload)
            :runner runner
            :apply-function apply-function
            :cancel-function cancel-function))
         (start-gate
           (bt:make-semaphore :name "interactive-buffer-operation-start"))
         (start-error nil)
         (admission-refusal nil))
    (handler-case
        (call-with-runtime-admission
         (lambda ()
           (let ((generation
                     (bt:with-lock-held ((buffer-runtime-lock buf))
                       (unless
                           (interactive-buffer-operation-publication-allowed-p
                            buf)
                         (return-from start-interactive-buffer-operation nil))
                       (buffer-runtime-generation buf)))
                   (owned-buffer nil)
                   (worker nil)
                   (published-p nil))
               ;; Snapshotting is a frame-owned read and stays inside runtime
               ;; admission, but outside BUF's inner lock and all worker code.
               (handler-case
                   (setf owned-buffer
                         (or snapshot
                             (make-tool-execution-buffer-snapshot buf)))
                 (error (condition)
                   (setf start-error (format nil "~A" condition))))
               (unless start-error
                 (handler-case
                     (setf worker
                           (make-interactive-buffer-operation-worker
                            (lambda ()
                              (bt:wait-on-semaphore start-gate)
                              (let ((result nil)
                                    (error-text nil)
                                    (effects nil))
                                (flet ((record-tool-effect (kind effect)
                                         (push (cons kind effect) effects))
                                       (record-message-effect (effect)
                                         (push (cons :message effect) effects))
                                       (cancel-requested-p ()
                                         (interactive-buffer-operation-cancel-requested-p-safe
                                          operation))
                                       (stream-callback (stream)
                                         (publish-interactive-buffer-operation-stream
                                          operation stream))
                                       (process-callback (process)
                                         (publish-interactive-buffer-operation-process
                                          operation process)))
                                  (handler-case
                                      (unless (cancel-requested-p)
                                        (let ((*buffer-message-effect-recorder*
                                                #'record-message-effect)
                                              (*tool-effect-recorder*
                                                #'record-tool-effect)
                                              (*interactive-operation-cancel-requested-p*
                                                #'cancel-requested-p)
                                              (*interactive-operation-stream-state-callback*
                                                #'stream-callback)
                                              (*interactive-operation-process-callback*
                                                #'process-callback))
                                          (setf result
                                                (funcall runner
                                                         owned-buffer
                                                         operation))))
                                    (prompt-run-cancelled (condition)
                                      ;; PROMPT-RUN-CANCELLED deliberately
                                      ;; derives from CONDITION rather than
                                      ;; ERROR so provider loops can unwind
                                      ;; without being rewritten as failures.
                                      ;; Managed worker roots must still
                                      ;; contain it instead of invoking the
                                      ;; debugger in the background thread.
                                      (setf error-text
                                            (format nil "~A" condition)))
                                    (error (condition)
                                      (setf error-text
                                            (format nil "~A" condition))))
                                  (bt:with-lock-held
                                      ((interactive-buffer-operation-lock
                                        operation))
                                    (setf
                                     (interactive-buffer-operation-result
                                      operation)
                                     (copy-runtime-owned-data result)
                                     (interactive-buffer-operation-error
                                      operation)
                                     error-text
                                     (interactive-buffer-operation-effects
                                      operation)
                                     (nreverse effects)
                                     (interactive-buffer-operation-done-p
                                      operation)
                                     t))
                                  (wake-buffer-display-change
                                   buf :interactive-operation-complete))))
                            (format nil "rplaca-~(~A~)-~A"
                                    kind (buffer-name buf))))
                   (error (condition)
                     (setf start-error (format nil "~A" condition)))))
               (unless worker
                 (setf start-error
                       (or start-error
                           "Thread constructor returned no worker"))
                 (bt:with-lock-held
                     ((interactive-buffer-operation-lock operation))
                   (setf (interactive-buffer-operation-error operation)
                         start-error
                         (interactive-buffer-operation-done-p operation) t)))
               (bt:with-lock-held ((buffer-runtime-lock buf))
                 (let ((allowed-p
                         (handler-case
                             (interactive-buffer-operation-publication-allowed-p
                              buf)
                           (error (condition)
                             (setf start-error (format nil "~A" condition))
                             nil))))
                   (when (and allowed-p
                              (= generation (buffer-runtime-generation buf)))
                     (setf
                      (interactive-buffer-operation-buffer-generation operation)
                      generation
                      (interactive-buffer-operation-buffer-snapshot operation)
                      owned-buffer
                      (interactive-buffer-operation-worker operation) worker
                      (buffer-pending-interactive-operation buf) operation
                      (buffer-status buf) status
                      published-p t))))
               (unless published-p
                 ;; The worker has not crossed START-GATE.  Cancel, release it,
                 ;; and join its short no-run path before losing the handle.
                 (cancel-interactive-buffer-operation operation)
                 (bt:signal-semaphore start-gate)
                 (when worker
                   (ignore-errors (bt:join-thread worker)))
                 (return-from start-interactive-buffer-operation nil))
               (bt:signal-semaphore start-gate))
           operation)
         :operation (format nil "interactive ~(~A~) for ~A"
                            kind (buffer-name buf)))
      (runtime-admission-closed (condition)
        (setf admission-refusal (format nil "~A" condition))))
    (when start-error
      (file-debug-event
       "runtime-interactive-operation-worker-start-error"
       :buffer-name (buffer-name buf)
       :kind kind
       :condition start-error))
    (if admission-refusal
        (values nil admission-refusal)
        (progn
          (notify-buffer-display-change buf :interactive-operation-started)
          operation))))

(defun claim-buffer-runtime-application (buf kind subject &optional auxiliary)
  "Atomically detach SUBJECT from BUF and publish a unique application token."
  (bt:with-lock-held ((buffer-runtime-lock buf))
    (when (or (buffer-runtime-stopping-p buf)
              (buffer-disposing-p buf)
              (buffer-disposed-p buf)
              (buffer-runtime-application buf))
      (return-from claim-buffer-runtime-application nil))
    (ecase kind
      (:stream
       (unless (and (eq subject (buffer-pending-stream buf))
                    (eq auxiliary (buffer-streaming-message buf)))
         (return-from claim-buffer-runtime-application nil))
       (setf (buffer-pending-stream buf) nil
             (buffer-streaming-message buf) nil))
      (:tool
       (unless (and (not (buffer-runtime-tool-cancellation-p buf))
                    (eq subject (buffer-pending-tool-execution buf)))
         (return-from claim-buffer-runtime-application nil))
       (setf (buffer-pending-tool-execution buf) nil))
      (:operation
       (unless (eq subject (buffer-pending-interactive-operation buf))
         (return-from claim-buffer-runtime-application nil))
       (setf (buffer-pending-interactive-operation buf) nil)))
    (let ((context
            (make-buffer-runtime-application
             :token (cons kind (gensym "APPLICATION-"))
             :owner (bt:current-thread)
             :generation (buffer-runtime-generation buf)
             :kind kind
             :subject subject
             :auxiliary auxiliary)))
      (setf (buffer-runtime-application buf) context)
      context)))

(declaim (ftype (function (buffer) t) maybe-finish-buffer-runtime-teardown))

(defun release-buffer-runtime-application (buf context)
  "Release exact application CONTEXT and wake/finalize a waiting teardown."
  (let ((teardown-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (when (eq context (buffer-runtime-application buf))
        (setf (buffer-runtime-application buf) nil
              teardown-p (not (null (buffer-runtime-teardown buf))))
        (bt:condition-notify (buffer-runtime-condition buf))))
    (when teardown-p
      (maybe-finish-buffer-runtime-teardown buf)))
  context)

(defun frame-process-tool-p (tool-name)
  "Return true when TOOL-NAME is a semantic UI adapter, not background work.

The Quaestor input tool deliberately transfers control with a catch/throw into
its frame-process advice.  It does not perform blocking I/O and must stay at
the CLIM command boundary; ordinary tools run in managed workers."
  (string= (normalize-tool-name tool-name) "request_user_input"))

(defun interactive-tool-execution-snapshot (state)
  "Return an immutable snapshot of interactive tool worker STATE."
  (bt:with-lock-held ((interactive-tool-execution-lock state))
    (list :done-p (interactive-tool-execution-done-p state)
          :cancel-requested-p
          (interactive-tool-execution-cancel-requested-p state)
          :result-text (interactive-tool-execution-result-text state)
          :error (interactive-tool-execution-error state)
          :effects (copy-list (interactive-tool-execution-effects state)))))

(defun cancel-interactive-tool-execution (state)
  "Request cooperative cancellation of STATE.

Arbitrary in-process Lisp tool functions cannot be interrupted safely.  This
prevents a not-yet-started call and makes its eventual result inapplicable; a
tool already executing must return through its own cancellation mechanism."
  (let ((cancel-function nil))
    (when state
      (bt:with-lock-held ((interactive-tool-execution-lock state))
        (setf (interactive-tool-execution-cancel-requested-p state) t
              cancel-function (interactive-tool-execution-cancel-function state)
              (interactive-tool-execution-cancel-function state) nil))
      (when cancel-function
        (handler-case
            (funcall cancel-function)
          (error (condition)
            (file-debug-event
             "runtime-tool-cancellation-error"
             :tool-name (interactive-tool-execution-tool-name state)
             :condition (format nil "~A" condition))))))
    state))

(defun make-interactive-tool-worker-thread
    (function name &key initial-bindings)
  "Create one managed interactive tool worker.

Keeping construction behind this boundary makes resource-exhaustion failure
testable without replacing Bordeaux Threads process-wide."
  (bt:make-thread function
                  :name name
                  :initial-bindings initial-bindings))

(defun interactive-tool-publication-allowed-p (buf)
  "Validate BUF for a managed tool publication under its runtime lock."
  (let ((application *buffer-runtime-application-context*))
    (cond
      (application
       (unless (and (eq application (buffer-runtime-application buf))
                    (= (buffer-runtime-application-generation application)
                       (buffer-runtime-generation buf)))
         (return-from interactive-tool-publication-allowed-p nil)))
      ((buffer-runtime-application buf)
       (error "Buffer ~A is applying a terminal runtime result"
              (buffer-name buf)))))
  (when (or (buffer-disposed-p buf)
            (buffer-disposing-p buf)
            (buffer-runtime-stopping-p buf)
            (buffer-runtime-stopped-notification-p buf))
    (if *buffer-runtime-application-context*
        (return-from interactive-tool-publication-allowed-p nil)
        (error "Cannot execute a tool while buffer ~A is stopping"
               (buffer-name buf))))
  (when (or (buffer-pending-tool-execution buf)
            (buffer-pending-interactive-operation buf)
            (buffer-pending-stream buf)
            (buffer-runtime-start-generation buf))
    (error "Buffer ~A already has a runtime operation" (buffer-name buf)))
  t)

(defun start-interactive-tool-execution (buf tool-name tool-input tool-id)
  "Reserve one managed tool invocation and return to the frame process.

Background tools receive a detached buffer and publish immutable effects.
Frame-owned tools publish a pending state but do not execute until the CLIM
updater claims it.  A safe-reload admission refusal is returned as a second
value and never unwinds the command loop."
  (let* ((owned-tool-input (copy-runtime-owned-data tool-input))
         (agent-keyword
           (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (execution
           (interactive-tool-execution-policy tool-name owned-tool-input))
         (state
           (make-interactive-tool-execution
            :tool-name (copy-seq tool-name)
            :tool-input owned-tool-input
            :tool-id (if (stringp tool-id) (copy-seq tool-id) tool-id)
            :agent-keyword agent-keyword
            :execution execution
            :buffer-generation 0))
         (start-gate (bt:make-lock "interactive-tool-start"))
         (worker-start-error nil)
         (admission-refusal nil))
    (when (eq execution :command-only)
      (return-from start-interactive-tool-execution
        (values nil
                (format nil
                        "Tool ~A is available only through its trusted user command; provider-driven ~:(~A~) execution is refused."
                        tool-name execution))))
    (handler-case
        (call-with-runtime-admission
         (lambda ()
           ;; The worker waits on START-GATE.  While holding that gate, reserve
           ;; the buffer and publish the actual thread handle under its runtime
           ;; lock.  Safe reload cannot claim admission between those steps.
           (bt:with-lock-held (start-gate)
             (let ((worker nil)
                   (tool-buffer nil)
                   (execution-plan nil)
                   (publication-generation
                     (bt:with-lock-held ((buffer-runtime-lock buf))
                       (unless (interactive-tool-publication-allowed-p buf)
                         (return-from start-interactive-tool-execution nil))
                       (buffer-runtime-generation buf))))
               ;; Extension policy and veto-capable before hooks belong to the
               ;; frame process.  Run them outside BUF's inner runtime lock,
               ;; but while safe-reload admission remains closed around this
               ;; entire publication transaction.
               (when (eq execution :background)
                 (handler-case
                     (let ((*current-caller* agent-keyword))
                       (setf execution-plan
                             (capture-tool-execution-plan tool-name buf))
                       (preflight-background-tool-execution
                        tool-name owned-tool-input buf agent-keyword
                        execution-plan)
                       ;; A legitimate before hook may adjust frame-owned
                       ;; context.  Snapshot only after that hook succeeds.
                       (setf tool-buffer
                             (make-tool-execution-buffer-snapshot buf)))
                   (error (condition)
                     (setf worker-start-error (format nil "~A" condition)))))
               (bt:with-lock-held ((buffer-runtime-lock buf))
                 (unless (interactive-tool-publication-allowed-p buf)
                   (return-from start-interactive-tool-execution nil))
                 (unless (= publication-generation
                            (buffer-runtime-generation buf))
                   (return-from start-interactive-tool-execution nil))
                 (setf (interactive-tool-execution-buffer-generation state)
                       (buffer-runtime-generation buf))
                 (cond
                   ((eq execution :frame)
                    (setf (buffer-pending-tool-execution buf) state
                          (buffer-status buf) :tool-running))
                   (t
                    (when (null worker-start-error)
                      (handler-case
                          (let ((media-thread-bindings
                                  ;; Bordeaux Threads does not inherit dynamic
                                  ;; bindings.  Generated-media providers use
                                  ;; these registries to publish, find, and
                                  ;; cancel their operation, so preserve the
                                  ;; application context explicitly at this
                                  ;; background-tool boundary.
                                  (list (cons '*media-provider-registry*
                                              *media-provider-registry*)
                                        (cons '*media-operation-registry*
                                              *media-operation-registry*)
                                        (cons '*media-provider-registry-lock*
                                              *media-provider-registry-lock*)
                                        (cons '*media-default-provider*
                                              *media-default-provider*))))
                            (setf worker
                                  (make-interactive-tool-worker-thread
                                  (lambda ()
                                    (bt:with-lock-held (start-gate))
                                    (let ((result-text nil)
                                          (condition-text nil)
                                          (effects nil))
                                      (flet ((record-tool-effect (kind effect)
                                               (push (cons kind effect) effects))
                                             (record-message-effect (effect)
                                               (push (cons :message effect)
                                                     effects)))
                                        (handler-case
                                            (unless
                                                (bt:with-lock-held
                                                    ((interactive-tool-execution-lock
                                                      state))
                                                  (interactive-tool-execution-cancel-requested-p
                                                   state))
                                              (let ((*current-caller*
                                                      agent-keyword)
                                                    (*current-tool-buffer*
                                                      tool-buffer)
                                                    (*current-tool-execution-plan*
                                                      execution-plan)
                                                    (*tool-execution-preflight-completed-p*
                                                      (not (null execution-plan)))
                                                    (*tool-effect-recorder*
                                                      #'record-tool-effect)
                                                    (*tool-cancellation-registration-function*
                                                      (lambda (cancel-function)
                                                        (let ((cancel-now-p nil))
                                                          (bt:with-lock-held
                                                              ((interactive-tool-execution-lock
                                                                state))
                                                            (setf cancel-now-p
                                                                  (interactive-tool-execution-cancel-requested-p
                                                                   state))
                                                            (unless cancel-now-p
                                                              (setf (interactive-tool-execution-cancel-function
                                                                     state)
                                                                    cancel-function)))
                                                          (when cancel-now-p
                                                            (funcall cancel-function))
                                                          cancel-now-p)))
                                                    (*buffer-message-effect-recorder*
                                                      #'record-message-effect))
                                                (setf result-text
                                                      (execute-tool-safely
                                                       tool-name owned-tool-input
                                                       :buffer tool-buffer
                                                       :tool-id
                                                       (interactive-tool-execution-tool-id
                                                        state)))))
                                          (error (condition)
                                            (setf condition-text
                                                  (format nil "~A" condition)
                                                  result-text
                                                  (tool-error-result-data
                                                   condition-text))))
                                        (bt:with-lock-held
                                            ((interactive-tool-execution-lock
                                              state))
                                          (setf
                                           (interactive-tool-execution-result-text
                                            state)
                                           result-text
                                           (interactive-tool-execution-error state)
                                           condition-text
                                           (interactive-tool-execution-effects state)
                                           (nreverse effects)
                                           (interactive-tool-execution-done-p state)
                                           t))
                                        (wake-buffer-display-change
                                         buf :tool-complete))))
                                  (format nil "rplaca-tool-~A" tool-name)
                                  ;; Caller-local notification suppression is
                                  ;; not portably inherited by Bordeaux Threads.
                                  :initial-bindings
                                  (append media-thread-bindings
                                          (acons '*suppress-chat-redisplay-requests*
                                                 nil
                                                 bt:*default-special-bindings*)))))
                        (error (condition)
                          (setf worker-start-error
                                (format nil "~A" condition)))))
                    (unless worker
                      (setf worker-start-error
                            (or worker-start-error
                                "Thread constructor returned no worker"))
                       ;; Constructor/preparation failure is still applied as
                       ;; the exact provider tool result at the normal boundary.
                       (bt:with-lock-held
                           ((interactive-tool-execution-lock state))
                         (setf
                          (interactive-tool-execution-result-text state)
                          (tool-error-result-data worker-start-error)
                          (interactive-tool-execution-error state)
                          worker-start-error
                          (interactive-tool-execution-done-p state) t)))
                    (setf
                     (interactive-tool-execution-tool-buffer-snapshot state)
                     tool-buffer
                     (interactive-tool-execution-execution-plan state)
                     execution-plan
                     (interactive-tool-execution-worker state) worker
                     (buffer-pending-tool-execution buf) state
                     (buffer-status buf) :tool-running))))))
           state)
         :operation (format nil "interactive tool ~A" tool-name))
      (runtime-admission-closed (condition)
        (setf admission-refusal (format nil "~A" condition))))
    (when worker-start-error
      (file-debug-event
       "runtime-tool-worker-start-error"
       :buffer-name (buffer-name buf)
       :tool-name tool-name
       :condition worker-start-error))
    (if admission-refusal
        (values nil admission-refusal)
        (progn
          (notify-buffer-display-change buf :tool-started)
          state))))

(defun interactive-tool-result-display (state result-text)
  "Return the provider-visible display text for completed STATE."
  (format-tool-result-display
   (interactive-tool-execution-tool-name state)
   result-text))

(defun execute-frame-owned-interactive-tool (buf state)
  "Execute pending frame-owned STATE at the claimed CLIM update boundary."
  (let ((result-text nil)
        (condition-text nil))
    (handler-case
        (let ((*current-caller*
                (interactive-tool-execution-agent-keyword state))
              (*current-tool-buffer* buf))
          (setf result-text
                (execute-tool-safely
                 (interactive-tool-execution-tool-name state)
                 (interactive-tool-execution-tool-input state)
                 :buffer buf
                 :tool-id (interactive-tool-execution-tool-id state))))
      (error (condition)
        (setf condition-text (format nil "~A" condition)
              result-text (tool-error-result-data condition-text))))
    (bt:with-lock-held ((interactive-tool-execution-lock state))
      (setf (interactive-tool-execution-result-text state) result-text
            (interactive-tool-execution-error state) condition-text
            (interactive-tool-execution-done-p state) t))
    (interactive-tool-execution-snapshot state)))

(defun apply-interactive-tool-effect (buf effect)
  "Apply one immutable background tool EFFECT on the frame process."
  (ecase (car effect)
    (:message
     (apply-buffer-message-insertion-effect buf (cdr effect)))
    (:journal
     (record-tool-execution-event
      buf (copy-runtime-owned-data (cdr effect))))
    (:project-buffer
     (apply-project-buffer-effect (cdr effect)))
    (:hook
     (let ((hook-effect (cdr effect)))
       (ecase (tool-hook-effect-phase hook-effect)
         (:before
          (run-hook-with-args
           '*before-tool-hook*
           (tool-hook-effect-tool-name hook-effect)
           (copy-runtime-owned-data (tool-hook-effect-args hook-effect))))
         (:after
          (run-hook-with-args
           '*after-tool-hook*
           (tool-hook-effect-tool-name hook-effect)
           (copy-runtime-owned-data (tool-hook-effect-args hook-effect))
           (copy-runtime-owned-data
            (tool-hook-effect-result hook-effect)))))))))

(defun apply-interactive-tool-effects (buf effects)
  "Apply worker EFFECTS in order, containing extension hook failures."
  (dolist (effect effects)
    (handler-case
        (apply-interactive-tool-effect buf effect)
      (error (condition)
        (file-debug-event
         "runtime-tool-effect-error"
         :buffer-name (buffer-name buf)
         :effect-kind (car effect)
         :condition (format nil "~A" condition)))))
  buf)

(defun release-interactive-tool-execution-payload (state)
  "Release worker-only heavy references after terminal application."
  (bt:with-lock-held ((interactive-tool-execution-lock state))
    (setf (interactive-tool-execution-tool-buffer-snapshot state) nil
          (interactive-tool-execution-execution-plan state) nil
          (interactive-tool-execution-effects state) nil))
  state)

(defun ensure-interactive-tool-settlement-waiter (buf state worker)
  "Ensure a post-WORKER-exit notifier exists for pending STATE."
  (let ((notify (or *runtime-settlement-notify-function*
                    #'wake-buffer-display-change)))
    (bt:with-lock-held ((interactive-tool-execution-lock state))
      (unless (interactive-tool-execution-cancel-requested-p state)
        (or (interactive-tool-execution-settlement-waiter state)
          (handler-case
              (progn
                (setf (interactive-tool-execution-settlement-waiter-done-p
                       state)
                      nil)
                (setf
                 (interactive-tool-execution-settlement-waiter state)
                 (bt:make-thread
                  (lambda ()
                    (unwind-protect
                         (progn
                           (ignore-errors (bt:join-thread worker))
                           ;; This notifier has ordinary thread bindings, so
                           ;; the private CLIM wake is not swallowed by
                           ;; updater-local redisplay suppression.  Public
                           ;; observers run only after the frame applies the
                           ;; completed state.
                           (call-runtime-settlement-notify-safely
                            notify buf :tool-settled
                            "runtime-tool-settlement-waiter-error"))
                      (bt:with-lock-held
                          ((interactive-tool-execution-lock state))
                        (when
                            (or
                             (null
                              (interactive-tool-execution-settlement-waiter
                               state))
                             (eq
                              (bt:current-thread)
                              (interactive-tool-execution-settlement-waiter
                               state)))
                          (setf
                           (interactive-tool-execution-settlement-waiter-done-p
                            state)
                           t)))))
                  :name "rplaca-tool-settlement"
                  :initial-bindings
                  (acons '*suppress-chat-redisplay-requests*
                         nil
                         bt:*default-special-bindings*))))
            (error (condition)
              (file-debug-event
               "runtime-tool-settlement-waiter-error"
               :buffer-name (buffer-name buf)
               :condition (format nil "~A" condition))
              nil)))))))

(defun interactive-tool-settlement-waiter-snapshot (state)
  "Return STATE's settlement waiter under its ownership lock."
  (and state
       (bt:with-lock-held ((interactive-tool-execution-lock state))
         (interactive-tool-execution-settlement-waiter state))))

(defun interactive-tool-execution-worker-settled-p (buf state)
  "Return true once STATE's worker exits.

The settlement waiter is notification-only and is joined only when dead."
  (let ((worker (interactive-tool-execution-worker state)))
    (cond
      ((null worker) t)
      ((bt:thread-alive-p worker)
       (ensure-interactive-tool-settlement-waiter buf state worker)
       nil)
      (t
       (ignore-errors (bt:join-thread worker))
       (multiple-value-bind (waiter waiter-done-p)
           (bt:with-lock-held ((interactive-tool-execution-lock state))
             (values
              (interactive-tool-execution-settlement-waiter state)
              (interactive-tool-execution-settlement-waiter-done-p state)))
         (declare (ignore waiter-done-p))
         (when (and waiter (not (bt:thread-alive-p waiter)))
           (ignore-errors (bt:join-thread waiter))))
       t))))

(defun update-interactive-tool-execution-under-admission (buf)
  "Apply a managed tool invocation to BUF in the frame process.

Background workers publish only result state plus immutable effects. Frame-owned
tools execute only after this updater claims the exact runtime application.
All live buffer mutation, continuation, and redisplay therefore remain owned by
CLIM."
  (let ((state
          (bt:with-lock-held ((buffer-runtime-lock buf))
            (unless (buffer-runtime-tool-cancellation-p buf)
              (buffer-pending-tool-execution buf)))))
    (unless state
      (return-from update-interactive-tool-execution-under-admission nil))
    (let ((snapshot (interactive-tool-execution-snapshot state)))
      (unless (or (getf snapshot :done-p)
                  (eq (interactive-tool-execution-execution state) :frame))
        (return-from update-interactive-tool-execution-under-admission nil))
      ;; DONE is published before a worker unwinds its dynamic bindings and
      ;; cleanup.  Do not detach application ownership until the thread itself
      ;; is dead; requeue a CLIM display wakeup to close the tiny final race.
      (when (and (eq (interactive-tool-execution-execution state) :background)
                 (not (interactive-tool-execution-worker-settled-p buf state)))
        (return-from update-interactive-tool-execution-under-admission nil))
      ;; Claim, detach, and publish :APPLYING in one runtime-lock transaction.
      ;; Teardown must now wait for this exact context (or leave BUF stopping)
      ;; instead of mistaking the detached worker for a settled runtime.
      (let ((context
              (call-with-runtime-settlement-admission
               (lambda ()
                 ;; Publish exact application ownership while reload
                 ;; admission is closed.  The published context then remains
                 ;; visible throughout effect application and any continuation.
                 (claim-buffer-runtime-application buf :tool state))
               :operation (format nil "interactive tool claim for ~A"
                                  (buffer-name buf)))))
        (unless context
          (return-from update-interactive-tool-execution-under-admission nil))
        (let ((*buffer-runtime-application-context* context))
          (unwind-protect
               (handler-case
                   (progn
                     ;; Cancellation/generation mismatch is checked before a
                     ;; frame-owned tool gets any chance to execute.
                     (when (and (eq (interactive-tool-execution-execution state)
                                    :frame)
                                (not (getf snapshot :cancel-requested-p))
                                (= (interactive-tool-execution-buffer-generation
                                    state)
                                   (buffer-runtime-application-generation
                                    context))
                                (buffer-runtime-application-valid-p buf context))
                       (setf snapshot
                             (execute-frame-owned-interactive-tool buf state)))
                     (if (or (getf snapshot :cancel-requested-p)
                             (/= (interactive-tool-execution-buffer-generation
                                  state)
                                 (buffer-runtime-application-generation
                                  context))
                             (not (buffer-runtime-application-valid-p
                                   buf context)))
                         (progn
                           ;; Deferred worker effects are intentionally dropped
                           ;; once cancellation or teardown invalidates owner.
                           (finalize-cancelled-tool-queue buf)
                           (setf (buffer-status buf) :idle)
                           (notify-buffer-display-change buf :tool-cancelled))
                         (progn
                           (when (eq
                                  (interactive-tool-execution-execution state)
                                  :background)
                             (apply-interactive-tool-effects
                              buf (getf snapshot :effects)))
                           (let ((result-text
                                   (or (getf snapshot :result-text)
                                       (tool-error-result-data
                                        (or (getf snapshot :error)
                                            "Tool worker returned no result")))))
                             (push
                              `((:result . ,result-text)
                                (:display
                                 . ,(interactive-tool-result-display
                                     state result-text))
                                (:tool-id
                                 . ,(interactive-tool-execution-tool-id state)))
                              (buffer-tool-call-results buf)))
                           (pop (buffer-pending-tool-calls buf))
                           (notify-buffer-display-change buf :tool-complete)
                           (when (buffer-runtime-application-valid-p buf context)
                             (advance-tool-calls buf)))))
                 (error (condition)
                   (finalize-cancelled-tool-queue buf)
                   (setf (buffer-status buf) :error)
                   (notify-buffer-display-change buf :tool-error)
                   (error condition)))
            (release-interactive-tool-execution-payload state)
            (call-with-runtime-settlement-admission
             (lambda ()
               (release-buffer-runtime-application buf context))
             :operation (format nil "interactive tool release for ~A"
                                (buffer-name buf)))))
        t))))

(defun update-interactive-tool-execution (buf)
  "Apply/settle BUF while exact runtime ownership remains observable.

Admission is closed atomically for the claim and release transitions.  The
application context between them prevents safe reload, while releasing the
outer admission lock permits a continuation to reserve its next provider/tool
owner without recursively acquiring that lock."
  (update-interactive-tool-execution-under-admission buf))

(defun release-interactive-buffer-operation-payload (operation)
  "Release worker-only references after OPERATION reaches a frame boundary."
  (bt:with-lock-held ((interactive-buffer-operation-lock operation))
    (setf (interactive-buffer-operation-buffer-snapshot operation) nil
          (interactive-buffer-operation-runner operation) nil
          (interactive-buffer-operation-apply-function operation) nil
          (interactive-buffer-operation-cancel-function operation) nil
          (interactive-buffer-operation-effects operation) nil
          (interactive-buffer-operation-current-stream operation) nil
          (interactive-buffer-operation-process operation) nil))
  operation)

(defun ensure-interactive-buffer-operation-settlement-waiter
    (buf operation worker)
  "Ensure one post-WORKER-exit CLIM wakeup for OPERATION."
  (let ((notify (or *runtime-settlement-notify-function*
                    #'wake-buffer-display-change)))
    (bt:with-lock-held ((interactive-buffer-operation-lock operation))
      (unless (interactive-buffer-operation-cancel-requested-p operation)
        (or (interactive-buffer-operation-settlement-waiter operation)
          (handler-case
              (progn
                (setf
                 (interactive-buffer-operation-settlement-waiter-done-p
                  operation)
                 nil)
                (setf
                 (interactive-buffer-operation-settlement-waiter operation)
                 (bt:make-thread
                  (lambda ()
                    (unwind-protect
                         (progn
                           (ignore-errors (bt:join-thread worker))
                           (call-runtime-settlement-notify-safely
                            notify buf :interactive-operation-settled
                            "runtime-interactive-operation-settlement-waiter-error"))
                      (bt:with-lock-held
                          ((interactive-buffer-operation-lock operation))
                        (when
                            (or
                             (null
                              (interactive-buffer-operation-settlement-waiter
                               operation))
                             (eq
                              (bt:current-thread)
                              (interactive-buffer-operation-settlement-waiter
                               operation)))
                          (setf
                           (interactive-buffer-operation-settlement-waiter-done-p
                            operation)
                           t)))))
                  :name "rplaca-interactive-operation-settlement"
                  :initial-bindings
                  (acons '*suppress-chat-redisplay-requests*
                         nil
                         bt:*default-special-bindings*))))
            (error (condition)
              (file-debug-event
               "runtime-interactive-operation-settlement-waiter-error"
               :buffer-name (buffer-name buf)
               :condition (format nil "~A" condition))
              nil)))))))

(defun interactive-buffer-operation-settlement-waiter-snapshot (operation)
  "Return OPERATION's settlement waiter under its ownership lock."
  (and operation
       (bt:with-lock-held ((interactive-buffer-operation-lock operation))
         (interactive-buffer-operation-settlement-waiter operation))))

(defun interactive-buffer-operation-worker-settled-p
    (buf operation)
  "Return true once OPERATION's worker exits.

The settlement waiter is notification-only and is joined only when dead."
  (let ((worker (interactive-buffer-operation-worker operation)))
    (cond
      ((null worker) t)
      ((bt:thread-alive-p worker)
       (ensure-interactive-buffer-operation-settlement-waiter
        buf operation worker)
       nil)
      (t
       (ignore-errors (bt:join-thread worker))
         (multiple-value-bind (waiter waiter-done-p)
           (bt:with-lock-held
               ((interactive-buffer-operation-lock operation))
             (values
              (interactive-buffer-operation-settlement-waiter operation)
              (interactive-buffer-operation-settlement-waiter-done-p
               operation)))
         (declare (ignore waiter-done-p))
         (when (and waiter
                    (not (eq waiter (bt:current-thread)))
                    (not (bt:thread-alive-p waiter)))
           (ignore-errors (bt:join-thread waiter))))
       t))))

(defun update-interactive-buffer-operation (buf)
  "Apply BUF's completed shell/pipeline/compaction operation on the CLIM process.

Terminal result publication is intentionally distinct from worker settlement.
The first update creates a short waiter and returns; only a later wakeup can
claim and apply the operation."
  (let ((operation
          (bt:with-lock-held ((buffer-runtime-lock buf))
            (buffer-pending-interactive-operation buf))))
    (unless operation
      (return-from update-interactive-buffer-operation nil))
    (let ((snapshot (interactive-buffer-operation-snapshot operation)))
      (unless (getf snapshot :done-p)
        (return-from update-interactive-buffer-operation nil))
      (unless (interactive-buffer-operation-worker-settled-p buf operation)
        (return-from update-interactive-buffer-operation nil))
      (let ((context
              (call-with-runtime-settlement-admission
               (lambda ()
                 (claim-buffer-runtime-application
                  buf :operation operation))
               :operation (format nil "interactive operation claim for ~A"
                                  (buffer-name buf)))))
        (unless context
          (return-from update-interactive-buffer-operation nil))
        (let ((*buffer-runtime-application-context* context))
          (unwind-protect
               (let ((cancelled-p
                       (or (getf snapshot :cancel-requested-p)
                           (/= (interactive-buffer-operation-buffer-generation
                                operation)
                               (buffer-runtime-application-generation context))
                           (not (buffer-runtime-application-valid-p
                                 buf context)))))
                 (cond
                   (cancelled-p
                    ;; Late worker effects and continuation are never applied
                    ;; after Stop/teardown invalidates the generation.
                    (setf (buffer-status buf) :idle)
                    (let ((cancel-function
                            (interactive-buffer-operation-cancel-function
                             operation)))
                      (when cancel-function
                        (handler-case
                            (funcall cancel-function buf operation)
                          (error (condition)
                            (setf (buffer-status buf) :error)
                            (file-debug-event
                             "runtime-interactive-operation-cancel-apply-error"
                             :buffer-name (buffer-name buf)
                             :kind
                             (interactive-buffer-operation-kind operation)
                             :condition (format nil "~A" condition))
                            (handler-case
                                (buffer-insert-system-message
                                 buf
                                 (format nil
                                         "[Cancellation callback failed: ~A]"
                                         condition))
                              (error () nil))))))
                   (notify-buffer-display-change
                     buf :interactive-operation-cancelled))
                   (t
                    (handler-case
                        (progn
                          (apply-interactive-tool-effects
                           buf (getf snapshot :effects))
                          (let ((apply-function
                                  (interactive-buffer-operation-apply-function
                                   operation)))
                            (when apply-function
                              (funcall apply-function
                                       buf
                                       operation
                                       (getf snapshot :result)
                                       (getf snapshot :error)))))
                      (error (condition)
                        (setf (buffer-status buf) :error)
                        (file-debug-event
                         "runtime-interactive-operation-apply-error"
                         :buffer-name (buffer-name buf)
                         :kind (interactive-buffer-operation-kind operation)
                         :condition (format nil "~A" condition))
                        (handler-case
                            (buffer-insert-system-message
                             buf
                             (format nil
                                     "[~:(~A~) failed while applying: ~A]"
                                     (interactive-buffer-operation-kind
                                      operation)
                                     condition))
                          (error () nil))))
                    (notify-buffer-display-change
                     buf :interactive-operation-applied))))
            (release-interactive-buffer-operation-payload operation)
            (call-with-runtime-settlement-admission
             (lambda ()
               (release-buffer-runtime-application buf context))
             :operation (format nil "interactive operation release for ~A"
                                (buffer-name buf)))))
        t))))

(defparameter *runtime-worker-settlement-timeout-seconds* 0.25
  "Maximum teardown wait for one owned worker before reporting it as detached.")

(defun cancelled-tool-result-entry (tool-use)
  "Return a protocol-completing cancellation result for TOOL-USE."
  (let ((tool-name (or (cdr (assoc :name tool-use)) "tool")))
    `((:result . ,(tool-error-result-data "Cancelled by user"))
      (:display . ,(format nil "[~A CANCELLED]" tool-name))
      (:tool-id . ,(cdr (assoc :id tool-use))))))

(defun finalize-cancelled-tool-queue
    (buf &key (run-hook-p t) (notify-p t))
  "Record results for every unresolved tool call without continuing the run.

Provider protocols require one tool_result for each assistant tool_use.  Stop
and teardown therefore preserve already-completed results and synthesize
cancellation results for the pending suffix before clearing runtime state."
  (let ((results
          (append (nreverse (buffer-tool-call-results buf))
                  (mapcar #'cancelled-tool-result-entry
                          (buffer-pending-tool-calls buf)))))
    ;; Retire ownership before message insertion or extension hooks run.  A
    ;; failing or reentrant hook must not expose the same protocol results to a
    ;; second teardown delivery.
    (setf (buffer-tool-call-results buf) nil
          (buffer-pending-tool-calls buf) nil)
    (when results
      (insert-tool-results-message
       buf results :run-hook-p run-hook-p :notify-p notify-p))
    results))

(defun wait-for-owned-worker-settlement
    (thread label &key
                    (timeout *runtime-worker-settlement-timeout-seconds*))
  "Boundedly join THREAD after cancellation and return true when it exited."
  (when (or (null thread)
            (eq thread (bt:current-thread)))
    (return-from wait-for-owned-worker-settlement t))
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout
                               internal-time-units-per-second)))))
    (loop :while (and (bt:thread-alive-p thread)
                      (< (get-internal-real-time) deadline))
          :do (sleep 0.005))
    (if (bt:thread-alive-p thread)
        (progn
          (file-debug-event "runtime-worker-settlement-timeout"
                            :worker label
                            :timeout timeout)
          nil)
        (progn
          (bt:join-thread thread)
          t))))

(defun stream-state-reader-thread-snapshot (state)
  "Return STATE's current reader thread under its ownership lock."
  (and state
       (bt:with-lock-held ((stream-state-lock state))
         (stream-state-reader-thread state))))

(defun settle-stream-state-reader (state)
  "Join STATE's exact reader and clear its retained handles.

Return true when no reader exists or the non-current reader was joined.  A
reader callback cannot join itself and returns NIL, leaving ownership visible
for the next frame/prompt boundary.  Its notification-only waiter is reaped
only when already dead."
  (let ((worker (stream-state-reader-thread-snapshot state)))
    (cond
      ((null worker) t)
      ((eq worker (bt:current-thread)) nil)
      (t
       (bt:join-thread worker)
       (let ((waiter
               (bt:with-lock-held ((stream-state-lock state))
                 (stream-state-reader-settlement-thread state))))
         (when (and waiter
                    (not (eq waiter (bt:current-thread)))
                    (not (bt:thread-alive-p waiter)))
           (bt:join-thread waiter)))
       (call-with-runtime-settlement-admission
        (lambda () (clear-stream-state-reader-thread state worker))
        :operation "provider reader handle settlement")
       t))))

(defun ensure-stream-state-reader-settlement-waiter (buf state worker)
  "Ensure a managed post-WORKER-exit private CLIM wake for STATE."
  (let ((notify (or *runtime-settlement-notify-function*
                    #'wake-buffer-display-change)))
    (bt:with-lock-held ((stream-state-lock state))
      (unless (or (stream-state-cancel-requested-p state)
                  (stream-state-cancelled-p state))
        (or (stream-state-reader-settlement-thread state)
          (handler-case
              (progn
                (setf (stream-state-reader-settlement-waiter-done-p state)
                      nil)
                (setf (stream-state-reader-settlement-thread state)
                      (bt:make-thread
                       (lambda ()
                         (unwind-protect
                              (progn
                                (ignore-errors (bt:join-thread worker))
                                ;; This ordinary worker binding makes the
                                ;; private CLIM wake visible even when the
                                ;; first frame update suppressed redisplay.
                                ;; Public observers remain frame-owned.
                                (call-runtime-settlement-notify-safely
                                 notify buf :stream-settled
                                 "runtime-stream-settlement-waiter-error"))
                           (bt:with-lock-held ((stream-state-lock state))
                             (when
                                 (or
                                  (null
                                   (stream-state-reader-settlement-thread state))
                                  (eq
                                   (bt:current-thread)
                                   (stream-state-reader-settlement-thread state)))
                               (setf
                                (stream-state-reader-settlement-waiter-done-p
                                 state)
                                t)))))
                       :name "rplaca-stream-settlement"
                       :initial-bindings
                       (acons '*suppress-chat-redisplay-requests*
                              nil
                              bt:*default-special-bindings*))))
            (error (condition)
              (file-debug-event
               "runtime-stream-settlement-waiter-error"
               :buffer-name (buffer-name buf)
               :condition (format nil "~A" condition))
              nil)))))))

(defun stream-state-reader-settlement-thread-snapshot (state)
  "Return STATE's reader settlement waiter under its ownership lock."
  (and state
       (bt:with-lock-held ((stream-state-lock state))
         (stream-state-reader-settlement-thread state))))

(defun stream-state-reader-worker-settled-p (buf state)
  "Return true once STATE's exact reader exits.

The settlement waiter is notification-only and is joined only when dead."
  (let ((worker (stream-state-reader-thread-snapshot state)))
    (cond
      ((null worker) t)
      ((bt:thread-alive-p worker)
       (ensure-stream-state-reader-settlement-waiter buf state worker)
       nil)
      (t
       (bt:join-thread worker)
       (multiple-value-bind (waiter waiter-done-p)
           (bt:with-lock-held ((stream-state-lock state))
             (values
              (stream-state-reader-settlement-thread state)
              (stream-state-reader-settlement-waiter-done-p state)))
         (declare (ignore waiter-done-p))
         (when (and waiter
                    (not (eq waiter (bt:current-thread)))
                    (not (bt:thread-alive-p waiter)))
           (bt:join-thread waiter)))
       (call-with-runtime-settlement-admission
        (lambda () (clear-stream-state-reader-thread state worker))
        :operation "provider reader frame settlement")
       t))))

(defun runtime-teardown-ready-p (buf teardown)
  "Return true, with BUF's runtime lock held, when TEARDOWN may finalize."
  (and (eq teardown (buffer-runtime-teardown buf))
       (not (buffer-runtime-teardown-initializing-p teardown))
       (or (not (buffer-runtime-teardown-frame-delivery-p teardown))
           ;; Disposal has no future frame delivery.  It may reclaim an exact
           ;; teardown that was already published to the former frame.
           (buffer-disposing-p buf))
       (not (buffer-runtime-teardown-finalizing-p teardown))
       (buffer-runtime-teardown-workers-settled-p teardown)
       (null (buffer-runtime-application buf))
       (null (buffer-runtime-start-owner buf))))

(defun maybe-finish-buffer-runtime-teardown (buf)
  "Settle exact runtime resources and publish frame-owned teardown delivery.

Managed reapers never apply visible buffer state.  A live buffer retains its
exact teardown and STOPPING blocker until `deliver-buffer-runtime-stopped-
notification' runs on the owning frame process.  Disposal has no frame and
therefore records protocol completion silently before releasing ownership."
  (let ((teardown nil)
        (disposing-p nil)
        (finalization-completed-p nil)
        (wake-frame-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (let ((candidate (buffer-runtime-teardown buf)))
        (when (and candidate (runtime-teardown-ready-p buf candidate))
          (setf (buffer-runtime-teardown-finalizing-p candidate) t
                (buffer-runtime-teardown-frame-delivery-p candidate) nil
                disposing-p (buffer-disposing-p buf)
                teardown candidate))))
    (unless teardown
      (return-from maybe-finish-buffer-runtime-teardown nil))
    (unwind-protect
         (progn
           ;; The reaper has joined every managed provider reader before
           ;; WORKERS-SETTLED-P becomes true.  Clear the exact state handles
           ;; here so teardown cannot report completion with dead retained
           ;; reader or settlement-thread references.
           (dolist (pair
                    (buffer-runtime-teardown-stream-pairs teardown))
             (handler-case
                 (settle-stream-state-reader (getf pair :state))
               (error (condition)
                 (file-debug-event
                  "runtime-stream-settlement-error"
                  :buffer-name (buffer-name buf)
                  :condition (format nil "~A" condition)))))
           (let ((oauth-flow
                   (buffer-runtime-teardown-oauth-flow teardown)))
             (when oauth-flow
               (handler-case
                   (join-openai-oauth-flow-worker oauth-flow)
                 (error (condition)
                   (file-debug-event
                    "runtime-oauth-settlement-error"
                    :buffer-name (buffer-name buf)
                    :condition (format nil "~A" condition))))))
           (dolist (tool-state
                    (buffer-runtime-teardown-tool-states teardown))
             (release-interactive-tool-execution-payload tool-state))
           (let ((frame-operation-cancellations
                   (and
                    (buffer-runtime-teardown-stop-p teardown)
                    (copy-list
                     (buffer-runtime-teardown-operation-cancellation-states
                      teardown)))))
           (dolist (operation
                    (buffer-runtime-teardown-operation-states teardown))
             (unless (member operation frame-operation-cancellations
                             :test #'eq)
               (release-interactive-buffer-operation-payload operation)))
             ;; Dead worker payloads are retired now.  Keep only the exact
             ;; frame-owned cancellation callbacks and pending stream messages.
             (setf (buffer-runtime-teardown-tool-states teardown) nil
                   (buffer-runtime-teardown-operation-states teardown)
                   frame-operation-cancellations
                   (buffer-runtime-teardown-oauth-flow teardown) nil
                   (buffer-runtime-teardown-workers teardown) nil)
             (when disposing-p
               ;; Retire exact deferred work before performing any persistence;
               ;; a retry can never record the same protocol result twice.
               (let ((stream-pairs
                       (prog1
                           (remove-if
                            (lambda (pair) (getf pair :application-p))
                            (buffer-runtime-teardown-stream-pairs teardown))
                         (setf
                          (buffer-runtime-teardown-stream-pairs teardown) nil)))
                     (operation-cancellations
                       (prog1 frame-operation-cancellations
                         (setf
                          (buffer-runtime-teardown-operation-cancellation-states
                           teardown)
                          nil))))
                 (dolist (pair stream-pairs)
                   (handler-case
                       (let ((state (getf pair :state))
                             (message (getf pair :message)))
                         (when (and state message)
                           (finalize-cancelled-streaming-response
                            buf state message :notify-p nil)))
                     (error (condition)
                       (file-debug-event
                        "runtime-stream-cleanup-error"
                        :buffer-name (buffer-name buf)
                        :condition (format nil "~A" condition)))))
                 (handler-case
                     (finalize-cancelled-tool-queue
                      buf :run-hook-p nil :notify-p nil)
                   (error (condition)
                     (file-debug-event
                      "runtime-tool-queue-cleanup-error"
                      :buffer-name (buffer-name buf)
                      :condition (format nil "~A" condition))))
                 (dolist (operation operation-cancellations)
                   (release-interactive-buffer-operation-payload operation))
                 (setf (buffer-user-input-pending buf) nil
                       (buffer-stashed-input buf) nil
                       (buffer-status buf) :idle)))
             (setf finalization-completed-p t)))
      (bt:with-lock-held ((buffer-runtime-lock buf))
        (when (eq teardown (buffer-runtime-teardown buf))
          (cond
            ((not finalization-completed-p)
             ;; Leave exact ownership retryable after an unexpected settlement
             ;; failure.  The private wake lets the frame retry, while a second
             ;; Stop/dispose call may also claim it.
             (setf (buffer-runtime-teardown-finalizing-p teardown) nil
                   (buffer-runtime-teardown-reaper-started-p teardown) nil
                   wake-frame-p t))
            (disposing-p
             (setf (buffer-runtime-teardown buf) nil
                   (buffer-runtime-stopping-p buf) nil
                   (buffer-runtime-tool-cancellation-p buf) nil
                   (buffer-runtime-stopped-notification-p buf) nil
                   (buffer-disposing-p buf) nil
                   (buffer-disposed-p buf) t))
            (t
             ;; Retain TEARDOWN and STOPPING through all frame-owned effects.
             ;; STOPPED-NOTIFICATION is an additional admission blocker for
             ;; reentrant hooks during delivery.
             (setf (buffer-runtime-teardown-finalizing-p teardown) nil
                   (buffer-runtime-teardown-frame-delivery-p teardown) t
                   (buffer-runtime-stopped-notification-p buf) t
                   wake-frame-p t)))
          (bt:condition-notify (buffer-runtime-condition buf))))
      ;; This is deliberately the private wake-only path.  The reaper may be a
      ;; managed worker; package hooks and all visible buffer mutation run only
      ;; after HANDLE-CHAT-FRAME-REDISPLAY claims the exact retained teardown.
      (when wake-frame-p
        (wake-buffer-display-change buf :runtime-stopped-pending)))
    t))

(defun deliver-buffer-runtime-stopped-notification (buf)
  "Apply BUF's exact completed teardown from the owning frame process."
  ;; A frame wake can also be the retry boundary for a transient settlement
  ;; failure that left the exact teardown ready but unclaimed.
  (maybe-finish-buffer-runtime-teardown buf)
  (let ((teardown nil)
        (simple-delivery-p nil)
        (completed-p nil)
        (released-p nil)
        (retry-wake-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (let ((candidate (buffer-runtime-teardown buf)))
        (cond
          ((and candidate
                (buffer-runtime-teardown-frame-delivery-p candidate)
                (buffer-runtime-stopped-notification-p buf)
                (not (buffer-runtime-teardown-finalizing-p candidate)))
           (setf (buffer-runtime-teardown-finalizing-p candidate) t
                 (buffer-runtime-teardown-frame-delivery-p candidate) nil
                 teardown candidate))
          ;; Compatibility for an already-published notification from an older
          ;; in-process definition.  There is no deferred payload in this case.
          ((and (buffer-runtime-stopped-notification-p buf)
                (not (buffer-runtime-stopping-p buf))
                (null candidate))
           (setf simple-delivery-p t)))))
    (when teardown
      (unwind-protect
           (let ((stream-pairs
                   (prog1
                       (buffer-runtime-teardown-stream-pairs teardown)
                     (setf (buffer-runtime-teardown-stream-pairs teardown) nil)))
                 (operation-cancellations
                   (prog1
                       (buffer-runtime-teardown-operation-cancellation-states
                        teardown)
                     (setf
                      (buffer-runtime-teardown-operation-cancellation-states
                       teardown)
                      nil))))
             (dolist (pair stream-pairs)
               (when (and (not (getf pair :application-p))
                          (getf pair :state)
                          (getf pair :message))
                 (handler-case
                     (finalize-cancelled-streaming-response
                      buf (getf pair :state) (getf pair :message))
                   (error (condition)
                     (file-debug-event
                      "runtime-stream-cleanup-error"
                      :buffer-name (buffer-name buf)
                      :condition (format nil "~A" condition))))))
             ;; Snapshot/clear happens inside this helper before any insertion
             ;; or extension hook can reenter.
             (handler-case
                 (finalize-cancelled-tool-queue buf)
               (error (condition)
                 (file-debug-event
                  "runtime-tool-queue-cleanup-error"
                  :buffer-name (buffer-name buf)
                  :condition (format nil "~A" condition))))
             (when (buffer-stashed-input buf)
               (set-message-text (buffer-input-message buf)
                                 (buffer-stashed-input buf)))
             (setf (buffer-user-input-pending buf) nil
                   (buffer-stashed-input buf) nil
                   (buffer-status buf) :idle)
             (dolist (operation operation-cancellations)
               (unwind-protect
                    (let ((cancel-function
                            (interactive-buffer-operation-cancel-function
                             operation)))
                      (when cancel-function
                        (handler-case
                            (funcall cancel-function buf operation)
                          (error (condition)
                            (setf (buffer-status buf) :error)
                            (file-debug-event
                             "runtime-interactive-operation-cancel-apply-error"
                             :buffer-name (buffer-name buf)
                             :kind
                             (interactive-buffer-operation-kind operation)
                             :condition (format nil "~A" condition))
                            (handler-case
                                (buffer-insert-system-message
                                 buf
                                 (format nil
                                         "[Cancellation callback failed: ~A]"
                                         condition))
                              (error () nil))))))
                 (release-interactive-buffer-operation-payload operation))
               (notify-buffer-display-change
                buf :interactive-operation-cancelled))
             (setf completed-p t))
        (bt:with-lock-held ((buffer-runtime-lock buf))
          (when (eq teardown (buffer-runtime-teardown buf))
            (if completed-p
                (setf (buffer-runtime-teardown buf) nil
                      (buffer-runtime-stopping-p buf) nil
                      (buffer-runtime-tool-cancellation-p buf) nil
                      (buffer-runtime-stopped-notification-p buf) nil
                      released-p t)
                (setf (buffer-runtime-teardown-finalizing-p teardown) nil
                      (buffer-runtime-teardown-frame-delivery-p teardown) t
                      (buffer-runtime-stopped-notification-p buf) t
                      retry-wake-p t))
            (when (and completed-p (buffer-disposing-p buf))
              (setf (buffer-disposing-p buf) nil
                    (buffer-disposed-p buf) t))
            (bt:condition-notify (buffer-runtime-condition buf))))
        (when retry-wake-p
          (wake-buffer-display-change buf :runtime-stopped-pending))))
    (when simple-delivery-p
      (bt:with-lock-held ((buffer-runtime-lock buf))
        (when (and (buffer-runtime-stopped-notification-p buf)
                   (not (buffer-runtime-stopping-p buf))
                   (null (buffer-runtime-teardown buf)))
          (setf (buffer-runtime-stopped-notification-p buf) nil
                released-p t))))
    ;; This is the intentional admission point.  Deferred insertion/callback
    ;; hooks observed all blockers above; queued follow-ups may start only now.
    (when released-p
      (notify-buffer-display-change buf :runtime-stopped))
    released-p))

(defun wait-for-buffer-runtime-teardown
    (buf teardown &key
                    (timeout *runtime-worker-settlement-timeout-seconds*))
  "Wait boundedly for exact TEARDOWN and return true when it completed."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (loop :while (and (eq teardown (buffer-runtime-teardown buf))
                        (< (get-internal-real-time) deadline))
            :for remaining :=
              (/ (- deadline (get-internal-real-time))
                 (float internal-time-units-per-second 1.0))
            :do (bt:condition-wait
                 (buffer-runtime-condition buf)
                 (buffer-runtime-lock buf)
                 :timeout (max 0.001 remaining)))
      (not (eq teardown (buffer-runtime-teardown buf))))))

(defvar *runtime-teardown-reaper-thread-constructor* #'bt:make-thread
  "Constructor used for retryable managed teardown reapers.")

(defun launch-buffer-runtime-teardown-reaper (buf teardown workers)
  "Join unsettled WORKERS off the caller, then make TEARDOWN finalizable."
  (handler-case
      (funcall
       *runtime-teardown-reaper-thread-constructor*
       (lambda ()
         (handler-case
             (progn
               (dolist (worker workers)
                 (when (and worker
                            (not (eq worker (bt:current-thread))))
                   (ignore-errors (bt:join-thread worker))))
               (bt:with-lock-held ((buffer-runtime-lock buf))
                 (when (eq teardown (buffer-runtime-teardown buf))
                   (setf (buffer-runtime-teardown-workers-settled-p teardown) t)
                   (bt:condition-notify (buffer-runtime-condition buf))))
               (maybe-finish-buffer-runtime-teardown buf))
           (error (condition)
             ;; Retain the exact teardown and STOPPING state.  A failed reaper
             ;; must never manufacture idle ownership after incomplete cleanup.
             (file-debug-event
              "runtime-worker-reaper-error"
              :buffer-name (buffer-name buf)
              :condition (format nil "~A" condition))
             (bt:with-lock-held ((buffer-runtime-lock buf))
               (when (eq teardown (buffer-runtime-teardown buf))
                 (setf (buffer-runtime-teardown-reaper-started-p teardown)
                       nil)))
             nil)))
       :name (format nil "rplaca-runtime-reaper-~A" (buffer-name buf)))
    (error (condition)
      ;; Retain STOPPING on spawn failure.  Safety wins over falsely exposing
      ;; the buffer as idle while an unjoined worker may still have effects.
      (file-debug-event
       "runtime-worker-reaper-error"
       :buffer-name (buffer-name buf)
       :condition (format nil "~A" condition))
      nil)))

(defun ensure-buffer-runtime-teardown-reaper (buf teardown)
  "Start TEARDOWN's exact reaper once, resetting admission on spawn failure."
  (let ((workers nil)
        (claimed-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (when (and (eq teardown (buffer-runtime-teardown buf))
                 (not (buffer-runtime-teardown-initializing-p teardown))
                 (not (buffer-runtime-teardown-workers-settled-p teardown))
                 (not (buffer-runtime-teardown-reaper-started-p teardown)))
        (setf workers (copy-list (buffer-runtime-teardown-workers teardown))
              (buffer-runtime-teardown-reaper-started-p teardown) t
              claimed-p t)))
    (when claimed-p
      (let ((reaper
              (launch-buffer-runtime-teardown-reaper buf teardown workers)))
        (unless reaper
          (bt:with-lock-held ((buffer-runtime-lock buf))
            (when (eq teardown (buffer-runtime-teardown buf))
              (setf (buffer-runtime-teardown-reaper-started-p teardown) nil))))
        reaper))))

(defun cancel-buffer-runtime-operations-internal
    (buf wait-p &key only-if-owned-p stop-p)
  "Single-flight cancellation of every runtime operation owned by BUF.

WAIT-P permits bounded worker and final teardown waits for disposal/reload.
When false, live owners are transferred directly to the managed reaper and the
caller returns after cancellation has been published.  ONLY-IF-OWNED-P avoids
creating an empty teardown for an idle Stop command.  The second return value
is true only when teardown is fully settled; the third says an owned runtime
was found."
  (let ((teardown nil)
        (winner-p nil)
        (application nil)
        (pending-stream nil)
        (pending-message nil)
        (pending-tool nil)
        (pending-operation nil)
        (start-owner nil)
        (deferred-workers-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      (setf teardown (buffer-runtime-teardown buf))
      (unless teardown
        (when (and only-if-owned-p
                   (null (buffer-runtime-application buf))
                   (null (buffer-pending-stream buf))
                   (null (buffer-pending-tool-execution buf))
                   (null (buffer-pending-interactive-operation buf))
                   (null (buffer-runtime-start-owner buf)))
          (return-from cancel-buffer-runtime-operations-internal
            (values buf t nil)))
        (setf winner-p t
              application (buffer-runtime-application buf)
              pending-stream (buffer-pending-stream buf)
              pending-message (buffer-streaming-message buf)
              pending-tool (buffer-pending-tool-execution buf)
              pending-operation
              (buffer-pending-interactive-operation buf)
              start-owner (buffer-runtime-start-owner buf)
              teardown
              (make-buffer-runtime-teardown
               :token (cons :teardown (gensym "TEARDOWN-"))
               :owner (bt:current-thread)
               :stop-p stop-p))
        (setf (buffer-runtime-teardown buf) teardown
              (buffer-runtime-stopping-p buf) t)
        (when stop-p
          (setf (buffer-status buf) :cancelling))
        (incf (buffer-runtime-generation buf))
        (setf (buffer-pending-stream buf) nil
              (buffer-streaming-message buf) nil
              (buffer-pending-tool-execution buf) nil
              (buffer-pending-interactive-operation buf) nil
              (buffer-runtime-start-generation buf) nil
              (buffer-runtime-tool-cancellation-p buf) nil)))
    (unless winner-p
      (ensure-buffer-runtime-teardown-reaper buf teardown)
      (maybe-finish-buffer-runtime-teardown buf)
      (return-from cancel-buffer-runtime-operations-internal
        (values
         buf
         (and wait-p
              (not (eq (buffer-runtime-teardown-owner teardown)
                       (bt:current-thread)))
              (wait-for-buffer-runtime-teardown buf teardown))
         t)))
    (let* ((application-stream-p
             (and application
                  (eq :stream
                      (buffer-runtime-application-kind application))))
           (application-tool-p
             (and application
                  (eq :tool
                      (buffer-runtime-application-kind application))))
           (application-operation-p
             (and application
                  (eq :operation
                      (buffer-runtime-application-kind application))))
           (stream-pairs
             (remove-duplicates
              (remove nil
                      (list
                       (and pending-stream
                            (list :state pending-stream
                                  :message pending-message
                                  :application-p nil))
                       (and application-stream-p
                            (list
                             :state
                             (buffer-runtime-application-subject application)
                             :message
                             (buffer-runtime-application-auxiliary application)
                             :application-p t))))
              :test (lambda (left right)
                      (eq (getf left :state) (getf right :state)))))
           (tool-states
             (remove-duplicates
              (remove nil
                      (list pending-tool
                            (and application-tool-p
                                 (buffer-runtime-application-subject
                                  application))))
              :test #'eq))
           (operation-states
             (remove-duplicates
              (remove nil
                      (list pending-operation
                            (and application-operation-p
                                 (buffer-runtime-application-subject
                                  application))))
              :test #'eq))
           (oauth-flow (claim-buffer-openai-oauth-pending-flow buf)))
      (setf (buffer-runtime-teardown-stream-pairs teardown) stream-pairs
            (buffer-runtime-teardown-tool-states teardown) tool-states
            (buffer-runtime-teardown-operation-states teardown)
            operation-states
            (buffer-runtime-teardown-operation-cancellation-states teardown)
            (and stop-p pending-operation (list pending-operation))
            (buffer-runtime-teardown-oauth-flow teardown) oauth-flow)
      (dolist (pair stream-pairs)
        (let ((stream (getf pair :state)))
          (handler-case
              ;; Cancellation is worker-safe state publication.  Visible
              ;; message finalization remains attached to the exact teardown
              ;; for frame delivery (or silent disposal).
              (cancel-stream-state stream)
            (error (condition)
              (file-debug-event
               "runtime-stream-cleanup-error"
               :buffer-name (buffer-name buf)
               :condition (format nil "~A" condition))))))
      (dolist (tool-state tool-states)
        (cancel-interactive-tool-execution tool-state))
      (dolist (operation-state operation-states)
        (cancel-interactive-buffer-operation operation-state))
      (when oauth-flow
        (handler-case
            (cancel-openai-codex-oauth-login oauth-flow)
          (error (condition)
            (file-debug-event
             "runtime-oauth-cleanup-error"
             :buffer-name (buffer-name buf)
             :condition (format nil "~A" condition)))))
      ;; Only primary workers own teardown.  Settlement waiters merely deliver
      ;; late idempotent wakeups; tracking them here would let a blocked event
      ;; backend keep the buffer in :STOPPING after all application work ended.
      (let* ((workers
               (remove-duplicates
                (remove nil
                        (append
                         (mapcar
                          (lambda (pair)
                            (stream-state-reader-thread-snapshot
                             (getf pair :state)))
                          stream-pairs)
                         (mapcar #'interactive-tool-execution-worker
                                 tool-states)
                         (mapcar #'interactive-buffer-operation-worker
                                 operation-states)
                         (list (openai-oauth-flow-thread-snapshot oauth-flow)
                               start-owner)))
                :test #'eq))
             (unsettled
               (remove-if
                (lambda (worker)
                  (cond
                    ((null worker) t)
                    ;; A reentrant provider start may name the long-lived frame
                    ;; thread itself.  START-OWNER readiness and its normal
                    ;; release path track that caller, so never hand it to a
                    ;; reaper.  Any other current managed worker is finite and
                    ;; must be joined by the newly spawned reaper after this
                    ;; cancellation call returns.
                    ((eq worker (bt:current-thread))
                     (eq worker start-owner))
                    (wait-p
                     (wait-for-owned-worker-settlement
                      worker "runtime-owner"))
                    ((bt:thread-alive-p worker) nil)
                    (t
                     (ignore-errors (bt:join-thread worker))
                     t)))
                workers)))
        (setf (buffer-runtime-teardown-workers teardown) unsettled)
        (bt:with-lock-held ((buffer-runtime-lock buf))
          (when (eq teardown (buffer-runtime-teardown buf))
            (setf (buffer-runtime-teardown-initializing-p teardown) nil
                  (buffer-runtime-teardown-workers-settled-p teardown)
                  (null unsettled))))
        (when unsettled
          (setf deferred-workers-p t)
          (ensure-buffer-runtime-teardown-reaper buf teardown)))
      ;; Pending-stream finalization sets :IDLE.  While any application, start
      ;; owner, or worker still holds the teardown, expose :CANCELLING instead.
      (when stop-p
        (bt:with-lock-held ((buffer-runtime-lock buf))
          (when (eq teardown (buffer-runtime-teardown buf))
            (setf (buffer-status buf) :cancelling))))
      (maybe-finish-buffer-runtime-teardown buf)
      (values buf
              (or (null (bt:with-lock-held ((buffer-runtime-lock buf))
                          (buffer-runtime-teardown buf)))
                  (and wait-p
                   (not deferred-workers-p)
                   (not (or
                         (and application
                              (eq (buffer-runtime-application-owner application)
                                  (bt:current-thread)))
                         (eq start-owner (bt:current-thread))))
                   (wait-for-buffer-runtime-teardown buf teardown)))
              t))))

(defmethod cancel-buffer-runtime-operations ((buf buffer))
  "Cancel BUF and retain the historical bounded-wait teardown contract."
  (multiple-value-bind (result settled-p owned-p)
      (cancel-buffer-runtime-operations-internal buf t)
    (declare (ignore owned-p))
    (values result settled-p)))

(defun advance-tool-calls (buf)
  "Process the next pending tool call without blocking the frame process.
Semantic UI adapters execute at the command boundary; every other exposed tool
is dispatched to one managed worker."
  (when (and *buffer-runtime-application-context*
             (not (buffer-runtime-application-valid-p
                   buf *buffer-runtime-application-context*)))
    (return-from advance-tool-calls nil))
  (when (buffer-pending-tool-execution buf)
    (return-from advance-tool-calls
      (buffer-pending-tool-execution buf)))
  (let ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword)))
    (loop :while (and (buffer-pending-tool-calls buf)
                      (or (null *buffer-runtime-application-context*)
                          (buffer-runtime-application-valid-p
                           buf *buffer-runtime-application-context*)))
          :for tu := (first (buffer-pending-tool-calls buf))
          :for tool-name := (cdr (assoc :name tu))
          :for tool-input := (cdr (assoc :input tu))
          :for tool-id := (cdr (assoc :id tu))
          :do (cond
                ;; A semantic UI adapter intentionally transfers control to
                ;; frame-process advice and performs no blocking work.
                ((frame-process-tool-p tool-name)
                 (let* ((*current-caller* agent-kw)
                        (*current-tool-buffer* buf)
                        (result-text
                          (execute-tool-safely tool-name tool-input
                                               :buffer buf
                                               :tool-id tool-id)))
                   (push `((:result . ,result-text)
                           (:display . ,(format-tool-result-display tool-name result-text))
                           (:tool-id . ,tool-id))
                         (buffer-tool-call-results buf)))
                 (pop (buffer-pending-tool-calls buf)))
                ;; Ordinary tools never execute in redisplay or a command.
                (t
                 (multiple-value-bind (state refusal)
                     (start-interactive-tool-execution
                      buf tool-name tool-input tool-id)
                   (cond
                     (state
                      (return))
                     (refusal
                      (push
                       `((:result . ,(tool-error-result-data refusal))
                         (:display
                          . ,(format nil "[~A REFUSED: ~A]"
                                    tool-name refusal))
                         (:tool-id . ,tool-id))
                       (buffer-tool-call-results buf))
                      (pop (buffer-pending-tool-calls buf))
                      (notify-buffer-display-change buf :tool-refused))
                     (t
                      (return)))))))
    ;; If no more pending tools, finalize
    (unless (or (and *buffer-runtime-application-context*
                     (not (buffer-runtime-application-valid-p
                           buf *buffer-runtime-application-context*)))
                (buffer-pending-tool-calls buf)
                (buffer-pending-tool-execution buf)
                (buffer-user-input-pending buf))
      (finalize-tool-results buf))))

(defun insert-tool-results-message
    (buf results &key (run-hook-p t) (notify-p t))
  "Insert RESULTS as a tool-result message before BUF's input message.
RESULTS is a chronological list of alists containing :RESULT, :DISPLAY,
and :TOOL-ID entries. Returns the inserted message."
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (display-parts (mapcar (lambda (r) (cdr (assoc :display r))) results))
         (result-blocks (mapcar (lambda (r)
                                  `((:type . "tool_result")
                                    (:tool--use--id . ,(cdr (assoc :tool-id r)))
                                    (:content . ,(cdr (assoc :result r)))))
                                results))
         (canonical-result-blocks (canonicalize-message-content "user" result-blocks))
         (display-text (format nil "~{~A~^~%~}" display-parts)))
    (buffer-insert-read-only-message
     buf
     :tool-result
     display-text
     :raw-content canonical-result-blocks
     :run-hook-p run-hook-p
     :notify-p notify-p)))

(defun finalize-tool-results (buf)
  "Insert the accumulated tool results as a message and continue the conversation."
  (let ((results (nreverse (buffer-tool-call-results buf))))
    ;; Retire the queue before INSERT's user hooks run.  The complete snapshot
    ;; is now owned by this application context, so reentrant teardown cannot
    ;; insert the same results a second time.
    (setf (buffer-tool-call-results buf) nil
          (buffer-pending-tool-calls buf) nil)
    (insert-tool-results-message buf results)
    ;; Restore stashed input if still stashed
    (when (buffer-stashed-input buf)
      (set-message-text (buffer-input-message buf) (buffer-stashed-input buf))
      (setf (buffer-stashed-input buf) nil))
    (notify-buffer-display-change buf :tool-results)
    ;; Continue: inject any queued steering before the next provider turn.
    (when (or (null *buffer-runtime-application-context*)
              (buffer-runtime-application-valid-p
               buf *buffer-runtime-application-context*))
      (or (deliver-next-buffer-steering-message buf)
          (start-streaming-response buf)))))

(defun queued-buffer-message-text (entry)
  "Return ENTRY's normalized queued message text, or NIL when blank."
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or (getf entry :text) ""))))
    (unless (blank-string-p text)
      text)))

(defun deliver-buffer-queued-message (buf entry)
  "Finalize ENTRY into BUF as the next user turn and continue the agent run."
  (let ((text (queued-buffer-message-text entry)))
    (when (and text (buffer-runtime-continuation-valid-p buf))
      (run-hook-with-args '*before-send-message-hook* buf text)
      (unless (buffer-runtime-continuation-valid-p buf)
        (return-from deliver-buffer-queued-message nil))
      (set-message-text (buffer-input-message buf) text)
      (buffer-finalize-input buf)
      (unless (buffer-runtime-continuation-valid-p buf)
        (return-from deliver-buffer-queued-message nil))
      (let ((result (send-to-agent-with-context buf)))
        (run-hook-with-args '*after-send-message-hook* buf text result)
        result))))

(defun deliver-next-buffer-steering-message (buf)
  "Deliver the next queued steering message for BUF, if any."
  (let ((entry (dequeue-buffer-steering-message buf)))
    (when entry
      (deliver-buffer-queued-message buf entry)
      t)))

(defun deliver-next-buffer-follow-up-message (buf)
  "Deliver the next queued follow-up message for BUF, if any."
  (let ((entry (dequeue-buffer-follow-up-message buf)))
    (when entry
      (deliver-buffer-queued-message buf entry)
      t)))

(defun reserve-buffer-stream-start (buf)
  "Reserve BUF's current runtime generation for one provider start."
  (bt:with-lock-held ((buffer-runtime-lock buf))
    (let ((application *buffer-runtime-application-context*))
      (cond
        (application
         (unless (and (eq application (buffer-runtime-application buf))
                      (= (buffer-runtime-application-generation application)
                         (buffer-runtime-generation buf)))
           (return-from reserve-buffer-stream-start nil)))
        ((buffer-runtime-application buf)
         (error "Buffer ~A is applying a terminal runtime result"
                (buffer-name buf)))))
    (when (or (buffer-disposed-p buf)
              (buffer-disposing-p buf)
              (buffer-runtime-stopping-p buf)
              (buffer-runtime-stopped-notification-p buf))
      (return-from reserve-buffer-stream-start nil))
    (when (or (buffer-pending-stream buf)
              (buffer-pending-tool-execution buf)
              (buffer-pending-interactive-operation buf)
              (buffer-runtime-start-generation buf))
      (error "Buffer ~A already has a runtime operation" (buffer-name buf)))
    (let ((generation (buffer-runtime-generation buf)))
      (setf (buffer-runtime-start-generation buf) generation
            (buffer-runtime-start-owner buf) (bt:current-thread))
      generation)))

(defun release-buffer-stream-start (buf generation)
  "Release BUF's provider-start reservation when it still names GENERATION."
  (let ((released-p nil)
        (teardown-p nil))
    (bt:with-lock-held ((buffer-runtime-lock buf))
      ;; Teardown clears the generation token but retains START-OWNER until the
      ;; preparing caller reaches this unwind cleanup.
      (when (or (eql generation (buffer-runtime-start-generation buf))
                (eq (bt:current-thread) (buffer-runtime-start-owner buf)))
        (setf (buffer-runtime-start-generation buf) nil
              (buffer-runtime-start-owner buf) nil
              released-p t
              teardown-p (not (null (buffer-runtime-teardown buf))))
        (bt:condition-notify (buffer-runtime-condition buf))))
    (when teardown-p
      (maybe-finish-buffer-runtime-teardown buf))
    released-p))

(defun start-streaming-response (buf)
  "Start one managed provider stream and publish it atomically on BUF.

A generation reservation serializes preparation with tools and teardown.  If
teardown wins while request data or the provider worker is being prepared, the
late state is cancelled and is never made visible as BUF's active operation."
  (let ((start-generation nil)
        (state nil)
        (accepted-p nil)
        (admission-refusal nil))
    (handler-case
        (setf start-generation
              (call-with-runtime-admission
               (lambda () (reserve-buffer-stream-start buf))
               :operation (format nil "provider stream for ~A"
                                  (buffer-name buf))))
      (runtime-admission-closed (condition)
        (setf admission-refusal condition)))
    (when admission-refusal
      (call-with-runtime-settlement-admission
       (lambda ()
         (setf (buffer-status buf) :idle)
         (buffer-insert-system-message
          buf (format nil "[Provider start refused: ~A]" admission-refusal))
         (notify-buffer-display-change buf :status))
       :operation "provider admission refusal reporting")
      (return-from start-streaming-response nil))
    (unless start-generation
      (return-from start-streaming-response nil))
    (setf (buffer-status buf) :thinking)
    (notify-buffer-display-change buf :status)
    (unwind-protect
         (handler-case
             (progn
               (load-active-packages
                :buffer buf
                :agent-name (buffer-agent-name buf))
               (let* ((agent-kw
                        (intern (string-upcase (buffer-agent-name buf))
                                :keyword))
                      (tools
                        (let ((*current-caller* agent-kw))
                          (tool-definitions-for-api :buffer buf)))
                      (messages (build-conversation-messages buf))
                      (system-prompt
                        (let ((*current-caller* agent-kw))
                          (build-agent-system-prompt
                           (buffer-agent-name buf) :buffer buf))))
                 (multiple-value-bind (provider model think-level)
                     (resolve-buffer-provider-and-model buf)
                   (let* ((service-tier (resolve-buffer-service-tier buf))
                          (req-json
                            (api-json-encode
                             `((:messages . ,(coerce messages 'vector))
                               ,@(when think-level
                                   `((:reasoning
                                      . ((:effort . ,think-level)))))
                               ,@(when service-tier
                                   `((:service_tier . ,service-tier)))
                               ,@(when (and tools (plusp (length tools)))
                                   `((:tools . ,tools))))))
                          (request-args
                            (list :model model
                                  :tools tools
                                  :system-prompt system-prompt
                                  :reasoning-effort think-level)))
                     (debug-log buf
                       (format nil
                               "[API REQUEST → ~(~A~)/~A~@[ think=~A~]~@[ tier=~A~]  msg:~D  tools:~D]~%~A"
                               provider model think-level service-tier
                               (length messages)
                               (if tools (length tools) 0)
                               req-json))
                     (file-debug-log
                      "api-request"
                      "provider=~(~A~) model=~A think=~A tier=~A msgs=~D tools=~D payload=~A"
                      provider model
                      (or think-level "default")
                      (or service-tier "auto")
                      (length messages)
                      (if tools (length tools) 0)
                      req-json)
                     (when service-tier
                       (setf request-args
                             (append request-args
                                     (list :service-tier service-tier))))
                     ;; Do not even create the connection worker after a
                     ;; completed teardown has invalidated this preparation.
                     (unless
                         (bt:with-lock-held ((buffer-runtime-lock buf))
                           (and (eql start-generation
                                     (buffer-runtime-start-generation buf))
                                (= start-generation
                                   (buffer-runtime-generation buf))
                                (not (buffer-runtime-stopping-p buf))
                                (not (buffer-disposing-p buf))
                                (not (buffer-disposed-p buf))))
                       (return-from start-streaming-response nil))
                     (setf state
                           (apply #'provider-request-streaming
                                  provider
                                  messages
                                  (lambda (stream-state)
                                    (declare (ignore stream-state))
                                    (wake-buffer-display-change
                                     buf :streaming))
                                  request-args))
                     (unless state
                       (error "Provider ~A returned no stream state" provider))
                     ;; Stream state and placeholder become buffer-owned in one
                     ;; transaction.  Generation equality rejects a late state
                     ;; even after teardown has finished and cleared STOPPING.
                     (let ((agent-msg
                             (make-message agent-kw :read-only-p t)))
                       (set-message-text agent-msg "")
                       (setf (message-timestamp agent-msg)
                             (get-universal-time))
                       (put-message-metadata
                        agent-msg
                        :agent (buffer-agent-name buf)
                        :provider provider
                        :model model
                        :think-level think-level
                        :service-tier service-tier
                        :reasoning-summary-mode
                        (and (eq provider :openai-codex)
                             *openai-codex-reasoning-summary*))
                       (bt:with-lock-held ((buffer-runtime-lock buf))
                         (when (and
                                (eql start-generation
                                     (buffer-runtime-start-generation buf))
                                (= start-generation
                                   (buffer-runtime-generation buf))
                                (not (buffer-disposed-p buf))
                                (not (buffer-disposing-p buf))
                                (not (buffer-runtime-stopping-p buf))
                                (null (buffer-pending-stream buf))
                                (null (buffer-pending-tool-execution buf))
                                (null
                                 (buffer-pending-interactive-operation buf)))
                           (insert-message-before-input buf agent-msg)
                           (setf (buffer-runtime-start-generation buf) nil
                                 (buffer-runtime-start-owner buf) nil
                                 (buffer-pending-stream buf) state
                                 (buffer-streaming-message buf) agent-msg
                                 (buffer-status buf) :thinking
                                 accepted-p t))))
                     (unless accepted-p
                       (return-from start-streaming-response nil))
                     (notify-buffer-display-change buf :stream-started)
                     state))))
           (error (condition)
             (let ((report-p nil))
               (bt:with-lock-held ((buffer-runtime-lock buf))
                 (when (and
                        (= start-generation (buffer-runtime-generation buf))
                        (not (buffer-disposed-p buf))
                        (not (buffer-disposing-p buf))
                        (not (buffer-runtime-stopping-p buf)))
                   (setf (buffer-status buf) :error
                         report-p t)))
               (when report-p
                 (buffer-insert-agent-message
                  buf (format nil "[Error: ~A]" condition))
                 (notify-buffer-display-change buf :status)))
             nil))
      (unless accepted-p
        (when state
          (cancel-stream-state state)
          (settle-stream-state-reader state)))
      (release-buffer-stream-start buf start-generation))))

(defun latest-text-block-text (content-blocks)
  "Return the text of the last text block in CONTENT-BLOCKS, or NIL."
  (let ((latest nil))
    (dolist (block content-blocks latest)
      (when (string= "text" (content-block-type block))
        (setf latest (or (cdr (assoc :text block)) ""))))))

(defun stream-reasoning-display-lines (reasoning-blocks &key visible-text)
  "Return reasoning lines for in-progress stream display text."
  (let ((visible (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (or visible-text "")))
        (lines nil))
    (dolist (reasoning reasoning-blocks)
      (let ((text (or reasoning "")))
        (unless (or (blank-string-p text)
                    (string= visible
                             (string-trim '(#\Space #\Tab #\Newline #\Return)
                                          text)))
          (push ";; reasoning" lines)
          (dolist (line (split-string-by-newline text))
            (push line lines)))))
    (nreverse lines)))

(defun stream-state-display-text (state &key show-reasoning-p)
  "Return STATE's in-progress text without double-counting accumulators.
Some providers keep the current partial text only in STREAM-STATE-TEXT, while
OpenAI-compatible providers also mirror it into CONTENT-BLOCKS on every delta."
  (bt:with-lock-held ((stream-state-lock state))
    (let* ((accumulator (stream-state-text state))
           (content-blocks (reverse (copy-list (stream-state-content-blocks state))))
           (content-text (content-text-blocks content-blocks))
           (reasoning-text
             (and show-reasoning-p
                  (join-lines-with-newlines
                   (stream-reasoning-display-lines
                    (content-reasoning-blocks content-blocks)
                    :visible-text content-text))))
           (latest-text (latest-text-block-text content-blocks)))
      (cond
        ((and reasoning-text (not (blank-string-p reasoning-text)))
         (if (blank-string-p content-text)
             reasoning-text
             (format nil "~A~%~A" content-text reasoning-text)))
        ((zerop (length accumulator))
         content-text)
        ((and latest-text (string= latest-text accumulator))
         content-text)
        (t
         (concatenate 'string content-text accumulator))))))

(defun cancelled-stream-content-blocks (state)
  "Return cancellable response content, excluding unfinished tool calls."
  (remove-if (lambda (block)
               (string= "tool_use" (content-block-type block)))
             (stream-state-final-content-blocks state)))

(defun finalize-cancelled-streaming-response
    (buf state msg &key (notify-p t))
  "Finalize MSG after STATE is stopped by the user."
  (let* ((content-blocks (cancelled-stream-content-blocks state))
         (canonical-content
           (canonicalize-message-content "assistant" content-blocks))
         (final-text
           (string-trim '(#\Space #\Tab #\Newline #\Return)
                        (content-text-blocks canonical-content)))
         (usage
           (bt:with-lock-held ((stream-state-lock state))
             (copy-list (stream-state-usage state)))))
    (put-message-metadata
     msg
     :agent (buffer-agent-name buf)
     :stop-reason "cancelled"
     :content-block-count (length content-blocks)
     :tool-call-count 0
     :reasoning-block-count (length (content-reasoning-blocks
                                      canonical-content)))
    (when usage
      (apply #'put-message-metadata
             msg
             (token-usage-metadata-pairs usage)))
    (if (blank-string-p final-text)
        (progn
          (setf (message-sender msg) :system
                (message-raw-content msg) nil)
          (set-message-text msg "[Response stopped by user]"))
        (progn
          (setf (message-raw-content msg) canonical-content)
          (set-message-text msg
                            (format nil "~A~%[Stopped by user]"
                                    final-text))))
    (record-buffer-message buf msg)
    ;; The caller atomically detached this exact stream before finalization.
    ;; Never clear ownership slots here: a later generation may already exist.
    (setf (buffer-status buf) :idle)
    (when notify-p
      (notify-buffer-display-change buf :stream-cancelled))
    nil))

(defun stop-streaming-response (buf)
  "Stop BUF's active provider or tool operation without blocking the UI."
  (when buf
    (multiple-value-bind (ignored settled-p owned-p)
        (cancel-buffer-runtime-operations-internal
         buf nil :only-if-owned-p t :stop-p t)
      (declare (ignore ignored settled-p))
      (when (and owned-p
                 (bt:with-lock-held ((buffer-runtime-lock buf))
                   (buffer-runtime-stopping-p buf)))
        (wake-buffer-display-change buf :runtime-cancelling))
      owned-p)))

(defun update-streaming-response (buf)
  "Poll the active streaming response and update the display.
Returns T if still streaming, NIL if done."
  (multiple-value-bind (state msg)
      (bt:with-lock-held ((buffer-runtime-lock buf))
        (values (buffer-pending-stream buf)
                (buffer-streaming-message buf)))
    (unless (and state msg)
      (return-from update-streaming-response nil))
    (multiple-value-bind (done err cancelled)
        (bt:with-lock-held ((stream-state-lock state))
          (values (stream-state-done-p state)
                  (stream-state-error-p state)
                  (stream-state-cancelled-p state)))
      ;; While streaming: update display with in-progress text
      ;; (stream-state-text accumulates the CURRENT block's text;
      ;; completed blocks have their text finalized in content-blocks)
      (unless (or done err cancelled)
        (let ((all-text (stream-state-display-text
                         state
                         :show-reasoning-p (buffer-show-reasoning-p buf))))
          (when (plusp (length all-text))
            (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                                     all-text)))
              (unless (string= text (message-text msg))
                (set-message-text msg text)
                (notify-buffer-display-change buf :streaming)))))
        (return-from update-streaming-response t))
      ;; Provider terminal publication precedes transport/callback unwind.
      ;; Keep BUF's pending owner in place while joining the exact reader.
      (unless (stream-state-reader-worker-settled-p buf state)
        (return-from update-streaming-response t))
      ;; Claim + detach + :APPLYING publication is atomic.  A concurrent
      ;; teardown can invalidate this generation, but cannot finish or allow a
      ;; successor until the unwind-safe release below.
      (let ((context
              (claim-buffer-runtime-application buf :stream state msg)))
        (unless context
          (return-from update-streaming-response nil))
        (let ((*buffer-runtime-application-context* context))
          (unwind-protect
               (handler-case
                   (cond
                     (cancelled
                      (finalize-cancelled-streaming-response buf state msg))
                     (err
                      (set-message-text msg
                                        (format nil "[Streaming error: ~A]" err))
                      (record-buffer-message buf msg)
                      (setf (buffer-status buf) :error)
                      (notify-buffer-display-change buf :stream-error)
                      nil)
                     (done
                      (let* ((content-blocks
                               (bt:with-lock-held ((stream-state-lock state))
                                 (nreverse
                                  (copy-list
                                   (stream-state-content-blocks state)))))
                             (canonical-content
                               (canonicalize-message-content
                                "assistant" content-blocks))
                             (stop-reason
                               (bt:with-lock-held ((stream-state-lock state))
                                 (stream-state-stop-reason state)))
                             (usage
                               (bt:with-lock-held ((stream-state-lock state))
                                 (copy-list (stream-state-usage state))))
                             (tool-uses
                               (content-tool-use-blocks canonical-content))
                             (tool-turn-p
                               (and (string= "tool_use" (or stop-reason ""))
                                    tool-uses))
                             (final-text
                               (content-text-blocks canonical-content)))
                        (let ((display
                                (with-output-to-string (stream)
                                  (write-string final-text stream)
                                  (dolist (tool-use tool-uses)
                                    (write-char #\Newline stream)
                                    (write-string
                                     (format-tool-call-display tool-use)
                                     stream)))))
                          (set-message-text
                           msg
                           (string-trim
                            '(#\Space #\Tab #\Newline #\Return) display))
                          (setf (message-raw-content msg) canonical-content))
                        (put-message-metadata
                         msg
                         :stop-reason (or stop-reason "nil")
                         :content-block-count (length content-blocks)
                         :tool-call-count (length tool-uses)
                         :reasoning-block-count
                         (length (content-reasoning-blocks canonical-content)))
                        (when usage
                          (apply #'put-message-metadata
                                 msg
                                 (token-usage-metadata-pairs usage)))
                        ;; Seed unresolved IDs before persistence or user hooks.
                        ;; A same-thread reentrant teardown can now always
                        ;; synthesize protocol-completing cancellation results.
                        (when tool-turn-p
                          (prepare-tool-calls buf tool-uses))
                        (record-buffer-message buf msg)
                        (let ((resp-json
                                (api-json-encode
                                 (coerce canonical-content 'vector)))
                              (usage-line
                                (format-token-usage-summary usage)))
                          (debug-log buf
                            (format nil
                                    "[API RESPONSE  stop:~A  blocks:~D~@[  ~A~]]~%~A"
                                    (or stop-reason "nil")
                                    (length content-blocks)
                                    usage-line
                                    resp-json))
                          (file-debug-log
                           "api-response"
                           "stop=~A blocks=~D~@[ ~A~] content=~A"
                           (or stop-reason "nil")
                           (length content-blocks)
                           usage-line
                           resp-json))
                        (let ((metadata (message-metadata msg)))
                          (maybe-run-hook-with-args
                           '*after-provider-response-hook*
                           buf
                           canonical-content
                           (message-metadata-value metadata :provider)
                           (message-metadata-value metadata :model)
                           usage))
                        (notify-buffer-display-change buf :stream-complete)
                        (when (buffer-runtime-application-valid-p buf context)
                          (if tool-turn-p
                              (progn
                                (advance-tool-calls buf)
                                t)
                              (or
                               (deliver-next-buffer-steering-message buf)
                               (deliver-next-buffer-follow-up-message buf)
                               (progn
                                 (setf (buffer-status buf) :idle)
                                 (notify-buffer-display-change buf :status)
                                 nil)))))))
                 (error (condition)
                   ;; A recorded tool_use must remain protocol-complete even
                   ;; when a user hook or result application itself fails.
                   (when (buffer-pending-tool-calls buf)
                     (finalize-cancelled-tool-queue buf))
                   (setf (buffer-status buf) :error)
                   (notify-buffer-display-change buf :stream-error)
                   (error condition)))
            (release-buffer-runtime-application buf context)))))))

(defun update-openai-oauth-login (&optional expected-buffer)
  "Apply a completed OAuth result from the frame process.
Only the exact still-pending flow may be claimed.  When EXPECTED-BUFFER is
non-nil, a flow owned by another buffer remains pending for its owning frame."
  (let ((flow (openai-oauth-pending-flow)))
    (unless flow
      (return-from update-openai-oauth-login nil))
    (let* ((snapshot (openai-oauth-flow-snapshot flow))
           (buf (openai-oauth-flow-buffer flow)))
      (unless (getf snapshot :done-p)
        (return-from update-openai-oauth-login t))
      (unless (or (null expected-buffer)
                  (eq expected-buffer buf))
        (return-from update-openai-oauth-login
          (openai-oauth-login-pending-p)))
      ;; DONE-P is provider result publication, not worker settlement.  Keep
      ;; the exact pending flow observable and return promptly while one
      ;; managed waiter joins listener/client cleanup and queues a retry wake.
      (unless (openai-oauth-flow-worker-settled-p-safe flow)
        (return-from update-openai-oauth-login t))
      (unless (claim-openai-oauth-pending-flow flow)
        ;; Another flow replaced this snapshot, or another frame owns it.
        (return-from update-openai-oauth-login
          (openai-oauth-login-pending-p)))
      (when (and buf
                 (not (buffer-disposing-p buf))
                 (not (buffer-disposed-p buf)))
        (setf (buffer-status buf) :idle)
        (buffer-insert-system-message
         buf
         (cond
           ((getf snapshot :success-p)
            "[OpenAI Codex OAuth: Login successful. Credentials saved to ~/.codex/auth.json.]")
           ((getf snapshot :cancelled-p)
            "[OAuth cancelled]")
           (t
            (format nil "[OAuth error: ~A]"
                    (or (getf snapshot :error) "Unknown error"))))))
      nil)))

(declaim (ftype (function (buffer) buffer) send-to-agent-with-context))
(defun send-to-agent-with-context (buf)
  "Start a streaming conversation with the LLM. Non-blocking --
provider reader threads surface updates asynchronously."
  (unless (buffer-runtime-continuation-valid-p buf)
    (return-from send-to-agent-with-context buf))
  (start-streaming-response buf)
  buf)

;;; --------------------------------------------------------------------------
;;; Non-interactive Prompt Mode
;;; --------------------------------------------------------------------------

(defun make-prompt-buffer
    (prompt agent-name &key
                        (working-directory (default-prompt-working-directory))
                        (session-persistence-mode
                         *default-buffer-session-persistence-mode*))
  "Create a buffer seeded with PROMPT as the only finalized user message."
  (let* ((*buffer-system-prompt-display-enabled* nil)
         (buf (make-chat-buffer "rplaca:prompt"
                                :agent-name agent-name
                                :working-directory working-directory
                                :session-persistence-mode
                                session-persistence-mode)))
    (set-message-text (buffer-input-message buf) prompt)
    (buffer-finalize-input buf)
    buf))

(defun make-empty-session-prompt-buffer (session-name agent-name)
  "Create an empty prompt-mode buffer attached to SESSION-NAME."
  (let* ((*buffer-system-prompt-display-enabled* nil)
         (buf (make-chat-buffer session-name
                                :agent-name agent-name
                                :working-directory (truename "."))))
    (autosave-session-snapshot buf)
    buf))

(defun make-session-prompt-buffer (session-name agent-name)
  "Load SESSION-NAME for prompt mode, or create it when missing."
  (let* ((*buffer-system-prompt-display-enabled* nil)
         (buf (or (load-session session-name :agent-name agent-name)
                  (make-empty-session-prompt-buffer session-name agent-name))))
    (initialize-buffer-display-defaults buf)
    buf))

(defun append-session-prompt-input (buf prompt)
  "Append PROMPT as BUF's next finalized user message."
  (set-message-text (buffer-input-message buf) prompt)
  (buffer-finalize-input buf)
  buf)

(defun prompt-cache-key-for-buffer (buf)
  "Return a stable OpenAI prompt cache routing key for BUF."
  (format nil "rplaca-~(~A~)-~(~A~)"
          (buffer-agent-name buf)
          (session-name-hash (buffer-name buf))))

(defun maybe-apply-prompt-routing-overrides
    (buf provider model think-level &key model-role service-tier)
  "Apply optional routing overrides to BUF."
  (when provider
    (set-buffer-provider-override buf (normalize-provider provider)))
  (when model
    (set-buffer-model-override buf model))
  (when think-level
    (set-buffer-think-level-override buf think-level))
  (when model-role
    (set-buffer-model-role-override buf model-role))
  (when service-tier
    (set-buffer-service-tier-override buf service-tier))
  buf)

(defun prompt-stream-state-response (state)
  "Convert a completed streaming STATE into a canonical response."
  (bt:with-lock-held ((stream-state-lock state))
    (when (stream-state-error-p state)
      (error "Streaming error: ~A" (stream-state-error-p state)))
    (canonical-response
     (or (stream-state-stop-reason state) "end_turn")
     (nreverse (copy-list (stream-state-content-blocks state)))
     :usage (copy-list (stream-state-usage state)))))

(defun prompt-cancellation-requested-p (callback)
  "Return true when CALLBACK requests cooperative prompt cancellation."
  (and callback (funcall callback)))

(defun check-prompt-cancellation (callback &optional stream-state)
  "Signal PROMPT-RUN-CANCELLED when CALLBACK requests cancellation.
When STREAM-STATE is active, close it before unwinding the prompt loop."
  (when (prompt-cancellation-requested-p callback)
    (when stream-state
      (cancel-stream-state stream-state :stop-reason "cancelled"))
    (error 'prompt-run-cancelled)))

(defun wait-for-prompt-stream-state (state &key cancel-requested-p)
  "Block until streaming STATE completes, then return its canonical response."
  (loop
    (check-prompt-cancellation cancel-requested-p state)
    (when (bt:with-lock-held ((stream-state-lock state))
            (stream-state-done-p state))
      (settle-stream-state-reader state)
      (return (prompt-stream-state-response state)))
    (sleep 0.02)))

(defun prompt-request-once (buf &key event-callback stream-state-callback
                                     cancel-requested-p)
  "Send BUF's current conversation once via the streaming provider path.
Returns values RESPONSE, PROVIDER, MODEL, THINK-LEVEL."
  (check-prompt-cancellation cancel-requested-p)
  (load-active-packages :buffer buf)
  (unless *suppress-prompt-compaction*
    (let ((*compaction-stream-state-callback* stream-state-callback)
          (*compaction-cancel-requested-p* cancel-requested-p))
      (maybe-compact-buffer buf :reason :prompt-request)))
  (check-prompt-cancellation cancel-requested-p)
  (let* ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
         (tools (let ((*current-caller* agent-kw))
                  (tool-definitions-for-api :buffer buf)))
         (messages (build-conversation-messages buf))
         (system-prompt (let ((*current-caller* agent-kw))
                          (build-agent-system-prompt (buffer-agent-name buf)
                                                     :buffer buf))))
    (multiple-value-bind (provider model think-level)
        (resolve-buffer-provider-and-model buf)
      (let ((service-tier (resolve-buffer-service-tier buf))
            (prompt-cache-key (and (eq provider :openai-codex)
                                   (prompt-cache-key-for-buffer buf)))
            (prompt-cache-retention nil))
        (file-debug-log "prompt-request"
                        "provider=~(~A~) model=~A think=~A tier=~A msgs=~D tools=~D cache-key=~A cache-retention=~A"
                        provider model (or think-level "default")
                        (or service-tier "auto")
                        (length messages)
                        (if tools (length tools) 0)
                        (or prompt-cache-key "none")
                        (or prompt-cache-retention "none"))
        (let ((*openai-codex-prompt-cache-key* prompt-cache-key)
              (*openai-codex-prompt-cache-retention*
                prompt-cache-retention))
          (check-prompt-cancellation cancel-requested-p)
          (let* ((last-stream-text "")
                 (request-args (list :model model
                                     :tools tools
                                     :system-prompt system-prompt
                                     :reasoning-effort think-level))
                 (_ignored (when service-tier
                             (setf request-args
                                   (append request-args
                                           (list :service-tier service-tier)))))
                 (state (apply #'provider-request-streaming
                               provider messages
                               (lambda (state)
                                 (when event-callback
                                   (let ((text (stream-state-display-text
                                                state
                                                :show-reasoning-p nil)))
                                     (when (and (stringp text)
                                                (not (string= text
                                                              last-stream-text)))
                                       (let ((delta (if (and (<= (length last-stream-text)
                                                                (length text))
                                                             (string= last-stream-text
                                                                      text
                                                                      :end2
                                                                      (length last-stream-text)))
                                                        (subseq text
                                                                (length last-stream-text))
                                                        text)))
                                         (funcall event-callback
                                                  (list :event "assistant.chunk"
                                                        :delta delta
                                                        :text text)))
                                       (setf last-stream-text text)))))
                               request-args)))
            (let ((completed-normally-p nil))
              (unwind-protect
                   (multiple-value-prog1
                       (progn
                         (when stream-state-callback
                           (funcall stream-state-callback state))
                         (check-prompt-cancellation cancel-requested-p state)
                         (let ((response
                                 (wait-for-prompt-stream-state
                                  state
                                  :cancel-requested-p cancel-requested-p)))
                           (declare (ignore _ignored))
                           (check-prompt-cancellation
                            cancel-requested-p state)
                           (maybe-run-hook-with-args
                            '*after-provider-response-hook*
                            buf
                            response
                            provider
                            model
                            (response-usage response))
                           (values response
                                   provider
                                   model
                                   think-level
                                   service-tier)))
                     (setf completed-normally-p t))
                ;; Once the provider state exists, every abnormal caller-side
                ;; exit owns its cancellation and settlement.  In particular,
                ;; a failing public STREAM-STATE-CALLBACK must not strand the
                ;; socket reader that it was asked to observe.
                (unless completed-normally-p
                  (cancel-stream-state state :stop-reason "cancelled")
                  (settle-stream-state-reader state))))))))))

(defun execute-prompt-tool-call
    (buf tool-use-block agent-kw &key event-callback cancel-requested-p)
  "Execute TOOL-USE-BLOCK for prompt mode and return values RESULT and EVENT.
RESULT is the alist consumed by INSERT-TOOL-RESULTS-MESSAGE. EVENT is a
PROMPT-TOOL-EVENT for terminal/debug output."
  (check-prompt-cancellation cancel-requested-p)
  (let* ((tool-name (cdr (assoc :name tool-use-block)))
         (tool-input (cdr (assoc :input tool-use-block)))
         (tool-id (cdr (assoc :id tool-use-block)))
         (execution-policy
           (interactive-tool-execution-policy tool-name tool-input))
         (policy-refusal
           (unless (eq execution-policy :background)
             (format nil
                     "Tool is ~:(~A~) and cannot run from a provider/pipeline worker. Use its trusted user command instead."
                     execution-policy)))
         (denied-p (not (null policy-refusal)))
         (result-text
           (if denied-p
               (tool-error-result-data policy-refusal)
               (let ((*current-caller* agent-kw)
                     (*current-tool-buffer* buf))
                 (execute-tool-safely tool-name tool-input
                                      :buffer buf
                                      :tool-id tool-id))))
         (display (if denied-p
                      (format nil "[~A REFUSED: ~A]"
                              tool-name policy-refusal)
                      (format-tool-result-display tool-name result-text)))
         (result `((:result . ,result-text)
                   (:display . ,display)
                   (:tool-id . ,tool-id)))
         (event (make-prompt-tool-event
                 :id tool-id
                 :name tool-name
                 :input tool-input
                 :result-text result-text
                 :display display
                 :denied-p denied-p)))
    (check-prompt-cancellation cancel-requested-p)
    (when event-callback
      (funcall event-callback
               (list :event "tool.call"
                     :id tool-id
                     :name tool-name
                     :input tool-input))
      (funcall event-callback
               (list :event "tool.result"
                     :id tool-id
                     :name tool-name
                     :result result-text
                     :denied-p denied-p)))
    (values result event)))

(defun execute-prompt-tool-calls
    (buf tool-uses &key event-callback cancel-requested-p)
  "Execute TOOL-USES, persist a complete result message, and return events.

Once the assistant tool_use message is in BUF, every call must receive exactly
one provider-visible tool_result.  If cancellation or another error unwinds
this loop, completed results are preserved and the unresolved suffix receives
an error result before the original condition continues unwinding."
  (let ((agent-kw (intern (string-upcase (buffer-agent-name buf)) :keyword))
        (results nil)
        (events nil)
        (remaining (copy-list tool-uses))
        (results-recorded-p nil)
        (failure nil))
    (labels ((aborted-result (tool-use)
               (let* ((cancelled-p (typep failure 'prompt-run-cancelled))
                      (detail
                        (if cancelled-p
                            "Cancelled by user"
                            (format nil "Prompt tool loop aborted~@[: ~A~]"
                                    failure)))
                      (tool-name (or (cdr (assoc :name tool-use)) "tool")))
                 `((:result . ,(tool-error-result-data detail))
                   (:display
                    . ,(format nil "[~A ~A]"
                               tool-name
                               (if cancelled-p "CANCELLED" "ERROR")))
                   (:tool-id . ,(cdr (assoc :id tool-use)))))))
      (handler-bind ((prompt-run-cancelled
                       (lambda (condition)
                         (setf failure condition)))
                     (error (lambda (condition)
                              (setf failure condition))))
        (unwind-protect
             (progn
               (dolist (tool-use tool-uses)
                 (check-prompt-cancellation cancel-requested-p)
                 (multiple-value-bind (result event)
                     (execute-prompt-tool-call
                      buf tool-use agent-kw
                      :event-callback event-callback
                      :cancel-requested-p cancel-requested-p)
                   (push result results)
                   (push event events)
                   (pop remaining)))
               (check-prompt-cancellation cancel-requested-p)
               ;; Mark ownership before the insertion's notification hooks.
               ;; A hook failure after durable insertion must not duplicate the
               ;; provider-visible tool_result message during unwind cleanup.
               (setf results-recorded-p t)
               (insert-tool-results-message buf (nreverse results))
               (nreverse events))
          (unless results-recorded-p
            (let ((terminal-results
                    (append (nreverse results)
                            (mapcar #'aborted-result remaining))))
              (when terminal-results
                (handler-case
                    (insert-tool-results-message buf terminal-results)
                  (error (condition)
                    (file-debug-event
                     "prompt-tool-result-cleanup-error"
                     :buffer-name (buffer-name buf)
                     :condition (format nil "~A" condition))))))))))))

(defun run-prompt-buffer-loop (buf prompt max-tool-iterations
                               &key output-schema event-callback
                                 stream-state-callback cancel-requested-p)
  "Run BUF through prompt-mode provider/tool iterations for PROMPT."
  (let ((tool-events nil)
        (final-provider nil)
        (final-model nil)
        (final-think-level nil)
        (final-service-tier nil)
        (aggregate-usage nil)
        (iterations 0))
    (labels ((fail (format-string &rest format-args)
               (error 'prompt-run-error
                      :message (apply #'format nil format-string format-args)
                      :tool-events tool-events
                      :iterations iterations
                      :provider final-provider
                      :model final-model
                      :think-level final-think-level)))
      (loop
        (check-prompt-cancellation cancel-requested-p)
        (when (>= iterations max-tool-iterations)
          (fail "Exceeded maximum tool iterations (~D)"
                max-tool-iterations))
        (incf iterations)
        (multiple-value-bind (response provider* model* think-level* service-tier*)
            (handler-case
                (prompt-request-once buf
                                     :event-callback event-callback
                                     :stream-state-callback
                                     stream-state-callback
                                     :cancel-requested-p
                                     cancel-requested-p)
              (prompt-run-cancelled (condition)
                (error condition))
              (error (condition)
                (fail "Prompt provider request failed: ~A" condition)))
          (setf final-provider provider*
                final-model model*
                final-think-level think-level*
                final-service-tier service-tier*)
          (check-prompt-cancellation cancel-requested-p)
          (setf aggregate-usage
                (merge-token-usage aggregate-usage
                                   (response-usage response)))
          (let* ((content-blocks (response-content response))
                 (canonical-content (canonicalize-message-content
                                     "assistant"
                                     content-blocks))
                 (tool-uses (content-tool-use-blocks canonical-content))
                 (stop-reason (response-stop-reason response))
                 (agent-kw (intern (string-upcase (buffer-agent-name buf))
                                   :keyword)))
            (insert-agent-message-from-content buf canonical-content agent-kw)
            (if tool-uses
                (setf tool-events
                      (append tool-events
                              (handler-case
                                  (execute-prompt-tool-calls
                                   buf tool-uses
                                   :event-callback event-callback
                                   :cancel-requested-p cancel-requested-p)
                                (prompt-run-cancelled (condition)
                                  (error condition))
                                (error (condition)
                                  (fail "Prompt tool loop failed: ~A"
                                        condition)))))
                (let ((result
                        (make-prompt-run-result
                         :prompt prompt
                         :final-text (content-text-blocks canonical-content)
                         :tool-events tool-events
                         :reasoning-blocks (content-reasoning-blocks
                                            canonical-content)
                         :agent-name (buffer-agent-name buf)
                         :provider final-provider
                         :model final-model
                         :think-level final-think-level
                         :service-tier final-service-tier
                         :iterations iterations
                         :stop-reason stop-reason
                         :usage aggregate-usage
                         :session-name (and (buffer-session buf)
                                            (session-name
                                             (buffer-session buf)))
                         :session-id (and (buffer-session buf)
                                          (session-id
                                           (buffer-session buf))))))
                  (handler-case
                      (return
                        (apply-output-schema-to-prompt-run-result
                         result output-schema))
                    (error (condition)
                      (fail "Structured output validation failed: ~A"
                            condition)))))))))))

(defun run-prompt-with-buffer (buf prompt custom-tool-definitions
                               max-tool-iterations
                               tool-names tool-names-supplied-p
                               &key output-schema event-callback
                                 stream-state-callback cancel-requested-p)
  "Run a prepared prompt BUF with optional custom tools."
  (let* ((temporary-tool-table
           (temporary-tool-table-from-definitions custom-tool-definitions))
         (effective-tool-names
           (resolve-prompt-tool-names (buffer-agent-name buf)
                                      custom-tool-definitions
                                      tool-names
                                      tool-names-supplied-p)))
    (let ((*active-tool-names* effective-tool-names)
          (*temporary-tool-table* (or temporary-tool-table
                                      *temporary-tool-table*)))
      (run-prompt-buffer-loop buf
                              prompt
                              max-tool-iterations
                              :output-schema output-schema
                              :event-callback event-callback
                              :stream-state-callback stream-state-callback
                              :cancel-requested-p cancel-requested-p))))

(defun run-single-prompt (prompt &key (agent-name *default-agent-name*)
                                 provider model think-level
                                 model-role service-tier
                                 output-schema
                                 (working-directory
                                  (default-prompt-working-directory))
                                 (session-persistence-mode
                                  *default-buffer-session-persistence-mode*)
                                 (max-tool-iterations *prompt-max-tool-iterations*)
                                 package-names
                                 (tool-names nil tool-names-supplied-p)
                                 custom-tools
                                 event-callback
                                 stream-state-callback
                                 cancel-requested-p)
  "Run PROMPT once without a UI and return a PROMPT-RUN-RESULT.
The request loops through tool_use responses until the provider returns a final
assistant response or MAX-TOOL-ITERATIONS is exceeded."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (let* ((event-callback
           (make-bounded-runtime-callback event-callback
                                          :label "single prompt"))
         (custom-tool-definitions (normalize-run-custom-tools custom-tools))
         (buf (make-prompt-buffer
               prompt agent-name
               :working-directory working-directory
               :session-persistence-mode session-persistence-mode)))
    (maybe-apply-prompt-routing-overrides buf provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buf)
            (normalize-package-name-list package-names)))
    (run-prompt-with-buffer buf
                            prompt
                            custom-tool-definitions
                            max-tool-iterations
                            tool-names
                            tool-names-supplied-p
                            :output-schema output-schema
                            :event-callback event-callback
                            :stream-state-callback stream-state-callback
                            :cancel-requested-p cancel-requested-p)))

(defun run-session-prompt (prompt &key session-name
                                  (agent-name *default-agent-name*)
                                  provider model think-level
                                  model-role service-tier
                                  output-schema
                                  (max-tool-iterations *prompt-max-tool-iterations*)
                                  package-names
                                  (tool-names nil tool-names-supplied-p)
                                  custom-tools
                                  event-callback
                                  stream-state-callback
                                  cancel-requested-p)
  "Append PROMPT to SESSION-NAME, run the agent, and save the session."
  (when (blank-string-p prompt)
    (error "Prompt must be non-empty"))
  (unless (and (stringp session-name)
               (not (blank-string-p session-name)))
    (error "Session prompt mode requires a non-empty session name"))
  (let* ((event-callback
           (make-bounded-runtime-callback event-callback
                                          :label "session prompt"))
         (custom-tool-definitions (normalize-run-custom-tools custom-tools))
         (buf (make-session-prompt-buffer session-name agent-name)))
    (maybe-apply-prompt-routing-overrides buf provider model think-level
                                          :model-role model-role
                                          :service-tier service-tier)
    (when package-names
      (setf (buffer-enabled-packages buf)
            (normalize-package-name-list package-names)))
    (append-session-prompt-input buf prompt)
    (run-prompt-with-buffer buf
                            prompt
                            custom-tool-definitions
                            max-tool-iterations
                            tool-names
                            tool-names-supplied-p
                            :output-schema output-schema
                            :event-callback event-callback
                            :stream-state-callback stream-state-callback
                            :cancel-requested-p cancel-requested-p)))
