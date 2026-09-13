;;;; Native redisplay and graphical restart proof in a private CLX display.

(ql:quickload :rplaca)

(defvar *probe-clx-port*
  (or (clim:find-port)
      (error "The private Xvfb did not produce a CLX port.")))
(defvar *probe-frame-manager*
  (or (clim:find-frame-manager :port *probe-clx-port*)
      (error "The private CLX port did not produce a frame manager.")))

(defvar *probe-frame-thread-errors* nil)
(defvar *probe-frame-thread-error-lock*
  (bt:make-lock "appearance live probe frame errors"))

(defstruct (probe-frame-call
             (:constructor make-probe-frame-call (label expected-thread function)))
  label
  expected-thread
  function
  (lock (bt:make-lock "appearance live probe frame call"))
  done-p
  values
  condition
  actual-thread)

(defclass probe-frame-call-event (clim:window-manager-event)
  ((call :initarg :call :reader probe-frame-call-event-call)))

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin) (event probe-frame-call-event))
  "Run one probe assertion on the real frame process that owns SHEET."
  (let* ((call (probe-frame-call-event-call event))
         (frame (ignore-errors (clim:pane-frame sheet)))
         (values nil)
         (condition nil))
    (handler-case
        (setf values
              (multiple-value-list
               (funcall (probe-frame-call-function call) frame)))
      (error (caught)
        (setf condition caught)))
    (bt:with-lock-held ((probe-frame-call-lock call))
      (setf (probe-frame-call-actual-thread call) (bt:current-thread)
            (probe-frame-call-values call) values
            (probe-frame-call-condition call) condition
            (probe-frame-call-done-p call) t))))

(defun probe-record-frame-thread-error (name condition)
  (bt:with-lock-held (*probe-frame-thread-error-lock*)
    (push (list name condition) *probe-frame-thread-errors*)))

(defun probe-check-frame-thread-errors ()
  (let ((errors
          (bt:with-lock-held (*probe-frame-thread-error-lock*)
            (copy-list *probe-frame-thread-errors*))))
    (when errors
      (error "Frame event process failed: ~{~S~^, ~}" errors))))

(defun probe-wait (predicate label &key (seconds 15))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop do (probe-check-frame-thread-errors)
          until (funcall predicate)
          do (when (>= (get-internal-real-time) deadline)
               (error "Timed out waiting for ~A." label))
             (sleep 0.05))))

(defun probe-call-on-frame (frame expected-thread label function &key (seconds 15))
  "Call FUNCTION on FRAME's actual event process and return its values."
  (let* ((call (make-probe-frame-call label expected-thread function))
         (sheet (or (rplaca::chat-frame-grafted-top-level-sheet frame)
                    (error "Cannot queue ~A before frame adoption." label))))
    (clim:queue-event
     sheet
     (make-instance 'probe-frame-call-event :sheet sheet :call call))
    (probe-wait
     (lambda ()
       (bt:with-lock-held ((probe-frame-call-lock call))
         (probe-frame-call-done-p call)))
     label :seconds seconds)
    (bt:with-lock-held ((probe-frame-call-lock call))
      (unless (eq expected-thread (probe-frame-call-actual-thread call))
        (error "~A ran on ~S instead of owning frame process ~S."
               label
               (probe-frame-call-actual-thread call)
               expected-thread))
      (when (probe-frame-call-condition call)
        (error "~A failed on its owning frame process: ~A"
               label (probe-frame-call-condition call)))
      (values-list (probe-frame-call-values call)))))

(defun probe-profile-theme (frame)
  (rplaca:appearance-profile-selected-theme
   (rplaca::chat-frame-appearance-profile frame)))

(defun probe-start-frame (name profile)
  (let* ((buffer (rplaca:make-buffer name :session-persistence-mode :ephemeral))
         (frame (clim:make-application-frame
                 'rplaca::rplaca-chat-frame
                 :buffer buffer :appearance-profile profile :pretty-name name))
         (thread (bt:make-thread
                  (lambda ()
                    (handler-case
                        ;; Port selection belongs to the owning event process.
                        ;; Passing :FRAME-MANAGER during construction makes
                        ;; pinned McCLIM synchronously adopt the frame on this
                        ;; probe's coordinator thread before ownership starts.
                        (clim:run-frame-top-level frame
                                                  :port *probe-clx-port*)
                      (error (condition)
                        (probe-record-frame-thread-error name condition))))
                  :name name)))
    (values frame thread)))



(defvar *recovery-rendered* nil)
(defvar *recovery-fail-next* nil)
(defvar *recovery-display* (symbol-function 'rplaca::display-chat-transcript))
(setf (symbol-function 'rplaca::display-chat-transcript)
      (lambda (frame stream)
        (when *recovery-fail-next*
          (setf *recovery-fail-next* nil)
          (error "RPLACA transient display recovery probe"))
        (funcall *recovery-display* frame stream)
        (setf *recovery-rendered*
              (prin1-to-string (mapcar #'rplaca::chat-display-item-cache-value
                                        (rplaca::chat-transcript-display-items
                                         (rplaca::chat-frame-buffer frame)))))))
(defun recovery-debugger ()
  (let ((found nil))
    (clim:map-over-frames
     (lambda (frame)
       (when (and (typep frame 'clim-debugger::clim-debugger)
                  (eq (clim:frame-state frame) :enabled))
         (setf found frame))) :port *probe-clx-port*)
    found))
(defun recovery-capture (name)
  (uiop:run-program (list "import" "-window" "root"
                          (namestring (merge-pathnames (format nil "~A.png" name)
                                                      (uiop:ensure-directory-pathname
                                                       (uiop:getenv "RPLACA_RECOVERY_ARTIFACT_DIR")))))))
(clim:define-command
    (recovery-failing-action :command-table rplaca::rplaca-chat-frame)
    ()
  (error "RPLACA command recovery probe"))

(multiple-value-bind (frame thread)
    (probe-start-frame "RPLACA recovery probe" (rplaca:make-appearance-profile))
  (unwind-protect
       (progn
         (probe-wait (lambda () (eq (rplaca::chat-frame-lifecycle-state frame) :running)) "ready")
         (sleep 1)
         (probe-call-on-frame
          frame thread "stream with a pending key prefix"
          (lambda (f)
            (setf (esa::remaining-keys f) '(#\x))
            (let ((buf (rplaca::chat-frame-buffer f)))
              (rplaca:set-buffer-provider-override buf :e2e)
              (rplaca:set-buffer-model-override buf "e2e-model")
              (rplaca:buffer-insert-read-only-message buf :user "hello" :record-p nil)
              (rplaca::start-streaming-response buf))))
         (probe-wait (lambda () (and (null (rplaca::buffer-pending-stream (rplaca::chat-frame-buffer frame)))
                                     (search "RPLACA_E2E_HELLO_SENTINEL" *recovery-rendered*))) "idle final output")
         (format t "~&RECOVERY idle-stream-with-prefix=PASS~%")
         (probe-call-on-frame
          frame thread "inject display failure"
          (lambda (f)
            (setf (esa::remaining-keys f) nil *recovery-fail-next* t)
            (rplaca:buffer-insert-agent-message (rplaca::chat-frame-buffer f)
                                              "RECOVERED_WITHOUT_MESSAGE" :record-p nil)))
         (probe-wait #'recovery-debugger "native debugger")
         (sleep .5)
         (recovery-capture "graphical-debugger")
         (let* ((dbg (recovery-debugger))
                (info (clim-debugger::the-condition dbg))
                (restarts (clim-debugger::restarts info))
                (index (position 'climi::clear-pane-try-again restarts :key #'restart-name)))
           (format t "~&RECOVERY restart-names=~S~%" (mapcar #'restart-name restarts))
           (assert index)
           (assert (< index 10))
           ;; Use the native debugger's numbered keyboard command.
           (uiop:run-program (list "xdotool" "key" (write-to-string index))))
         (probe-wait (lambda () (search "RECOVERED_WITHOUT_MESSAGE" *recovery-rendered*)) "native retry")
         (sleep .5)
         (recovery-capture "graphical-retry-recovered")
         (format t "~&RECOVERY native-retry-without-message=PASS~%")
         (probe-call-on-frame
          frame thread "set unsent draft and queue failing action"
          (lambda (f)
            (setf (clim:gadget-value (clim:find-pane-named f 'rplaca::compose)) "unsent draft")))
         (let* ((sheet (clim:frame-top-level-sheet frame))
                (call (make-probe-frame-call
                       "failing command" thread
                       (lambda (f) (clim:execute-frame-command f '(recovery-failing-action))))))
           (clim:queue-event sheet (make-instance 'probe-frame-call-event :sheet sheet :call call)))
         (probe-wait #'recovery-debugger "action debugger")
         (sleep .5)
         (uiop:run-program '("xdotool" "key" "q"))
         (probe-wait (lambda () (null (recovery-debugger))) "debugger abort")
         (probe-call-on-frame
          frame thread "verify draft after abort"
          (lambda (f)
            (assert (string= "unsent draft" (clim:gadget-value (clim:find-pane-named f 'rplaca::compose))))))
         (format t "~&RECOVERY abort-preserves-draft=PASS~%")
         (uiop:run-program '("xdotool" "type" "x"))
         (sleep .3)
         (probe-call-on-frame
          frame thread "verify input focus after abort"
          (lambda (f)
            (assert (search "x" (clim:gadget-value (clim:find-pane-named f 'rplaca::compose))))))
         (format t "~&RECOVERY input-focus-after-abort=PASS~%"))
    (ignore-errors (probe-call-on-frame frame thread "exit" #'clim:frame-exit :seconds 2))
    (ignore-errors (bt:join-thread thread))))
