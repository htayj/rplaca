(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Listener command dispatch: read-frame-command and todo-6 commands.
;; --------------------------------------------------------------------------
;;;
;;; read-frame-command accepts command-form-or-prose (todo 4) and maps each
;;; listener-input-token kind to a command form.  This file defines the hidden
;;; no-op/error/shell/mode commands and com-eval (todo 7); com-say (todo 8)
;;; remains forward-declared by the mapping but unimplemented.

(defun listener-token->command (token frame stream)
  "Map a listener-input-token to the command form for read-frame-command.

Pure function: no side effects, no input editing.  The :command case uses
climi::ensure-complete-command (intentional McCLIM core dependency, matching the
McCLIM Listener) to complete partial commands from the frame's command table."
  (let ((kind (listener-input-token-kind token))
        (value (listener-input-token-value token))
        (source (listener-input-token-source token)))
    (case kind
      ((:form :eval-form) (list 'com-eval value source))
      (:command (climi::ensure-complete-command
                 value (clim:frame-command-table frame) stream))
      (:prose (list 'com-say source))
      (:enter-say (list 'com-set-input-mode :say))
      (:exit-say (list 'com-set-input-mode :eval))
      (:shell (list 'com-run-shell source))
      (:no-op (list 'com-no-op))
      (:error (list 'com-report-input-error value))
      (t (error "Unknown listener input token kind: ~S" kind)))))

(defmethod clim:read-frame-command ((frame rplaca-listener)
                                    &key (stream *standard-input*))
  "Accept a command-form-or-prose token and map it to the next command form.

Uses *command-dispatchers* '(#\,) so comma is the command prefix.  Reader errors
remain input-editor/accept concerns; this method does not catch arbitrary
conditions (matching McCLIM Listener behavior at frames.lisp:506)."
  (let* ((clim:*command-dispatchers* '(#\,))
         (token (clim:accept 'command-form-or-prose
                             :stream stream :prompt nil)))
    (listener-token->command token frame stream)))

;;; --------------------------------------------------------------------------
;;; Hidden commands (no menu exposure, no user-facing name).
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-no-op :name nil) ()
  "Hidden: do nothing and return through normal command execution so the loop
re-prompts.  Used for empty/whitespace-only input."
  nil)

(define-rplaca-listener-command (com-report-input-error :name nil)
    ((hint string))
  "Hidden: write the input error hint to the interactor."
  (let ((stream (clim:frame-standard-output clim:*application-frame*)))
    (write-string hint stream)
    (terpri stream)))

;;; --------------------------------------------------------------------------
;;; Shell command (delegates to the todo-3 bounded helper).
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-run-shell :name nil)
    ((source string))
  "Hidden: run a shell command via the bounded run-listener-shell-command helper,
writing stdout/stderr/exit-status to the interactor.  Never mutates global cwd."
  (let* ((frame clim:*application-frame*)
         (buffer (rplaca-listener-conversation-buffer frame))
         (directory (uiop:ensure-directory-pathname
                     (buffer-working-directory buffer)))
         (result (run-listener-shell-command
                  source directory)))
    (let ((stream (clim:frame-standard-output frame)))
      (let ((stdout (getf result :stdout))
            (stderr (getf result :stderr))
            (exit-code (getf result :exit-code)))
        (when (and stdout (plusp (length stdout)))
          (write-string stdout stream))
        (when (and stderr (plusp (length stderr)))
          (write-string stderr stream))
        (when (/= 0 exit-code)
          (format stream "~&[exit ~A]~%" exit-code))))))

;;; --------------------------------------------------------------------------
;;; Input mode setter (used by enter-say/exit-say token kinds).
;;; --------------------------------------------------------------------------

(define-rplaca-listener-command (com-set-input-mode :name nil)
    ((mode (member :eval :say)))
  "Hidden: update the frame's listener-context input mode immutably so the next
prompt reflects the new mode."
  (let ((frame clim:*application-frame*))
    (setf (rplaca-listener-context frame)
          (listener-context-set-input-mode
           (rplaca-listener-context frame) mode))))

;;; --------------------------------------------------------------------------
;;; Eval core (todo 7): com-eval with bounded output, value presentations,
;;; REPL history, ASK-AGENT restart, and context synchronization.
;;; --------------------------------------------------------------------------

(defparameter +listener-eval-output-limit+ 20000)
(defparameter +listener-eval-truncation-marker+ " [...truncated]")

(defclass listener-bounded-output (sb-gray:fundamental-character-output-stream)
  ((target :initarg :target :reader bounded-target)
   (remaining :initarg :remaining :accessor bounded-remaining)
   (marker-len :initform (length +listener-eval-truncation-marker+)
               :reader bounded-marker-len)
   (marker-p :initform nil :accessor bounded-marker-p)))

(defmethod sb-gray:stream-write-char ((stream listener-bounded-output) char)
  (with-slots (target remaining marker-len marker-p) stream
    (cond
      ((> remaining marker-len)
       (write-char char target) (decf remaining))
      ((not marker-p)
       (setf marker-p t)
       (write-string +listener-eval-truncation-marker+ target)
       (setf remaining 0))
      (t nil))))

(defmethod sb-gray:stream-write-string ((stream listener-bounded-output)
                                         string &optional (start 0) end)
  (let* ((end (or end (length string)))
         (len (- end start)))
    (with-slots (target remaining marker-len marker-p) stream
      (unless marker-p
        (let* ((allowance (max 0 (- remaining marker-len)))
               (count (min len allowance)))
          (when (plusp count)
            (write-string string target :start start :end (+ start count))
            (decf remaining count))
          (when (> len count)
            (setf marker-p t)
            (write-string +listener-eval-truncation-marker+ target)
            (decf remaining marker-len))))))
  string)

(defmethod sb-gray:stream-line-column ((stream listener-bounded-output)) nil)
(defmethod sb-gray:stream-finish-output ((stream listener-bounded-output))
  (finish-output (bounded-target stream)))
(defmethod sb-gray:stream-force-output ((stream listener-bounded-output))
  (force-output (bounded-target stream)))

(defun listener-sync-eval-context (frame package directory)
  (setf (rplaca-listener-context frame)
        (listener-context-set-package
         (rplaca-listener-context frame) (package-name package)))
  (setf (buffer-working-directory (rplaca-listener-conversation-buffer frame))
        directory))

(defun listener-update-repl-history (form values)
  "Shuffle +/+++ and */*** unconditionally per the standard REPL contract.
Zero values sets * to NIL and still shifts ** /***."
  (setq cl:+++ cl:++ cl:++ cl:+ cl:+ form
        cl:/// cl:// cl:// cl:/ cl:/ values)
  (setq cl:*** cl:** cl:** cl:* cl:* (first values)))

(defun listener-eval-form (form source-text)
  "Core eval logic: evaluate FORM, present values, update history, offer ASK-AGENT.
Returns the source-text if ASK-AGENT was invoked, or NIL otherwise."
  (let* ((frame clim:*application-frame*)
         (interactor (clim:frame-standard-output frame))
         (bounded (make-instance 'listener-bounded-output
                                  :target interactor
                                  :remaining +listener-eval-output-limit+))
         (ask-tag (gensym "ASK-AGENT-TRANSFER")))
    (let ((*package* *package*)
          (*default-pathname-defaults* *default-pathname-defaults*))
      (unwind-protect
           (catch ask-tag
             (let ((*standard-output* bounded)
                   (*error-output* bounded)
                   (*trace-output* bounded)
                   (*standard-input* interactor)
                   (cl:- form))
               (handler-bind
                   ((unbound-variable
                      (lambda (condition)
                        (restart-case
                            (invoke-debugger condition)
                          (ask-agent ()
                            :report "Ask the agent about this"
                            (throw ask-tag source-text)))))
                    (undefined-function
                      (lambda (condition)
                        (restart-case
                            (invoke-debugger condition)
                          (ask-agent ()
                            :report "Ask the agent about this"
                            (throw ask-tag source-text))))))
                 (let ((values (multiple-value-list (eval form))))
                   (listener-update-repl-history form values)
                   (let ((*print-length* 100)
                         (*print-level* 8)
                         (*print-circle* t)
                         (*print-readably* nil)
                         (*print-pretty* nil)
                         (*print-escape* t))
                     (dolist (value values)
                       (clim:with-output-as-presentation
                           (interactor value 'clim:expression)
                         (let ((*standard-output* bounded))
                           (prin1 value)
                           (terpri)))))))
             nil))
        (listener-sync-eval-context frame
                                    *package*
                                    *default-pathname-defaults*)))))

(define-rplaca-listener-command (com-eval) ((form t) (source-text string))
  (let ((ask-source (listener-eval-form form source-text)))
    (when (and ask-source (fboundp 'com-say))
      (funcall (symbol-function 'com-say) ask-source))))
