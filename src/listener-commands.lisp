(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Listener command dispatch: read-frame-command and todo-6 commands.
;; --------------------------------------------------------------------------
;;;
;;; read-frame-command accepts command-form-or-prose (todo 4) and maps each
;;; listener-input-token kind to a command form.  This file defines only the
;;; hidden no-op/error/shell/mode commands needed now; com-eval (todo 7) and
;;; com-say (todo 8) remain forward-declared by the mapping but unimplemented.

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
