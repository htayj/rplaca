(in-package :rplaca)

(defconstant +listener-shell-output-limit+ 20000)

(define-condition unknown-listener-package (error)
  ((name
    :initarg :name
    :reader unknown-listener-package-name))
  (:report
   (lambda (condition stream)
     (format stream "No package named ~A."
             (unknown-listener-package-name condition)))))

(define-condition empty-listener-directory-stack (error) ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "Directory stack is empty." stream))))

(defstruct (listener-context
            (:constructor %make-listener-context
                (&key package-name directory-stack input-mode)))
  (package-name "CL-USER" :type string)
  (directory-stack nil :type list)
  (input-mode :eval :type (member :eval :say)))

(defun listener-context-default-package-name ()
  (let ((name (and (boundp '*lisp-eval-default-package*)
                   *lisp-eval-default-package*)))
    (if (and (stringp name)
             (plusp (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                         name))))
        (string-upcase name)
        "CL-USER")))

(defun resolve-listener-context-package-name (name)
  (let* ((text (and name (ignore-errors (string name))))
         (trimmed (and text
                       (string-trim '(#\Space #\Tab #\Newline #\Return)
                                    text)))
         (normalized (and trimmed (string-upcase trimmed)))
         (package (and normalized
                       (plusp (length normalized))
                       (find-package normalized))))
    (unless package
      (error 'unknown-listener-package :name name))
    normalized))

(defun make-listener-context
    (&key (package-name (listener-context-default-package-name))
          directory-stack
          (input-mode :eval))
  (require-listener-input-mode input-mode)
  (%make-listener-context
   :package-name (resolve-listener-context-package-name package-name)
   :directory-stack directory-stack
   :input-mode input-mode))

(defun listener-context-with
    (context &key
               (package-name (listener-context-package-name context))
               (directory-stack (listener-context-directory-stack context))
               (input-mode (listener-context-input-mode context)))
  (%make-listener-context
   :package-name package-name
   :directory-stack directory-stack
   :input-mode input-mode))

(defun listener-context-push-directory (context directory)
  (listener-context-with
   context
   :directory-stack
   (cons (uiop:ensure-directory-pathname directory)
         (listener-context-directory-stack context))))

(defun listener-context-pop-directory (context)
  (let ((stack (listener-context-directory-stack context)))
    (unless stack
      (error 'empty-listener-directory-stack))
    (values (listener-context-with context :directory-stack (rest stack))
            (first stack))))

(defun listener-context-set-package (context name)
  (listener-context-with
   context :package-name (resolve-listener-context-package-name name)))

(defun listener-context-set-input-mode (context mode)
  (require-listener-input-mode mode)
  (listener-context-with context :input-mode mode))

(defun run-listener-shell-command
    (command directory
     &key (timeout *interactive-subprocess-default-timeout*)
          (output-limit +listener-shell-output-limit+))
  (when (blank-string-p command)
    (error "Shell command is required."))
  (run-interactive-subprocess
   command
   :directory (uiop:ensure-directory-pathname directory)
   :timeout timeout
   :output-limit output-limit))
