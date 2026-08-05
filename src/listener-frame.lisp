(in-package :rplaca)

;;; Listener frame fixture, presentation types, and delegated acceptance.
;;; This module introduces the rplaca-listener application frame (panes,
;;; layouts, and lifecycle are completed in a later todo), the frame-generated
;;; command definer, the listener-input-token result type, and three owned
;;; presentation types at the input boundary: rplaca-package, prose, and
;;; command-form-or-prose.  command-form-or-prose classifies the submitted
;;; region and delegates acceptance to clim:form, clim:command, or prose; it
;;; never executes.  Mode is read from the active frame's listener-context at
;;; accept time, with *listener-input-mode* as a headless fallback.

(defvar *listener-input-mode* :eval
  "Headless fallback for the listener input mode when no frame is active.")

(defstruct (listener-input-token
             (:constructor make-listener-input-token
                 (&key kind value source)))
  (kind nil :type symbol)
  (value nil)
  (source "" :type string))

(defun current-listener-input-mode ()
  "Return the validated listener input mode for the current accept.

An active rplaca-listener frame's listener-context supplies the mode and always
wins; *listener-input-mode* is the fallback for the headless accept-from-string
path.  A frame whose listener-context is missing or invalid is not silently
ignored: the context accessor or require-listener-input-mode signals."
  (require-listener-input-mode
   (if (typep clim:*application-frame* 'rplaca-listener)
       (listener-context-input-mode
        (rplaca-listener-context clim:*application-frame*))
       *listener-input-mode*)))

(defun current-listener-command-table ()
  "Return the active frame's command table, or the rplaca-listener table."
  (or (when clim:*application-frame*
        (ignore-errors (clim:frame-command-table clim:*application-frame*)))
      (clim:find-command-table 'rplaca-listener)))

;;; Minimal rplaca-listener frame fixture.

(clim:define-application-frame rplaca-listener ()
  ((listener-context
    :initarg :listener-context
    :accessor rplaca-listener-context
    :initform (make-listener-context)))
  (:command-table (rplaca-listener))
  (:menu-bar nil)
  (:panes
   (interactor :interactor))
  (:layouts
   (listener-only interactor)))

;;; rplaca-package presentation type.

(clim:define-presentation-type rplaca-package ()
  :inherit-from t)

(clim:define-presentation-method clim:presentation-typep
    (object (type rplaca-package))
  (typep object 'package))

(clim:define-presentation-method clim:present
    (object (type rplaca-package) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string (package-name object) stream))

(defun listener-package-completion-entries ()
  "Return deterministic sorted (display-string . package) completion entries
over every package name and nickname."
  (let ((entries '()))
    (flet ((add-entry (label package)
             (push (cons (string-upcase label) package) entries)))
      (dolist (package (list-all-packages))
        (add-entry (package-name package) package)
        (dolist (nickname (package-nicknames package))
          (add-entry nickname package))))
    (coerce
     (sort (delete-duplicates entries :test #'string= :key #'car)
           #'string< :key #'car)
     'list)))

(clim:define-presentation-method clim:accept
    ((type rplaca-package) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (let ((entries (listener-package-completion-entries)))
    (clim:completing-from-suggestions (stream)
      (dolist (entry entries)
        (clim:suggest (car entry) (cdr entry))))))

;;; prose presentation type.

(clim:define-presentation-type prose ()
  :inherit-from t)

(clim:define-presentation-method clim:present
    (object (type prose) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string object stream))

(clim:define-presentation-method clim:accept
    ((type prose) stream (view clim:textual-view) &key)
  (declare (ignore view))
  ;; Rest of line verbatim, empty allowed: no delimiter gestures, so spaces and
  ;; most punctuation accumulate; the token ends at activation or end of input.
  (values (clim:with-delimiter-gestures (nil :override t)
            (clim:read-token stream))
          'prose))

;;; command-form-or-prose presentation type (delegated acceptance).

(clim:define-presentation-type command-form-or-prose ()
  :inherit-from t)

(clim:define-presentation-method clim:present
    (object (type command-form-or-prose) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (typecase object
    (listener-input-token
     (write-string (listener-input-token-source object) stream))
    (otherwise (princ object stream))))

(defun listener-submission-end (buffer)
  "Return the submission endpoint.  McCLIM 1.0's accept-from-string stream uses
the raw string as its buffer (no fill pointer), so length is the endpoint."
  (if (array-has-fill-pointer-p buffer)
      (fill-pointer buffer)
      (length buffer)))

(defun listener-region-whitespace-p (string)
  (or (zerop (length string))
      (every #'listener-whitespace-p string)))

(defun listener-position-newline (string)
  (position #\Newline string))

(defmacro with-listener-submission ((region-var
                                     start-var end-var buffer-var stream-var)
                                    stream-form &body body)
  `(let* ((,stream-var ,stream-form)
          (,buffer-var (clim:stream-input-buffer ,stream-var))
          (,start-var (clim:stream-scan-pointer ,stream-var))
          (,end-var (listener-submission-end ,buffer-var))
          (,region-var (subseq ,buffer-var ,start-var ,end-var)))
     (declare (ignorable ,buffer-var))
     ,@body))

(defun listener-consume-to (stream submission-end)
  (setf (clim:stream-scan-pointer stream) submission-end))

(defun listener-advance-prefix (stream submission-start prefix-length)
  (setf (clim:stream-scan-pointer stream)
        (+ submission-start prefix-length)))

(defun listener-multiline-prose-error-token (region submission-end stream)
  (listener-consume-to stream submission-end)
  (make-listener-input-token
   :kind :error
   :value "Use ,Compose for multiline prose."
   :source region))

(clim:define-presentation-method clim:accept
    ((type command-form-or-prose) stream (view clim:textual-view) &key)
  (with-listener-submission
      (region submission-start submission-end buffer stream) stream
    (labels ((source-after (prefix-length)
               (subseq region prefix-length))
             (finish (kind value prefix-length)
               ;; Delegated acceptance only positions for parsing; consume the
               ;; complete submission so no semantic residual tail remains (the
               ;; CLIM accept return confirms this in tests).
               (listener-consume-to stream submission-end)
               (make-listener-input-token
                :kind kind
                :value value
                :source (source-after prefix-length))))
      (let ((mode (current-listener-input-mode)))
        (cond
          ;; Empty or whitespace-only submission: no-op before any delegation.
          ((listener-region-whitespace-p region)
           (listener-consume-to stream submission-end)
           (make-listener-input-token :kind :no-op :value nil
                                      :source region))
          ;; #! -> shell token (checked on two characters before the ! rule).
          ((and (>= (length region) 2)
                (char= (char region 0) #\#)
                (char= (char region 1) #\!))
           (let ((shell-source (source-after 2)))
              (listener-consume-to stream submission-end)
              (make-listener-input-token :kind :shell :value shell-source
                                         :source shell-source)))
          ;; , -> consume, then peek to distinguish eval-form / literal prose /
          ;; bare-comma / command.
          ((char= (char region 0) #\,)
           (listener-advance-prefix stream submission-start 1)
           (let ((rest (source-after 1)))
             (cond
               ((zerop (length rest))
                 ;; Bare comma: mode-dependent error or exit-say.
                 (listener-consume-to stream submission-end)
                 (ecase mode
                   (:eval
                    (make-listener-input-token
                     :kind :error
                     :value "did you mean ,Command or ! to talk?"
                     :source ""))
                   (:say
                    (make-listener-input-token
                     :kind :exit-say :value nil
                     :source ""))))
               ((char= (char rest 0) #\()
                ;; ,( -> one-shot eval form.
                (let ((value (clim:accept 'clim:form :stream stream
                                          :prompt nil :view view)))
                  (finish :eval-form value 1)))
               ((and (>= (length rest) 2)
                     (char= (char rest 0) #\,)
                     (char= (char rest 1) #\())
                ;; ,,( -> literal ,( prose.  Reject multiline paste first.
                (if (listener-position-newline region)
                    (listener-multiline-prose-error-token
                     region submission-end stream)
                    (progn
                      (listener-advance-prefix stream submission-start 2)
                      (let ((value (clim:accept 'prose :stream stream
                                                :prompt nil :view view)))
                        (finish :prose value 1)))))
               (t
                ;; ,X -> CLIM command in the listener command table.
                (let ((value (clim:accept
                              `(clim:command
                                :command-table ,(current-listener-command-table))
                              :stream stream :prompt nil :view view)))
                  (finish :command value 1))))))
          ;; ! -> consume, then prose; only exact bare ! toggles the mode.
          ((char= (char region 0) #\!)
           (if (listener-position-newline region)
               (listener-multiline-prose-error-token
                region submission-end stream)
               (let ((rest (source-after 1)))
                 (cond
                   ((zerop (length rest))
                     ;; Exact bare bang: mode toggle.
                     (listener-consume-to stream submission-end)
                     (make-listener-input-token
                      :kind (ecase mode (:eval :enter-say) (:say :exit-say))
                      :value nil :source ""))
                   (t
                    ;; Nonempty rest (spaces count) is force-prose.
                    (listener-advance-prefix stream submission-start 1)
                    (let ((value (clim:accept 'prose :stream stream
                                              :prompt nil :view view)))
                      (finish :prose value 1)))))))
          ;; Leading whitespace disables dispatch; payload is the full line.
          ((listener-whitespace-p (char region 0))
           (if (eq mode :say)
               (if (listener-position-newline region)
                   (listener-multiline-prose-error-token
                    region submission-end stream)
                   (let ((value (clim:accept 'prose :stream stream
                                             :prompt nil :view view)))
                     (finish :prose value 0)))
               (let ((value (clim:accept 'clim:form :stream stream
                                         :prompt nil :view view)))
                 (finish :form value 0))))
          ;; Default: mode default (form in :eval, prose in :say).
          (t
           (if (eq mode :say)
               (if (listener-position-newline region)
                   (listener-multiline-prose-error-token
                    region submission-end stream)
                   (let ((value (clim:accept 'prose :stream stream
                                             :prompt nil :view view)))
                     (finish :prose value 0)))
               (let ((value (clim:accept 'clim:form :stream stream
                                         :prompt nil :view view)))
                 (finish :form value 0)))))))))
