(in-package :rplaca)

(declaim (inline listener-input-mode-p listener-dispatch-kind-p))

(defun listener-input-mode-p (mode)
  (member mode '(:eval :say) :test #'eq))

(defun listener-dispatch-kind-p (kind)
  (member kind '(:form :command :eval-form :prose :enter-say :exit-say
                 :error :shell :no-op)
          :test #'eq))

(defun require-listener-input-mode (mode)
  (unless (listener-input-mode-p mode)
    (error "Invalid listener input mode: ~S" mode))
  mode)

(defun require-listener-dispatch-kind (kind)
  (unless (listener-dispatch-kind-p kind)
    (error "Invalid listener dispatch kind: ~S" kind))
  kind)

(defun listener-whitespace-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\Page)
          :test #'char=))

(defun listener-dispatch-record (kind payload)
  (list :kind kind :payload payload))

(defun listener-mode-default-kind (mode)
  (ecase mode
    (:eval :form)
    (:say :prose)))

(defun classify-listener-line (line mode)
  (require-listener-input-mode mode)
  (let ((length (length line)))
    (cond
      ((or (zerop length)
           (every #'listener-whitespace-p line))
       (listener-dispatch-record :no-op ""))
      ((and (>= length 2)
            (char= (char line 0) #\#)
            (char= (char line 1) #\!))
       (listener-dispatch-record :shell (subseq line 2)))
      ((listener-whitespace-p (char line 0))
       (listener-dispatch-record (listener-mode-default-kind mode) line))
      ((char= (char line 0) #\,)
       (let ((rest (subseq line 1)))
         (cond
           ((zerop (length rest))
            (ecase mode
              (:eval
               (listener-dispatch-record
                :error "did you mean ,Command or ! to talk?"))
              (:say
               (listener-dispatch-record :exit-say ""))))
           ((char= (char rest 0) #\()
            (listener-dispatch-record :eval-form rest))
           ((and (>= (length rest) 2)
                 (char= (char rest 0) #\,)
                 (char= (char rest 1) #\())
            (listener-dispatch-record :prose rest))
           (t
            (listener-dispatch-record :command rest)))))
      ((char= (char line 0) #\!)
       (let ((rest (subseq line 1)))
         (if (zerop (length rest))
             (ecase mode
               (:eval (listener-dispatch-record :enter-say ""))
               (:say (listener-dispatch-record :exit-say "")))
             (listener-dispatch-record :prose rest))))
      (t
       (listener-dispatch-record (listener-mode-default-kind mode) line)))))

(defun next-listener-input-mode (mode kind)
  (require-listener-input-mode mode)
  (require-listener-dispatch-kind kind)
  (case kind
    (:enter-say :say)
    (:exit-say :eval)
    (otherwise mode)))

(defun listener-prompt-string (package-name mode)
  (require-listener-input-mode mode)
  (ecase mode
    (:eval (format nil "~A> " package-name))
    (:say (format nil "~A!> " package-name))))

(defconstant +prose-interpolation-max-value-characters+ 20000)

(define-condition prose-interpolation-error (error)
  ((substring
    :initarg :substring
    :reader prose-interpolation-error-substring)
   (cause
    :initarg :cause
    :reader prose-interpolation-error-cause))
  (:report
   (lambda (condition stream)
     (format stream "Could not interpolate prose near ~S."
             (prose-interpolation-error-substring condition)))))

(defclass bounded-character-output-stream
    (sb-gray:fundamental-character-output-stream)
  ((characters
    :initform (make-array +prose-interpolation-max-value-characters+
                          :element-type 'character
                          :fill-pointer 0)
    :reader bounded-character-output-stream-characters)))

(defmethod sb-gray:stream-write-char
    ((stream bounded-character-output-stream) character)
  (let ((characters (bounded-character-output-stream-characters stream)))
    (when (= (fill-pointer characters) (array-total-size characters))
      (throw 'prose-interpolation-output-limit nil))
    (vector-push character characters))
  character)

(defmethod sb-gray:stream-write-string
    ((stream bounded-character-output-stream) string
     &optional (start 0) end)
  (loop :for index :from start :below (or end (length string))
        :do (sb-gray:stream-write-char stream (char string index)))
  string)

(defmethod sb-gray:stream-line-column
    ((stream bounded-character-output-stream))
  (declare (ignore stream))
  nil)

(defun bounded-prose-interpolation-value-string (value)
  (handler-case
      (let ((stream (make-instance 'bounded-character-output-stream))
            (*print-length* 100)
            (*print-level* 8)
            (*print-circle* t)
            (*print-pretty* nil)
            (*print-readably* nil)
            (*print-escape* t))
        (catch 'prose-interpolation-output-limit
          (write value :stream stream))
        (coerce (bounded-character-output-stream-characters stream) 'string))
    (error ()
      "#<unprintable value>")))

(defun signal-prose-interpolation-error (text start end cause)
  (error 'prose-interpolation-error
         :substring (subseq text start end)
         :cause cause))

(defun read-prose-interpolation (text start package)
  (handler-case
      (let ((*package* package)
            (*read-eval* nil))
        (read-from-string text t nil
                          :start (1+ start)
                          :preserve-whitespace t))
    (error (cause)
      (signal-prose-interpolation-error text start (length text) cause))))

(defun evaluate-prose-interpolation (form text start end package eval-fn)
  (handler-case
      (let ((*package* package)
            (* *)
            (** **)
            (*** ***)
            (+ +)
            (++ ++)
            (+++ +++)
            (/ /)
            (// //)
            (/// ///)
            (- -))
        (let ((values (multiple-value-list (funcall eval-fn form))))
          (if values
              (bounded-prose-interpolation-value-string (first values))
              "")))
    (error (cause)
      (signal-prose-interpolation-error text start end cause))))

(defun expand-prose-interpolations (text package eval-fn)
  (with-output-to-string (output)
    (loop :with cursor := 0
          :for marker := (search ",(" text :start2 cursor)
          :do
             (unless marker
               (write-string text output :start cursor)
               (loop-finish))
             (if (and (> marker 0)
                      (char= (char text (1- marker)) #\,))
                 (progn
                   (write-string text output :start cursor :end (1- marker))
                   (write-string ",(" output)
                   (setf cursor (+ marker 2)))
                 (progn
                   (write-string text output :start cursor :end marker)
                   (multiple-value-bind (form end)
                       (read-prose-interpolation text marker package)
                     (write-string
                      (evaluate-prose-interpolation
                       form text marker end package eval-fn)
                      output)
                     (setf cursor end)))))))
