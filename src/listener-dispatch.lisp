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
