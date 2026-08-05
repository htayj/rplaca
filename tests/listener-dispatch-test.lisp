(in-package :rplaca/tests)

(in-suite listener-dispatch-suite)

(defun assert-listener-dispatch (line mode kind payload)
  (is (equal (list :kind kind :payload payload)
             (rplaca:classify-listener-line line mode))))

(test listener-dispatch-record-shape-is-exact
  (is (equal '(:kind :form :payload "value")
             (rplaca:classify-listener-line "value" :eval))))

(test listener-character-spec-table
  (dolist (mode '(:eval :say))
    (assert-listener-dispatch "" mode :no-op "")
    (assert-listener-dispatch (format nil " ~C~C" #\Tab #\Newline)
                              mode :no-op "")
    (assert-listener-dispatch "#!echo hi" mode :shell "echo hi")
    (assert-listener-dispatch ",(print :ok)" mode :eval-form "(print :ok)")
    (assert-listener-dispatch ",,(literal)" mode :prose ",(literal)")
    (assert-listener-dispatch ",Describe object" mode :command "Describe object")
    (assert-listener-dispatch "!explain this" mode :prose "explain this"))
  (assert-listener-dispatch " (+ 1 2)" :eval :form " (+ 1 2)")
  (assert-listener-dispatch " !foo" :eval :form " !foo")
  (assert-listener-dispatch " prose" :say :prose " prose")
  (assert-listener-dispatch "value" :eval :form "value")
  (assert-listener-dispatch "hello" :say :prose "hello")
  (assert-listener-dispatch "!" :eval :enter-say "")
  (assert-listener-dispatch "!" :say :exit-say "")
  (assert-listener-dispatch "," :eval :error
                            "did you mean ,Command or ! to talk?")
  (assert-listener-dispatch "," :say :exit-say ""))

(test listener-classifier-amendments-a1-through-a7
  (assert-listener-dispatch "," :eval :error
                            "did you mean ,Command or ! to talk?")
  (assert-listener-dispatch ",(values 1 2)" :say :eval-form "(values 1 2)")
  (assert-listener-dispatch (format nil "~C~C" #\Tab #\Space) :eval :no-op "")
  (assert-listener-dispatch " !foo" :eval :form " !foo")
  (assert-listener-dispatch "#!printf bang" :say :shell "printf bang")
  (assert-listener-dispatch ",,(literal)" :eval :prose ",(literal)")
  (assert-listener-dispatch (format nil "(+ 1~% 2)") :eval :form
                            (format nil "(+ 1~% 2)"))
  (assert-listener-dispatch (format nil "!first~%second") :say :prose
                            (format nil "first~%second"))
  (assert-listener-dispatch (format nil ",(list~% 1)") :say :eval-form
                            (format nil "(list~% 1)")))

(test listener-input-mode-transition-is-exhaustive
  (dolist (mode '(:eval :say))
    (dolist (kind '(:form :command :eval-form :prose :enter-say :exit-say
                    :error :shell :no-op))
      (is (eq (case kind
                (:enter-say :say)
                (:exit-say :eval)
                (otherwise mode))
              (rplaca:next-listener-input-mode mode kind))))))

(test listener-prompt-reflects-package-and-mode
  (is (string= "CL-USER> "
               (rplaca:listener-prompt-string "CL-USER" :eval)))
  (is (string= "CL-USER!> "
               (rplaca:listener-prompt-string "CL-USER" :say)))
  (is (string= "MY-PACKAGE> "
               (rplaca:listener-prompt-string "MY-PACKAGE" :eval))))

(test listener-dispatch-rejects-invalid-mode-and-kind
  (signals error (rplaca:classify-listener-line "value" :unknown))
  (signals error (rplaca:next-listener-input-mode :unknown :form))
  (signals error (rplaca:next-listener-input-mode :eval :unknown))
  (signals error (rplaca:listener-prompt-string "CL-USER" :unknown)))
