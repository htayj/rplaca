(in-package :rplaca/tests)

(in-suite listener-commands-core-suite)

(test listener-context-defaults-from-lisp-eval-package
  (let ((rplaca::*lisp-eval-default-package* "cl-user"))
    (let ((context (rplaca::make-listener-context)))
      (is (string= "CL-USER" (rplaca::listener-context-package-name context)))
      (is (null (rplaca::listener-context-directory-stack context)))
      (is (eq :eval (rplaca::listener-context-input-mode context))))))

(test listener-context-directory-transitions-are-immutable
  (let* ((first-directory (uiop:ensure-directory-pathname #P"/tmp/first/"))
         (second-directory (uiop:ensure-directory-pathname #P"/tmp/second/"))
         (initial (rplaca::make-listener-context))
         (first (rplaca::listener-context-push-directory
                 initial first-directory))
         (second (rplaca::listener-context-push-directory
                  first second-directory)))
    (is (null (rplaca::listener-context-directory-stack initial)))
    (is (equal (list first-directory)
               (rplaca::listener-context-directory-stack first)))
    (is (equal (list second-directory first-directory)
               (rplaca::listener-context-directory-stack second)))
    (multiple-value-bind (popped-context popped-directory)
        (rplaca::listener-context-pop-directory second)
      (is (equal second-directory popped-directory))
      (is (equal (list first-directory)
                 (rplaca::listener-context-directory-stack popped-context)))
      (is (equal (list second-directory first-directory)
                 (rplaca::listener-context-directory-stack second))))))

(test listener-context-package-transition-validates-without-global-mutation
  (let* ((initial (rplaca::make-listener-context :package-name "CL-USER"))
         (before *package*)
         (updated (rplaca::listener-context-set-package
                   initial "rplaca/tests")))
    (is (string= "CL-USER" (rplaca::listener-context-package-name initial)))
    (is (string= "RPLACA/TESTS"
                 (rplaca::listener-context-package-name updated)))
    (is (eq before *package*))
    (signals rplaca::unknown-listener-package
      (rplaca::listener-context-set-package initial "NO-SUCH-PACKAGE"))))

(test listener-context-input-mode-transition-validates
  (let* ((initial (rplaca::make-listener-context))
         (say (rplaca::listener-context-set-input-mode initial :say)))
    (is (eq :eval (rplaca::listener-context-input-mode initial)))
    (is (eq :say (rplaca::listener-context-input-mode say)))
    (signals error
      (rplaca::listener-context-set-input-mode initial :unknown))))

(test listener-context-empty-directory-pop-signals
  (signals rplaca::empty-listener-directory-stack
    (rplaca::listener-context-pop-directory
     (rplaca::make-listener-context))))

(test listener-shell-helper-is-bounded-and-preserves-exit-status
  (let* ((directory (truename "."))
         (success
           (rplaca::run-listener-shell-command
            "printf 'out'; printf 'err' >&2"
            directory :timeout 5))
         (nonzero
           (rplaca::run-listener-shell-command
            "printf 'bad' >&2; exit 7"
            directory :timeout 5))
         (bounded
           (rplaca::run-listener-shell-command
            "printf '123456789'"
            directory :timeout 5 :output-limit 5)))
    (is (= 0 (getf success :exit-code)))
    (is (string= "out" (getf success :stdout)))
    (is (string= "err" (getf success :stderr)))
    (is (string= (namestring (uiop:ensure-directory-pathname directory))
                 (getf success :directory)))
    (is (= 7 (getf nonzero :exit-code)))
    (is (string= "bad" (getf nonzero :stderr)))
    (is (= 5 (length (getf bounded :stdout))))
    (is-true (getf bounded :stdout-truncated-p))
    (signals error
      (rplaca::run-listener-shell-command "" directory :timeout 5))
    (signals error
      (rplaca::run-listener-shell-command nil directory :timeout 5))))
