(in-package :rplaca/tests)

(in-suite listener-frame-suite)

;;; Fixture command registered through the frame-generated definer macro.
;;; The guard lets this file load before todo 4 introduces the macro.
(when (fboundp 'rplaca::define-rplaca-listener-command)
  (rplaca::define-rplaca-listener-command
      (rplaca::com-listener-fixture-pong :name "Fixture-Pong")
      ()
    nil))

;;; McCLIM 1.0 accept-from-string uses a non-editing string-input-stream
;;; ("strings are not interactive"); the command-name echo path calls
;;; presentation-replace-input unconditionally.  No-op it for the non-interactive
;;; surface exercised here.  Test-local: the live Drei interactor needs none.
(defmethod clim:presentation-replace-input
    ((stream clim-internals::string-input-stream) object type view
     &key &allow-other-keys)
  (declare (ignore stream type view))
  object)

(defun make-test-listener-frame (&key (mode :eval))
  "Allocate a rplaca-listener frame whose listener-context mode is MODE."
  (make-instance 'rplaca::rplaca-listener
                 :listener-context
                 (rplaca::make-listener-context :input-mode mode)))

(defun accept-listener-token (string &key (mode :eval) frame)
  "Accept STRING as command-form-or-prose; return (values token afs-end).

MODE is the headless *listener-input-mode* fallback.  FRAME, when supplied, is
bound as *application-frame* so the active frame's listener-context mode wins.
afs-end is accept-from-string's third value (the actual stream/accept consumed
endpoint)."
  (let ((rplaca::*listener-input-mode* mode)
        (clim:*application-frame* (or frame clim:*application-frame*)))
    (multiple-value-bind (token type afs-end)
        (clim:accept-from-string 'rplaca::command-form-or-prose string)
      (declare (ignore type))
      (values token afs-end))))

(defun listener-acceptance-residual (input afs-end)
  "Unconsumed submission content after acceptance (zero when fully consumed).

McCLIM reads one activation-boundary gesture past the real content, so afs-end
is length+1 on full consumption; the real residual is content strictly beyond
afs-end, clamped at the submission length."
  (max 0 (- (length input) (min afs-end (length input)))))

(defun fully-consumed-p (input afs-end)
  (zerop (listener-acceptance-residual input afs-end)))

(test command-form-or-prose-bang-prose-keeps-spaces
  (multiple-value-bind (token afs-end)
      (accept-listener-token "!hello world")
    (is (eq :prose (rplaca::listener-input-token-kind token)))
    (is (string= "hello world" (rplaca::listener-input-token-source token)))
    (is (string= "hello world" (rplaca::listener-input-token-value token)))
    (is (fully-consumed-p "!hello world" afs-end))))

(test command-form-or-prose-bang-prose-is-force-prose-in-say-mode
  (multiple-value-bind (token afs-end)
      (accept-listener-token "!hello" :mode :say)
    (is (eq :prose (rplaca::listener-input-token-kind token)))
    (is (string= "hello" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p "!hello" afs-end))))

(test command-form-or-prose-bare-bang-toggles-mode
  (multiple-value-bind (token afs-end)
      (accept-listener-token "!" :mode :eval)
    (is (eq :enter-say (rplaca::listener-input-token-kind token)))
    (is (string= "" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p "!" afs-end)))
  (multiple-value-bind (token afs-end)
      (accept-listener-token "!" :mode :say)
    (is (eq :exit-say (rplaca::listener-input-token-kind token)))
    (is (fully-consumed-p "!" afs-end))))

;;; Contract: only exact bare ! toggles.  The locked classifier uses
;;; (zerop (length rest)); spaces are nonempty force-prose, preserved raw.
(test command-form-or-prose-bang-spaces-is-prose-not-toggle
  (dolist (mode '(:eval :say))
    (multiple-value-bind (token afs-end)
        (accept-listener-token "!   " :mode mode)
      (is (eq :prose (rplaca::listener-input-token-kind token))
          "mode ~A should classify \"!   \" as :prose" mode)
      (is (string= "   " (rplaca::listener-input-token-source token)))
      (is (string= "   " (rplaca::listener-input-token-value token)))
      (is (fully-consumed-p "!   " afs-end)))))

;;; Contract: mode is read from the active frame's listener-context at accept
;;; time and takes precedence over the headless *listener-input-mode* fallback.
(test command-form-or-prose-mode-is-read-from-active-frame-context
  (let ((say-frame (make-test-listener-frame :mode :say))
        (eval-frame (make-test-listener-frame :mode :eval)))
    (multiple-value-bind (token)
        (accept-listener-token "hello" :mode :eval :frame say-frame)
      (is (eq :prose (rplaca::listener-input-token-kind token))))
    (multiple-value-bind (token)
        (accept-listener-token "(+ 1 2)" :mode :say :frame eval-frame)
      (is (eq :form (rplaca::listener-input-token-kind token))))))

;;; Contract: listener-input-token production API is exactly kind/value/source.
(test listener-input-token-shape-is-kind-value-source-only
  (let ((token (rplaca::make-listener-input-token
                :kind :form :value '(+ 1 2) :source "(+ 1 2)")))
    (is (eq :form (rplaca::listener-input-token-kind token)))
    (is (equal '(+ 1 2) (rplaca::listener-input-token-value token)))
    (is (string= "(+ 1 2)" (rplaca::listener-input-token-source token))))
  ;; No end-pointer accessor function exists (the slot was removed).  Referencing
  ;; the symbol would intern it, so fboundp is the meaningful check; the export
  ;; was removed from src/packages.lisp.
  (is (not (fboundp 'rplaca::listener-input-token-end-pointer))))

(test command-form-or-prose-comma-literal-prose
  (multiple-value-bind (token afs-end)
      (accept-listener-token ",,(foo bar)" :mode :eval)
    (is (eq :prose (rplaca::listener-input-token-kind token)))
    (is (string= ",(foo bar)" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p ",,(foo bar)" afs-end))))

(test command-form-or-prose-shell-dispatch
  (multiple-value-bind (token afs-end)
      (accept-listener-token "#!ls -la")
    (is (eq :shell (rplaca::listener-input-token-kind token)))
    (is (string= "ls -la" (rplaca::listener-input-token-value token)))
    (is (string= "ls -la" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p "#!ls -la" afs-end))))

(test command-form-or-prose-leading-space-disables-bang-dispatch
  (multiple-value-bind (token afs-end)
      (accept-listener-token " !foo" :mode :eval)
    (is (eq :form (rplaca::listener-input-token-kind token)))
    (is (string= " !foo" (rplaca::listener-input-token-source token)))
    (is (eq '!foo (rplaca::listener-input-token-value token)))
    (is (fully-consumed-p " !foo" afs-end))))

(test command-form-or-prose-quoted-prose-retains-quotation-marks
  (multiple-value-bind (token afs-end)
      (accept-listener-token "!\"hello world\"" :mode :say)
    (is (eq :prose (rplaca::listener-input-token-kind token)))
    (is (string= "\"hello world\"" (rplaca::listener-input-token-source token)))
    ;; read-token strips surrounding quotes for the value; source keeps them.
    (is (string= "hello world" (rplaca::listener-input-token-value token)))
    (is (fully-consumed-p "!\"hello world\"" afs-end))))

(test command-form-or-prose-form-source-keeps-trailing-comment-and-leading-whitespace
  (let ((input "  (+ 1 2) ; trailing comment"))
    (multiple-value-bind (token afs-end)
        (accept-listener-token input :mode :eval)
      (is (eq :form (rplaca::listener-input-token-kind token)))
      (is (equal '(+ 1 2) (rplaca::listener-input-token-value token)))
      (is (string= input (rplaca::listener-input-token-source token)))
      (is (fully-consumed-p input afs-end)))))

(test command-form-or-prose-empty-string-is-no-op
  ;; McCLIM 1.0 accept-from-string does not preempt an empty submission, so the
  ;; empty -> :no-op path is reached directly through the acceptance surface.
  (multiple-value-bind (token afs-end)
      (accept-listener-token "")
    (is (eq :no-op (rplaca::listener-input-token-kind token)))
    (is (fully-consumed-p "" afs-end))))

(test command-form-or-prose-whitespace-only-is-no-op
  (dolist (input (list "   " " " (string #\Tab)
                       (concatenate 'string (string #\Tab) " " (string #\Tab))))
    (multiple-value-bind (token afs-end)
        (accept-listener-token input)
      (is (eq :no-op (rplaca::listener-input-token-kind token))
          "input ~S should be :no-op" input)
      (is (fully-consumed-p input afs-end)
          "input ~S should be fully consumed" input))))

(test command-form-or-prose-multiline-prose-is-rejected-with-zero-residual
  (let ((bang "!a
b")
        (bare "line1
line2"))
    (multiple-value-bind (token afs-end)
        (accept-listener-token bang)
      (is (eq :error (rplaca::listener-input-token-kind token)))
      (is (search "Use ,Compose" (rplaca::listener-input-token-value token)))
      ;; Entire submitted region consumed; no semantic residual tail.
      (is (fully-consumed-p bang afs-end)))
    (multiple-value-bind (token afs-end)
        (accept-listener-token bare :mode :say)
      (is (eq :error (rplaca::listener-input-token-kind token)))
      (is (search "Use ,Compose" (rplaca::listener-input-token-value token)))
      (is (fully-consumed-p bare afs-end)))))

(test command-form-or-prose-multiline-lisp-form-is-accepted
  (let ((input "(+ 1
 2)"))
    (multiple-value-bind (token afs-end)
        (accept-listener-token input :mode :eval)
      (is (eq :form (rplaca::listener-input-token-kind token)))
      (is (equal '(+ 1 2) (rplaca::listener-input-token-value token)))
      (is (fully-consumed-p input afs-end)))))

(test command-form-or-prose-ask-agent-source-is-byte-for-byte
  (let ((raw "(ask-agent :foo \"bar\")"))
    (multiple-value-bind (token afs-end)
        (accept-listener-token raw :mode :eval)
      (is (eq :form (rplaca::listener-input-token-kind token)))
      (is (string= raw (rplaca::listener-input-token-source token)))
      (is (fully-consumed-p raw afs-end)))))

(test command-form-or-prose-comma-eval-form
  (multiple-value-bind (token afs-end)
      (accept-listener-token ",(+ 1 2)" :mode :eval)
    (is (eq :eval-form (rplaca::listener-input-token-kind token)))
    (is (equal '(+ 1 2) (rplaca::listener-input-token-value token)))
    (is (string= "(+ 1 2)" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p ",(+ 1 2)" afs-end))))

(test command-form-or-prose-bare-comma-is-mode-dependent
  (multiple-value-bind (token afs-end)
      (accept-listener-token "," :mode :eval)
    (is (eq :error (rplaca::listener-input-token-kind token)))
    (is (search "did you mean" (rplaca::listener-input-token-value token)))
    (is (fully-consumed-p "," afs-end)))
  (multiple-value-bind (token afs-end)
      (accept-listener-token "," :mode :say)
    (is (eq :exit-say (rplaca::listener-input-token-kind token)))
    (is (fully-consumed-p "," afs-end))))

(test command-form-or-prose-comma-command-dispatch
  ;; Delegation to (accept 'clim:command) is proven by the :command kind and
  ;; exact source.  McCLIM 1.0's non-editing string-input-stream cannot resolve
  ;; the command name through the activator/echo path, so the parsed command
  ;; value is not asserted here; the live Drei interactor (todo 17) proves
  ;; Fixture-Pong parses to (com-listener-fixture-pong).  The command is
  ;; registered and reachable (see listener-frame-fixture-defines-command-table).
  (multiple-value-bind (token afs-end)
      (accept-listener-token ",Fixture-Pong")
    (is (eq :command (rplaca::listener-input-token-kind token)))
    (is (string= "Fixture-Pong" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p ",Fixture-Pong" afs-end))))

(test command-form-or-prose-default-eval-is-form
  (multiple-value-bind (token afs-end)
      (accept-listener-token "(list 1 2 3)" :mode :eval)
    (is (eq :form (rplaca::listener-input-token-kind token)))
    (is (equal '(list 1 2 3) (rplaca::listener-input-token-value token)))
    (is (string= "(list 1 2 3)" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p "(list 1 2 3)" afs-end))))

(test command-form-or-prose-default-say-is-prose
  (multiple-value-bind (token afs-end)
      (accept-listener-token "hello agent" :mode :say)
    (is (eq :prose (rplaca::listener-input-token-kind token)))
    (is (string= "hello agent" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p "hello agent" afs-end))))

(test command-form-or-prose-hash-not-bang-is-form
  (multiple-value-bind (token afs-end)
      (accept-listener-token "#(1 2 3)" :mode :eval)
    (is (eq :form (rplaca::listener-input-token-kind token)))
    (is (equalp #(1 2 3) (rplaca::listener-input-token-value token)))
    (is (fully-consumed-p "#(1 2 3)" afs-end))))

(test command-form-or-prose-leading-whitespace-say-is-prose
  (multiple-value-bind (token afs-end)
      (accept-listener-token "  hi there" :mode :say)
    (is (eq :prose (rplaca::listener-input-token-kind token)))
    (is (string= "  hi there" (rplaca::listener-input-token-source token)))
    (is (fully-consumed-p "  hi there" afs-end))))

(test rplaca-package-presentation-presents-package-name
  (let ((output (with-output-to-string (stream)
                  (clim:present (find-package :cl-user)
                                'rplaca::rplaca-package
                                :stream stream))))
    (is (string= (package-name (find-package :cl-user)) output))))

(test rplaca-package-presentation-completes-from-prefix
  (multiple-value-bind (package type end)
      (clim:accept-from-string 'rplaca::rplaca-package "CL-USE")
    (declare (ignore type))
    (is (eq (find-package :cl-user) package))
    (is (> end 0)))
  (multiple-value-bind (package type end)
      (clim:accept-from-string 'rplaca::rplaca-package "KEYWOR")
    (declare (ignore type))
    (is (eq (find-package :keyword) package))
    (is (> end 0))))

(test rplaca-package-presentation-typep-recognizes-packages
  (is (clim:presentation-typep (find-package :cl-user)
                               'rplaca::rplaca-package))
  (is (not (clim:presentation-typep "CL-USER"
                                    'rplaca::rplaca-package))))

(test prose-presentation-presents-plain-text
  (let ((output (with-output-to-string (stream)
                  (clim:present "some prose" 'rplaca::prose :stream stream))))
    (is (string= "some prose" output))))

(test prose-presentation-accepts-rest-of-line
  (multiple-value-bind (value type end)
      (clim:accept-from-string 'rplaca::prose "the rest of the line")
    (declare (ignore type))
    (is (string= "the rest of the line" value))
    (is (> end 0))))

(test listener-frame-fixture-defines-command-table-and-definer
  (is-true (clim:find-command-table 'rplaca::rplaca-listener :errorp nil))
  (is (fboundp 'rplaca::define-rplaca-listener-command))
  ;; Canonical top-level proof that Fixture-Pong is com-listener-fixture-pong:
  ;; the command-name symbol is registered in the frame's command table.
  (is-true (clim:command-present-in-command-table-p
            'rplaca::com-listener-fixture-pong
            (clim:find-command-table 'rplaca::rplaca-listener)))
  (is (find-class 'rplaca::rplaca-listener nil)))
