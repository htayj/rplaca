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

;;; ===========================================================================
;;; Todo 5: single-interactor frame, dual layouts, output mapping, assistant/
;;; detail state, lifecycle, and startup seams.
;;; ===========================================================================

(defmacro with-listener-function-override ((name lambda-list &body impl) &body body)
  "Temporarily replace NAME during a serial listener-frame test."
  (let ((original (gensym "ORIGINAL")))
    `(let ((,original (symbol-function ',name)))
       (unwind-protect
            (progn (setf (symbol-function ',name)
                         (lambda ,lambda-list ,@impl))
                   ,@body)
         (setf (symbol-function ',name) ,original)))))

(defun listener-flatten (tree)
  (typecase tree
    (null nil)
    (atom (list tree))
    (t (append (listener-flatten (car tree)) (listener-flatten (cdr tree))))))

(defun listener-layout-pane-names (frame layout-name)
  "Return pane-name symbols referenced by LAYOUT-NAME in FRAME's stored layout
declaration (test-only introspection of McCLIM layout metadata)."
  (let ((entry (assoc layout-name
                      (slot-value frame 'clim-internals::layouts)
                      :test #'eq)))
    (remove-duplicates
     (remove-if (lambda (s)
                  (or (not (symbolp s))
                      (null s)
                      (member s '(clim:vertically clim:horizontally
                                  clim:outlining clim:tabling clim:spacing)
                              :test #'eq)))
                (listener-flatten (rest entry)))
     :test #'eq)))

(defun make-test-conversation-buffer (&key (working-directory (truename "."))
                                          (package-name "CL-USER"))
  "Build a sessionless persistent conversation buffer for frame tests."
  (rplaca::make-buffer "listener-test"
                       :agent-name "tester"
                       :working-directory working-directory
                       :kind :chat
                       :session-persistence-mode :persistent
                       :session nil))

(defun make-test-listener-frame* (&rest initargs &key &allow-other-keys)
  (apply #'clim:make-application-frame 'rplaca::rplaca-listener initargs))

(test rplaca-listener-frame-has-single-interactor-and-detail-panes
  (let ((frame (make-test-listener-frame*
                :conversation-buffer (make-test-conversation-buffer))))
    (is (find 'rplaca::interactor
              (listener-layout-pane-names frame 'rplaca::listener-only)))
    (is (find 'rplaca::pointer-doc
              (listener-layout-pane-names frame 'rplaca::listener-only)))
    (is (find 'rplaca::wholine
              (listener-layout-pane-names frame 'rplaca::listener-only)))))

(test rplaca-listener-default-layout-excludes-details
  (let ((frame (make-test-listener-frame*
                :conversation-buffer (make-test-conversation-buffer))))
    (is (eq 'rplaca::listener-only (clim:frame-current-layout frame)))
    (is (not (find 'rplaca::details
                   (listener-layout-pane-names frame 'rplaca::listener-only))))
    (is (find 'rplaca::details
              (listener-layout-pane-names frame 'rplaca::listener+details)))
    ;; listener+details reuses the same interactor/pointer-doc/wholine.
    (dolist (pane '(rplaca::interactor rplaca::pointer-doc rplaca::wholine))
      (is (find pane (listener-layout-pane-names frame 'rplaca::listener+details))))))

(test rplaca-listener-standard-and-error-output-always-map-to-interactor
  (let ((frame (make-test-listener-frame*
                :conversation-buffer (make-test-conversation-buffer)))
        (sentinel-interactor (cons :sentinel nil)))
    (with-listener-function-override
        (clim:find-pane-named (requested-frame pane-name)
          (if (and (eq requested-frame frame)
                   (eq pane-name 'rplaca::interactor))
              sentinel-interactor
              nil))
      (is (eq sentinel-interactor (clim:frame-standard-output frame)))
      (is (eq sentinel-interactor (clim:frame-error-output frame))))))

(test rplaca-listener-frame-owns-scoped-state-slots
  (let* ((profile (rplaca::make-appearance-profile))
         (buf (make-test-conversation-buffer))
         (frame (make-test-listener-frame*
                 :conversation-buffer buf
                 :pending-session-name "later"
                 :session-label "Label"
                 :appearance-profile profile)))
    (is (eq buf (rplaca::rplaca-listener-conversation-buffer frame)))
    (is (string= "later" (rplaca::rplaca-listener-pending-session-name frame)))
    (is (string= "Label" (rplaca::rplaca-listener-session-label frame)))
    (is (eq profile (rplaca::rplaca-listener-appearance-profile frame)))
    (is (eq :live (rplaca::rplaca-listener-liveness frame)))))

(test rplaca-listener-prompt-is-package-and-mode-aware
  (let ((eval-frame (make-test-listener-frame*
                     :conversation-buffer (make-test-conversation-buffer)
                     :listener-context
                     (rplaca::make-listener-context
                      :package-name "CL-USER" :input-mode :eval)))
        (say-frame (make-test-listener-frame*
                    :conversation-buffer (make-test-conversation-buffer)
                    :listener-context
                    (rplaca::make-listener-context
                     :package-name "CL-USER" :input-mode :say))))
    (is (string= "CL-USER> "
                 (with-output-to-string (s)
                   (rplaca::listener-print-prompt s eval-frame))))
    (is (string= "CL-USER!> "
                 (with-output-to-string (s)
                   (rplaca::listener-print-prompt s say-frame))))))

(test assistant-turn-object-carries-final-facets
  (let ((turn (rplaca::make-assistant-turn
               :primary-text "hello"
               :tool-uses (list (list :name "read"))
               :reasoning (list "thought")
               :metadata (list :model "m")
               :artifact-refs (list "art-1")
               :media-refs (list "media-1")
               :inspect-payload (list :inspect t)
               :status :complete)))
    (is (string= "hello" (rplaca::assistant-turn-primary-text turn)))
    (is (equal '((:name "read")) (rplaca::assistant-turn-tool-uses turn)))
    (is (eq :complete (rplaca::assistant-turn-status turn))))
  (let ((facet (rplaca::make-turn-facet :kind :tools)))
    (is (eq :tools (rplaca::turn-facet-kind facet)))))

(test assistant-turn-and-turn-facet-presentation-types-exist
  (is (find-class 'rplaca::assistant-turn nil))
  (is (find-class 'rplaca::turn-facet nil))
  (let ((turn (rplaca::make-assistant-turn :primary-text "body")))
    (is (string= "body"
                 (with-output-to-string (s)
                   (clim:present turn 'rplaca::assistant-turn :stream s))))))

(test rplaca-listener-selected-detail-state-helpers-store-and-clear
  (let ((frame (make-test-listener-frame*
                :conversation-buffer (make-test-conversation-buffer)))
        (turn (rplaca::make-assistant-turn :primary-text "t")))
    (is (null (rplaca::rplaca-listener-selected-detail frame)))
    (rplaca::set-rplaca-listener-selected-detail frame turn :reasoning)
    (is (eq turn (car (rplaca::rplaca-listener-selected-detail frame))))
    (is (eq :reasoning (cdr (rplaca::rplaca-listener-selected-detail frame))))
    (rplaca::clear-rplaca-listener-selected-detail frame)
    (is (null (rplaca::rplaca-listener-selected-detail frame)))))

(test display-turn-details-is-pure-and-reads-selected-state
  (let ((frame (make-test-listener-frame*
                :conversation-buffer (make-test-conversation-buffer)))
        (turn (rplaca::make-assistant-turn
               :primary-text "body"
               :tool-uses (list (list :name "read"))
               :reasoning (list "because"))))
    ;; No selected detail: rendering is a no-op that does not mutate state.
    (with-output-to-string (s) (rplaca::display-turn-details frame s))
    (is (null (rplaca::rplaca-listener-selected-detail frame)))
    (rplaca::set-rplaca-listener-selected-detail frame turn :tools)
    ;; With a selected tools facet, rendering emits content without mutating.
    (let ((text (with-output-to-string (s) (rplaca::display-turn-details frame s))))
      (is (search "read" text)))
    (is (eq turn (car (rplaca::rplaca-listener-selected-detail frame))))))

(test make-initial-chat-buffer-is-eager-sessionless-and-persistent
  (let ((rplaca::*buffer-ring* nil)
        (rplaca::*initial-buffer-hook* nil)
        (rplaca::*suppress-session-autosave* t)
        (rplaca::*sessions-dir* (ensure-directories-exist
                                 #P"/tmp/rplaca-5-sessionless/")))
    (let ((buf (rplaca::make-initial-chat-buffer "sessionless-test" "tester"
                                                 :working-directory (truename "."))))
      (is (null (rplaca::buffer-session buf)))
      (is (eq :chat (rplaca::buffer-kind buf)))
      (is (rplaca::buffer-persistent-session-p buf))
      (is (= 1 (length rplaca::*buffer-ring*)))
      (is (eq buf (first rplaca::*buffer-ring*))))))

(test run-rplaca-listener-owns-frame-and-disowns-on-return
  (let ((buffer (make-test-conversation-buffer))
        (captured nil)
        (disowned nil))
    (with-listener-function-override
        (clim:make-application-frame (class &rest initargs)
          (is (eq 'rplaca::rplaca-listener class))
          (setf captured initargs)
          :listener-test-frame)
      (with-listener-function-override
          (clim:run-frame-top-level (frame)
            (is (eq :listener-test-frame frame)))
      (with-listener-function-override
          (clim:disown-frame (manager frame)
            (declare (ignore manager frame))
            (setf disowned t))
        (let ((result (rplaca::run-rplaca-listener
                       buffer :window-title "L"
                              :appearance-profile (rplaca::make-appearance-profile))))
          (is (eq :listener-test-frame result))
          (is (eq buffer (getf captured :conversation-buffer)))
          (is (string= "L" (getf captured :pretty-name)))
          (is (typep (getf captured :appearance-profile) 'rplaca::appearance-profile))
          (is-true disowned)))))))

(test run-rplaca-listener-supports-new-process
  (let ((buffer (make-test-conversation-buffer))
        (process :made-process))
    (with-listener-function-override
        (clim:make-application-frame (class &rest initargs)
          (declare (ignore class initargs))
          :listener-process-frame)
      (with-listener-function-override
          (clim:run-frame-top-level (frame) (declare (ignore frame)))
        (with-listener-function-override
            (clim:disown-frame (manager frame) (declare (ignore manager frame)))
          (with-listener-function-override
              (clim-sys:make-process (function &key name)
                (declare (ignore function name))
                process)
            (multiple-value-bind (proc frame)
                (rplaca::run-rplaca-listener buffer :new-process t)
              (is (eq process proc))
              (is (eq :listener-process-frame frame)))))))))

(test listener-frame-cleanup-marks-dead-and-disposes-owned-buffer
  (let* ((buffer (make-test-conversation-buffer))
         (frame (make-test-listener-frame* :conversation-buffer buffer)))
    (setf (rplaca::rplaca-listener-liveness frame) :live)
    (rplaca::listener-frame-cleanup frame nil)
    (is (eq :dead (rplaca::rplaca-listener-liveness frame)))
    (is (rplaca::buffer-disposed-p buffer))))

(test listener-frame-cleanup-removes-wake-hook-and-ignores-late-wakes
  (let* ((buffer (make-test-conversation-buffer))
         (frame (make-test-listener-frame* :conversation-buffer buffer))
         (hook-called 0)
         (hook (lambda (buf reason)
                 (declare (ignore buf reason))
                 (incf hook-called))))
    (rplaca::add-hook 'rplaca::*buffer-display-wakeup-hook* hook :append t)
    (unwind-protect
         (progn
           (setf (rplaca::rplaca-listener-liveness frame) :live)
           (is (member hook rplaca::*buffer-display-wakeup-hook*))
           (rplaca::listener-frame-cleanup frame hook)
           ;; The hook is retired; a late wake cannot reach the dead frame.
           (is (not (member hook rplaca::*buffer-display-wakeup-hook*)))
           (is (= 0 hook-called)))
      (rplaca::remove-hook 'rplaca::*buffer-display-wakeup-hook* hook))
    (is (eq :dead (rplaca::rplaca-listener-liveness frame)))))

(test rplaca-main-run-path-invokes-run-rplaca-listener
  (let ((invoked nil)
        (captured nil))
    (with-listener-function-override
        (rplaca::parse-rplaca-args ())
      (with-listener-function-override
          (rplaca::initialize-rplaca-runtime ())
        (with-listener-function-override
            (rplaca::ensure-scratch-buffer ())
          (with-listener-function-override
              (rplaca::resolve-startup-appearance-profile ()
                (rplaca::make-appearance-profile))
            (with-listener-function-override
                (rplaca::run-rplaca-listener (buffer &rest keys &key &allow-other-keys)
                  (setf invoked t
                        captured (list :buffer buffer :keys keys))
                  buffer)
              (let ((buf (rplaca::rplaca-main :session-name "listener-main"
                                              :agent-name "tester"
                                              :window-title "Listener"
                                              :working-directory (truename "."))))
                (is-true invoked)
                (is (eq buf (getf captured :buffer)))
                (is (string= "Listener" (getf (getf captured :keys) :window-title)))))))))))

;;; ===========================================================================
;;; Todo-5 fix: construction-time pane appearance snapshot, metadata plist, and
;;; diagnostic-free startup signature.
;;; ===========================================================================

(test listener-pane-appearance-snapshot-resolves-all-four-panes
  (let* ((profile (rplaca::make-appearance-profile :selected-theme :dark))
         (catalog (rplaca::make-classic-appearance-catalog))
         (snapshot (rplaca::listener-resolve-pane-appearance-initargs profile catalog)))
    (is (= 4 (length snapshot)))
    (dolist (pane '(rplaca::interactor rplaca::pointer-doc
                                      rplaca::wholine rplaca::details))
      (let ((initargs (cdr (assoc pane snapshot :test #'eq))))
        (is-true initargs "pane ~A has no appearance initargs" pane)
        (is-true (getf initargs :foreground)
                 "pane ~A missing :foreground initarg" pane)
        (is (typep (getf initargs :foreground) 'clim:color)
            "pane ~A foreground is not a CLIM color" pane)))))

(test listener-pane-appearance-profile-override-participates
  (let* ((catalog (rplaca::make-classic-appearance-catalog))
         (base (rplaca::listener-resolve-pane-appearance-initargs
                (rplaca::make-appearance-profile :selected-theme :dark) catalog))
         (override-profile
           (rplaca::make-appearance-profile
            :selected-theme :dark
            :role-overrides
            (list (cons :default-text
                        (rplaca::make-appearance-role-style
                         :foreground-ink
                         (rplaca::make-appearance-ink-spec
                          :foreground '(:rgb 1 0 0)))))))
         (over (rplaca::listener-resolve-pane-appearance-initargs
                override-profile catalog)))
    ;; interactor uses the :default-text content role; the override changes it.
    (let ((base-fg (getf (cdr (assoc 'rplaca::interactor base)) :foreground))
          (over-fg (getf (cdr (assoc 'rplaca::interactor over)) :foreground)))
      (is (not (equal (multiple-value-list (clim:color-rgb base-fg))
                      (multiple-value-list (clim:color-rgb over-fg))))))))

(test rplaca-listener-frame-pane-appearance-snapshot-is-resolved-once
  (let ((rplaca::*package-appearance-catalog* (rplaca::make-classic-appearance-catalog)))
    (let ((frame (make-test-listener-frame*
                  :conversation-buffer (make-test-conversation-buffer)
                  :appearance-profile
                  (rplaca::make-appearance-profile :selected-theme :dark))))
      ;; The snapshot is an immutable slot resolved at construction; reading it
      ;; twice returns the same object, and the per-pane reader is stable too.
      (let ((snap-a (rplaca::rplaca-listener-pane-appearance-snapshot frame))
            (snap-b (rplaca::rplaca-listener-pane-appearance-snapshot frame)))
        (is (eq snap-a snap-b)))
      (let ((args-a (rplaca::listener-pane-appearance-initargs
                     frame 'rplaca::interactor))
            (args-b (rplaca::listener-pane-appearance-initargs
                     frame 'rplaca::interactor)))
        (is (eq args-a args-b)))
      (is (typep (getf (rplaca::listener-pane-appearance-initargs
                        frame 'rplaca::details) :foreground)
                 'clim:color)))))

(test listener-pane-appearance-initargs-use-exported-clim-values
  (let* ((profile (rplaca::make-appearance-profile :selected-theme :dark))
         (catalog (rplaca::make-classic-appearance-catalog))
         (snapshot (rplaca::listener-resolve-pane-appearance-initargs profile catalog)))
    ;; background and text-style, when present, are exported CLIM values, not
    ;; medium objects or internal appearance structs.
    (dolist (pane '(rplaca::interactor rplaca::wholine))
      (let ((initargs (cdr (assoc pane snapshot :test #'eq))))
        (let ((bg (getf initargs :background)))
          (when bg (is (typep bg 'clim:color))))
        (let ((ts (getf initargs :text-style)))
          (when ts (is (typep ts 'clim:text-style))))))))

(test display-turn-details-metadata-renders-proper-plist-pairs
  (let ((frame (make-test-listener-frame*
                :conversation-buffer (make-test-conversation-buffer)))
        (turn (rplaca::make-assistant-turn
               :metadata (list :model "m" :tokens 5))))
    (rplaca::set-rplaca-listener-selected-detail frame turn :metadata)
    (let ((text (with-output-to-string (s) (rplaca::display-turn-details frame s))))
      (is (search "MODEL = m" text))
      (is (search "TOKENS = 5" text))
      ;; The prior plist-tail bug printed "MODEL = (m TOKENS 5)".
      (is (not (search "(m TOKENS 5)" text))))))

(test display-turn-details-metadata-malformed-plist-signals
  (dolist (bad (list (list :model)                 ; odd length
                     (cons :model (cons "m" 5)))) ; improper tail
    (let ((frame (make-test-listener-frame*
                  :conversation-buffer (make-test-conversation-buffer)))
          (turn (rplaca::make-assistant-turn :metadata bad)))
      (rplaca::set-rplaca-listener-selected-detail frame turn :metadata)
      (signals error
        (with-output-to-string (s) (rplaca::display-turn-details frame s))))))

(test rplaca-main-run-path-and-signature-emit-no-fatal-diagnostic
  ;; All five keywords exercised; the launcher is fully stubbed so no real
  ;; startup subsystem runs.  :run-frame nil suppresses the launcher while the
  ;; public signature and startup hooks remain intact.
  (let ((invoked nil)
        (captured nil))
    (flet ((with-stubbed-startup (thunk)
             (with-listener-function-override
                 (rplaca::parse-rplaca-args ())
               (with-listener-function-override
                   (rplaca::initialize-rplaca-runtime ())
                 (with-listener-function-override
                     (rplaca::ensure-scratch-buffer ())
                   (with-listener-function-override
                       (rplaca::resolve-startup-appearance-profile ()
                         (rplaca::make-appearance-profile))
                     (with-listener-function-override
                         (rplaca::run-rplaca-listener
                             (buffer &rest keys &key &allow-other-keys)
                           (setf invoked t
                                 captured (list :buffer buffer :keys keys))
                           buffer)
                       (funcall thunk))))))))
      ;; run-frame t with all five keywords invokes run-rplaca-listener.
      (setf invoked nil captured nil)
      (with-stubbed-startup
          (lambda ()
            (rplaca::rplaca-main :session-name "s1"
                                 :agent-name "a1"
                                 :window-title "T1"
                                 :working-directory (truename ".")
                                 :run-frame t)))
      (is-true invoked)
      (is (string= "T1" (getf (getf captured :keys) :window-title)))
      ;; run-frame nil returns the buffer and suppresses the launcher.
      (setf invoked nil captured nil)
      (let ((buf (with-stubbed-startup
                    (lambda ()
                      (rplaca::rplaca-main :session-name "s2"
                                           :agent-name "a2"
                                           :window-title "T2"
                                           :working-directory (truename ".")
                                           :run-frame nil)))))
        (is-false invoked)
        (is (string= "s2" (rplaca::buffer-name buf)))))))

;;; ===========================================================================
;;; Todo-5 P0 adoption fix: pane specs must receive the generated constructor's
;;; lexical frame, not an unbound package-captured symbol.
;;; ===========================================================================

(defclass listener-test-fake-pane ()
  ((clim-internals::name :reader listener-test-fake-pane-name))
  (:documentation "Stand-in pane for generated-constructor tests; coerce-pane-name
sets its CLIM-INTERNALS::NAME slot via SLOT-VALUE."))

(test rplaca-listener-pane-specs-receive-the-generated-constructor-frame
  "Drive the ACTUAL generated panes-constructor (the same lambda the frame
manager funcalls at adoption) and verify every pane factory call receives the
real frame AND passes an existing exported CLIM pane-type class."
  (let ((rplaca::*package-appearance-catalog* (rplaca::make-classic-appearance-catalog))
        (frame (clim:make-application-frame
                'rplaca::rplaca-listener
                :conversation-buffer (make-test-conversation-buffer)
                :appearance-profile (rplaca::make-appearance-profile :selected-theme :dark)))
        (seen nil))
    (with-listener-function-override
        (rplaca::make-rplaca-listener-pane
            (pane-name pane-type display-function frame-arg)
          (push (list pane-name pane-type display-function frame-arg) seen)
          (make-instance 'listener-test-fake-pane))
      (funcall (clim-internals::frame-panes-constructor frame) nil frame))
    (is (= 4 (length seen)))
    (is (every (lambda (entry) (eq frame (fourth entry))) seen)
        "every pane factory call must receive the actual frame")
    ;; Each pane-type must name an existing exported CLIM class.
    (dolist (entry seen)
      (is (find-class (second entry) nil)
          "pane ~A type ~S is not an existing class" (first entry) (second entry)))
    ;; Exact expected pane-name -> pane-type mapping.
    (let ((expected '((rplaca::interactor . clim:interactor-pane)
                      (rplaca::pointer-doc . clim:pointer-documentation-pane)
                      (rplaca::wholine . clim:application-pane)
                      (rplaca::details . clim:application-pane))))
      (dolist (entry seen)
        (let ((want (cdr (assoc (first entry) expected :test #'eq))))
          (is (eq want (second entry))
              "pane ~A: expected type ~A, got ~A" (first entry) want (second entry)))))
    (dolist (pane '(rplaca::interactor rplaca::pointer-doc
                                  rplaca::wholine rplaca::details))
      (is (assoc pane (mapcar (lambda (e) (cons (first e) t)) seen) :test #'eq)
          "pane ~A was not constructed" pane))))

;;; ===========================================================================
;;; Todo-5 pane identity: the same named pane objects survive layout
;;; regeneration so interactor output history is never lost.
;;; ===========================================================================

(defun call-pane-constructor (frame)
  "Call the actual generated panes-constructor and return the pane alist."
  (funcall (clim-internals::frame-panes-constructor frame) nil frame))

(defun with-stubbed-pane-construction (thunk)
  "Run THUNK with clim:make-clim-stream-pane stubbed to return fake panes.
The real factory (make-rplaca-listener-pane) runs; only the CLIM pane creation
is stubbed so no port/graft is needed."
  (let ((n 0))
    (with-listener-function-override
        (clim:make-clim-stream-pane (&rest args &key &allow-other-keys)
          (declare (ignore args))
          (incf n)
          (make-instance 'listener-test-fake-pane))
      (funcall thunk n))))

(test rplaca-listener-pane-identity-persists-across-layout-regeneration
  "The same named interactor object must survive layout regeneration (the
McCLIM layout-switch path that clears frame-panes-for-layout and rebuilds)."
  (let ((rplaca::*package-appearance-catalog* (rplaca::make-classic-appearance-catalog))
        (frame (clim:make-application-frame
                'rplaca::rplaca-listener
                :conversation-buffer (make-test-conversation-buffer)
                :appearance-profile (rplaca::make-appearance-profile :selected-theme :dark))))
    (with-stubbed-pane-construction
        (lambda (make-count)
          ;; First generation: factory creates panes.
          (let* ((panes-1 (call-pane-constructor frame))
                 (inter-1 (cdr (assoc 'rplaca::interactor panes-1 :test #'eq))))
            (is-true inter-1)
            ;; Simulate layout switch: McCLIM clears frame-panes-for-layout.
            (setf (clim-internals::frame-panes-for-layout frame) nil)
            ;; Second generation: factory must return the SAME pane objects.
            (let* ((panes-2 (call-pane-constructor frame))
                   (inter-2 (cdr (assoc 'rplaca::interactor panes-2 :test #'eq))))
              (is (eq inter-1 inter-2)
                  "interactor identity must persist across regeneration")
              (dolist (name '(rplaca::interactor rplaca::pointer-doc
                                            rplaca::wholine rplaca::details))
                (is (eq (cdr (assoc name panes-1 :test #'eq))
                        (cdr (assoc name panes-2 :test #'eq)))
                    "pane ~A identity must persist" name))))))))

(test rplaca-listener-pane-cache-is-per-frame
  "Two frames must not share pane objects; caching is per-frame only."
  (let ((rplaca::*package-appearance-catalog* (rplaca::make-classic-appearance-catalog))
        (frame-a (clim:make-application-frame
                  'rplaca::rplaca-listener
                  :conversation-buffer (make-test-conversation-buffer)
                  :appearance-profile (rplaca::make-appearance-profile :selected-theme :dark)))
        (frame-b (clim:make-application-frame
                  'rplaca::rplaca-listener
                  :conversation-buffer (make-test-conversation-buffer)
                  :appearance-profile (rplaca::make-appearance-profile :selected-theme :dark))))
    (with-stubbed-pane-construction
        (lambda (make-count)
          (let* ((panes-a (call-pane-constructor frame-a))
                 (panes-b (call-pane-constructor frame-b))
                 (inter-a (cdr (assoc 'rplaca::interactor panes-a :test #'eq)))
                 (inter-b (cdr (assoc 'rplaca::interactor panes-b :test #'eq))))
            (is (not (eq inter-a inter-b))
                "two frames must not share the interactor pane object"))))))
