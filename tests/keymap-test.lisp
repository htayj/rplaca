(in-package :rplaca/tests)

(def-suite keymap-suite
  :description "Keymap tests"
  :in rplaca-suite)

(in-suite keymap-suite)

(test keymap-bind-and-lookup
  "Binding a key and looking it up returns the command."
  (let ((km (make-keymap :test)))
    (keymap-bind km #\a 'some-command)
    (is (eq 'some-command (keymap-lookup km #\a)))
    (is (null (keymap-lookup km #\b)))))

(test keymap-parent-chain-lookup
  "Lookup falls back to parent keymap."
  (let* ((parent (make-keymap :parent))
         (child (make-keymap :child :parent parent)))
    (keymap-bind parent #\a 'parent-command)
    (keymap-bind child #\b 'child-command)
    (is (eq 'child-command (keymap-lookup child #\b)))
    (is (eq 'parent-command (keymap-lookup child #\a)))
    (is (null (keymap-lookup parent #\b)))))

(test keymap-child-overrides-parent
  "Child bindings shadow parent bindings for the same key."
  (let* ((parent (make-keymap :parent))
         (child (make-keymap :child :parent parent)))
    (keymap-bind parent #\a 'parent-version)
    (keymap-bind child #\a 'child-version)
    (is (eq 'child-version (keymap-lookup child #\a)))
    (is (eq 'parent-version (keymap-lookup parent #\a)))))

(test default-keymap-readline-argument-yank-bindings
  "Default keymap includes readline-style argument yank keybindings."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::yank-previous-command-first-arg-command
          (keymap-lookup *default-keymap* '(:meta #\Em))))
  (is (eq 'rplaca::yank-previous-command-last-arg-command
          (keymap-lookup *default-keymap* '(:meta #\.))))
  (is (eq 'rplaca::yank-previous-command-last-arg-command
          (keymap-lookup *default-keymap* '(:meta #\_)))))

(test default-keymap-ctrl-d-binding
  "Default keymap binds Ctrl+d to forward delete."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::delete-char-forward-command
          (keymap-lookup *default-keymap* (code-char 4)))))

(test default-keymap-redraw-screen-binding
  "Default keymap binds Ctrl+l to request a full redraw."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::redraw-screen-command
          (keymap-lookup *default-keymap* (code-char 12)))))

(test default-keymap-escape-stop-llm-binding
  "Default keymap binds Escape to stopping the active LLM response."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::stop-llm-command
          (keymap-lookup *default-keymap* #\Esc))))

(test default-keymap-backspace-bindings
  "Default keymap binds backspace variants to backward char delete."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::delete-char-backward-command
          (keymap-lookup *default-keymap* #\Backspace)))
  (is (eq 'rplaca::delete-char-backward-command
          (keymap-lookup *default-keymap* #\Rubout)))
  (is (eq 'rplaca::delete-char-backward-command
          (keymap-lookup *default-keymap* :backspace))))

(test default-keymap-backward-kill-word-backspace-bindings
  "Default keymap binds C-Backspace and M-Backspace to backward-kill-word."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:meta #\Backspace))))
  (is (eq 'rplaca::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:meta #\Rubout))))
  (is (eq 'rplaca::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:meta :backspace))))
  (is (null (keymap-lookup *default-keymap* '(:alt :backspace))))
  (is (eq 'rplaca::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl #\Backspace))))
  (is (eq 'rplaca::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl #\Rubout))))
  (is (eq 'rplaca::backward-kill-word-command
          (keymap-lookup *default-keymap* '(:ctrl :backspace)))))



(test default-keymap-buffer-selector-binding
  "Default keymap binds C-x b and C-x C-b to the minibuffer buffer selector."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::minibuffer-select-buffer-command
          (keymap-lookup *default-keymap* (list :ctrl-x (code-char 2)))))
  (is (eq 'rplaca::minibuffer-select-buffer-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\b)))))

(test default-keymap-project-bindings
  "Default keymap binds project selection and project file opening."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::open-project-file-command
          (keymap-lookup *default-keymap* (list :ctrl-x (code-char 6)))))
  (is (eq 'rplaca::minibuffer-select-project-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\p))))
  (is (eq 'rplaca::load-session-command
          (keymap-lookup *default-keymap* (list :ctrl-x (code-char 18))))))

(test default-keymap-listener-binding
  "Default keymap binds C-x l to the native McCLIM Listener command."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::new-listener-buffer-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\l)))))

(test new-listener-buffer-command-opens-native-mcclim-listener
  "The historical command name launches McCLIM's real Listener frame."
  (let ((captured-arguments nil))
    (with-mcclim-test-function-override
        (clim-listener:run-listener (&rest arguments)
          (setf captured-arguments arguments)
          (values :listener-process :listener-frame))
      (is (eq :listener-frame
              (rplaca::new-listener-buffer-command nil))))
    (is-true (getf captured-arguments :new-process))
    (is (string= "RPLACA McCLIM Listener"
                 (getf captured-arguments :process-name)))
    (is (string= (rplaca::listener-default-package-name)
                 (getf captured-arguments :package)))))

(test file-keymap-emacs-editor-bindings
  "File buffers have Emacs-style editor bindings over the global keymap."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::insert-newline-command
          (keymap-lookup rplaca::*file-keymap* #\Return)))
  (is (eq 'rplaca::insert-tab-command
          (keymap-lookup rplaca::*file-keymap* #\Tab)))
  (is (eq 'rplaca::previous-line-command
          (keymap-lookup rplaca::*file-keymap* (code-char 16))))
  (is (eq 'rplaca::next-line-command
          (keymap-lookup rplaca::*file-keymap* (code-char 14))))
  (is (eq 'rplaca::search-forward-command
          (keymap-lookup rplaca::*file-keymap* (code-char 19))))
  (is (eq 'rplaca::set-mark-command
          (keymap-lookup rplaca::*file-keymap* (code-char 0))))
  (is (eq 'rplaca::kill-region-command
          (keymap-lookup rplaca::*file-keymap* (code-char 23))))
  (is (eq 'rplaca::copy-region-command
          (keymap-lookup rplaca::*file-keymap* '(:meta #\w))))
  (is (eq 'rplaca::save-session-command
          (keymap-lookup rplaca::*file-keymap*
                         (list :ctrl-x (code-char 19)))))
  (is (eq 'rplaca::write-project-file-as-command
          (keymap-lookup rplaca::*file-keymap*
                         (list :ctrl-x (code-char 23)))))
  (is (eq 'rplaca::revert-file-buffer-command
          (keymap-lookup rplaca::*file-keymap*
                         (list :ctrl-x (code-char 22))))))

(test default-keymap-toggle-tool-results-uses-c-c-prefix
  "Toggle tool results stays under C-c; C-x t is session-tree navigation."
  (rplaca::init-default-keymap)
  ;; C-c t should be bound
  (is (eq 'rplaca::toggle-tool-results-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\t))))
  (is (eq 'rplaca::session-tree-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\t))))
  (is (eq 'rplaca::fork-session-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\T)))))

(test default-keymap-toggle-reasoning-output-uses-c-c-prefix
  "Toggle reasoning output is bound under C-c (mode-specific), not C-x."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::toggle-reasoning-output-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\V))))
  (is (eq 'rplaca::describe-variable-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\v))))
  (is (null (keymap-lookup *default-keymap* '(:ctrl-x #\V)))))

(test default-keymap-toggle-metadata-output-uses-c-c-prefix
  "Toggle metadata output is bound under C-c (mode-specific), not C-x."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::toggle-metadata-output-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\I))))
  (is (null (keymap-lookup *default-keymap* '(:ctrl-x #\I)))))

(test default-keymap-compaction-binding
  "Manual compaction is bound under the chat-mode C-c prefix."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::compact-buffer-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\c)))))

(test default-keymap-agent-selector-binding
  "Default keymap binds C-c A to the minibuffer agent selector."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::minibuffer-select-agent-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\A)))))

(test default-keymap-skill-bindings
  "Default keymap binds skill insertion and toggling under C-c."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::minibuffer-insert-skill-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\s))))
  (is (eq 'rplaca::minibuffer-toggle-skill-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\S)))))

(test default-keymap-model-selector-binding
  "Default keymap binds C-c C-m (C-c Return) to the minibuffer model selector,
and C-c M (capital M) to its compatibility command alias."
  (rplaca::init-default-keymap)
  ;; C-c C-m -> minibuffer model selector (new)
  (is (eq 'rplaca::minibuffer-select-model-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\Return))))
  ;; Some terminals send #\Newline (LF) for Enter
  (is (eq 'rplaca::minibuffer-select-model-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\Newline))))
  ;; C-c M (capital M) -> compatibility command alias
  (is (eq 'rplaca::select-model-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\M)))))

(test default-keymap-think-selector-binding
  "Default keymap binds C-c C-r to the minibuffer think selector,
and C-c R to its compatibility command alias."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::minibuffer-select-think-level-command
          (keymap-lookup *default-keymap* (list :ctrl-c (code-char 18)))))
  (is (eq 'rplaca::select-think-level-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\R)))))

(test default-keymap-describe-function-binding
  "Default keymap binds C-c f to describe-function-command."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::describe-function-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\f)))))

(test default-keymap-appearance-bindings-preserve-neighboring-commands
  "Uppercase F opens appearance while established lowercase and global keys remain."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::customize-appearance-command
          (keymap-lookup *default-keymap* '(:ctrl-h #\F))))
  (is (eq 'rplaca::customize-appearance-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\F))))
  (is (eq 'rplaca::describe-function-command
          (keymap-lookup *default-keymap* '(:ctrl-h #\f))))
  (is (eq 'rplaca::describe-function-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\f))))
  (is (eq 'rplaca::toggle-tool-results-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\t))))
  (is (eq 'rplaca::font-editor-command
          (keymap-lookup *default-keymap* '(:ctrl-x #\F)))))

(test default-keymap-describe-bindings-binding
  "Default keymap binds C-c b to describe-bindings-command."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::describe-bindings-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\b)))))

(test default-keymap-describe-variable-binding
  "Default keymap binds C-c v to describe-variable-command."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::describe-variable-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\v)))))

(test default-keymap-describe-type-binding
  "Default keymap binds C-c T (capital T) to describe-type-command."
  (rplaca::init-default-keymap)
  (is (eq 'rplaca::describe-type-command
          (keymap-lookup *default-keymap* '(:ctrl-c #\T)))))
