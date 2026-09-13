(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Keymap
;;; --------------------------------------------------------------------------

(defclass keymap ()
  ((name     :initarg :name
             :reader keymap-name
             :type keyword
             :documentation "Keyword name identifying this keymap (e.g. :default).")
   (bindings :initarg :bindings
             :reader keymap-bindings
             :type hash-table
             :documentation "Hash table mapping key specs to command symbols.")
   (parent   :initarg :parent
             :reader keymap-parent
             :initform nil
             :type (or null keymap)
             :documentation "Parent keymap for fallback lookups, or nil."))
  (:documentation "A key-to-command mapping with optional parent chain for fallback."))

(declaim (ftype (function (keyword &key (:parent (or null keymap))) keymap) make-keymap))
(defun make-keymap (name &key parent)
  "Create a new empty keymap with NAME and optional PARENT for fallback."
  (make-instance 'keymap
    :name name
    :bindings (make-hash-table :test #'equal)
    :parent parent))

(declaim (ftype (function (keymap t symbol) keymap) keymap-bind))
(defun keymap-bind (keymap key command)
  "Bind KEY to COMMAND in KEYMAP."
  (setf (gethash key (keymap-bindings keymap)) command)
  keymap)

(declaim (ftype (function (keymap t) (or null symbol)) keymap-lookup))
(defun keymap-lookup (keymap key)
  "Look up KEY in KEYMAP, walking the parent chain if not found."
  (or (gethash key (keymap-bindings keymap))
      (when (keymap-parent keymap)
        (keymap-lookup (keymap-parent keymap) key))))

;;; --------------------------------------------------------------------------
;;; Default Keymap
;;; --------------------------------------------------------------------------

(defvar *default-keymap* nil
  "The default keymap for chat buffers. Initialized by init-default-keymap.")

(defvar *scratch-keymap* nil
  "Keymap for scratch buffers. Inherits global bindings from *default-keymap*.")

(defvar *file-keymap* nil
  "Keymap for project-backed file buffers. Inherits global bindings.")

(defun init-scratch-keymap ()
  "Build and install the scratch buffer keymap."
  (let ((km (make-keymap :scratch :parent *default-keymap*)))
    ;; In scratch, RET edits the buffer instead of sending a chat message.
    (keymap-bind km #\Return 'insert-newline-command)
    (keymap-bind km #\Newline 'insert-newline-command)
    (setf *scratch-keymap* km)))

(defun init-file-keymap ()
  "Build and install the project file editing keymap."
  (let ((km (make-keymap :file :parent *default-keymap*)))
    ;; In file buffers, ordinary editor keys edit the document.
    (keymap-bind km #\Return 'insert-newline-command)
    (keymap-bind km #\Newline 'insert-newline-command)
    (keymap-bind km #\Tab 'insert-tab-command)
    (keymap-bind km :tab 'insert-tab-command)
    (keymap-bind km :left 'backward-char-command)
    (keymap-bind km :right 'forward-char-command)
    (keymap-bind km :up 'previous-line-command)
    (keymap-bind km :down 'next-line-command)
    (keymap-bind km (code-char 16) 'previous-line-command) ; C-p
    (keymap-bind km (code-char 14) 'next-line-command)     ; C-n
    (keymap-bind km '(:meta #\<) 'beginning-of-buffer-command)
    (keymap-bind km '(:meta #\>) 'end-of-buffer-command)
    (keymap-bind km (code-char 19) 'search-forward-command) ; C-s
    (keymap-bind km (code-char 18) 'search-backward-command) ; C-r
    (keymap-bind km (code-char 0) 'set-mark-command)         ; C-SPC
    (keymap-bind km '(:ctrl #\Space) 'set-mark-command)
    (keymap-bind km (code-char 7) 'keyboard-quit-command)    ; C-g
    (keymap-bind km (code-char 23) 'kill-region-command)     ; C-w
    (keymap-bind km '(:meta #\w) 'copy-region-command)       ; M-w
    (keymap-bind km '(:ctrl-x #\x) 'exchange-point-and-mark-command)
    (keymap-bind km '(:ctrl-x #\h) 'mark-whole-buffer-command)
    (keymap-bind km (list :ctrl-x (code-char 22)) 'revert-file-buffer-command) ; C-x C-v
    (keymap-bind km '(:ctrl-x #\i) 'insert-file-command) ; C-x i
    (keymap-bind km (list :ctrl-x (code-char 23)) 'write-project-file-as-command) ; C-x C-w
    (keymap-bind km '(:ctrl-x #\w) 'write-project-file-as-command) ; C-x w
    (keymap-bind km (list :ctrl-x (code-char 14)) 'create-project-file-command) ; C-x C-n
    (setf *file-keymap* km)))

(defun init-default-keymap ()
  "Build and install the default keymap with standard chat buffer bindings."
  (let ((km (make-keymap :default)))
    ;; Send message: bind both #\Return (CR, ASCII 13) and #\Newline
    ;; (LF, ASCII 10), since CLIM ports can report Enter either way.
    (keymap-bind km #\Return 'send-message)
    (keymap-bind km #\Newline 'send-message)
    ;; Stop an active LLM response.
    (keymap-bind km #\Esc 'stop-llm-command)
    ;; Insert newline: C-o (open-line, ASCII 15) since C-j (#\Newline)
    ;; is indistinguishable from Enter in this key abstraction.
    (keymap-bind km (code-char 15) 'insert-newline-command) ; C-o = ASCII 15
    ;; Cursor movement
    (keymap-bind km #\Soh 'beginning-of-line-command)       ; C-a = ASCII 1
    (keymap-bind km #\Enq 'end-of-line-command)             ; C-e = ASCII 5
    (keymap-bind km (code-char 6) 'forward-char-command)    ; C-f = ASCII 6
    (keymap-bind km (code-char 2) 'backward-char-command)   ; C-b = ASCII 2
    (keymap-bind km '(:meta #\f) 'forward-word-command)     ; M-f
    (keymap-bind km '(:meta #\b) 'backward-word-command)    ; M-b
    ;; Kill/cut
    (keymap-bind km #\Vt 'kill-line-command)                ; C-k = ASCII 11
    (keymap-bind km (code-char 21) 'kill-backward-line-command) ; C-u = ASCII 21
    (keymap-bind km '(:meta #\d) 'kill-word-command)        ; M-d
    (keymap-bind km (code-char 23) 'backward-kill-word-command) ; C-w = ASCII 23
    (keymap-bind km '(:meta #\Backspace) 'backward-kill-word-command)
    (keymap-bind km '(:meta #\Rubout) 'backward-kill-word-command)
    (keymap-bind km '(:meta :backspace) 'backward-kill-word-command)
    (keymap-bind km '(:ctrl #\Backspace) 'backward-kill-word-command)
    (keymap-bind km '(:ctrl #\Rubout) 'backward-kill-word-command)
    (keymap-bind km '(:ctrl :backspace) 'backward-kill-word-command)
    ;; Yank/paste
    (keymap-bind km #\Em 'yank-command)                     ; C-y = ASCII 25
    (keymap-bind km '(:meta #\y) 'yank-pop-command)         ; M-y
    (keymap-bind km '(:meta #\Em) 'yank-previous-command-first-arg-command) ; M-C-y
    (keymap-bind km '(:meta #\.) 'yank-previous-command-last-arg-command)   ; M-.
    (keymap-bind km '(:meta #\_) 'yank-previous-command-last-arg-command)   ; M-_
    ;; Delete
    ;; Bind direct Backspace variants for normalized events that distinguish them.
    (keymap-bind km #\Backspace 'delete-char-backward-command)
    (keymap-bind km (code-char 4) 'delete-char-forward-command) ; C-d = ASCII 4
    (keymap-bind km #\Rubout 'delete-char-backward-command)
    (keymap-bind km :backspace 'delete-char-backward-command)
    (keymap-bind km (code-char 4) 'delete-char-forward-command) ; C-d = ASCII 4
    ;; Home / End — cursor to beginning / end of line
    (keymap-bind km :home 'beginning-of-line-command)
    (keymap-bind km :end 'end-of-line-command)
    ;; Scroll: Page Up / Page Down
    (keymap-bind km :page-up 'scroll-up-command)
    (keymap-bind km :page-down 'scroll-down-command)
    ;; Emacs-style scroll: M-v (scroll up/back), C-v (scroll down/forward)
    ;; M-v arrives as ESC then v, normalized to (:meta #\v).
    (keymap-bind km '(:meta #\v) 'scroll-up-command)
    (keymap-bind km (code-char 22) 'scroll-down-command)  ; C-v = ASCII 22
    ;; Execute command
    (keymap-bind km '(:meta #\x) 'execute-extended-command) ; M-x
    ;; Redraw
    (keymap-bind km (code-char 12) 'redraw-screen-command) ; C-l = ASCII 12
    ;; ----- C-c prefix: buffer-mode-specific commands -----
    ;; C-c is reserved for commands that act on or within the current
    ;; buffer's major mode (e.g. chat-specific toggles, mode actions).
    ;; Note: C-x C-c quits the application (handled in handle-key-event).
    (keymap-bind km '(:ctrl-c #\t) 'toggle-tool-results-command) ; C-c t = toggle tool results
    (keymap-bind km '(:ctrl-c #\V) 'toggle-reasoning-output-command) ; C-c V = toggle verbose/reasoning output
    (keymap-bind km '(:ctrl-c #\I) 'toggle-metadata-output-command) ; C-c I = toggle response metadata
    (keymap-bind km '(:ctrl-c #\c) 'compact-buffer-command) ; C-c c = compact conversation
    ;; C-c A = minibuffer agent selector
    (keymap-bind km '(:ctrl-c #\A) 'minibuffer-select-agent-command)
    ;; C-c s = insert skill mention, C-c S = toggle skills.
    (keymap-bind km '(:ctrl-c #\s) 'minibuffer-insert-skill-command)
    (keymap-bind km '(:ctrl-c #\S) 'minibuffer-toggle-skill-command)
    ;; C-c C-m = minibuffer model selector (helm/ivy/vertico style).
    ;; C-m = ASCII 13 = #\Return. Some CLIM ports send #\Newline
    ;; (LF, ASCII 10) for Enter, so bind both.
    (keymap-bind km '(:ctrl-c #\Return) 'minibuffer-select-model-command)  ; C-c C-m
    (keymap-bind km '(:ctrl-c #\Newline) 'minibuffer-select-model-command) ; C-c C-m (LF variant)
    ;; C-c M (capital M) = compatibility alias for the visible minibuffer selector
    (keymap-bind km '(:ctrl-c #\M) 'select-model-command)
    ;; C-c C-r = minibuffer think selector
    (keymap-bind km (list :ctrl-c (code-char 18)) 'minibuffer-select-think-level-command)
    ;; C-c R (capital R) = compatibility alias for the visible minibuffer selector
    (keymap-bind km '(:ctrl-c #\R) 'select-think-level-command)
    ;; C-c C-d = toggle API debug mode (echo requests/responses in chat)
    ;; C-d = ASCII 4 = #\Eot (end of transmission)
    (keymap-bind km (list :ctrl-c (code-char 4)) 'toggle-debug-mode-command)
    ;; ----- C-h prefix: help & introspection commands -----
    ;; C-h is the help prefix key (Emacs standard).
    ;; Keep C-c aliases for compatibility with older bindings and users who
    ;; prefer avoiding the C-h / Backspace ambiguity.
    (keymap-bind km '(:ctrl-h #\b) 'describe-bindings-command)  ; C-h b = describe keybindings
    (keymap-bind km '(:ctrl-h #\f) 'describe-function-command)  ; C-h f = describe function
    (keymap-bind km '(:ctrl-h #\F) 'customize-appearance-command) ; C-h F = appearance editor
    (keymap-bind km '(:ctrl-h #\v) 'describe-variable-command)  ; C-h v = describe variable
    (keymap-bind km '(:ctrl-h #\i) 'info-directory-command)     ; C-h i = Info directory
    (keymap-bind km '(:ctrl-h #\I) 'rplaca-manual-command)    ; C-h I = RPLACA manual
    (keymap-bind km '(:ctrl-h #\T) 'describe-type-command)      ; C-h T = describe type
    (keymap-bind km '(:ctrl-c #\b) 'describe-bindings-command)  ; compatibility alias
    (keymap-bind km '(:ctrl-c #\f) 'describe-function-command)  ; compatibility alias
    (keymap-bind km '(:ctrl-c #\F) 'customize-appearance-command) ; appearance editor alias
    (keymap-bind km '(:ctrl-c #\v) 'describe-variable-command)  ; compatibility alias
    (keymap-bind km '(:ctrl-c #\T) 'describe-type-command)      ; compatibility alias
    ;; ----- C-x prefix: global / cross-buffer commands -----
    ;; C-x is reserved for global commands that operate across buffers
    ;; or affect the application as a whole (buffer management, I/O, etc.).
    (keymap-bind km (list :ctrl-x (code-char 6)) 'open-project-file-command) ; C-x C-f
    (keymap-bind km '(:ctrl-x #\p) 'minibuffer-select-project-command) ; C-x p
    (keymap-bind km '(:ctrl-x #\n) 'new-buffer-command)       ; C-x n = new buffer
    (keymap-bind km '(:ctrl-x #\l) 'new-listener-buffer-command) ; C-x l = native McCLIM Listener
    (keymap-bind km '(:ctrl-x #\F) 'font-editor-command) ; C-x F = font editor
    (keymap-bind km '(:ctrl-x #\k) 'kill-buffer-command)      ; C-x k = kill buffer
    (keymap-bind km (list :ctrl-x (code-char 19)) 'save-session-command) ; C-x C-s
    (keymap-bind km (list :ctrl-x (code-char 18)) 'load-session-command) ; C-x C-r
    (keymap-bind km '(:ctrl-x #\t) 'session-tree-command) ; C-x t
    (keymap-bind km '(:ctrl-x #\T) 'fork-session-command) ; C-x T
    ;; C-x b / C-x C-b = minibuffer buffer selector (helm/ivy/vertico style).
    ;; The old overlay selector remains available as `list-buffers-command', but
    ;; the standard switch-buffer chord uses the ESA minibuffer path.
    (keymap-bind km '(:ctrl-x #\b) 'minibuffer-select-buffer-command)
    (keymap-bind km (list :ctrl-x (code-char 2)) 'minibuffer-select-buffer-command)
    (setf *default-keymap* km)
    (init-scratch-keymap)
    (init-file-keymap)))
