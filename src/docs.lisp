(in-package :rplaca)

;;; =========================================================================
;;; Extended Documentation Entries
;;;
;;; This file provides extended documentation for all exported symbols using
;;; the defdoc macro. Each entry may include:
;;;   :category     — grouping for organized browsing
;;;   :usage        — parameter types and example call
;;;   :returns      — return type and example value
;;;   :see-also     — list of related symbols
;;;   :side-effects — description of mutations, I/O, or global state changes
;;; =========================================================================

;;; --------------------------------------------------------------------------
;;; Undocumented Symbol Detection
;;; --------------------------------------------------------------------------

(defun undocumented-functions ()
  "Return a sorted list of exported function symbols that lack extended
documentation in *extended-docs*."
  (let ((undocumented nil))
    (dolist (sym (list-functions))
      (unless (extended-doc sym)
        (push sym undocumented)))
    (sort undocumented #'string< :key #'symbol-name)))

(defun undocumented-variables ()
  "Return a sorted list of exported variable symbols that lack extended
documentation in *extended-docs*."
  (let ((undocumented nil))
    (dolist (sym (list-variables))
      (unless (extended-doc sym)
        (push sym undocumented)))
    (sort undocumented #'string< :key #'symbol-name)))

;;; ==========================================================================
;;; Category: line — Line data structure
;;; ==========================================================================

(defdoc line
  :category "line"
  :usage "(make-line \"Hello world\") to create a line."
  :returns "Class — A line object in a doubly-linked list."
  :see-also (make-line line-content line-next line-prev message))

(defdoc make-line
  :category "line"
  :usage "(make-line &optional CONTENT:string) — (make-line \"Hello\"), (make-line)"
  :returns "line — A new unlinked line with the given content."
  :see-also (line line-content line-next line-prev))

(defdoc line-content
  :category "line"
  :usage "(line-content LINE:line) — (line-content my-line)"
  :returns "string — The text content of this line."
  :see-also (line make-line line-next line-prev))

(defdoc line-next
  :category "line"
  :usage "(line-next LINE:line) — (line-next my-line)"
  :returns "line or nil — Next line in the linked list, or nil if last."
  :see-also (line line-prev line-content))

(defdoc line-prev
  :category "line"
  :usage "(line-prev LINE:line) — (line-prev my-line)"
  :returns "line or nil — Previous line in the linked list, or nil if first."
  :see-also (line line-next line-content))

;;; ==========================================================================
;;; Category: message — Message structure and accessors
;;; ==========================================================================

(defdoc message
  :category "message"
  :usage "(make-message :user) to create a user message."
  :returns "Class — A message containing a doubly-linked list of lines."
  :see-also (make-message message-text message-sender message-first-line buffer))

(defdoc make-message
  :category "message"
  :usage "(make-message SENDER:keyword &key FACE-SET READ-ONLY-P) — (make-message :user), (make-message :agent :read-only-p t)"
  :returns "message — A new message with a single empty line."
  :see-also (message message-text message-sender message-first-line))

(defdoc message-first-line
  :category "message"
  :usage "(message-first-line MSG:message) — (message-first-line my-msg)"
  :returns "line — First line in the message's doubly-linked list."
  :see-also (message message-last-line message-text line))

(defdoc message-last-line
  :category "message"
  :usage "(message-last-line MSG:message) — (message-last-line my-msg)"
  :returns "line — Last line in the message's doubly-linked list."
  :see-also (message message-first-line message-text line))

(defdoc message-point-line
  :category "message"
  :usage "(message-point-line MSG:message) — (message-point-line my-msg)"
  :returns "line — The line containing the editing cursor (point)."
  :see-also (message message-point-offset message-first-line))

(defdoc message-point-offset
  :category "message"
  :usage "(message-point-offset MSG:message) — (message-point-offset my-msg)"
  :returns "fixnum — Character offset of the cursor within the point line."
  :see-also (message message-point-line message-forward-char message-backward-char))

(defdoc message-mark-line
  :category "message"
  :usage "(message-mark-line MSG:message) — (message-mark-line my-msg)"
  :returns "line or nil — The line containing the mark for selection, or nil."
  :see-also (message message-mark-offset message-point-line))

(defdoc message-mark-offset
  :category "message"
  :usage "(message-mark-offset MSG:message) — (message-mark-offset my-msg)"
  :returns "fixnum or nil — Character offset of the mark, or nil."
  :see-also (message message-mark-line message-point-offset))

(defdoc message-sender
  :category "message"
  :usage "(message-sender MSG:message) — (message-sender my-msg)"
  :returns "keyword — :USER, :AGENT, :SYSTEM, :TOOL-RESULT, etc."
  :see-also (message make-message message-text))

(defdoc message-timestamp
  :category "message"
  :usage "(message-timestamp MSG:message) — (message-timestamp my-msg)"
  :returns "integer or nil — Universal time when finalized, or nil if not yet."
  :see-also (message message-sender buffer-finalize-input))
(defdoc message-read-only-p
  :category "message"
  :usage "(message-read-only-p MSG:message) — (message-read-only-p my-msg)"
  :returns "boolean — T if the message cannot be edited."
  :see-also (message buffer-finalize-input buffer-insert-agent-message))

(defdoc message-next
  :category "message"
  :usage "(message-next MSG:message) — (message-next my-msg)"
  :returns "message or nil — Next message in the buffer, or nil if last."
  :see-also (message message-prev buffer-first-message))

(defdoc message-prev
  :category "message"
  :usage "(message-prev MSG:message) — (message-prev my-msg)"
  :returns "message or nil — Previous message in the buffer, or nil if first."
  :see-also (message message-next buffer-last-message))

(defdoc message-raw-content
  :category "message"
  :usage "(message-raw-content MSG:message) — (message-raw-content my-msg)"
  :returns "list or nil — API-format content blocks for round-tripping tool_use/tool_result."
  :see-also (message message-text build-conversation-messages))

(defdoc message-metadata
  :category "message"
  :usage "(message-metadata MSG:message) — (message-metadata my-msg)"
  :returns "alist or nil — Display-only provider/response metadata for a message."
  :see-also (message toggle-metadata-output-command))

(defdoc message-text
  :category "message"
  :usage "(message-text MSG:message) — (message-text my-msg)"
  :returns "string — Full text with newlines between lines. \"Hello\\nworld\""
  :see-also (message message-first-line message-line-count))

(defdoc message-line-count
  :category "message"
  :usage "(message-line-count MSG:message) — (message-line-count my-msg)"
  :returns "fixnum — Number of lines. (message-line-count (make-message :user)) => 1"
  :see-also (message message-text message-first-line))

;;; ==========================================================================
;;; Category: message-editing — Message editing operations
;;; ==========================================================================

(defdoc message-insert-char
  :category "message-editing"
  :usage "(message-insert-char MSG:message CHAR:character) — (message-insert-char msg #\\a)"
  :returns "message — The modified message."
  :side-effects "Mutates the point line's content and advances point by 1."
  :see-also (message-insert-newline message-delete-char-backward self-insert-command))

(defdoc message-insert-newline
  :category "message-editing"
  :usage "(message-insert-newline MSG:message) — (message-insert-newline msg)"
  :returns "message — The modified message."
  :side-effects "Splits the current line into two at point. Updates the line linked list."
  :see-also (message-insert-char insert-newline-command message-kill-line))

(defdoc message-delete-char-backward
  :category "message-editing"
  :usage "(message-delete-char-backward MSG:message) — (message-delete-char-backward msg)"
  :returns "message — The modified message."
  :side-effects "Deletes the character before point. Joins lines if at start."
  :see-also (message-delete-char-forward delete-char-backward-command message-insert-char))

(defdoc message-delete-char-forward
  :category "message-editing"
  :usage "(message-delete-char-forward MSG:message) — (message-delete-char-forward msg)"
  :returns "message — The modified message."
  :side-effects "Deletes the character after point. Joins lines if at end."
  :see-also (message-delete-char-backward delete-char-forward-command))

(defdoc message-forward-char
  :category "message-editing"
  :usage "(message-forward-char MSG:message) — (message-forward-char msg)"
  :returns "message — The modified message."
  :side-effects "Moves point one character forward. Wraps to next line at end."
  :see-also (message-backward-char message-forward-word forward-char-command))

(defdoc message-backward-char
  :category "message-editing"
  :usage "(message-backward-char MSG:message) — (message-backward-char msg)"
  :returns "message — The modified message."
  :side-effects "Moves point one character backward. Wraps to previous line at start."
  :see-also (message-forward-char message-backward-word backward-char-command))

(defdoc message-forward-word
  :category "message-editing"
  :usage "(message-forward-word MSG:message) — (message-forward-word msg)"
  :returns "message — The modified message."
  :side-effects "Moves point forward to end of next word."
  :see-also (message-backward-word message-forward-char forward-word-command))

(defdoc message-backward-word
  :category "message-editing"
  :usage "(message-backward-word MSG:message) — (message-backward-word msg)"
  :returns "message — The modified message."
  :side-effects "Moves point backward to beginning of previous word."
  :see-also (message-forward-word message-backward-char backward-word-command))

(defdoc message-move-beginning-of-line
  :category "message-editing"
  :usage "(message-move-beginning-of-line MSG:message) — (message-move-beginning-of-line msg)"
  :returns "message — The modified message."
  :side-effects "Sets point offset to 0."
  :see-also (message-move-end-of-line beginning-of-line-command))

(defdoc message-move-end-of-line
  :category "message-editing"
  :usage "(message-move-end-of-line MSG:message) — (message-move-end-of-line msg)"
  :returns "message — The modified message."
  :side-effects "Sets point offset to end of current line."
  :see-also (message-move-beginning-of-line end-of-line-command))

(defdoc message-kill-line
  :category "message-editing"
  :usage "(message-kill-line MSG:message) — (message-kill-line msg)"
  :returns "message — The modified message."
  :side-effects "Kills from point to end of line. Pushes killed text to kill ring."
  :see-also (message-kill-backward-line message-kill-word kill-line-command kill-ring-push))

(defdoc message-kill-backward-line
  :category "message-editing"
  :usage "(message-kill-backward-line MSG:message) — (message-kill-backward-line msg)"
  :returns "message — The modified message."
  :side-effects "Kills from start of line to point. Pushes killed text to kill ring."
  :see-also (message-kill-line kill-backward-line-command kill-ring-push))

(defdoc message-kill-word
  :category "message-editing"
  :usage "(message-kill-word MSG:message) — (message-kill-word msg)"
  :returns "message — The modified message."
  :side-effects "Kills from point to end of current word. Pushes to kill ring."
  :see-also (message-backward-kill-word kill-word-command kill-ring-push))

(defdoc message-backward-kill-word
  :category "message-editing"
  :usage "(message-backward-kill-word MSG:message) — (message-backward-kill-word msg)"
  :returns "message — The modified message."
  :side-effects "Kills from beginning of current word to point. Pushes to kill ring."
  :see-also (message-kill-word backward-kill-word-command kill-ring-push))

(defdoc message-yank
  :category "message-editing"
  :usage "(message-yank MSG:message) — (message-yank msg)"
  :returns "message — The modified message."
  :side-effects "Inserts the top of the kill ring at point."
  :see-also (message-yank-pop yank-command kill-ring-top))

(defdoc message-yank-pop
  :category "message-editing"
  :usage "(message-yank-pop MSG:message) — (message-yank-pop msg)"
  :returns "message — The modified message."
  :side-effects "Replaces the just-yanked text with the next kill ring entry."
  :see-also (message-yank yank-pop-command *kill-ring*))

;;; ==========================================================================
;;; Category: kill-ring — Kill ring operations
;;; ==========================================================================

(defdoc *kill-ring*
  :category "kill-ring"
  :see-also (kill-ring-push kill-ring-top message-kill-line))

(defdoc kill-ring-push
  :category "kill-ring"
  :usage "(kill-ring-push STRING:string) — (kill-ring-push \"deleted text\")"
  :returns "string — The pushed string."
  :side-effects "Pushes STRING onto *kill-ring*. Trims ring to *kill-ring-max* entries."
  :see-also (*kill-ring* kill-ring-top message-kill-line))

(defdoc kill-ring-top
  :category "kill-ring"
  :usage "(kill-ring-top) — (kill-ring-top)"
  :returns "string or nil — Most recent kill ring entry, or nil if empty."
  :see-also (*kill-ring* kill-ring-push message-yank))

;;; ==========================================================================
;;; Category: buffer — Buffer structure, ring, and operations
;;; ==========================================================================

(defdoc *default-context-limit*
  :category "buffer"
  :see-also (make-buffer buffer-context-limit))

(defdoc *default-agent-name*
  :category "buffer"
  :see-also (make-buffer buffer-agent-name load-session))

(defdoc *default-show-tool-results*
  :category "buffer"
  :see-also (buffer-show-tool-results-p))

(defdoc *default-show-reasoning-output*
  :category "buffer"
  :see-also (buffer-show-reasoning-p))

(defdoc *default-show-metadata-output*
  :category "buffer"
  :see-also (buffer-show-metadata-p))

(defdoc *scratch-buffer-name*
  :category "buffer"
  :see-also (ensure-scratch-buffer scratch-buffer))

(defdoc *scratch-buffer-initial-text*
  :category "buffer"
  :see-also (ensure-scratch-buffer scratch-buffer-text))

(defdoc *listener-buffer-name*
  :category "buffer"
  :see-also (make-listener-buffer ensure-listener-buffer listener-buffer-p))

(defdoc buffer
  :category "buffer"
  :usage "(make-buffer \"session-01\" :agent-name \"agent\") to create a buffer."
  :returns "Class — A buffer with a doubly-linked list of messages."
  :see-also (make-buffer buffer-name buffer-input-message *buffer-ring*))

(defdoc make-buffer
  :category "buffer"
  :usage "(make-buffer NAME:string &key AGENT-NAME:string KIND:keyword WORKING-DIRECTORY:pathname CONTEXT-LIMIT:integer SESSION:session MAJOR-MODE:string) — (make-buffer \"session-01\" :agent-name \"agent\")"
  :returns "buffer — A new buffer with a single empty input message."
  :side-effects "Allocates a buffer with an empty face registry."
  :see-also (buffer buffer-name buffer-kind register-buffer-type buffer-session add-buffer-to-ring current-buffer))

(defdoc buffer-type
  :category "buffer"
  :usage "Struct — metadata for a registered buffer kind."
  :returns "buffer-type — Includes kind name, description, default major-mode, document flag, optional presentation functions, and package owner."
  :see-also (register-buffer-type define-buffer-type list-buffer-types))

(defdoc register-buffer-type
  :category "buffer"
  :usage "(register-buffer-type :dashboard :description \"...\" :major-mode \"dashboard\" :presentation-function 'dashboard-entries)"
  :returns "buffer-type — The registered metadata. Presentation hooks accept (BUFFER COLUMNS) and return entry plists with :TEXT plus optional :FACE, :OBJECT, :PRESENTATION-TYPE, :UNIQUE-ID, and :CACHE-VALUE."
  :side-effects "Adds or replaces a buffer type in *buffer-type-registry*. Presentation hooks should be side-effect-free render models; commands and presentation translators own mutations."
  :see-also (define-buffer-type find-buffer-type buffer-presentation-function))

(defdoc define-buffer-type
  :category "buffer"
  :usage "(define-buffer-type :dashboard :major-mode \"dashboard\" :presentation-function 'dashboard-entries)"
  :returns "buffer-type — The registered metadata. Presentation hooks accept (BUFFER COLUMNS) and return generic entry plists. INPUT-PRESENTATION-FUNCTION appends an interactive overlay after normal transcript output."
  :side-effects "Package-facing macro wrapper around register-buffer-type; package ownership defaults to the currently loading RPLACA package."
  :see-also (register-buffer-type package-owned-buffer-types))

(defdoc find-buffer-type
  :category "buffer"
  :usage "(find-buffer-type :chat)"
  :returns "buffer-type or NIL."
  :see-also (list-buffer-types register-buffer-type))

(defdoc list-buffer-types
  :category "buffer"
  :usage "(list-buffer-types)"
  :returns "list — Registered buffer-type structures sorted by name."
  :see-also (find-buffer-type package-owned-buffer-types))

(defdoc buffer-presentation-function
  :category "buffer"
  :usage "(buffer-presentation-function BUF)"
  :returns "function designator or NIL — The registered whole-buffer presenter for BUF's type. The McCLIM UI calls it as (FUNCALL FUNCTION BUF COLUMNS) and renders the returned entry plists with CLIM presentations when :OBJECT and :PRESENTATION-TYPE are present."
  :see-also (buffer-input-presentation-function register-buffer-type))

(defdoc buffer-input-presentation-function
  :category "buffer"
  :usage "(buffer-input-presentation-function BUF)"
  :returns "function designator or NIL — The registered input-overlay presenter for BUF's type, called as (FUNCALL FUNCTION BUF COLUMNS) after normal transcript content."
  :see-also (buffer-presentation-function register-buffer-type buffer-input-presentation-functions))

(defdoc register-buffer-input-presentation-provider
  :category "buffer"
  :usage "(register-buffer-input-presentation-provider :chat 'package-input-panel)"
  :returns "buffer-input-presentation-provider — A package-owned input-overlay provider for an existing buffer kind."
  :side-effects "Adds a provider removed automatically when the owning RPLACA package is reset or reloaded."
  :see-also (buffer-input-presentation-functions remove-buffer-input-presentation-providers-for-package))

(defdoc buffer-input-presentation-functions
  :category "buffer"
  :usage "(buffer-input-presentation-functions BUF)"
  :returns "list — Active direct and package-owned input presentation functions for BUF."
  :see-also (buffer-input-presentation-function register-buffer-input-presentation-provider))

(defdoc listener-state
  :category "buffer"
  :usage "Struct — process-local state for a listener buffer."
  :returns "listener-state — Tracks package, directory stack, last values, and command history."
  :see-also (listener-buffer-state make-listener-buffer listener-wholine-text))

(defdoc listener-buffer-p
  :category "buffer"
  :usage "(listener-buffer-p BUF)"
  :returns "boolean — true when BUF has kind :LISTENER."
  :see-also (make-listener-buffer ensure-listener-buffer submit-listener-input))

(defdoc listener-buffer-state
  :category "buffer"
  :usage "(listener-buffer-state BUF)"
  :returns "listener-state — BUF's process-local listener state."
  :side-effects "Creates the listener state when missing."
  :see-also (listener-state listener-prompt-text listener-wholine-text))

(defdoc listener-prompt-text
  :category "buffer"
  :usage "(listener-prompt-text BUF)"
  :returns "string — The package-sensitive prompt, e.g. \"CL-USER> \"."
  :see-also (listener-buffer-state submit-listener-input))

(defdoc listener-wholine-text
  :category "buffer"
  :usage "(listener-wholine-text BUF)"
  :returns "string — Listener status text with user, host, package, directory, stack, and memory summary."
  :see-also (listener-prompt-text listener-state))

(defdoc listener-command-help-text
  :category "buffer"
  :usage "(listener-command-help-text)"
  :returns "string — Help text for supported listener comma commands."
  :see-also (submit-listener-input make-listener-buffer))

(defdoc make-listener-buffer
  :category "buffer"
  :usage "(make-listener-buffer :working-directory #P\"/tmp/\" :add-to-ring-p t)"
  :returns "buffer — A :LISTENER buffer with Lisp evaluation and comma commands."
  :side-effects "Initializes buffer faces/keymap, creates listener state, and optionally adds the buffer to *buffer-ring*."
  :see-also (ensure-listener-buffer listener-buffer-p submit-listener-input))

(defdoc ensure-listener-buffer
  :category "buffer"
  :usage "(ensure-listener-buffer)"
  :returns "buffer — Existing listener buffer, or a newly created default listener."
  :side-effects "May create and add a listener buffer to *buffer-ring*."
  :see-also (make-listener-buffer new-listener-buffer-command))

(defdoc submit-listener-input
  :category "buffer"
  :usage "(submit-listener-input BUF)"
  :returns ":redraw or NIL."
  :side-effects "Finalizes BUF's input, evaluates Lisp forms, dispatches comma commands, inserts output, and updates listener state."
  :see-also (send-message listener-command-help-text listener-prompt-text))

(defdoc buffer-name
  :category "buffer"
  :usage "(buffer-name BUF:buffer) — (buffer-name (current-buffer))"
  :returns "string — \"session-01\", \"*help:make-buffer*\", etc."
  :see-also (buffer make-buffer find-buffer-by-name buffer-names))

(defdoc buffer-first-message
  :category "buffer"
  :usage "(buffer-first-message BUF:buffer) — (buffer-first-message (current-buffer))"
  :returns "message — First message in the buffer's linked list."
  :see-also (buffer buffer-last-message buffer-input-message message))

(defdoc buffer-last-message
  :category "buffer"
  :usage "(buffer-last-message BUF:buffer) — (buffer-last-message (current-buffer))"
  :returns "message — Last message (always the input message)."
  :see-also (buffer buffer-first-message buffer-input-message))

(defdoc buffer-input-message
  :category "buffer"
  :usage "(buffer-input-message BUF:buffer) — (buffer-input-message (current-buffer))"
  :returns "message — The current editable input message (not read-only)."
  :see-also (buffer buffer-last-message buffer-finalize-input))

(defdoc buffer-agent-name
  :category "buffer"
  :usage "(buffer-agent-name BUF:buffer) — (buffer-agent-name (current-buffer))"
  :returns "string — \"agent\", \"help\", etc."
  :see-also (buffer make-buffer buffer-name))

(defdoc buffer-kind
  :category "buffer"
  :usage "(buffer-kind BUF:buffer) — (buffer-kind (current-buffer))"
  :returns "keyword — :CHAT, :SCRATCH, or another custom buffer kind."
  :see-also (buffer make-buffer scratch-buffer-p))

(defdoc buffer-enabled-packages
  :category "buffer"
  :usage "(buffer-enabled-packages BUF:buffer)"
  :returns "list — package names explicitly enabled for this buffer."
  :side-effects "Setf updates buffer-local package enablement; saved sessions persist the list."
  :see-also (active-package-names package-enablement-scope save-session load-session))

(defdoc buffer-session
  :category "buffer"
  :usage "(buffer-session BUF:buffer)"
  :returns "session or nil — Persistent session metadata attached to this buffer."
  :side-effects "Setf changes which transcript receives future finalized messages."
  :see-also (session load-or-create-session session-current-transcript-path))

(defdoc buffer-working-directory
  :category "buffer"
  :usage "(buffer-working-directory BUF:buffer) — (buffer-working-directory (current-buffer))"
  :returns "pathname — The working directory for shell commands."
  :see-also (buffer make-buffer shell-prefix-handler))

(defdoc buffer-project-name
  :category "buffer"
  :usage "(buffer-project-name BUF:buffer)"
  :returns "string or nil — The selected project or backing project for this buffer."
  :see-also (project buffer-resource-path project-open-file))

(defdoc buffer-resource-path
  :category "buffer"
  :usage "(buffer-resource-path BUF:buffer)"
  :returns "string or nil — Project-relative resource path for file buffers."
  :see-also (buffer-project-name file-buffer-p project-open-file))

(defdoc buffer-original-text
  :category "buffer"
  :usage "(buffer-original-text BUF:buffer)"
  :returns "string — Last saved text snapshot for project file buffers."
  :see-also (buffer-dirty-p file-buffer-text project-save-buffer))

(defdoc buffer-dirty-p
  :category "buffer"
  :usage "(buffer-dirty-p BUF:buffer)"
  :returns "boolean — True when a file buffer has unsaved changes."
  :see-also (mark-buffer-dirty project-save-buffer file-buffer-text))

(defdoc buffer-token-count
  :category "buffer"
  :usage "(buffer-token-count BUF:buffer) — (buffer-token-count (current-buffer))"
  :returns "integer — Running count of tokens used in this buffer's conversation."
  :see-also (buffer buffer-context-limit))

(defdoc buffer-context-limit
  :category "buffer"
  :usage "(buffer-context-limit BUF:buffer) — (buffer-context-limit (current-buffer))"
  :returns "integer — Maximum token context window size (default 200000)."
  :see-also (buffer buffer-token-count))

(defdoc *compaction-point*
  :category "compaction"
  :usage "(setf *compaction-point* 9/10) — NIL disables automatic compaction."
  :returns "number, function, or nil — Automatic compaction threshold policy."
  :see-also (maybe-compact-buffer compaction-threshold-tokens))

(defdoc *compaction-function*
  :category "compaction"
  :usage "(setf *compaction-function* #'default-compact-buffer)"
  :returns "function — Called as (FUNCTION BUFFER :REASON REASON)."
  :see-also (maybe-compact-buffer default-compact-buffer))

(defdoc *compaction-prompt*
  :category "compaction"
  :usage "(setf *compaction-prompt* \"Summarize this conversation...\")"
  :returns "string — Prompt used to create compaction summaries."
  :see-also (default-compact-buffer *compaction-summary-prefix*))

(defdoc *compaction-summary-prefix*
  :category "compaction"
  :usage "(setf *compaction-summary-prefix* \"Previous context summary:\")"
  :returns "string — Prefix inserted before generated compaction summaries."
  :see-also (default-compact-buffer *compaction-prompt*))

(defdoc *compaction-preserved-user-message-token-limit*
  :category "compaction"
  :usage "(setf *compaction-preserved-user-message-token-limit* 20000)"
  :returns "integer — Approximate token budget for exact recent user messages."
  :see-also (default-compact-buffer))

(defdoc buffer-conversation-token-estimate
  :category "compaction"
  :usage "(buffer-conversation-token-estimate BUF :include-current-input-p t)"
  :returns "integer — Heuristic estimate of model-visible conversation tokens."
  :see-also (compaction-needed-p buffer-context-limit))

(defdoc compaction-threshold-tokens
  :category "compaction"
  :usage "(compaction-threshold-tokens BUF)"
  :returns "integer or nil — Absolute token estimate where auto-compaction starts."
  :see-also (*compaction-point* compaction-needed-p))

(defdoc compaction-needed-p
  :category "compaction"
  :usage "(compaction-needed-p BUF :include-current-input-p t)"
  :returns "values — NEEDED-P, ESTIMATE, and THRESHOLD."
  :see-also (maybe-compact-buffer compaction-threshold-tokens))

(defdoc maybe-compact-buffer
  :category "compaction"
  :usage "(maybe-compact-buffer BUF :reason :auto)"
  :returns "values — COMPACTED-P, ESTIMATE, and THRESHOLD."
  :side-effects "May replace BUF's chat history with compacted summary context."
  :see-also (*compaction-function* default-compact-buffer compact-buffer-command))

(defdoc default-compact-buffer
  :category "compaction"
  :usage "(default-compact-buffer BUF :reason :manual)"
  :returns "buffer or nil — BUF when compaction succeeded."
  :side-effects "Calls the active provider without tools and rewrites BUF history."
  :see-also (*compaction-prompt* *compaction-summary-prefix*))

(defdoc compact-buffer-command
  :category "compaction"
  :usage "M-x compact-buffer-command or C-c c"
  :returns "nil — Interactive command."
  :side-effects "Forces compaction of the current chat buffer."
  :see-also (maybe-compact-buffer))

(defdoc buffer-status
  :category "buffer"
  :usage "(buffer-status BUF:buffer) — (buffer-status (current-buffer))"
  :returns "keyword — Current runtime state such as :IDLE, :THINKING, :TOOL-RUNNING, :QUESTION, :ERROR, or :OAUTH."
  :see-also (buffer send-to-agent-with-context))

(defdoc buffer-provider-override
  :category "buffer"
  :usage "(buffer-provider-override BUF:buffer) — (buffer-provider-override (current-buffer))"
  :returns "keyword or nil — :OPENAI-CODEX, :ZAI, :OPENROUTER, or nil."
  :see-also (set-buffer-provider-override clear-buffer-provider-override buffer-model-override))

(defdoc buffer-model-override
  :category "buffer"
  :usage "(buffer-model-override BUF:buffer) — (buffer-model-override (current-buffer))"
  :returns "string or nil — \"glm-5.2\" or nil to use default."
  :see-also (set-buffer-model-override clear-buffer-model-override buffer-provider-override))

(defdoc buffer-think-level-override
  :category "buffer"
  :usage "(buffer-think-level-override BUF:buffer) — (buffer-think-level-override (current-buffer))"
  :returns "string or nil — \"high\" or nil to use the model default."
  :see-also (set-buffer-think-level-override clear-buffer-think-level-override buffer-model-override))

(defdoc set-buffer-provider-override
  :category "buffer"
  :usage "(set-buffer-provider-override BUF:buffer PROVIDER:keyword) — (set-buffer-provider-override buf :zai)"
  :returns "buffer — The modified buffer."
  :side-effects "Sets the buffer's provider override."
  :see-also (buffer-provider-override clear-buffer-provider-override set-buffer-model-override))

(defdoc set-buffer-model-override
  :category "buffer"
  :usage "(set-buffer-model-override BUF:buffer MODEL:string) — (set-buffer-model-override buf \"glm-5.2\")"
  :returns "buffer — The modified buffer."
  :side-effects "Sets the buffer's model override."
  :see-also (buffer-model-override clear-buffer-model-override set-buffer-provider-override))

(defdoc set-buffer-think-level-override
  :category "buffer"
  :usage "(set-buffer-think-level-override BUF:buffer THINK-LEVEL:string) — (set-buffer-think-level-override buf \"high\")"
  :returns "buffer — The modified buffer."
  :side-effects "Sets the buffer's think-level override."
  :see-also (buffer-think-level-override clear-buffer-think-level-override))

(defdoc clear-buffer-provider-override
  :category "buffer"
  :usage "(clear-buffer-provider-override BUF:buffer) — (clear-buffer-provider-override buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears the buffer's provider override to nil."
  :see-also (set-buffer-provider-override buffer-provider-override clear-buffer-routing-overrides))

(defdoc clear-buffer-model-override
  :category "buffer"
  :usage "(clear-buffer-model-override BUF:buffer) — (clear-buffer-model-override buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears the buffer's model override to nil."
  :see-also (set-buffer-model-override buffer-model-override clear-buffer-routing-overrides))

(defdoc clear-buffer-think-level-override
  :category "buffer"
  :usage "(clear-buffer-think-level-override BUF:buffer) — (clear-buffer-think-level-override buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears the buffer's think-level override to nil."
  :see-also (set-buffer-think-level-override buffer-think-level-override clear-buffer-routing-overrides))

(defdoc clear-buffer-routing-overrides
  :category "buffer"
  :usage "(clear-buffer-routing-overrides BUF:buffer) — (clear-buffer-routing-overrides buf)"
  :returns "buffer — The modified buffer."
  :side-effects "Clears provider, model, and think-level overrides."
  :see-also (clear-buffer-provider-override clear-buffer-model-override clear-buffer-think-level-override))
(defdoc buffer-keymap
  :category "buffer"
  :usage "(buffer-keymap BUF:buffer) — (buffer-keymap (current-buffer))"
  :returns "keymap or nil — The keymap for this buffer."
  :see-also (buffer keymap *default-keymap*))

(defdoc buffer-scroll-offset
  :category "buffer"
  :usage "(buffer-scroll-offset BUF:buffer) — (buffer-scroll-offset (current-buffer))"
  :returns "integer — Number of rows scrolled up. 0 means auto-scroll."
  :see-also (buffer scroll-up-command scroll-down-command))

(defdoc buffer-show-tool-results-p
  :category "buffer"
  :usage "(buffer-show-tool-results-p BUF:buffer) — (buffer-show-tool-results-p (current-buffer))"
  :returns "boolean — T if tool result messages are shown."
  :see-also (buffer toggle-tool-results-command))

(defdoc buffer-show-reasoning-p
  :category "buffer"
  :usage "(buffer-show-reasoning-p BUF:buffer) — (buffer-show-reasoning-p (current-buffer))"
  :returns "boolean — T if provider-supplied reasoning blocks are shown."
  :see-also (buffer toggle-reasoning-output-command))

(defdoc buffer-show-metadata-p
  :category "buffer"
  :usage "(buffer-show-metadata-p BUF:buffer) — (buffer-show-metadata-p (current-buffer))"
  :returns "boolean — T if provider/response metadata is shown."
  :see-also (buffer toggle-metadata-output-command))

(defdoc buffer-pending-stream
  :category "buffer"
  :usage "(buffer-pending-stream BUF:buffer) — (buffer-pending-stream (current-buffer))"
  :returns "stream-state or nil — The active streaming response state."
  :see-also (buffer buffer-streaming-message send-to-agent-with-context))

(defdoc buffer-streaming-message
  :category "buffer"
  :usage "(buffer-streaming-message BUF:buffer) — (buffer-streaming-message (current-buffer))"
  :returns "message or nil — The message being updated by streaming."
  :see-also (buffer buffer-pending-stream))

(defdoc buffer-major-mode
  :category "buffer"
  :usage "(buffer-major-mode BUF:buffer) — (buffer-major-mode (current-buffer))"
  :returns "string — \"chat\", \"scratch\", \"help\", etc."
  :see-also (buffer make-help-buffer))

(defdoc *buffer-ring*
  :category "buffer"
  :see-also (current-buffer add-buffer-to-ring switch-to-buffer kill-buffer-from-ring))

(defdoc current-buffer
  :category "buffer"
  :usage "(current-buffer) — (current-buffer)"
  :returns "buffer — The current buffer (first in the ring)."
  :see-also (*buffer-ring* switch-to-buffer add-buffer-to-ring))

(defdoc add-buffer-to-ring
  :category "buffer"
  :usage "(add-buffer-to-ring BUF:buffer) — (add-buffer-to-ring new-buf)"
  :returns "buffer — The added buffer."
  :side-effects "Pushes BUF to the front of *buffer-ring*."
  :see-also (*buffer-ring* current-buffer switch-to-buffer kill-buffer-from-ring))

(defdoc switch-to-buffer
  :category "buffer"
  :usage "(switch-to-buffer BUF:buffer) — (switch-to-buffer other-buf)"
  :returns "buffer — The switched-to buffer."
  :side-effects "Moves BUF to front of *buffer-ring*, making it current."
  :see-also (*buffer-ring* current-buffer add-buffer-to-ring))

(defdoc kill-buffer-from-ring
  :category "buffer"
  :usage "(kill-buffer-from-ring BUF:buffer) — (kill-buffer-from-ring (current-buffer))"
  :returns "buffer or nil — The new current buffer, or nil if ring is now empty."
  :side-effects "Removes BUF from *buffer-ring* unless BUF is the scratch buffer."
  :see-also (*buffer-ring* add-buffer-to-ring kill-buffer-command scratch-buffer-p))

(defdoc find-buffer-by-name
  :category "buffer"
  :usage "(find-buffer-by-name NAME:string) — (find-buffer-by-name \"session-01\")"
  :returns "buffer or nil — The matching buffer, or nil."
  :see-also (buffer-name buffer-names *buffer-ring*))

(defdoc scratch-buffer-p
  :category "buffer"
  :usage "(scratch-buffer-p BUF:buffer) — (scratch-buffer-p (current-buffer))"
  :returns "boolean — T when BUF is the process-local scratch buffer."
  :see-also (scratch-buffer ensure-scratch-buffer buffer-kind))

(defdoc file-buffer-p
  :category "buffer"
  :usage "(file-buffer-p BUF:buffer)"
  :returns "boolean — T when BUF edits a project resource."
  :see-also (project-open-file file-buffer-text document-buffer-p))

(defdoc document-buffer-p
  :category "buffer"
  :usage "(document-buffer-p BUF:buffer)"
  :returns "boolean — T for scratch and project-backed file buffers."
  :see-also (scratch-buffer-p file-buffer-p send-message))

(defdoc scratch-buffer
  :category "buffer"
  :usage "(scratch-buffer) — (scratch-buffer)"
  :returns "buffer or nil — The loaded scratch buffer."
  :see-also (ensure-scratch-buffer scratch-buffer-text *scratch-buffer-name*))

(defdoc ensure-scratch-buffer
  :category "buffer"
  :usage "(ensure-scratch-buffer) — (ensure-scratch-buffer)"
  :returns "buffer — The process-local scratch buffer."
  :side-effects "Creates and loads the scratch buffer when absent; preserves the current buffer."
  :see-also (scratch-buffer scratch-buffer-p *scratch-buffer-initial-text*))

(defdoc scratch-buffer-text
  :category "buffer"
  :usage "(scratch-buffer-text &optional BUF) — (setf (scratch-buffer-text) \"notes\")"
  :returns "string or nil — The editable scratch document text."
  :side-effects "SETF replaces the scratch buffer's editable text."
  :see-also (scratch-buffer ensure-scratch-buffer buffer-input-message))

(defdoc file-buffer-text
  :category "buffer"
  :usage "(file-buffer-text &optional BUF) — (setf (file-buffer-text buf) \"...\")"
  :returns "string — The editable text for a project-backed file buffer."
  :side-effects "SETF replaces text and updates buffer-dirty-p."
  :see-also (project-open-file project-save-buffer buffer-dirty-p))

(defdoc file-buffer-dirty-p
  :category "buffer"
  :usage "(file-buffer-dirty-p &optional BUF)"
  :returns "boolean — T when a project-backed file buffer has unsaved edits."
  :see-also (buffer-dirty-p file-buffer-text project-save-buffer))

(defdoc mark-buffer-dirty
  :category "buffer"
  :usage "(mark-buffer-dirty BUF)"
  :returns "buffer — The same buffer."
  :side-effects "Marks project-backed file buffers as modified."
  :see-also (buffer-dirty-p file-buffer-p project-save-buffer))

(defdoc next-buffer-name
  :category "buffer"
  :usage "(next-buffer-name) — (next-buffer-name)"
  :returns "string — \"session-1\", \"session-2\", etc."
  :side-effects "Increments the global buffer counter."
  :see-also (make-buffer buffer-name new-buffer-command))

(defdoc buffer-names
  :category "buffer"
  :usage "(buffer-names) — (buffer-names)"
  :returns "list of strings — (\"session-01\" \"session-02\" \"*help:make-buffer*\")"
  :see-also (*buffer-ring* buffer-name find-buffer-by-name))

(defdoc buffer-finalize-input
  :category "buffer"
  :usage "(buffer-finalize-input BUF:buffer) — (buffer-finalize-input (current-buffer))"
  :returns "buffer — The modified buffer."
  :side-effects "Makes input message read-only, timestamps it, creates new empty input."
  :see-also (buffer buffer-input-message send-message))

(defdoc buffer-insert-agent-message
  :category "buffer"
  :usage "(buffer-insert-agent-message BUF:buffer TEXT:string) — (buffer-insert-agent-message buf \"Hello!\")"
  :returns "message — The newly inserted agent message."
  :side-effects "Creates a read-only message and inserts it before the input message."
  :see-also (buffer-insert-system-message buffer-finalize-input buffer-input-message))

(defdoc buffer-insert-system-message
  :category "buffer"
  :usage "(buffer-insert-system-message BUF:buffer TEXT:string) — (buffer-insert-system-message buf \"[Session saved]\")"
  :returns "message — The newly inserted system message."
  :side-effects "Creates a read-only :system message. Excluded from API conversation history."
  :see-also (buffer-insert-agent-message build-conversation-messages))

(defdoc buffer-message-count
  :category "buffer"
  :usage "(buffer-message-count BUF:buffer) — (buffer-message-count (current-buffer))"
  :returns "fixnum — Total number of messages including the input message."
  :see-also (buffer buffer-first-message message-next))

;;; ==========================================================================
;;; Category: session — Session persistence
;;; ==========================================================================

(defdoc *sessions-dir*
  :category "session"
  :returns "pathname — Root directory for saved session snapshots and transcript sidecars."
  :see-also (save-session load-session list-saved-sessions load-or-create-session))

(defdoc session
  :category "session"
  :usage "(load-or-create-session \"session-01\")"
  :returns "Structure — Persistent metadata for a chat session and its current transcript segment."
  :see-also (buffer-session load-or-create-session session-current-transcript-path))

(defdoc session-name
  :category "session"
  :usage "(session-name SESSION:session)"
  :returns "string — Display name of the session."
  :see-also (session buffer-name))

(defdoc session-display-name
  :category "session"
  :usage "(session-display-name SESSION:session)"
  :returns "string or nil — User-facing label stored separately from the session name."
  :see-also (session-name set-session-display-name session-display-name-or-name))

(defdoc session-id
  :category "session"
  :usage "(session-id SESSION:session)"
  :returns "string — Filesystem-safe session id derived from the session name."
  :see-also (session session-directory))

(defdoc session-directory
  :category "session"
  :usage "(session-directory SESSION:session)"
  :returns "pathname — Sidecar directory containing session metadata and transcripts."
  :see-also (session-manifest-path session-transcript-directory))

(defdoc session-manifest-path
  :category "session"
  :usage "(session-manifest-path SESSION:session)"
  :returns "pathname — JSON metadata file for the session sidecar."
  :see-also (session-directory load-or-create-session))

(defdoc session-transcript-directory
  :category "session"
  :usage "(session-transcript-directory SESSION:session)"
  :returns "pathname — Directory containing numbered JSONL transcript segments."
  :see-also (session-current-transcript-path rotate-session-transcript))

(defdoc session-current-transcript-index
  :category "session"
  :usage "(session-current-transcript-index SESSION:session)"
  :returns "integer — Number of the current transcript segment."
  :see-also (session-current-transcript-path rotate-session-transcript))

(defdoc load-or-create-session
  :category "session"
  :usage "(load-or-create-session NAME:string)"
  :returns "session — Existing or newly initialized session metadata."
  :side-effects "Creates a session sidecar directory, manifest, and initial JSONL transcript when missing."
  :see-also (*sessions-dir* buffer-session session-current-transcript-path))

(defdoc session-display-name-or-name
  :category "session"
  :usage "(session-display-name-or-name SESSION:session)"
  :returns "string — Display name when present, otherwise the underlying session name."
  :see-also (session-display-name session-name set-session-display-name))

(defdoc set-session-display-name
  :category "session"
  :usage "(set-session-display-name SESSION:session VALUE:string)"
  :returns "session — SESSION with updated display-name metadata."
  :side-effects "Updates the session manifest immediately; blank values clear the display name."
  :see-also (session-display-name session-display-name-or-name save-session))

(defdoc session-current-transcript-path
  :category "session"
  :usage "(session-current-transcript-path SESSION:session)"
  :returns "pathname — Current append-only JSONL transcript segment."
  :see-also (session rotate-session-transcript record-session-message))

(defdoc record-session-message
  :category "session"
  :usage "(record-session-message SESSION:session MESSAGE:message)"
  :returns "message — The recorded message."
  :side-effects "Appends one JSONL message event to the current transcript."
  :see-also (session-current-transcript-path message-text))

(defdoc rotate-session-transcript
  :category "session"
  :usage "(rotate-session-transcript SESSION:session :reason :manual)"
  :returns "values — New transcript path and previous transcript path."
  :side-effects "Starts a new transcript segment whose first event references the previous transcript path."
  :see-also (default-compact-buffer session-current-transcript-path))

(defdoc save-session
  :category "session"
  :usage "(save-session BUF:buffer) — (save-session (current-buffer))"
  :returns "pathname — Path to the saved session file."
  :side-effects "Writes buffer's current snapshot to a JSON file in *sessions-dir*. Persistent chat buffers also refresh this snapshot automatically after transcripted messages."
  :see-also (load-session list-saved-sessions save-session-command *sessions-dir* buffer-session))

(defdoc load-session
  :category "session"
  :usage "(load-session SESSION-NAME:string &key AGENT-NAME:string) — (load-session \"session-01\")"
  :returns "buffer or nil — The loaded buffer, or nil if no snapshot or transcript sidecar exists."
  :side-effects "Reads a JSON snapshot or transcript sidecar, creates a buffer with replayed messages, and attaches session transcript metadata."
  :see-also (save-session list-saved-sessions *sessions-dir* buffer-session))

(defdoc list-saved-sessions
  :category "session"
  :usage "(list-saved-sessions) — (list-saved-sessions)"
  :returns "list of strings from saved snapshots and transcript sidecar manifests — (\"session-01\" \"session-02\") or nil."
  :see-also (save-session load-session *sessions-dir*))

;;; ==========================================================================
;;; Category: command — Command infrastructure
;;; ==========================================================================

(defdoc *current-caller*
  :category "tool"
  :returns "keyword — :USER for interactive use, or an agent keyword while provider tools run."
  :see-also (tool-definitions-for-api execute-tool deftool))

(defdoc *tool-working-directory*
  :category "tool"
  :returns "pathname or nil — Base directory used to resolve relative tool paths; absolute paths and parent traversal remain valid."
  :side-effects "Dynamically bound from the buffer's captured working directory while a tool executes. This is path context, not containment."
  :see-also (execute-tool resolve-tool-path tool-working-directory-pathname))

(defdoc *command-table*
  :category "command"
  :see-also (defcommand command-metadata list-available-commands))

(defdoc command-metadata
  :category "command"
  :usage "Created automatically by defcommand."
  :returns "Structure — Holds name, docstring, keybindings, lambda list, and minibuffer prompt metadata for a command."
  :see-also (defcommand *command-table* command-metadata-name command-metadata-docstring))

(defdoc command-metadata-name
  :category "command"
  :usage "(command-metadata-name META:command-metadata)"
  :returns "symbol — The command's name."
  :see-also (command-metadata command-metadata-docstring))

(defdoc command-metadata-docstring
  :category "command"
  :usage "(command-metadata-docstring META:command-metadata)"
  :returns "string — The command's documentation string."
  :see-also (command-metadata command-metadata-name))

(defdoc command-metadata-keybindings
  :category "command"
  :usage "(command-metadata-keybindings META:command-metadata)"
  :returns "list — Key specifications declared in the defcommand form."
  :see-also (command-metadata find-keybindings-for-command))

(defdoc command-metadata-lambda-list
  :category "command"
  :usage "(command-metadata-lambda-list META:command-metadata)"
  :returns "list — The command's required positional lambda list."
  :see-also (command-required-arguments defcommand))

(defdoc command-metadata-prompts
  :category "command"
  :usage "(command-metadata-prompts META:command-metadata)"
  :returns "list — Minibuffer prompt specs for the command's non-buffer arguments."
  :see-also (command-required-arguments defcommand))

(defdoc command-metadata-package
  :category "command"
  :usage "(command-metadata-package META:command-metadata)"
  :returns "string or NIL — Owning RPLACA package name for package-defined commands."
  :see-also (command-metadata list-available-commands package-enablement-scope))

(defdoc defcommand
  :category "command"
  :usage "(defun NAME (BUFFER &rest REQUIRED-ARGS) DOCSTRING ...) then (defcommand NAME :keys (...) :prompts (...))"
  :returns "command-metadata — Metadata for the registered command."
  :side-effects "Registers an existing function as an M-x command in *command-table*. PROMPTS supplies minibuffer readers for non-buffer arguments."
  :see-also (*command-table* command-metadata list-available-commands deftool defdoc))

(defdoc list-available-commands
  :category "command"
  :usage "(list-available-commands :buffer BUF)"
  :returns "list of symbols — Registered commands visible in the package context."
  :see-also (*command-table* defcommand command-metadata-package))

(defdoc command-required-arguments
  :category "command"
  :usage "(command-required-arguments COMMAND-NAME:symbol)"
  :returns "list — The non-buffer required argument names for the command."
  :see-also (command-metadata-lambda-list command-metadata-prompts))

(defdoc resolve-command-prompt-reader
  :category "command"
  :usage "(resolve-command-prompt-reader READER)"
  :returns "function — A callable reader for minibuffer text."
  :see-also (defcommand command-metadata-prompts))

;;; ==========================================================================
;;; Category: docs — Extended documentation system
;;; ==========================================================================

(defdoc *extended-docs*
  :category "docs"
  :see-also (defdoc extended-doc undocumented-functions undocumented-variables))

(defdoc defdoc
  :category "docs"
  :usage "(defdoc SYMBOL :category \"cat\" :usage \"...\" :returns \"...\" :see-also (sym1 sym2) :side-effects \"...\")"
  :returns "plist — The documentation plist stored in *extended-docs*."
  :side-effects "Stores a documentation plist in *extended-docs* keyed by SYMBOL."
  :see-also (*extended-docs* extended-doc deftool undocumented-functions))

(defdoc deftool
  :category "tool"
  :usage "(deftool SYMBOL :name \"provider_name\" :description \"...\" :args ((arg :type \"string\") (items :type \"array\" :items ((:type . \"object\")))))"
  :returns "agent-tool-metadata — Metadata for the registered provider-callable tool."
  :side-effects "Registers an existing function as an agent tool and syncs the provider tool table when available. Argument specs support :type, :description, :required, and :items for array schemas. If SYMBOL is a registered command, command call style is inferred and the current tool buffer is supplied automatically."
  :see-also (register-agent-tool-metadata defcommand execute-tool))

(defdoc extended-doc
  :category "docs"
  :usage "(extended-doc SYMBOL &optional KEY) — (extended-doc 'make-buffer :usage)"
  :returns "plist or value — Full doc plist without KEY, specific value with KEY."
  :see-also (*extended-docs* defdoc))

(defdoc undocumented-functions
  :category "docs"
  :usage "(undocumented-functions) — (undocumented-functions)"
  :returns "list of symbols — Exported function symbols without extended docs."
  :see-also (list-functions *extended-docs* undocumented-variables))

(defdoc undocumented-variables
  :category "docs"
  :usage "(undocumented-variables) — (undocumented-variables)"
  :returns "list of symbols — Exported variable symbols without extended docs."
  :see-also (list-variables *extended-docs* undocumented-functions))

;;; ==========================================================================
;;; Category: keymap — Keymap structure and bindings
;;; ==========================================================================

(defdoc keymap
  :category "keymap"
  :usage "(make-keymap \"my-keymap\") to create a keymap."
  :returns "Class — A key-to-command mapping with optional parent chain."
  :see-also (make-keymap keymap-bind keymap-lookup *default-keymap*))

(defdoc make-keymap
  :category "keymap"
  :usage "(make-keymap NAME:string &optional PARENT:keymap) — (make-keymap \"buffer-keymap\" *default-keymap*)"
  :returns "keymap — A new empty keymap."
  :see-also (keymap keymap-bind keymap-lookup))

(defdoc keymap-name
  :category "keymap"
  :usage "(keymap-name KM:keymap) — (keymap-name *default-keymap*)"
  :returns "string — The keymap's name."
  :see-also (keymap make-keymap))

(defdoc keymap-bindings
  :category "keymap"
  :usage "(keymap-bindings KM:keymap) — (keymap-bindings *default-keymap*)"
  :returns "hash-table — Maps key specs to command symbols."
  :see-also (keymap keymap-bind keymap-lookup))

(defdoc keymap-parent
  :category "keymap"
  :usage "(keymap-parent KM:keymap) — (keymap-parent my-keymap)"
  :returns "keymap or nil — Parent keymap for fallback lookups."
  :see-also (keymap make-keymap keymap-lookup))

(defdoc keymap-bind
  :category "keymap"
  :usage "(keymap-bind KM:keymap KEY COMMAND:symbol) — (keymap-bind km #\\Return 'send-message)"
  :returns "symbol — The bound command."
  :side-effects "Stores the key-to-command binding in the keymap's hash table."
  :see-also (keymap keymap-lookup make-keymap))

(defdoc keymap-lookup
  :category "keymap"
  :usage "(keymap-lookup KM:keymap KEY) — (keymap-lookup *default-keymap* #\\Return)"
  :returns "symbol or nil — The command bound to KEY, or nil."
  :see-also (keymap keymap-bind keymap-parent))

(defdoc *default-keymap*
  :category "keymap"
  :see-also (keymap make-keymap keymap-bind keymap-lookup))

(defdoc *alt-emulates-meta*
  :category "keymap"
  :usage "(setf *alt-emulates-meta* nil)"
  :returns "boolean — Whether McCLIM treats physical Alt as Meta."
  :side-effects "Controls McCLIM key normalization for standalone Alt key events."
  :see-also (*default-keymap* keymap-lookup))

;;; ==========================================================================
;;; Category: project — Persistent project resources
;;; ==========================================================================

(defdoc project
  :category "project"
  :usage "(make-project :name \"config\" :root #P\"~/.rplaca.d/\")"
  :returns "Structure — A named project resource root."
  :see-also (define-project list-projects project-read-file))

(defdoc make-project
  :category "project"
  :usage "(make-project :name NAME :root ROOT :description TEXT :source SOURCE)"
  :returns "project — A project structure."
  :see-also (project register-project define-project))

(defdoc project-name
  :category "project"
  :usage "(project-name PROJECT)"
  :returns "string — Project display name."
  :see-also (find-project list-projects))

(defdoc project-root
  :category "project"
  :usage "(project-root PROJECT)"
  :returns "pathname — Root directory for project resources."
  :see-also (project-resolve-path define-project))

(defdoc project-description
  :category "project"
  :usage "(project-description PROJECT)"
  :returns "string or nil — Human description for selectors and docs."
  :see-also (define-project create-project))

(defdoc project-source
  :category "project"
  :usage "(project-source PROJECT)"
  :returns "keyword — :PROGRAMMATIC, :MANIFEST, or :BUILTIN."
  :see-also (load-project-definitions reload-projects))

(defdoc project-systems
  :category "project"
  :usage "(project-systems PROJECT)"
  :returns "list — ASDF systems declared by the project."
  :see-also (define-project create-project))

(defdoc project-packages
  :category "project"
  :usage "(project-packages PROJECT)"
  :returns "list — package install requests declared by the project."
  :see-also (define-project create-project load-project-declared-packages))

(defdoc *project-definitions-directory*
  :category "project"
  :usage "Pathname — defaults to ~/.rplaca.projects.d/."
  :see-also (load-project-definitions create-project))

(defdoc *project-registry*
  :category "project"
  :usage "Hash table keyed by normalized project name."
  :see-also (register-project find-project list-projects))

(defdoc *project-manifest-extension*
  :category "project"
  :usage "String — extension for inert project manifests."
  :see-also (create-project load-project-definitions))

(defdoc *project-ignored-directory-names*
  :category "project"
  :usage "List of directory names skipped by project traversal."
  :see-also (project-list-files project-search))

(defdoc *project-list-file-limit*
  :category "project"
  :usage "Integer or nil — default PROJECT-LIST-FILES result limit."
  :see-also (project-list-files))

(defdoc *project-search-result-limit*
  :category "project"
  :usage "Integer or nil — default PROJECT-SEARCH match limit."
  :see-also (project-search))

(defdoc define-project
  :category "project"
  :usage "(define-project \"name\" :root #P\"/path/\" :description \"...\")"
  :returns "project — The registered project."
  :side-effects "Registers a project for the current process."
  :see-also (create-project register-project find-project))

(defdoc create-project
  :category "project"
  :usage "(create-project \"name\" :root #P\"/path/\" :description \"...\" :persist nil)"
  :returns "project — The created project."
  :side-effects "Creates the root directory when needed and persists an inert manifest by default."
  :see-also (define-project load-project-definitions *project-definitions-directory*))

(defdoc register-project
  :category "project"
  :usage "(register-project PROJECT &key REPLACE)"
  :returns "project — The registered or preserved project."
  :side-effects "Mutates *project-registry*."
  :see-also (project define-project list-projects))

(defdoc find-project
  :category "project"
  :usage "(find-project \"config\")"
  :returns "project or nil — The matching project."
  :see-also (list-projects define-project))

(defdoc list-projects
  :category "project"
  :usage "(list-projects)"
  :returns "list of projects — Registered projects sorted by name."
  :see-also (find-project load-project-definitions))

(defdoc load-project-definitions
  :category "project"
  :usage "(load-project-definitions)"
  :returns "list of projects — Loaded project registry."
  :side-effects "Loads inert manifests and ensures the config project."
  :see-also (reload-projects *project-definitions-directory*))

(defdoc reload-projects
  :category "project"
  :usage "(reload-projects)"
  :returns "list of projects — Reloaded project registry."
  :side-effects "Reloads manifest and builtin projects while preserving programmatic projects."
  :see-also (load-project-definitions define-project))

(defdoc project-resolve-path
  :category "project"
  :usage "(project-resolve-path \"project\" \"relative/path\")"
  :returns "pathname — Resolved path inside the project root."
  :see-also (project-read-file project-save-file))

(defdoc project-list-files
  :category "project"
  :usage "(project-list-files \"project\" :limit 100)"
  :returns "list of strings — Project-relative file paths."
  :see-also (project-read-file project-search))

(defdoc project-read-file
  :category "project"
  :usage "(project-read-file \"project\" \"path\")"
  :returns "string — Resource contents."
  :see-also (project-list-files project-save-file))

(defdoc project-create-file
  :category "project"
  :usage "(project-create-file \"project\" \"path\" :content \"...\" :if-exists :supersede)"
  :returns "plist — Save summary."
  :side-effects "Creates a project resource; by default errors if it exists. Updates any open buffer for the same resource."
  :see-also (project-save-file project-open-file))

(defdoc project-save-file
  :category "project"
  :usage "(project-save-file \"project\" \"path\" TEXT)"
  :returns "plist — Save summary."
  :side-effects "Writes TEXT to a project resource and synchronizes any open buffer for it."
  :see-also (project-read-file project-save-buffer))

(defdoc project-search
  :category "project"
  :usage "(project-search \"project\" \"query\" :limit 20)"
  :returns "list of plists — Each result has :PATH, :LINE, and :TEXT."
  :see-also (project-search-to-string project-list-files))

(defdoc project-search-to-string
  :category "project"
  :usage "(project-search-to-string \"project\" \"query\")"
  :returns "string — Agent-readable search results."
  :see-also (project-search))

(defdoc project-open-file
  :category "project"
  :usage "(project-open-file \"project\" \"path\")"
  :returns "buffer — Editable file buffer for the resource."
  :side-effects "Creates or switches to a project-backed file buffer."
  :see-also (file-buffer-text project-save-buffer))

(defdoc find-project-file-buffer
  :category "project"
  :usage "(find-project-file-buffer \"project\" \"path\")"
  :returns "buffer or nil — Existing open file buffer."
  :see-also (project-open-file file-buffer-p))

(defdoc project-save-buffer
  :category "project"
  :usage "(project-save-buffer &optional BUFFER)"
  :returns "plist — Save summary."
  :side-effects "Writes a project-backed file buffer and clears its dirty flag."
  :see-also (project-open-file file-buffer-text file-buffer-dirty-p))

(defdoc begin-change-set
  :category "project"
  :usage "(begin-change-set :name \"short-name\" :description \"...\")"
  :returns "change-set — Newly opened and current staged mutation set."
  :side-effects "Registers a change set and stores it in *current-change-set*."
  :see-also (stage-project-file change-set-diff-to-string apply-change-set))

(defdoc current-change-set
  :category "project"
  :usage "(current-change-set)"
  :returns "change-set or nil — The active staged mutation set."
  :see-also (begin-change-set list-change-sets))

(defdoc stage-project-file
  :category "project"
  :usage "(stage-project-file \"project\" \"path\" TEXT :change-set CHANGE-SET)"
  :returns "change-set-entry — Staged write entry."
  :side-effects "Adds a write entry to a change set without changing the project file."
  :see-also (change-set-project-file-text change-set-diff-to-string apply-change-set))

(defdoc stage-project-delete
  :category "project"
  :usage "(stage-project-delete \"project\" \"path\" :change-set CHANGE-SET)"
  :returns "change-set-entry — Staged delete entry."
  :side-effects "Adds a delete entry without changing the project file."
  :see-also (stage-project-file apply-change-set revert-change-set))

(defdoc stage-project-rename
  :category "project"
  :usage "(stage-project-rename \"project\" \"old.lisp\" \"new.lisp\" :change-set CHANGE-SET)"
  :returns "change-set-entry — Staged rename entry."
  :side-effects "Adds a rename entry without changing project files."
  :see-also (stage-project-delete apply-change-set revert-change-set))

(defdoc change-set-project-file-text
  :category "project"
  :usage "(change-set-project-file-text \"project\" \"path\" &optional CHANGE-SET)"
  :returns "string — Latest staged text when present, otherwise current project file text."
  :see-also (stage-project-file change-set-diff-to-string))

(defdoc change-set-diff-to-string
  :category "project"
  :usage "(change-set-diff-to-string &optional CHANGE-SET)"
  :returns "string — Agent-readable diff of staged entries."
  :see-also (begin-change-set apply-change-set discard-change-set))

(defdoc apply-change-set
  :category "project"
  :usage "(apply-change-set &optional CHANGE-SET)"
  :returns "change-set — Applied change set."
  :side-effects "Writes staged entries to project resources; rolls back entries already applied if an error occurs."
  :see-also (change-set-diff-to-string revert-change-set))

(defdoc discard-change-set
  :category "project"
  :usage "(discard-change-set &optional CHANGE-SET)"
  :returns "change-set — Discarded change set."
  :side-effects "Marks an unapplied change set discarded."
  :see-also (begin-change-set apply-change-set))

(defdoc revert-change-set
  :category "project"
  :usage "(revert-change-set &optional CHANGE-SET)"
  :returns "change-set — Reverted change set."
  :side-effects "Restores project files from snapshots captured during staging."
  :see-also (apply-change-set change-set-diff-to-string))

(defdoc run-project-checks
  :category "project"
  :usage "(run-project-checks \"project\")"
  :returns "list of plists — One status record per registered check."
  :side-effects "Calls project check functions."
  :see-also (define-project reload-project-system))

(defdoc reload-project-system
  :category "project"
  :usage "(reload-project-system \"project\" &optional SYSTEM)"
  :returns "list of plists — Reload results."
  :side-effects "Calls a project reload function or ASDF:LOAD-SYSTEM for registered systems."
  :see-also (define-project run-project-checks))

(defdoc project-outline-to-string
  :category "project"
  :usage "(project-outline-to-string \"project\" :path \"src/file.lisp\" :max-depth 1)"
  :returns "string — sexed outline for one file or all Lisp files."
  :see-also (project-find-definitions-to-string sexed-project-outline-to-string))

(defdoc project-find-definitions
  :category "project"
  :usage "(project-find-definitions \"project\" :name \"foo\" :head \"defun\")"
  :returns "list of plists — Definition forms with :PATH and sexed metadata."
  :see-also (project-find-definitions-to-string project-describe-definition-to-string))

(defdoc project-find-definitions-to-string
  :category "project"
  :usage "(project-find-definitions-to-string \"project\" :name \"foo\")"
  :returns "string — Agent-readable definition list."
  :see-also (project-find-definitions project-outline-to-string))

(defdoc project-find-references-to-string
  :category "project"
  :usage "(project-find-references-to-string \"project\" \"symbol-name\")"
  :returns "string — Agent-readable text references."
  :see-also (project-search-to-string project-find-definitions-to-string))

(defdoc project-package-map-to-string
  :category "project"
  :usage "(project-package-map-to-string \"project\")"
  :returns "string — defpackage and in-package forms found in Lisp resources."
  :see-also (project-outline-to-string))

(defdoc project-describe-definition-to-string
  :category "project"
  :usage "(project-describe-definition-to-string \"project\" \"foo\" :head \"defun\")"
  :returns "string — Location and source for the first matching definition."
  :see-also (project-find-definitions-to-string sexed-project-form-text))

;;; ==========================================================================
;;; Category: llm — LLM configuration, authentication, and API
;;; ==========================================================================

(defdoc *default-provider*
  :category "llm"
  :usage "*default-provider* → :zai"
  :returns "keyword — The default LLM provider keyword."
  :see-also (*default-model* *provider-fallback-models* agent-default resolve-buffer-provider-and-model)
  :side-effects "Changing this affects all new buffers that lack an agent-specific provider override.")

(defdoc *default-model*
  :category "llm"
  :usage "*default-model* → \"glm-5.2\""
  :returns "string — The default model name."
  :see-also (*default-provider* *provider-fallback-models* resolve-buffer-provider-and-model)
  :side-effects "Changing this affects all new buffers that lack both an agent-specific and provider-fallback model.")

(defdoc *default-core-system-prompt*
  :category "llm"
  :usage "String — default rplaca operating prompt inserted before the personality prompt."
  :see-also (*default-personality-prompt* build-agent-system-prompt)
  :side-effects "Changing this affects future requests for agents that do not provide their own core prompt.")

(defdoc *default-personality-prompt*
  :category "llm"
  :usage "String — default personality prompt inserted after the core system prompt."
  :see-also (*default-core-system-prompt* *personality-prompt-path* load-personality-prompt-file build-agent-system-prompt)
  :side-effects "Changing this affects future requests for agents that do not provide their own personality prompt.")

(defdoc *personality-prompt-path*
  :category "llm"
  :usage "Pathname — defaults to ~/.config/rplaca/system-prompt.txt"
  :see-also (*default-personality-prompt* load-personality-prompt-file)
  :side-effects "rplaca-main loads this file once during startup before init.lisp; init.lisp may then change the path and call load-personality-prompt-file again.")

(defdoc load-personality-prompt-file
  :category "llm"
  :usage "(load-personality-prompt-file &optional PATH) — (load-personality-prompt-file #P\"~/my-prompt.txt\")"
  :returns "string or nil — The trimmed prompt text, or NIL when PATH is missing."
  :side-effects "Reads PATH and stores its contents into *default-personality-prompt*."
  :see-also (*default-personality-prompt* *personality-prompt-path* build-agent-system-prompt))

(defdoc *boot-file-names*
  :category "llm"
  :usage "List of strings — boot markdown files loaded ahead of the core and personality prompts."
  :see-also (load-boot-files build-agent-system-prompt)
  :side-effects "Changing this affects which AGENTS.md/SOUL.md-style files are consulted for future prompt builds.")

(defdoc load-boot-files
  :category "llm"
  :usage "(load-boot-files &key :directory)"
  :returns "string or nil — Wrapped boot-file instruction content, or NIL when none are present."
  :side-effects "Reads AGENTS.md/SOUL.md-style files from the active buffer working-directory's ancestors, falling back to ~/.config/rplaca/ when no project-local file applies."
  :see-also (*boot-file-names* build-agent-system-prompt))

(defdoc *agent-defaults-path*
  :category "llm"
  :usage "Pathname — defaults to ~/.config/rplaca/agent-defaults.json"
  :see-also (load-agent-defaults save-agent-defaults))

(defdoc agent-definition
  :category "llm"
  :usage "Created by register-agent-definition for reusable agent defaults."
  :returns "Structure — Name, routing defaults, prompt fragments, and optional tool allowlist."
  :see-also (register-agent-definition find-agent-definition list-agent-definitions run-subagent))

(defdoc agent-definition-tool-names
  :category "llm"
  :usage "(agent-definition-tool-names DEFINITION:agent-definition)"
  :returns "list of strings or nil — Tool allowlist for this agent; nil means default tool visibility."
  :see-also (register-agent-definition run-subagent *active-tool-names*))

(defdoc register-agent-definition
  :category "llm"
  :usage "(register-agent-definition NAME &key PROVIDER MODEL THINK-LEVEL CORE-PROMPT PERSONALITY-PROMPT TOOL-NAMES)"
  :returns "agent-definition — The registered definition."
  :side-effects "Updates the process-local agent definition registry used by buffers and subagents."
  :see-also (find-agent-definition list-agent-definitions run-subagent))

(defdoc find-agent-definition
  :category "llm"
  :usage "(find-agent-definition AGENT-NAME:string)"
  :returns "agent-definition or nil."
  :see-also (register-agent-definition list-agent-definitions))

(defdoc list-agent-definitions
  :category "llm"
  :usage "(list-agent-definitions)"
  :returns "list of agent-definition — Sorted by agent name."
  :see-also (register-agent-definition find-agent-definition))

(defdoc *default-max-tokens*
  :category "llm"
  :see-also (*default-model* *default-provider* provider-request))

(defdoc *openai-codex-reasoning-summary*
  :category "llm"
  :usage "*openai-codex-reasoning-summary*"
  :returns "string or nil — Responses API reasoning.summary mode requested for OpenAI Codex calls; defaults to \"detailed\"."
  :see-also (openai-codex-request openai-codex-request-streaming buffer-show-reasoning-p))

(defdoc *zai-env-var*
  :category "llm"
  :see-also (read-env-token read-provider-token))

(defdoc *zai-model*
  :category "llm"
  :see-also (*zai-api-url* zai-request))

(defdoc *zai-api-url*
  :category "llm"
  :see-also (*zai-model* zai-request))

(defdoc *openai-oauth-client-id*
  :category "llm"
  :see-also (openai-codex-oauth-start exchange-openai-oauth-code))

(defdoc *openai-oauth-auth-url*
  :category "llm"
  :see-also (openai-codex-oauth-start *openai-oauth-client-id*))

(defdoc *openai-oauth-token-url*
  :category "llm"
  :see-also (exchange-openai-oauth-code refresh-openai-codex-oauth-token))

(defdoc *openai-oauth-redirect-uri*
  :category "llm"
  :see-also (openai-codex-oauth-start extract-oauth-callback-params))

(defdoc *openai-oauth-scopes*
  :category "llm"
  :see-also (openai-codex-oauth-start *openai-oauth-client-id*))

(defdoc *codex-auth-path*
  :category "llm"
  :see-also (read-openai-codex-oauth-tokens save-openai-codex-oauth-tokens))

(defdoc *openai-oauth-pending*
  :category "llm"
  :see-also (openai-codex-oauth-command openai-codex-oauth-start openai-codex-oauth-finish))

(defdoc *provider-known-models*
  :category "llm"
  :see-also (provider-known-models provider-has-token-p available-models-for-selector))

(defdoc read-env-token
  :category "llm"
  :usage "(read-env-token ENV-VAR:string) — (read-env-token \"ZAI_CODING_MAX_API_KEY\")"
  :returns "string or nil — Trimmed token value, or nil if unset/empty."
  :see-also (read-provider-token *zai-env-var* *openrouter-env-var*))

(defdoc provider-token-path
  :category "llm"
  :usage "(provider-token-path PROVIDER:keyword) — (provider-token-path :zai)"
  :returns "pathname — Provider-specific token file under ~/.config/rplaca/."
  :see-also (read-provider-token save-provider-token))

(defdoc read-provider-token
  :category "llm"
  :usage "(read-provider-token PROVIDER:keyword) — (read-provider-token :zai)"
  :returns "string or nil — The token with highest priority source, or nil."
  :see-also (save-provider-token read-env-token provider-token-path))

(defdoc save-provider-token
  :category "llm"
  :usage "(save-provider-token PROVIDER:keyword TOKEN:string) — (save-provider-token :zai \"sk-...\")"
  :returns "string — The saved token."
  :side-effects "Writes TOKEN to the provider's token file. Sets file permissions to 600."
  :see-also (read-provider-token provider-token-path))

(defdoc generate-code-verifier
  :category "llm"
  :usage "(generate-code-verifier) — (generate-code-verifier)"
  :returns "string — A 43-character PKCE code verifier (RFC 7636)."
  :see-also (compute-code-challenge openai-codex-oauth-start))

(defdoc compute-code-challenge
  :category "llm"
  :usage "(compute-code-challenge CODE-VERIFIER:string) — (compute-code-challenge (generate-code-verifier))"
  :returns "string — S256 PKCE code challenge."
  :side-effects "Invokes openssl via subprocess for SHA-256 + base64url encoding."
  :see-also (generate-code-verifier openai-codex-oauth-start))

(defdoc generate-oauth-state
  :category "llm"
  :usage "(generate-oauth-state) — (generate-oauth-state)"
  :returns "string — A base64url random state token for CSRF protection."
  :see-also (openai-codex-oauth-start extract-oauth-callback-params))

(defdoc url-encode-param
  :category "llm"
  :usage "(url-encode-param STRING:string) — (url-encode-param \"hello world\")"
  :returns "string — \"hello%20world\""
  :see-also (openai-codex-oauth-start exchange-openai-oauth-code))

(defdoc extract-oauth-callback-params
  :category "llm"
  :usage "(extract-oauth-callback-params URL:string) — (extract-oauth-callback-params \"http://localhost:1455/auth/callback?code=abc&state=xyz\")"
  :returns "values code:string state:string — The authorization code and state parameter."
  :see-also (openai-codex-oauth-finish openai-codex-oauth-start))

(defdoc openai-codex-oauth-start
  :category "llm"
  :usage "(openai-codex-oauth-start) — (openai-codex-oauth-start)"
  :returns "values auth-url:string code-verifier:string state:string"
  :see-also (openai-codex-oauth-finish exchange-openai-oauth-code openai-codex-oauth-command))

(defdoc exchange-openai-oauth-code
  :category "llm"
  :usage "(exchange-openai-oauth-code CODE:string CODE-VERIFIER:string)"
  :returns "plist — (:id-token ... :access-token ... :refresh-token ... :account-id ...)"
  :side-effects "Makes an HTTP POST to the OAuth token endpoint."
  :see-also (openai-codex-oauth-start openai-codex-oauth-finish save-openai-codex-oauth-tokens))

(defdoc save-openai-codex-oauth-tokens
  :category "llm"
  :usage "(save-openai-codex-oauth-tokens ACCESS-TOKEN REFRESH-TOKEN EXPIRES-IN)"
  :returns "string — The access token."
  :side-effects "Writes shared Codex auth.json credentials to disk with 600 permissions."
  :see-also (read-openai-codex-oauth-tokens exchange-openai-oauth-code *codex-auth-path*))

(defdoc read-openai-codex-oauth-tokens
  :category "llm"
  :usage "(read-openai-codex-oauth-tokens) — (read-openai-codex-oauth-tokens)"
  :returns "plist or nil — (:auth-mode ... :openai-api-key ... :id-token ... :access-token ... :refresh-token ... :account-id ... :last-refresh ...)"
  :see-also (save-openai-codex-oauth-tokens read-openai-codex-oauth-token *codex-auth-path*))

(defdoc refresh-openai-codex-oauth-token
  :category "llm"
  :usage "(refresh-openai-codex-oauth-token REFRESH-TOKEN:string)"
  :returns "string or nil — The refreshed ChatGPT access token, or nil on failure."
  :side-effects "Refreshes shared Codex auth.json credentials and saves the updated payload to disk."
  :see-also (read-openai-codex-oauth-token save-openai-codex-oauth-tokens))

(defdoc read-openai-codex-oauth-token
  :category "llm"
  :usage "(read-openai-codex-oauth-token) — (read-openai-codex-oauth-token)"
  :returns "string or nil — A valid ChatGPT access token, auto-refreshed when stale."
  :side-effects "May refresh shared Codex auth.json via HTTP when the ChatGPT token is stale."
  :see-also (read-openai-codex-oauth-tokens refresh-openai-codex-oauth-token))

(defdoc openai-codex-oauth-finish
  :category "llm"
  :usage "(openai-codex-oauth-finish CALLBACK-URL CODE-VERIFIER EXPECTED-STATE)"
  :returns "string — The access token."
  :side-effects "Exchanges the authorization code for tokens, optionally exchanges the ID token for an API key, and saves the shared Codex auth.json payload."
  :see-also (openai-codex-oauth-start extract-oauth-callback-params openai-codex-oauth-command))

(defdoc load-agent-defaults
  :category "llm"
  :usage "(load-agent-defaults) — (load-agent-defaults)"
  :returns "list — The agent defaults registry."
  :side-effects "Reads ~/.config/rplaca/agent-defaults.json and memoizes the result."
  :see-also (save-agent-defaults agent-default set-agent-default ensure-agent-defaults-loaded))

(defdoc save-agent-defaults
  :category "llm"
  :usage "(save-agent-defaults) — (save-agent-defaults)"
  :returns "pathname — Path to the saved defaults file."
  :side-effects "Writes the agent defaults registry to disk as JSON."
  :see-also (load-agent-defaults set-agent-default))

(defdoc agent-default
  :category "llm"
  :usage "(agent-default AGENT-NAME:string) — (agent-default \"coder\")"
  :returns "keyword — :OPENAI-CODEX, :ZAI, or :OPENROUTER."
  :see-also (set-agent-default resolve-buffer-provider-and-model load-agent-defaults))

(defdoc set-agent-default
  :category "llm"
  :usage "(set-agent-default AGENT-NAME:string PROVIDER:keyword &key MODEL:string) — (set-agent-default \"coder\" :zai :model \"glm-5.2\")"
  :returns "keyword — The normalized provider."
  :side-effects "Updates the agent defaults registry and persists to disk."
  :see-also (agent-default save-agent-defaults resolve-buffer-provider-and-model))

(defdoc ensure-agent-defaults-loaded
  :category "llm"
  :usage "(ensure-agent-defaults-loaded) — (ensure-agent-defaults-loaded)"
  :returns "list — The agent defaults registry."
  :side-effects "Loads defaults from disk on first call (lazy initialization)."
  :see-also (load-agent-defaults agent-default))

(defdoc resolve-buffer-provider-and-model
  :category "llm"
  :usage "(resolve-buffer-provider-and-model BUF:buffer) — (resolve-buffer-provider-and-model (current-buffer))"
  :returns "values provider:keyword model:string think-level:(or null string) — e.g. :ZAI, \"glm-5.2\", nil"
  :see-also (buffer-provider-override buffer-model-override buffer-think-level-override agent-default))

(defdoc provider-model-supported-think-levels
  :category "llm"
  :usage "(provider-model-supported-think-levels PROVIDER:keyword MODEL:string) — (provider-model-supported-think-levels :openai-codex \"gpt-5.6-sol\")"
  :returns "list of strings or nil — e.g. (\"none\" \"low\" \"medium\" \"high\" \"xhigh\")"
  :see-also (resolve-buffer-provider-and-model select-think-level-command))

(defdoc reconcile-buffer-think-level-override
  :category "llm"
  :usage "(reconcile-buffer-think-level-override BUF:buffer &key PROVIDER MODEL)"
  :returns "values status:keyword think-level:(or null string)"
  :side-effects "Clears stale think-level overrides that are unsupported by the active model."
  :see-also (buffer-think-level-override provider-model-supported-think-levels))

(defdoc build-conversation-messages
  :category "llm"
  :usage "(build-conversation-messages BUF:buffer) — (build-conversation-messages (current-buffer))"
  :returns "list — API-format messages array. :context messages are sent as user context; :system messages are omitted."
  :see-also (provider-request buffer-first-message message-raw-content))

(defdoc api-json-encode
  :category "llm"
  :usage "(api-json-encode OBJECT) — (api-json-encode '((:key . \"value\")))"
  :returns "string — JSON string with double-dash keys encoded as underscores."
  :see-also (api-json-decode))

(defdoc api-json-decode
  :category "llm"
  :usage "(api-json-decode STRING:string) — (api-json-decode \"{\\\"key\\\": \\\"value\\\"}\")"
  :returns "alist — Decoded JSON with underscore-preserving key mapping."
  :see-also (api-json-encode))

(defdoc zai-request
  :category "llm"
  :usage "(zai-request MESSAGES:list CALLBACK:function &key MODEL TOOLS)"
  :returns "list — The API response as an alist."
  :side-effects "Makes HTTP POST to the Z.AI Chat Completions API."
  :see-also (zai-request-streaming *zai-model* *zai-api-url*))

(defdoc zai-request-streaming
  :category "llm"
  :usage "(zai-request-streaming MESSAGES:list CALLBACK:function &key MODEL TOOLS)"
  :returns "stream-state — State object for polling streaming progress."
  :side-effects "Spawns a background thread for streaming from Z.AI API."
  :see-also (zai-request *zai-model*))

(defdoc provider-known-models
  :category "llm"
  :usage "(provider-known-models PROVIDER:keyword) — (provider-known-models :zai)"
  :returns "list of strings — (\"glm-5.2\" \"glm-5\" ...)"
  :see-also (*provider-known-models* provider-has-token-p available-models-for-selector))

(defdoc provider-has-token-p
  :category "llm"
  :usage "(provider-has-token-p PROVIDER:keyword) — (provider-has-token-p :zai)"
  :returns "boolean — T if the provider has a usable API key or OAuth token."
  :see-also (read-provider-token provider-known-models available-models-for-selector))

(defdoc available-models-for-selector
  :category "llm"
  :usage "(available-models-for-selector BUF:buffer) — (available-models-for-selector (current-buffer))"
  :returns "list of plists — ((:provider :zai :model \"name\" :active-p t) ...)"
  :see-also (provider-known-models provider-has-token-p select-model-command minibuffer-select-model-command))

;;; ==========================================================================
;;; Category: tool — Tool registry and execution
;;; ==========================================================================

(defdoc *http-fetch-max-chars*
  :category "tool"
  :returns "integer - Default character limit for the dormant http_fetch helper implementation."
  :see-also (*http-connection-timeout* *http-user-agent*))

(defdoc *http-connection-timeout*
  :category "tool"
  :returns "integer - Connection timeout for the dormant http_fetch helper implementation."
  :see-also (*http-fetch-max-chars* *http-user-agent*))

(defdoc *http-user-agent*
  :category "tool"
  :returns "string - User-Agent header used by the dormant http_fetch helper implementation."
  :see-also (*http-fetch-max-chars* *http-connection-timeout*))

(defdoc *file-read-default-limit*
  :category "tool"
  :returns "integer - Default line limit for the lispi read tool."
  :see-also (*tool-table* register-tool))

(defdoc *find-default-limit*
  :category "tool"
  :returns "integer - Default result limit for the lispi find tool."
  :see-also (*tool-table* register-tool))

(defdoc *grep-default-limit*
  :category "tool"
  :returns "integer - Default line-match limit for the lispi grep tool."
  :see-also (*tool-table* register-tool))

(defdoc *grep-max-file-bytes*
  :category "tool"
  :returns "integer - Maximum file size searched by the lispi grep tool."
  :see-also (*grep-default-limit*))

(defdoc *grep-max-line-length*
  :category "tool"
  :returns "integer - Maximum displayed characters for each lispi grep match line."
  :see-also (*grep-default-limit*))

(defdoc *search-ignored-directory-names*
  :category "tool"
  :returns "list of strings - Directory names skipped by the lispi find and grep tools."
  :see-also (*find-default-limit* *grep-default-limit*))

(defdoc *shell-exec-default-timeout*
  :category "tool"
  :returns "integer - Default timeout for the dormant shell_exec helper implementation."
  :see-also (*tool-table* register-tool))

(defdoc *diff-display-max-lines*
  :category "tool"
  :returns "integer - Maximum diff lines included in lispi file-edit result data."
  :see-also (compute-simple-diff execute-edit))

(defdoc *lisp-eval-default-package*
  :category "tool"
  :returns "string - Default package used by lisp_eval when :package is omitted."
  :side-effects "init-tools sets this to \"RPLACA\" for the rplaca default tool surface."
  :see-also (init-tools eval-history-to-string))

(defdoc *last-eval-result*
  :category "tool"
  :returns "list or nil — Multiple-value list from the last successful lisp_eval."
  :see-also (*last-eval-condition* eval-history-to-string))

(defdoc *last-eval-condition*
  :category "tool"
  :returns "condition or nil — Last condition captured by lisp_eval."
  :see-also (*last-eval-result* eval-history-to-string))

(defdoc *lisp-eval-history*
  :category "tool"
  :returns "list — Newest-first lisp_eval execution records."
  :see-also (lisp-eval-record eval-history-to-string))

(defdoc eval-history-to-string
  :category "tool"
  :usage "(eval-history-to-string :limit 10)"
  :returns "string — Agent-readable recent lisp_eval history."
  :see-also (*lisp-eval-history* *last-eval-result* *last-eval-condition*))

(defdoc execute-read
  :category "tool"
  :usage "(execute-read ARGS:lisp-data)"
  :returns "string — File contents, truncated by line window."
  :side-effects "Reads a text file resolved against the current tool working directory."
  :see-also (*file-read-default-limit* resolve-tool-path deftool))

(defdoc execute-find
  :category "tool"
  :usage "(execute-find ARGS:lisp-data)"
  :returns "string — Matching file paths as Lisp data."
  :side-effects "Searches file names below the requested path, resolved against the current tool working directory."
  :see-also (*find-default-limit* resolve-tool-path deftool))

(defdoc execute-grep
  :category "tool"
  :usage "(execute-grep ARGS:lisp-data)"
  :returns "string — Matching lines as Lisp data."
  :side-effects "Searches file contents below the requested path, resolved against the current tool working directory."
  :see-also (*grep-default-limit* *grep-max-file-bytes* resolve-tool-path deftool))

(defdoc execute-write
  :category "tool"
  :usage "(execute-write ARGS:lisp-data)"
  :returns "string — Write result as Lisp data."
  :side-effects "Creates or overwrites a text file resolved against the current tool working directory."
  :see-also (resolve-tool-path deftool))

(defdoc execute-edit
  :category "tool"
  :usage "(execute-edit ARGS:lisp-data)"
  :returns "string — Edit result as Lisp data."
  :side-effects "Replaces one exact text occurrence in a file resolved against the current tool working directory."
  :see-also (resolve-tool-path deftool))

(defdoc execute-lisp-eval
  :category "tool"
  :usage "(execute-lisp-eval ARGS:lisp-data)"
  :returns "string — Evaluation result as Lisp data."
  :side-effects "Evaluates one Common Lisp form in the running image and records lisp_eval history."
  :see-also (*lisp-eval-history* *last-eval-result* *last-eval-condition* deftool))

(defdoc *tool-table*
  :category "tool"
  :see-also (register-tool execute-tool tool-definitions-for-api init-tools))

(defdoc *active-tool-names*
  :category "tool"
  :returns "list of strings or nil — Dynamic allowlist for the current agent run; nil means default visibility."
  :side-effects "Binding this constrains tool-definitions-for-api and execute-tool for the dynamic extent."
  :see-also (run-subagent register-agent-definition tool-definitions-for-api execute-tool))

(defdoc *temporary-tool-table*
  :category "tool"
  :returns "hash-table or nil — Dynamic table of temporary tool definitions for the current prompt or subagent run."
  :side-effects "Binding this changes effective tool lookup for tool-definitions-for-api, execute-tool, and tool display helpers."
  :see-also (make-subagent-tool run-subagent run-subagent-async tool-definitions-for-api))

(defdoc *current-tool-buffer*
  :category "tool"
  :returns "buffer or nil — Dynamic buffer supplied automatically while provider tools execute."
  :side-effects "Command-style provider tools use this as their leading BUFFER argument."
  :see-also (deftool defcommand execute-tool))

(defdoc agent-tool-metadata
  :category "tool"
  :usage "Created by deftool or register-agent-tool-metadata."
  :returns "Structure — Holds provider name, description, explicit args, schema, call style, execution ownership, and owning Lisp symbol."
  :see-also (deftool defcommand register-agent-tool-metadata list-agent-tool-metadata))

(defdoc agent-tool-metadata-package
  :category "tool"
  :usage "(agent-tool-metadata-package META:agent-tool-metadata)"
  :returns "string or NIL — Owning RPLACA package name for package-defined tools."
  :see-also (agent-tool-metadata tool-definitions-for-api package-enablement-scope))

(defdoc register-agent-tool-metadata
  :category "tool"
  :usage "(register-agent-tool-metadata SYMBOL TOOL-SPEC &key COMMAND-P LAMBDA-LIST DOCSTRING)"
  :returns "agent-tool-metadata or nil — NIL when TOOL-SPEC is NIL and existing metadata is removed."
  :side-effects "Stores tagged tool metadata and immediately syncs it into the provider tool table when available."
  :see-also (deftool defcommand unregister-agent-tool-metadata find-agent-tool-metadata))

(defdoc unregister-agent-tool-metadata
  :category "tool"
  :usage "(unregister-agent-tool-metadata SYMBOL)"
  :returns "agent-tool-metadata or nil — The removed metadata, when present."
  :side-effects "Removes SYMBOL's tagged metadata and provider tool table entry."
  :see-also (register-agent-tool-metadata find-agent-tool-metadata))

(defdoc find-agent-tool-metadata
  :category "tool"
  :usage "(find-agent-tool-metadata SYMBOL-OR-NAME)"
  :returns "agent-tool-metadata or nil."
  :see-also (list-agent-tool-metadata register-agent-tool-metadata))

(defdoc list-agent-tool-metadata
  :category "tool"
  :usage "(list-agent-tool-metadata)"
  :returns "list of agent-tool-metadata — Sorted by provider tool name."
  :see-also (find-agent-tool-metadata render-agent-tools-section))

(defdoc tool-definition
  :category "tool"
  :usage "Created by register-tool."
  :returns "Structure — Holds name, description, schema, execution ownership, execute-fn, and optional owning package."
  :see-also (register-tool *tool-table* execute-tool))

(defdoc subagent-tool
  :category "tool"
  :usage "Created by make-subagent-tool for temporary subagent tool exposure."
  :returns "Structure — Holds name, description, schema, execution ownership, and execute-fn."
  :see-also (make-subagent-tool run-subagent run-subagent-async))

(defdoc make-subagent-tool
  :category "tool"
  :usage "(make-subagent-tool :name \"lookup\" :description \"Look up a value\" :input-schema '((:type . \"object\")) :execute-fn (lambda (args) ...))"
  :returns "subagent-tool — A temporary tool definition accepted by :custom-tools."
  :side-effects "Does not mutate *tool-table*. The tool is only active when passed through :custom-tools or manually bound in *temporary-tool-table*."
  :see-also (run-subagent run-subagent-async *temporary-tool-table* register-tool))

(defdoc register-tool
  :category "tool"
  :usage "(register-tool NAME DESCRIPTION SCHEMA EXECUTE-FN &key PACKAGE EXECUTION)"
  :returns "tool-definition — The registered tool."
  :side-effects "Stores the tool definition in *tool-table*."
  :see-also (*tool-table* tool-definition execute-tool init-tools))

(defdoc execute-tool
  :category "tool"
  :usage "(execute-tool NAME:string ARGS:lisp-data) — (execute-tool \"lisp_eval\" '(:code \"(+ 1 2)\"))"
  :returns "string — The tool execution result. File tools return plain text; lisp_eval returns a printed Lisp plist for structured display and history."
  :side-effects "Executes the tool's function. Side effects depend on the registered tool implementation."
  :see-also (register-tool *tool-table*))

(defdoc tool-definitions-for-api
  :category "tool"
  :usage "(tool-definitions-for-api :buffer BUF)"
  :returns "vector — Provider tool definitions visible to the current caller and package context."
  :see-also (*tool-table* *active-tool-names* register-tool provider-request render-agent-tools-section))

(defdoc render-agent-tools-section
  :category "tool"
  :usage "(render-agent-tools-section)"
  :returns "string or nil — Active provider tool names and descriptions rendered for the system prompt."
  :see-also (tool-definitions-for-api build-agent-system-prompt))

(defdoc format-tool-call-sexpr
  :category "tool"
  :usage "(format-tool-call-sexpr NAME:string ARGS:lisp-data)"
  :returns "string — S-expression formatted tool call. \"(lisp_eval :code \\\"(+ 1 2)\\\")\""
  :see-also (format-tool-call-expanded))

(defdoc format-tool-call-expanded
  :category "tool"
  :usage "(format-tool-call-expanded NAME:string ARGS:lisp-data)"
  :returns "string — Multi-line expanded display of a tool call."
  :see-also (format-tool-call-sexpr))

(defdoc init-tools
  :category "tool"
  :usage "(init-tools) — Called once at startup."
  :returns "nil"
  :side-effects "Removes the reserved lisp_eval entry and re-registers process-global provider tools tagged through deftool. Package tools are registered by active package loading."
  :see-also (*tool-table* deftool register-tool))

;;; ==========================================================================
;;; Category: safe-reload — Safe in-place source reload
;;; ==========================================================================

(defdoc rplaca-safe-reload
  :category "safe-reload"
  :usage "(rplaca-safe-reload :buffer BUF)"
  :returns "safe-reload-result — :OK, :BUSY, :PREFLIGHT-FAILED, or :LIVE-FAILED."
  :side-effects "Uses a nonblocking reload lock, runs an isolated worker preflight, and only then reloads :rplaca in the live image. Inserts a visible system notification when :BUFFER is supplied; does not reset buffers, sessions, or start a new frame."
  :see-also (rplaca-safe-reload-preflight rplaca-reload-result-ok-p safe-reload-rplaca-command))

(defdoc rplaca-safe-reload-preflight
  :category "safe-reload"
  :usage "(rplaca-safe-reload-preflight)"
  :returns "safe-reload-result — An isolated worker preflight result."
  :side-effects "Starts a fresh SBCL worker process to load RPLACA source without mutating the current image."
  :see-also (rplaca-safe-reload rplaca-reload-result-summary))

(defdoc rplaca-reload-result-ok-p
  :category "safe-reload"
  :usage "(rplaca-reload-result-ok-p RESULT)"
  :returns "boolean — T only for a completed :OK safe reload result."
  :see-also (rplaca-safe-reload rplaca-reload-result-summary))

(defdoc rplaca-reload-result-summary
  :category "safe-reload"
  :usage "(rplaca-reload-result-summary RESULT)"
  :returns "string — Human-readable reload result summary."
  :see-also (rplaca-safe-reload rplaca-reload-result-ok-p))

(defdoc safe-reload-rplaca-command
  :category "safe-reload"
  :usage "M-x safe-reload-rplaca-command"
  :returns "safe-reload-result — The command result."
  :side-effects "Delegates to RPLACA-SAFE-RELOAD for the current buffer and leaves the visible session/buffer state intact except for the system notification."
  :see-also (rplaca-safe-reload))

;;; ==========================================================================
;;; Category: tool-runtime — Tool sequencing state
;;; ==========================================================================

(defdoc buffer-stashed-input
  :category "tool-runtime"
  :usage "(buffer-stashed-input BUF:buffer) — (buffer-stashed-input (current-buffer))"
  :returns "string or nil — The compose text preserved while a tool batch runs."
  :see-also (buffer-pending-tool-calls buffer-status))

(defdoc buffer-pending-tool-calls
  :category "tool-runtime"
  :usage "(buffer-pending-tool-calls BUF:buffer)"
  :returns "list — Tool-use blocks awaiting sequential execution."
  :see-also (buffer-tool-call-results buffer-pending-tool-execution))

(defdoc buffer-tool-call-results
  :category "tool-runtime"
  :usage "(buffer-tool-call-results BUF:buffer)"
  :returns "list — Accumulated results from completed or refused tool calls."
  :see-also (buffer-pending-tool-calls buffer-pending-tool-execution))
(defdoc new-buffer-command
  :category "buffer-command"
  :usage "Bound to C-x n. Creates a new chat buffer and switches to it."
  :returns "nil"
  :side-effects "Creates a new buffer, initializes keymap, adds to ring."
  :see-also (make-buffer add-buffer-to-ring kill-buffer-command))

(defdoc new-listener-buffer-command
  :category "buffer-command"
  :usage "Bound to C-x l. Opens McCLIM's native Listener application."
  :returns "application-frame — The newly opened McCLIM Listener frame."
  :side-effects "Starts the Listener frame top level in a new process."
  :see-also (clim-listener:run-listener))

(defdoc kill-buffer-command
  :category "buffer-command"
  :usage "Bound to C-x k. Kills the current buffer."
  :returns "nil"
  :side-effects "Removes the current buffer from the ring. Switches to the next buffer."
  :see-also (kill-buffer-from-ring new-buffer-command))

(defdoc next-buffer-command
  :category "buffer-command"
  :usage "Switches to the next buffer in the ring."
  :returns "nil"
  :side-effects "Rotates *buffer-ring*, moving current buffer to end."
  :see-also (*buffer-ring* minibuffer-select-buffer-command))

(defdoc save-session-command
  :category "buffer-command"
  :usage "Bound to C-x C-s. Saves the current file buffer or chat session."
  :returns "nil"
  :side-effects "Writes project file text or conversation JSON. Inserts confirmation system message."
  :see-also (project-save-buffer save-session load-session *sessions-dir*))

(defdoc load-session-command
  :category "buffer-command"
  :usage "Bound to C-x C-r. Opens the minibuffer saved-session selector and loads the selected session into a new buffer."
  :returns "nil"
  :side-effects "Reads a saved session JSON snapshot, creates a chat buffer for it, and switches to that buffer."
  :see-also (load-session list-saved-sessions minibuffer-activate save-session-command))

(defdoc continue-session-command
  :category "buffer-command"
  :usage "M-x continue-session-command"
  :returns "nil"
  :side-effects "Loads the most recently updated saved session for the current buffer working directory."
  :see-also (load-session-command most-recent-saved-session-record))

(defdoc session-info-command
  :category "buffer-command"
  :usage "M-x session-info-command"
  :returns "nil"
  :side-effects "Opens a help buffer summarizing the active session's ids, paths, routing, and token/cache usage."
  :see-also (set-session-display-name-command continue-session-command))

(defdoc set-session-display-name-command
  :category "buffer-command"
  :usage "M-x set-session-display-name-command"
  :returns "nil"
  :side-effects "Sets or clears the current chat session's display name, persists it, and inserts a confirmation system message."
  :see-also (set-session-display-name session-info-command save-session-command))

(defdoc execute-extended-command
  :category "command"
  :usage "M-x, then type a fuzzy abbreviation such as tdbg and press RET"
  :returns "nil — Opens a fuzzy command selector, or invokes the selected command."
  :side-effects "Shows matching command rows in the minibuffer; RET dispatches the selected command through invoke-command, including command-argument minibuffer prompts when needed."
  :see-also (invoke-command make-command-selector-items minibuffer-activate))

(defdoc minibuffer-select-project-command
  :category "buffer-command"
  :usage "Bound to C-x p. Selects the active project for the current chat buffer."
  :returns "nil"
  :side-effects "Updates buffer-project-name and buffer-working-directory."
  :see-also (list-projects buffer-project-name open-project-file-command))

(defdoc open-project-file-command
  :category "buffer-command"
  :usage "Bound to C-x C-f. Opens a project file via minibuffer completion."
  :returns "nil"
  :side-effects "Creates or switches to a project-backed file buffer."
  :see-also (project-open-file minibuffer-select-project-command))

(defdoc create-project-file-command
  :category "buffer-command"
  :usage "M-x create-project-file-command prompts for project and path."
  :returns "nil"
  :side-effects "Creates a project resource and opens it as a file buffer."
  :see-also (project-create-file project-open-file))

(defdoc search-project-command
  :category "buffer-command"
  :usage "M-x search-project-command prompts for project and query."
  :returns "nil"
  :side-effects "Inserts search results as a system message."
  :see-also (project-search-to-string project-search))

(defdoc toggle-tool-results-command
  :category "buffer-command"
  :usage "Bound to C-c t. Toggles visibility of tool result messages."
  :returns "nil"
  :side-effects "Flips buffer-show-tool-results-p."
  :see-also (buffer-show-tool-results-p))

(defdoc toggle-reasoning-output-command
  :category "buffer-command"
  :usage "Bound to C-c V. Toggles visibility of provider-supplied reasoning/verbose output."
  :returns "nil"
  :side-effects "Flips buffer-show-reasoning-p."
  :see-also (buffer-show-reasoning-p))

(defdoc toggle-metadata-output-command
  :category "buffer-command"
  :usage "Bound to C-c I. Toggles visibility of provider/response metadata."
  :returns "nil"
  :side-effects "Flips buffer-show-metadata-p."
  :see-also (buffer-show-metadata-p))

(defdoc redraw-screen-command
  :category "buffer-command"
  :usage "Bound to C-l. Requests a full redraw."
  :returns ":redraw"
  :side-effects "Returns :redraw to the active event loop."
  :see-also (rplaca-main))

(defdoc openai-codex-oauth-command
  :category "buffer-command"
  :usage "Starts the OpenAI Codex OAuth login flow."
  :returns "nil"
  :side-effects "Starts a localhost callback server, launches the browser login when possible, and sets the current buffer to :oauth status."
  :see-also (openai-codex-oauth-start openai-codex-oauth-finish *openai-oauth-pending*))

;;; ==========================================================================
;;; Category: packages — User-installed package loading
;;; ==========================================================================

(defdoc package-channel
  :category "packages"
  :usage "Struct — local package channel with name, root, description, and source."
  :see-also (register-package-channel list-package-channels package-definition))

(defdoc package-definition
  :category "packages"
  :usage "Struct — package metadata discovered from a channel manifest."
  :see-also (list-available-packages find-available-package load-rplaca-package package-channel))

(defdoc package-prompt-section
  :category "packages"
  :usage "Struct — system-prompt section contributed by a loaded package."
  :see-also (register-package-prompt-section render-package-prompt-sections))

(defdoc *default-package-channel-directory*
  :category "packages"
  :usage "Pathname — defaults to packages/channels/default/ inside the rplaca repo."
  :see-also (*package-channels* register-package-channel))

(defdoc *package-channels*
  :category "packages"
  :usage "List of package-channel structures scanned by reload-package-channels."
  :see-also (register-package-channel list-package-channels reload-package-channels))

(defdoc *enabled-builtin-packages*
  :category "packages"
  :usage "Legacy compatibility variable for old builtin autoload init files; package enablement now uses packages.json."
  :see-also (load-autoload-packages *default-package-channel-directory* *package-configuration-path*))

(defdoc *package-configuration-path*
  :category "packages"
  :usage "Pathname — defaults to ~/.rplaca.d/packages.json"
  :see-also (active-package-names set-package-enablement-scope))

(defdoc *packages-directory*
  :category "packages"
  :usage "Pathname — defaults to ~/.rplaca.d/packages/"
  :see-also (rplaca-use-package *user-init-file* register-package-channel))

(defdoc *current-rplaca-package*
  :category "packages"
  :usage "Dynamically bound package name while a RPLACA package entrypoint is loading."
  :see-also (load-rplaca-package define-buffer-type register-package-prompt-section))

(defdoc register-package-channel
  :category "packages"
  :usage "(register-package-channel \"NAME\" #P\"/path/to/channel/\" :description \"...\")"
  :returns "package-channel — the registered channel definition."
  :side-effects "Adds or replaces a channel in *package-channels* and clears package discovery cache."
  :see-also (*package-channels* reload-package-channels list-package-channels))

(defdoc list-package-channels
  :category "packages"
  :usage "(list-package-channels)"
  :returns "list — registered package-channel structures."
  :see-also (register-package-channel reload-package-channels))

(defdoc reload-package-channels
  :category "packages"
  :usage "(reload-package-channels)"
  :returns "list — package-definition structures discovered from registered channels."
  :side-effects "Reads local channel manifests and updates the package discovery cache."
  :see-also (list-available-packages find-available-package))

(defdoc list-available-packages
  :category "packages"
  :usage "(list-available-packages)"
  :returns "list — cached package-definition structures advertised by channels."
  :see-also (reload-package-channels find-available-package list-installed-packages))

(defdoc find-available-package
  :category "packages"
  :usage "(find-available-package \"sexed\")"
  :returns "package-definition or NIL."
  :see-also (list-available-packages find-installed-package))

(defdoc list-installed-packages
  :category "packages"
  :usage "(list-installed-packages)"
  :returns "list — package definitions present on disk, including channel packages and cloned packages."
  :see-also (find-installed-package rplaca-use-package list-available-packages))

(defdoc find-installed-package
  :category "packages"
  :usage "(find-installed-package \"sexed\")"
  :returns "package-definition or NIL."
  :see-also (list-installed-packages load-rplaca-package))

(defdoc package-install-record-for-definition
  :category "packages"
  :usage "(package-install-record-for-definition DEFINITION)"
  :returns "plist or NIL — Persisted install metadata for DEFINITION when present."
  :see-also (rplaca-use-package package-status-to-string package-doctor-to-string))

(defdoc package-install-status-entry
  :category "packages"
  :usage "(package-install-status-entry DEFINITION :buffer BUF)"
  :returns "plist — install, enablement, source, and resource policy information."
  :see-also (package-status-to-string package-doctor-report))

(defdoc package-doctor-report
  :category "packages"
  :usage "(package-doctor-report :buffer BUF)"
  :returns "list — status plists for installed packages visible in the context."
  :see-also (package-doctor-to-string package-status-to-string))

(defdoc package-status-to-string
  :category "packages"
  :usage "(package-status-to-string :buffer BUF)"
  :returns "string — human-readable installed package status report."
  :see-also (package-doctor-report package-doctor-to-string))

(defdoc package-doctor-to-string
  :category "packages"
  :usage "(package-doctor-to-string :buffer BUF)"
  :returns "string — human-readable package health report."
  :see-also (package-doctor-report package-status-to-string))

(defdoc install-package-status-string
  :category "packages"
  :usage "(install-package-status-string \"sexed\")"
  :returns "string — one-line install summary for a package."
  :see-also (package-status-to-string package-doctor-to-string))

(defdoc package-resource-policy-string
  :category "packages"
  :usage "(package-resource-policy-string DEFINITION)"
  :returns "string — resource-policy summary for one package."
  :see-also (package-install-record-for-definition set-installed-package-resource-types))

(defdoc set-installed-package-resource-types
  :category "packages"
  :usage "(set-installed-package-resource-types \"sexed\" '(:tool :command))"
  :returns "list — the normalized allowed resource types."
  :side-effects "Updates the install record sidecar and reloads the package so filtered resources are reflected in memory."
  :see-also (package-install-record-for-definition package-status-to-string))

(defdoc remove-installed-package
  :category "packages"
  :usage "(remove-installed-package \"sexed\")"
  :returns "package-definition or NIL."
  :side-effects "Deletes the installed package directory and removes package-owned runtime registrations."
  :see-also (update-installed-package rplaca-use-package))

(defdoc update-installed-package
  :category "packages"
  :usage "(update-installed-package \"sexed\")"
  :returns "package-definition or NIL."
  :side-effects "Refreshes the installed package from its recorded source and reloads the package entrypoint."
  :see-also (remove-installed-package rplaca-use-package))

(defdoc package-enablement-scope
  :category "packages"
  :usage "(package-enablement-scope \"sexed\" :buffer BUF)"
  :returns "keyword — :BUFFER, :AGENT, :GLOBAL, or :DEFAULT."
  :see-also (set-package-enablement-scope cycle-package-enablement-scope active-package-names))

(defdoc set-package-enablement-scope
  :category "packages"
  :usage "(set-package-enablement-scope \"sexed\" :global) or (set-package-enablement-scope \"sexed\" :buffer :buffer BUF)"
  :returns "keyword — the selected scope."
  :side-effects "Persists global/agent package configuration and mutates buffer package state for :BUFFER scope."
  :see-also (package-enablement-scope cycle-package-enablement-scope *package-configuration-path*))

(defdoc cycle-package-enablement-scope
  :category "packages"
  :usage "(cycle-package-enablement-scope \"sexed\" :buffer BUF)"
  :returns "keyword — the new scope after cycling default → buffer → agent → global → default."
  :side-effects "Moves the package to the new scope and removes it from other scopes in the same context."
  :see-also (set-package-enablement-scope minibuffer-toggle-package-command))

(defdoc active-package-names
  :category "packages"
  :usage "(active-package-names :buffer BUF)"
  :returns "list — package names enabled by global, agent, or buffer scope."
  :see-also (load-active-packages package-enablement-scope))

(defdoc package-owned-buffer-types
  :category "packages"
  :usage "(package-owned-buffer-types \"dashboard-package\")"
  :returns "list — buffer-type structures registered by the package."
  :see-also (define-buffer-type describe-installed-package-to-string))

(defdoc load-rplaca-package
  :category "packages"
  :usage "(load-rplaca-package \"sexed\")"
  :returns "package-definition on success, NIL on warning or failure."
  :side-effects "Loads package dependencies, then loads the package entrypoint into the rplaca package."
  :see-also (list-installed-packages load-active-packages rplaca-use-package))

(defdoc load-active-packages
  :category "packages"
  :usage "(load-active-packages :buffer BUF)"
  :returns "list — active package definitions loaded for the given context."
  :side-effects "Loads enabled package entrypoints and their dependencies; inactive package registrations remain hidden."
  :see-also (active-package-names load-rplaca-package))

(defdoc load-autoload-packages
  :category "packages"
  :usage "(load-autoload-packages)"
  :returns "list — globally enabled package definitions loaded for compatibility."
  :side-effects "Compatibility wrapper around load-active-packages with no buffer context."
  :see-also (load-active-packages active-package-names))

(defdoc register-package-prompt-section
  :category "packages"
  :usage "(register-package-prompt-section \"NAME\" \"## Prompt text\" :package \"pkg\")"
  :returns "package-prompt-section — the registered prompt contribution."
  :side-effects "Adds or replaces a package prompt section used by build-agent-system-prompt."
  :see-also (list-package-prompt-sections render-package-prompt-sections))

(defdoc list-package-prompt-sections
  :category "packages"
  :usage "(list-package-prompt-sections)"
  :returns "list — registered package-prompt-section structures in prompt order."
  :see-also (register-package-prompt-section render-package-prompt-sections))

(defdoc render-package-prompt-sections
  :category "packages"
  :usage "(render-package-prompt-sections (list-package-prompt-sections) :buffer BUF)"
  :returns "string or NIL — active package prompt sections rendered for the system prompt."
  :see-also (register-package-prompt-section build-agent-system-prompt active-package-names))

(defdoc rplaca-use-package
  :category "packages"
  :usage "(rplaca-use-package :src-type :git :repo \"https://example.com/user/repo.git\")"
  :returns "package-definition — non-nil on successful install, NIL on warning or failure."
  :side-effects "Creates the package install directory when needed, runs git clone for missing packages, and validates manifest.lisp without loading or enabling the entrypoint."
  :see-also (*packages-directory* list-installed-packages set-package-enablement-scope *user-init-file* load-user-init-file))

(defdoc load-project-declared-packages
  :category "packages"
  :usage "(load-project-declared-packages)"
  :returns "list — newly installed project-declared package definitions."
  :side-effects "Auto-installs missing project package declarations into the correct project-local or global scope."
  :see-also (create-project load-project-definitions rplaca-use-package))

(defdoc minibuffer-toggle-package-command
  :category "packages"
  :usage "M-x minibuffer-toggle-package-command"
  :returns "nil"
  :side-effects "Opens a minibuffer package selector; RET cycles default, buffer, agent, and global enablement. Newly enabled packages append their prompt/tool context to non-empty buffers, and disabling a package retracts its injected context from the live buffer transcript."
  :see-also (cycle-package-enablement-scope describe-installed-package-command))

(defdoc describe-installed-package-command
  :category "packages"
  :usage "M-x describe-installed-package-command"
  :returns "nil"
  :side-effects "Opens a minibuffer package selector and displays package help derived from registered package commands, tools, docs, and prompt sections."
  :see-also (list-installed-packages load-rplaca-package))

;;; ==========================================================================
;;; Category: utilities — Small helpers useful from lisp_eval
;;; ==========================================================================

(defdoc count-occurrences
  :category "utilities"
  :usage "(count-occurrences \"needle\" \"haystack needle\")"
  :returns "integer — Non-overlapping substring occurrence count."
  :see-also (search count))

;;; ==========================================================================
;;; Category: init — User init file
;;; ==========================================================================

(defdoc *user-init-directory*
  :category "init"
  :usage "Pathname — defaults to ~/.rplaca.d/"
  :see-also (*user-init-file* *inhibit-user-init* load-user-init-file))

(defdoc *user-init-file*
  :category "init"
  :usage "Pathname — defaults to ~/.rplaca.d/init.lisp"
  :see-also (*user-init-directory* *inhibit-user-init* load-user-init-file))

(defdoc *inhibit-user-init*
  :category "init"
  :usage "Boolean — set to T via --no-init flag to skip loading the user init file."
  :see-also (*user-init-file* load-user-init-file))

(defdoc hook-metadata
  :category "init"
  :usage "Returned by list-hooks and find-hook-metadata."
  :returns "Metadata describing a hook variable name, argument list, and docstring."
  :see-also (defhook list-hooks find-hook-metadata))

(defdoc defhook
  :category "init"
  :usage "(defhook *example-hook* (buffer) \"Called with BUFFER.\")"
  :returns "hook-metadata for the registered hook variable."
  :side-effects "Defines a special hook variable and records its argument metadata."
  :see-also (add-hook run-hook-with-args list-hooks))

(defdoc find-hook-metadata
  :category "init"
  :usage "(find-hook-metadata '*startup-hook*)"
  :returns "hook-metadata or nil."
  :see-also (defhook list-hooks hook-metadata))

(defdoc list-hooks
  :category "init"
  :usage "(list-hooks)"
  :returns "list of hook-metadata sorted by hook variable name."
  :see-also (defhook find-hook-metadata))

(defdoc *startup-hook*
  :category "init"
  :usage "List of function designators run after init.lisp loads and before McCLIM startup."
  :see-also (add-hook remove-hook run-hooks *initial-buffer-hook* rplaca-main))

(defdoc *initial-buffer-hook*
  :category "init"
  :usage "List of function designators run with the initial buffer after it is created."
  :see-also (add-hook remove-hook run-hook-with-args *startup-hook* rplaca-main))

(defdoc *before-command-hook*
  :category "init"
  :usage "List of functions called as (FUNCTION BUFFER COMMAND) before an interactive command runs."
  :see-also (*after-command-hook* invoke-command add-hook))

(defdoc *after-command-hook*
  :category "init"
  :usage "List of functions called as (FUNCTION BUFFER COMMAND RESULT) after an interactive command returns."
  :see-also (*before-command-hook* invoke-command add-hook))

(defdoc *before-tool-hook*
  :category "init"
  :usage "List of functions called as (FUNCTION TOOL-NAME ARGS) before an agent tool runs."
  :see-also (*after-tool-hook* execute-tool add-hook))

(defdoc *after-tool-hook*
  :category "init"
  :usage "List of functions called as (FUNCTION TOOL-NAME ARGS RESULT) after an agent tool returns."
  :see-also (*before-tool-hook* execute-tool add-hook))

(defdoc *before-send-message-hook*
  :category "init"
  :usage "List of functions called as (FUNCTION BUFFER INPUT-TEXT) before a non-empty chat input is sent."
  :see-also (*after-send-message-hook* send-message add-hook))

(defdoc *after-send-message-hook*
  :category "init"
  :usage "List of functions called as (FUNCTION BUFFER INPUT-TEXT RESULT) after a non-empty chat input send returns."
  :see-also (*before-send-message-hook* send-message add-hook))

(defdoc add-hook
  :category "init"
  :usage "(add-hook '*startup-hook* #'my-fn &key APPEND)"
  :returns "The function designator that was added."
  :side-effects "Mutates the hook variable named by HOOK-VAR."
  :see-also (remove-hook run-hooks run-hook-with-args list-hooks))

(defdoc remove-hook
  :category "init"
  :usage "(remove-hook '*startup-hook* #'my-fn)"
  :returns "The function designator that was removed."
  :side-effects "Mutates the hook variable named by HOOK-VAR."
  :see-also (add-hook run-hooks run-hook-with-args))

(defdoc run-hooks
  :category "init"
  :usage "(run-hooks '*startup-hook*)"
  :returns "nil"
  :side-effects "Calls every function in each named hook variable with no arguments. Individual hook errors are reported and do not stop later hooks."
  :see-also (run-hook-with-args add-hook remove-hook))

(defdoc run-hook-with-args
  :category "init"
  :usage "(run-hook-with-args '*initial-buffer-hook* buffer)"
  :returns "nil"
  :side-effects "Calls every function in the named hook variable with ARGS. Individual hook errors are reported and do not stop later hooks."
  :see-also (run-hooks add-hook remove-hook))

(defdoc advice-entry
  :category "init"
  :usage "Returned by add-advice, list-advices, and advice-member-p."
  :returns "Metadata describing an advice name, position, and function designator."
  :see-also (add-advice list-advices defadvice))

(defdoc add-advice
  :category "init"
  :usage "(add-advice 'send-message :before #'my-before-send :name 'my-before-send)"
  :returns "advice-entry for the registered advice."
  :side-effects "Replaces SYMBOL's fdefinition with an advice dispatcher while preserving the original function."
  :see-also (remove-advice clear-advices defadvice))

(defdoc remove-advice
  :category "init"
  :usage "(remove-advice 'send-message 'my-before-send)"
  :returns "list of removed advice-entry values, or nil."
  :side-effects "Removes matching advice and restores the original function when the last advice is removed."
  :see-also (add-advice advice-member-p clear-advices))

(defdoc advice-member-p
  :category "init"
  :usage "(advice-member-p 'send-message 'my-before-send)"
  :returns "advice-entry or nil."
  :see-also (add-advice remove-advice list-advices))

(defdoc list-advices
  :category "init"
  :usage "(list-advices 'send-message)"
  :returns "list of advice-entry values in invocation order."
  :see-also (add-advice remove-advice advice-member-p))

(defdoc clear-advices
  :category "init"
  :usage "(clear-advices 'send-message)"
  :returns "T when advice was present, otherwise nil."
  :side-effects "Removes every advice entry from SYMBOL and restores its original function."
  :see-also (add-advice remove-advice list-advices))

(defdoc defadvice
  :category "init"
  :usage "(defadvice send-message log-send :before (buffer) ...)"
  :returns "advice-entry for the newly defined advice function."
  :side-effects "Defines the advice function and registers it on the target symbol."
  :see-also (add-advice remove-advice clear-advices))

(defdoc load-user-init-file
  :category "init"
  :usage "(load-user-init-file) — Called once during startup."
  :returns "T on success, NIL if skipped, inhibited, or on error."
  :side-effects "Loads and evaluates *user-init-file* in the :rplaca package. Errors are caught, printed to stderr, and logged via file-debug-log. By the time init.lisp runs, the default keymap, tool registry, faces, and configured system prompt file are already loaded; use *startup-hook* or *initial-buffer-hook* for additional startup customization."
  :see-also (*user-init-file* *user-init-directory* *inhibit-user-init* *startup-hook* *initial-buffer-hook* rplaca-main))

;;; ==========================================================================
;;; Category: main — Application entry point
;;; ==========================================================================

(defdoc send-to-agent-with-context
  :category "main"
  :usage "(send-to-agent-with-context BUF:buffer) — (send-to-agent-with-context (current-buffer))"
  :returns "buffer — The buffer."
  :side-effects "Sets buffer status to :thinking. Starts a non-blocking streaming API call."
  :see-also (send-message build-conversation-messages provider-request-streaming))

(defdoc *prompt-max-tool-iterations*
  :category "main"
  :usage "Integer default used by run-single-prompt."
  :returns "integer — Default maximum non-interactive tool-call turns."
  :see-also (run-single-prompt run-subagent rplaca-prompt-main))

(defdoc *default-subagent-name*
  :category "main"
  :usage "String default used when run-subagent is called without :agent-name."
  :returns "string — Default transient subagent name."
  :see-also (run-subagent register-agent-definition))

(defdoc run-single-prompt
  :category "main"
  :usage "(run-single-prompt PROMPT &key :agent-name :provider :model :think-level :working-directory :max-tool-iterations :tool-names :custom-tools)"
  :returns "prompt-run-result — Final text, routing metadata, iteration count, and captured tool events."
  :side-effects "Creates an in-memory prompt buffer, sends non-streaming provider requests, executes exposed tools, inserts tool_result messages into the prompt buffer, and loops until a final assistant response is returned."
  :see-also (rplaca-prompt-main run-subagent provider-request execute-tool build-conversation-messages))

(defdoc run-subagent
  :category "main"
  :usage "(run-subagent PROMPT &key :agent-name :provider :model :think-level :working-directory :core-prompt :personality-prompt :tool-names :custom-tools :max-tool-iterations)"
  :returns "prompt-run-result — The delegated agent's final response and tool evidence."
  :side-effects "Runs a synchronous prompt-mode subagent. Transient prompt overrides and custom tools are dynamically scoped and do not mutate the agent or tool registries."
  :see-also (run-subagent-async make-subagent-tool register-agent-definition prompt-run-result prompt-run-used-tool-p *active-tool-names*))

(defdoc run-subagent-async
  :category "main"
  :usage "(run-subagent-async PROMPT &key :agent-name :provider :model :think-level :working-directory :core-prompt :personality-prompt :tool-names :custom-tools :max-tool-iterations)"
  :returns "subagent-handle — A process-local handle for polling, waiting, cancellation, and result inspection."
  :side-effects "Starts a background thread and stores the returned handle in the process-local subagent registry."
  :see-also (wait-subagent cancel-subagent subagent-snapshot run-subagent make-subagent-tool))

(defdoc subagent-handle
  :category "main"
  :usage "Returned by run-subagent-async."
  :returns "Structure — Background subagent id, prompt, agent name, status, result, error text, timestamps, thread, and cancellation flag."
  :see-also (run-subagent-async subagent-status subagent-snapshot wait-subagent))

(defdoc find-subagent
  :category "main"
  :usage "(find-subagent HANDLE-OR-ID)"
  :returns "subagent-handle or nil — Looks up a process-local async subagent handle."
  :see-also (list-subagents run-subagent-async))

(defdoc list-subagents
  :category "main"
  :usage "(list-subagents)"
  :returns "list of subagent-handle — Process-local async subagents sorted by start time."
  :see-also (find-subagent run-subagent-async))

(defdoc subagent-status
  :category "main"
  :usage "(subagent-status HANDLE)"
  :returns "keyword — :RUNNING, :CANCELLING, :SUCCEEDED, :FAILED, or :CANCELLED."
  :see-also (subagent-done-p subagent-result subagent-error subagent-snapshot))

(defdoc subagent-done-p
  :category "main"
  :usage "(subagent-done-p HANDLE)"
  :returns "boolean — True when the subagent has reached a terminal status and its worker has exited."
  :see-also (subagent-status wait-subagent))

(defdoc subagent-result
  :category "main"
  :usage "(subagent-result HANDLE)"
  :returns "prompt-run-result or nil — Available after :SUCCEEDED."
  :see-also (wait-subagent prompt-run-result subagent-error))

(defdoc subagent-error
  :category "main"
  :usage "(subagent-error HANDLE)"
  :returns "string or nil — Error text available after :FAILED."
  :see-also (subagent-status subagent-result))

(defdoc subagent-snapshot
  :category "main"
  :usage "(subagent-snapshot HANDLE)"
  :returns "plist — Immutable snapshot of id, prompt, agent-name, status, done-p, result, error, timestamps, worker settlement, and cancellation flag."
  :see-also (subagent-status subagent-result subagent-error))

(defdoc wait-subagent
  :category "main"
  :usage "(wait-subagent HANDLE :timeout 120)"
  :returns "values — RESULT, STATUS, HANDLE. On timeout returns NIL, :TIMEOUT, HANDLE."
  :see-also (run-subagent-async subagent-status cancel-subagent))

(defdoc cancel-subagent
  :category "main"
  :usage "(cancel-subagent HANDLE)"
  :returns "subagent-handle — The cancelled handle."
  :side-effects "Marks the subagent cancelling, closes its active provider stream, prevents later provider/tool iterations, and publishes :CANCELLED after the worker exits. A tool already executing is not forcibly preempted."
  :see-also (run-subagent-async wait-subagent subagent-status))

(defdoc prompt-run-tool-names
  :category "main"
  :usage "(prompt-run-tool-names RESULT:prompt-run-result)"
  :returns "list of strings — Tool names used by the prompt run."
  :see-also (prompt-run-tool-count prompt-run-used-tool-p prompt-run-result-tool-events))

(defdoc prompt-run-tool-count
  :category "main"
  :usage "(prompt-run-tool-count RESULT &optional TOOL-NAME)"
  :returns "integer — Count of all tool calls or only TOOL-NAME calls."
  :see-also (prompt-run-tool-names prompt-run-used-tool-p prompt-run-result-tool-events))

(defdoc prompt-run-used-tool-p
  :category "main"
  :usage "(prompt-run-used-tool-p RESULT TOOL-NAME)"
  :returns "boolean — T if RESULT includes at least one TOOL-NAME call."
  :see-also (prompt-run-tool-names prompt-run-tool-count prompt-run-result-tool-events))

(defdoc rplaca-prompt-main
  :category "main"
  :usage "(rplaca-prompt-main) — CLI entry point used by prompt.sh."
  :returns "does not return — Exits the Lisp image with status 0 or 1."
  :side-effects "Parses command-line options, defaults plain prompt.sh runs to openai-codex/gpt-5.6-sol unless --agent, --provider, or --model supplies routing, initializes the rplaca runtime without starting McCLIM, runs one prompt through run-single-prompt, and writes the final response or JSON result to stdout."
  :see-also (run-single-prompt *prompt-max-tool-iterations* rplaca-main))

(defdoc rplaca-main
  :category "main"
  :usage "(rplaca-main &key session-name agent-name window-title (run-frame t))"
  :returns "buffer — The initial chat buffer."
  :side-effects "Initializes state, loads the configured personality prompt file, loads user init, runs *startup-hook*, creates the initial buffer, runs *initial-buffer-hook*, and starts the McCLIM frame unless RUN-FRAME is NIL."
  :see-also (current-buffer *default-keymap* init-tools *startup-hook* *initial-buffer-hook*))
