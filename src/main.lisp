(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Self-insert support (must be defined before commands that reference it)
;;; --------------------------------------------------------------------------

(defvar *self-insert-char* nil
  "The character to insert for self-insert-command. Bound by the event loop.")

;;; --------------------------------------------------------------------------
;;; Interactive Command Dispatch
;;; --------------------------------------------------------------------------

(defun command-display-name (command)
  "Return the display name used for COMMAND in the UI."
  (string-downcase (symbol-name command)))

(defun command-keybinding-hints (command)
  "Return formatted keybinding strings for COMMAND in the default keymap."
  (let ((bindings (find-keybindings-for-command command)))
    (sort (mapcar #'format-key-binding bindings) #'string<)))

(defun make-command-selector-items (&key buffer)
  "Build minibuffer items for fuzzy M-x command selection."
  (mapcar (lambda (command)
            (let* ((name (command-display-name command))
                   (keys (command-keybinding-hints command))
                   (display (if keys
                                (format nil "~A  [~{~A~^, ~}]" name keys)
                                name)))
              (list :command command
                    :display display
                    :match-text name)))
          (sort (copy-list (list-available-commands :buffer buffer))
                #'string<
                :key #'command-display-name)))

(defun call-command-function (buffer command args)
  "Invoke COMMAND with BUFFER and ARGS, running command hooks around it."
  (run-hook-with-args '*before-command-hook* buffer command)
  (let ((values (multiple-value-list
                 (apply (symbol-function command) buffer args))))
    (run-hook-with-args '*after-command-hook* buffer command (first values))
    (values-list values)))

(defun prompt-command-arguments (buffer command specs &optional (collected nil)
                                                   (initial-input ""))
  "Prompt for SPECS sequentially in the minibuffer, then invoke COMMAND."
  (if (endp specs)
      (call-command-function buffer command (nreverse collected))
      (let* ((spec (first specs))
             (arg-name (getf spec :name))
             (prompt (getf spec :prompt))
             (reader (resolve-command-prompt-reader (getf spec :reader))))
        (minibuffer-prompt
         prompt
         (lambda (input)
           (handler-case
               (let ((value (funcall reader input)))
                 (prompt-command-arguments buffer command (rest specs)
                                           (cons value collected)))
             (error (e)
               (buffer-insert-system-message
                buffer
                (format nil "[Invalid ~A for ~A: ~A]"
                        (command-display-name arg-name)
                        (command-display-name command)
                        e))
               (prompt-command-arguments buffer command specs
                                         collected input))))
         :initial-input initial-input))))

(defun invoke-command (buffer command)
  "Invoke COMMAND from the UI, prompting for command arguments when needed."
  (let* ((metadata (find-command-metadata command))
         (required-args (and metadata (command-required-arguments command)))
         (prompts (and metadata
                       (command-metadata-prompts metadata))))
    (cond
      ((null metadata)
       (error "Unknown command: ~A" command))
      ((not (command-metadata-visible-p metadata :buffer buffer))
       (error "Command ~A belongs to an inactive package." command))
      ((null required-args)
       (call-command-function buffer command nil))
      (prompts
       (prompt-command-arguments buffer command prompts)
       nil)
      (t
       (error "Command ~A is missing prompt metadata for arguments ~S."
              command required-args)))))

;;; --------------------------------------------------------------------------
;;; Commands
;;; --------------------------------------------------------------------------

(defun dispatch-finalized-chat-input (buffer input-text)
  "Dispatch finalized INPUT-TEXT and run its after-send hook exactly once.

Managed shell/pipeline operations defer the hook to their frame-process apply
callback.  Provider streaming and ordinary extension prefixes retain the
historical immediate hook boundary."
  (multiple-value-bind (prefix-handled-p prefix-result)
      (process-prefix-command buffer input-text)
    (let ((result
            (cond
              ((and prefix-handled-p
                    (interactive-buffer-operation-p prefix-result))
               prefix-result)
              (prefix-handled-p
               ;; Preserve the historical ordinary-prefix contract while
               ;; still retaining the exact handler value above to recognize
               ;; managed operations.
               t)
              ((buffer-pipeline-name buffer)
               (start-interactive-pipeline-for-buffer buffer input-text))
              (t
               (send-to-agent-with-context buffer)))))
      (unless (interactive-buffer-operation-p result)
        (run-hook-with-args '*after-send-message-hook*
                            buffer input-text result))
      result)))

(defun dispatch-finalized-prose-input (buffer input-text)
  "Dispatch finalized literal prose without slash, template, or prefix routing."
  (let ((result
          (if (buffer-pipeline-name buffer)
              (start-interactive-pipeline-for-buffer buffer input-text)
              (send-to-agent-with-context buffer))))
    (unless (interactive-buffer-operation-p result)
      (run-hook-with-args '*after-send-message-hook*
                          buffer input-text result))
    result))

(defstruct (listener-turn-boundary
              (:constructor make-listener-turn-boundary
                  (&key message)))
  (message nil :type (or null message))
  (compaction-handoff-count 0 :type (integer 0 1))
  completion-result
  (completion-result-p nil :type boolean))

(defun listener-turn-boundary-for-message (message)
  (make-listener-turn-boundary :message message))

(defun listener-last-finalized-user-message (buffer)
  (loop :for message := (message-prev (buffer-input-message buffer))
          :then (message-prev message)
        :while message
        :when (eq :user (message-sender message))
          :return message))

(defun handoff-listener-turn-boundary-after-compaction (buffer boundary)
  "Rebind BOUNDARY once to the recreated user before provider dispatch."
  (unless (zerop (listener-turn-boundary-compaction-handoff-count boundary))
    (error "Listener turn boundary received more than one compaction handoff."))
  (let ((message (listener-last-finalized-user-message buffer)))
    (unless message
      (error "Compaction did not retain the listener turn user boundary."))
    (setf (listener-turn-boundary-message boundary) message
          (listener-turn-boundary-compaction-handoff-count boundary) 1))
  boundary)

(defun send-prose-message (buffer text)
  "Finalize and dispatch TEXT as literal prose.

Returns the dispatch result, true when accepted, and a stable turn boundary.
Blank or busy submissions return NIL values without mutating BUFFER."
  (when (or (blank-string-p text)
            (buffer-agent-busy-p buffer))
    (return-from send-prose-message (values nil nil)))
  (set-message-text (buffer-input-message buffer) text)
  (ensure-buffer-session buffer)
  (run-hook-with-args '*before-send-message-hook* buffer text)
  (buffer-finalize-input buffer)
  (let ((boundary
          (listener-turn-boundary-for-message
           (message-prev (buffer-input-message buffer)))))
    (multiple-value-bind (operation needed-p)
        (start-interactive-compaction
         buffer
         :reason :pre-user-message
         :continuation
         (lambda (live-buffer)
            (handoff-listener-turn-boundary-after-compaction
             live-buffer boundary)
            (dispatch-finalized-prose-input live-buffer text)))
      (declare (ignore needed-p))
      (values (or operation
                  (dispatch-finalized-prose-input buffer text))
              t
              boundary))))

(defun resolve-listener-turn-boundary (buffer boundary)
  "Return BOUNDARY's exact user message when it remains in BUFFER."
  (let ((boundary-message (listener-turn-boundary-message boundary)))
    (loop :for message := (buffer-first-message buffer)
            :then (message-next message)
          :while (and message
                      (not (eq message (buffer-input-message buffer))))
          :when (eq message boundary-message)
            :return message)))

(defun listener-provider-assistant-message-p (message)
  "Return true when MESSAGE has the existing provider assistant role."
  (not (member (message-sender message)
               '(:user :tool-result :compaction-summary :branch-summary
                 :context :system)
               :test #'eq)))

(defun listener-agent-messages-after (buffer boundary)
  "Return provider assistant messages after resolved BOUNDARY."
  (let ((user-message (resolve-listener-turn-boundary buffer boundary)))
    (unless user-message
      (error "Listener turn user boundary is no longer present."))
    (loop :for message := (message-next user-message) :then (message-next message)
          :while (and message
                      (not (eq message (buffer-input-message buffer)))
                      (not (eq :user (message-sender message))))
          :when (listener-provider-assistant-message-p message)
            :collect message)))

(defun listener-message-metadata-plist (message)
  "Return MESSAGE's canonical metadata alist as a presentation plist."
  (loop :for (key . value) :in (and message (message-metadata message))
        :append (list key value)))

(defun listener-message-metadata-value (message key)
  (and message
       (message-metadata-value (message-metadata message) key)))

(defun listener-metadata-key-present-p (plist key)
  (loop :for tail :on plist :by #'cddr
        :thereis (eq key (first tail))))

(defun merge-listener-metadata (&rest plists)
  "Merge PLISTS in precedence order, retaining each key exactly once."
  (let ((merged nil))
    (dolist (plist plists merged)
      (loop :for (key value) :on plist :by #'cddr
            :unless (listener-metadata-key-present-p merged key)
              :do (setf merged (append merged (list key value)))))))

(defun listener-condition-metadata (condition)
  (when condition
    (append
     (list :error (prompt-run-error-message condition)
           :iterations (prompt-run-error-iterations condition))
     (when (prompt-run-error-provider condition)
       (list :provider (prompt-run-error-provider condition)))
     (when (prompt-run-error-model condition)
       (list :model (prompt-run-error-model condition)))
     (when (prompt-run-error-think-level condition)
       (list :think-level (prompt-run-error-think-level condition)))
     (when (prompt-run-error-tool-events condition)
       (list :prompt-tool-events
             (copy-list (prompt-run-error-tool-events condition)))))))

(defun listener-buffer-metadata (buffer)
  (list :buffer-agent (buffer-agent-name buffer)
        :buffer-status (buffer-status buffer)
        :buffer-session-name (and (buffer-session buffer)
                                  (session-name (buffer-session buffer)))
        :buffer-session-id (and (buffer-session buffer)
                                (session-id (buffer-session buffer)))))

(defun listener-turn-metadata
    (buffer message &optional condition prompt-result)
  "Assemble unique canonical, condition, message, and buffer metadata."
  (merge-listener-metadata
   (listener-prompt-result-metadata prompt-result)
   (listener-condition-metadata condition)
   (listener-message-metadata-plist message)
   (listener-buffer-metadata buffer)))

(defun listener-tool-use-plist (block)
  "Normalize canonical tool-use alist BLOCK to the assistant-turn plist shape."
  (loop :for (key . value) :in block
        :append (list key (copy-tree value))))

(defun listener-turn-content-facets (messages)
  "Return canonical tool uses and reasoning from MESSAGES."
  (values
   (loop :for message :in messages
         :append (mapcar #'listener-tool-use-plist
                         (content-tool-use-blocks
                          (message-raw-content message))))
   (loop :for message :in messages
         :append (content-reasoning-blocks (message-raw-content message)))))

(defun listener-turn-reference-values (messages key)
  "Return all list-valued metadata references named KEY in MESSAGES."
  (loop :for message :in messages
        :for refs := (listener-message-metadata-value message key)
        :when refs :append (copy-list refs)))

(defun listener-turn-completion-prompt-result (boundary)
  "Return BOUNDARY's supported canonical prompt result, when available."
  (when (listener-turn-boundary-completion-result-p boundary)
    (let ((result (listener-turn-boundary-completion-result boundary)))
      (cond
        ((prompt-run-result-p result) result)
        ((and (pipeline-run-result-p result)
              (eq :succeeded (pipeline-run-result-status result)))
         (pipeline-run-result->prompt-run-result result))))))

(defun listener-turn-canonical-pipeline-result-p (boundary prompt-result)
  "Return true when PROMPT-RESULT is BOUNDARY's successful pipeline result."
  (and prompt-result
       (listener-turn-boundary-completion-result-p boundary)
       (let ((result (listener-turn-boundary-completion-result boundary)))
         (and (pipeline-run-result-p result)
              (eq :succeeded (pipeline-run-result-status result))))))

(defun listener-prompt-result-tool-uses (result)
  "Return canonical tool-use blocks from RESULT's aggregate tool events."
  (when result
    (mapcar
     (lambda (event)
       (listener-tool-use-plist
        (canonical-tool-use-block
         (prompt-tool-event-id event)
         (prompt-tool-event-name event)
         (prompt-tool-event-input event))))
     (prompt-run-result-tool-events result))))

(defun listener-prompt-result-metadata (result)
  (when result
    (append
     (when (prompt-run-result-agent-name result)
       (list :agent (prompt-run-result-agent-name result)))
     (when (prompt-run-result-provider result)
       (list :provider (prompt-run-result-provider result)))
     (when (prompt-run-result-model result)
       (list :model (prompt-run-result-model result)))
     (when (prompt-run-result-think-level result)
       (list :think-level (prompt-run-result-think-level result)))
     (when (prompt-run-result-iterations result)
       (list :iterations (prompt-run-result-iterations result)))
     (when (prompt-run-result-stop-reason result)
       (list :stop-reason (prompt-run-result-stop-reason result)))
     (when (prompt-run-result-tool-events result)
       (list :prompt-tool-events
             (copy-list (prompt-run-result-tool-events result))))
     (when (prompt-run-result-usage result)
       (list :usage (copy-tree (prompt-run-result-usage result)))))))

(defun make-settled-listener-assistant-turn
    (buffer boundary status &key condition primary-text)
  "Build one assistant-turn from the result settled after BOUNDARY."
  (let* ((messages (listener-agent-messages-after buffer boundary))
         (latest (car (last messages)))
         (prompt-result (listener-turn-completion-prompt-result boundary))
         (canonical-pipeline-result-p
           (listener-turn-canonical-pipeline-result-p boundary prompt-result))
         (canonical-pipeline-primary-p
           (and canonical-pipeline-result-p
                (not (blank-string-p
                      (prompt-run-result-final-text prompt-result))))))
    (when (and (eq status :complete) (null latest) (null prompt-result))
      (error "Agent turn settled without an assistant message."))
    (multiple-value-bind (tool-uses reasoning)
        (listener-turn-content-facets messages)
      (make-assistant-turn
       :primary-text
       (or primary-text
           (and canonical-pipeline-primary-p
                (prompt-run-result-final-text prompt-result))
           (and latest
                (if (message-raw-content latest)
                    (content-text-blocks (message-raw-content latest))
                    (message-text latest)))
           (prompt-run-result-final-text prompt-result))
       :tool-uses (if canonical-pipeline-result-p
                      (listener-prompt-result-tool-uses prompt-result)
                      tool-uses)
       :reasoning (cond
                    (canonical-pipeline-result-p
                     (copy-list
                      (prompt-run-result-reasoning-blocks prompt-result)))
                    (messages reasoning)
                    (prompt-result
                     (copy-list
                      (prompt-run-result-reasoning-blocks prompt-result))))
       :metadata (listener-turn-metadata
                  buffer latest condition prompt-result)
       :artifact-refs (listener-turn-reference-values messages :artifact-refs)
       :media-refs (listener-turn-reference-values messages :media-refs)
       :inspect-payload (if canonical-pipeline-result-p
                            prompt-result
                            (and latest (message-raw-content latest)))
       :status status))))

(defun emit-listener-assistant-turn (frame turn)
  "Emit TURN's primary body once as one durable presentation."
  (let ((stream (clim:frame-standard-output frame)))
    (clim:with-output-as-presentation (stream turn 'assistant-turn)
      (write-string (assistant-turn-primary-text turn) stream))
    (terpri stream))
  turn)

(defun send-message (buffer)
  "Send the current input message to the agent."
  (cond
    ((document-buffer-p buffer)
     (insert-newline-command buffer))
    ((listener-buffer-p buffer)
     (submit-listener-input buffer))
    (t
     (let ((input-text (message-text (buffer-input-message buffer))))
       (when (plusp (length (string-trim '(#\Space #\Tab #\Newline) input-text)))
         (run-hook-with-args '*before-send-message-hook* buffer input-text)
         (multiple-value-bind (slash-handled-p slash-result)
             (process-slash-command buffer input-text)
           (when slash-handled-p
             (set-message-text (buffer-input-message buffer) "")
             (mark-buffer-dirty buffer)
             (run-hook-with-args '*after-send-message-hook*
                                 buffer input-text slash-result)
             (return-from send-message slash-result)))
         (let ((template-expansion
                 (expand-slash-template-input buffer input-text)))
           (when template-expansion
             (setf input-text template-expansion)
             (set-message-text (buffer-input-message buffer) input-text)))
         (buffer-finalize-input buffer)
         ;; Prefix commands are not provider context and never trigger
         ;; conversation compaction.  Normal sends compact asynchronously,
         ;; then continue the exact intended provider/pipeline dispatch once.
         (if (find-prefix-handler input-text)
             (dispatch-finalized-chat-input buffer input-text)
             (multiple-value-bind (operation needed-p)
                 (start-interactive-compaction
                  buffer
                  :reason :pre-user-message
                  :continuation
                  (lambda (live-buffer)
                    (dispatch-finalized-chat-input live-buffer input-text)))
               (declare (ignore needed-p))
               (or operation
                   (dispatch-finalized-chat-input buffer input-text)))))))))
(defcommand send-message :keys (#\Return))

(defun stop-llm-command (buffer)
  "Stop the active LLM response in BUFFER."
  (when (stop-streaming-response buffer)
    :redraw))
(defcommand stop-llm-command :keys (#\Esc))

(defun insert-newline-command (buffer)
  "Insert a newline in the input message."
  (message-insert-newline (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand insert-newline-command :keys (#\Linefeed))

(defun beginning-of-line-command (buffer)
  "Move point to the beginning of the current line."
  (message-move-beginning-of-line (buffer-input-message buffer)))
(defcommand beginning-of-line-command)

(defun end-of-line-command (buffer)
  "Move point to the end of the current line."
  (message-move-end-of-line (buffer-input-message buffer)))
(defcommand end-of-line-command)

(defun kill-line-command (buffer)
  "Kill from point to the end of the line."
  (message-kill-line (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand kill-line-command)

(defun yank-command (buffer)
  "Yank the top of the kill ring at point."
  (message-yank (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand yank-command)

(defun delete-char-backward-command (buffer)
  "Delete the character before point."
  (message-delete-char-backward (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand delete-char-backward-command)

(defun delete-char-forward-command (buffer)
  "Delete the character after point."
  (message-delete-char-forward (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand delete-char-forward-command)

(defun forward-char-command (buffer)
  "Move point one character forward."
  (message-forward-char (buffer-input-message buffer)))
(defcommand forward-char-command)

(defun backward-char-command (buffer)
  "Move point one character backward."
  (message-backward-char (buffer-input-message buffer)))
(defcommand backward-char-command)

(defun forward-word-command (buffer)
  "Move point forward to end of next word."
  (message-forward-word (buffer-input-message buffer)))
(defcommand forward-word-command)

(defun backward-word-command (buffer)
  "Move point backward to beginning of previous word."
  (message-backward-word (buffer-input-message buffer)))
(defcommand backward-word-command)

(defun kill-backward-line-command (buffer)
  "Kill from start of line to point."
  (message-kill-backward-line (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand kill-backward-line-command)

(defun kill-word-command (buffer)
  "Kill from point to end of current word."
  (message-kill-word (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand kill-word-command)

(defun backward-kill-word-command (buffer)
  "Kill from beginning of current word to point."
  (message-backward-kill-word (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand backward-kill-word-command)

(defun yank-pop-command (buffer)
  "Replace just-yanked text with next kill ring entry."
  (message-yank-pop (buffer-input-message buffer))
  (mark-buffer-dirty buffer))
(defcommand yank-pop-command)

(defun message-insert-string (msg text)
  "Insert TEXT at point in MSG."
  (loop :for char :across text
        :do (if (char= char #\Newline)
                (message-insert-newline msg)
                (message-insert-char msg char)))
  msg)

(defun insert-tab-command (buffer)
  "Insert a tab character at point."
  (message-insert-char (buffer-input-message buffer) #\Tab)
  (mark-buffer-dirty buffer))
(defcommand insert-tab-command)

(defun next-line-command (buffer)
  "Move point to the next line."
  (message-forward-line (buffer-input-message buffer)))
(defcommand next-line-command)

(defun previous-line-command (buffer)
  "Move point to the previous line."
  (message-backward-line (buffer-input-message buffer)))
(defcommand previous-line-command)

(defun beginning-of-buffer-command (buffer)
  "Move point to the beginning of the editable buffer."
  (message-beginning-of-buffer (buffer-input-message buffer)))
(defcommand beginning-of-buffer-command)

(defun end-of-buffer-command (buffer)
  "Move point to the end of the editable buffer."
  (message-end-of-buffer (buffer-input-message buffer)))
(defcommand end-of-buffer-command)

(defun set-mark-command (buffer)
  "Set the mark at point in the editable buffer."
  (message-set-mark-at-point (buffer-input-message buffer))
  (notify-buffer-display-change buffer :mark))
(defcommand set-mark-command)

(defun keyboard-quit-command (buffer)
  "Cancel the current editor mark/prefix state."
  (message-clear-mark (buffer-input-message buffer))
  (setf *meta-pending* nil
        *alt-pending* nil
        *cx-pending* nil
        *cc-pending* nil
        *ch-pending* nil)
  (deactivate-skill-completion)
  (notify-buffer-display-change buffer :keyboard-quit))
(defcommand keyboard-quit-command)

(defun kill-region-command (buffer)
  "Kill the active region into the kill ring."
  (handler-case
      (progn
        (message-kill-region (buffer-input-message buffer))
        (mark-buffer-dirty buffer))
    (error (e)
      (buffer-insert-system-message
       buffer
       (format nil "[Kill region failed: ~A]" e)))))
(defcommand kill-region-command)

(defun copy-region-command (buffer)
  "Copy the active region into the kill ring."
  (handler-case
      (progn
        (message-copy-region (buffer-input-message buffer))
        (notify-buffer-display-change buffer :copy-region))
    (error (e)
      (buffer-insert-system-message
       buffer
       (format nil "[Copy region failed: ~A]" e)))))
(defcommand copy-region-command)

(defun exchange-point-and-mark-command (buffer)
  "Exchange point and mark in the editable buffer."
  (handler-case
      (progn
        (message-exchange-point-and-mark (buffer-input-message buffer))
        (notify-buffer-display-change buffer :mark))
    (error (e)
      (buffer-insert-system-message
       buffer
       (format nil "[Exchange point and mark failed: ~A]" e)))))
(defcommand exchange-point-and-mark-command)

(defun mark-whole-buffer-command (buffer)
  "Mark the entire editable buffer."
  (message-mark-whole-buffer (buffer-input-message buffer))
  (notify-buffer-display-change buffer :mark))
(defcommand mark-whole-buffer-command)

(defun search-forward-command (buffer query)
  "Search forward for QUERY in the editable buffer."
  (unless (message-search-forward (buffer-input-message buffer) query)
    (buffer-insert-system-message
     buffer
     (format nil "[Search failed: ~A]" query))))
(defcommand search-forward-command
  :prompts ((query :prompt "Search forward")))

(defun search-backward-command (buffer query)
  "Search backward for QUERY in the editable buffer."
  (unless (message-search-backward (buffer-input-message buffer) query)
    (buffer-insert-system-message
     buffer
     (format nil "[Search backward failed: ~A]" query))))
(defcommand search-backward-command
  :prompts ((query :prompt "Search backward")))

(defun revert-file-buffer-command (buffer)
  "Reload the current file buffer from disk."
  (if (file-buffer-p buffer)
      (handler-case
          (let* ((text (project-read-file (buffer-project-name buffer)
                                          (buffer-resource-path buffer)))
                 (input (buffer-input-message buffer)))
            (set-message-text input text)
            (setf (buffer-original-text buffer) text
                  (buffer-dirty-p buffer) nil)
            (buffer-insert-system-message
             buffer
             (format nil "[Reverted ~A:~A]"
                     (buffer-project-name buffer)
                     (buffer-resource-path buffer))))
        (error (e)
          (buffer-insert-system-message
           buffer
           (format nil "[Revert failed: ~A]" e))))
      (buffer-insert-system-message buffer "[Current buffer is not a file.]")))
(defcommand revert-file-buffer-command)

(defun write-project-file-as-command (buffer path)
  "Write the current file buffer to PATH in its project and retarget it."
  (if (file-buffer-p buffer)
      (handler-case
          (let* ((project-name (buffer-project-name buffer))
                 (resource-path (project-resource-name path))
                 (text (file-buffer-text buffer))
                 (summary (project-save-file project-name resource-path text)))
            (setf (buffer-resource-path buffer) resource-path
                  (buffer-name buffer) (file-buffer-name
                                        (ensure-project project-name)
                                        resource-path)
                  (buffer-original-text buffer) text
                  (buffer-dirty-p buffer) nil
                  (buffer-major-mode buffer)
                  (file-major-mode-for-path resource-path))
            (buffer-insert-system-message
             buffer
             (format nil "[Wrote ~A:~A]"
                     (getf summary :project)
                     (getf summary :path))))
        (error (e)
          (buffer-insert-system-message
           buffer
           (format nil "[Write file failed: ~A]" e))))
      (buffer-insert-system-message buffer "[Current buffer is not a file.]")))
(defcommand write-project-file-as-command
  :prompts ((path :prompt "Write file")))

(defun insert-file-command (buffer path)
  "Insert a project file's contents at point."
  (let ((project (current-buffer-project buffer)))
    (if project
        (handler-case
            (let ((text (project-read-file project path)))
              (message-insert-string (buffer-input-message buffer) text)
              (mark-buffer-dirty buffer))
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Insert file failed: ~A]" e))))
        (buffer-insert-system-message
         buffer
         "[No project is selected for this buffer.]"))))
(defcommand insert-file-command
  :prompts ((path :prompt "Insert file")))

(defun yank-previous-command-first-arg-command (buffer)
  "Insert the first argument of the previous user command."
  (let ((arg (buffer-previous-command-first-argument buffer)))
    (when arg
      (message-insert-string (buffer-input-message buffer) arg)
      (mark-buffer-dirty buffer))))
(defcommand yank-previous-command-first-arg-command)

(defun yank-previous-command-last-arg-command (buffer)
  "Insert the last argument of the previous user command."
  (let ((arg (buffer-previous-command-last-argument buffer)))
    (when arg
      (message-insert-string (buffer-input-message buffer) arg)
      (mark-buffer-dirty buffer))))
(defcommand yank-previous-command-last-arg-command)

(defun self-insert-command (buffer)
  "Insert a character at point. The character is passed via *self-insert-char*."
  (when *self-insert-char*
    (message-insert-char (buffer-input-message buffer) *self-insert-char*)
    (mark-buffer-dirty buffer)))

;;; --------------------------------------------------------------------------
;;; Scroll Commands
;;; --------------------------------------------------------------------------

(defvar *scroll-page-size* nil
  "Number of rows to scroll per page. Set by the event loop based on window height.")

(defun scroll-up-command (buffer)
  "Scroll history up (back) by one page."
  (when *scroll-page-size*
    (incf (buffer-scroll-offset buffer) *scroll-page-size*)))
(defcommand scroll-up-command)

(defun scroll-down-command (buffer)
  "Scroll history down (forward) by one page."
  (when *scroll-page-size*
    (let ((offset (buffer-scroll-offset buffer))
          (page-size *scroll-page-size*))
      (if (<= offset (1+ page-size))
          (setf (buffer-scroll-offset buffer) 0)
          (progn
            (decf (buffer-scroll-offset buffer) page-size)
            (when (minusp (buffer-scroll-offset buffer))
              (setf (buffer-scroll-offset buffer) 0)))))))
(defcommand scroll-down-command)

(defun handle-help-key (buffer key)
  "Handle KEY in a read-only help buffer."
  (cond
    ((or (eq key :page-up)
         (eq key :page-down)
         (and (characterp key) (char= key (code-char 22))))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command
         (invoke-command buffer command))))
    ((and (characterp key)
          (or (char= key #\q)
              (char= key (code-char 7))))
     (kill-buffer-from-ring buffer))
    ((and (listp key) (member (first key) '(:ctrl-x :meta :alt)))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command
         (invoke-command buffer command))))
    (t nil)))

(defun handle-info-key (buffer key)
  "Handle KEY in a read-only Info buffer."
  (cond
    ((or (eq key :page-up)
         (eq key :page-down)
         (and (characterp key) (char= key (code-char 22))))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command
         (invoke-command buffer command))))
    ((or (eq key #\Return)
         (eq key #\Newline))
     (info-follow-selected-link buffer))
    ((or (eq key #\Tab)
         (eq key :tab))
     (info-select-next-link buffer))
    ((or (equal key '(:meta #\Tab))
         (equal key '(:meta :tab))
         (eq key :backtab)
         (eq key :shift-tab))
     (info-select-previous-link buffer))
    ((and (characterp key)
          (or (char= key #\q)
              (char= key (code-char 7))))
     (kill-buffer-from-ring buffer))
    ((and (characterp key) (char-equal key #\n))
     (info-go-next-node buffer))
    ((and (characterp key) (char-equal key #\p))
     (info-go-prev-node buffer))
    ((and (characterp key) (char-equal key #\u))
     (info-go-up-node buffer))
    ((and (characterp key) (char-equal key #\t))
     (info-go-top-node buffer))
    ((and (characterp key) (char-equal key #\d))
     (info-go-directory buffer))
    ((and (characterp key) (char-equal key #\l))
     (info-go-back buffer))
    ((and (characterp key) (char-equal key #\r))
     (info-go-forward buffer))
    ((and (characterp key) (char-equal key #\g))
     (invoke-command buffer 'info-goto-node-command))
    ((and (listp key) (member (first key) '(:ctrl-x :meta :alt)))
     (let ((command (keymap-lookup *default-keymap* key)))
       (when command
         (invoke-command buffer command))))
    (t nil)))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth Command
;;; --------------------------------------------------------------------------

(defun openai-codex-oauth-command (buffer)
  "Start the OpenAI Codex OAuth login flow using a localhost browser callback."
  (let ((flow nil)
        (start-generation nil)
        (published-p nil)
        (retired-p nil))
    (handler-case
        (unwind-protect
             (handler-case
                 (progn
                   (call-with-runtime-admission
                    (lambda ()
                      ;; Admission is the outer lock.  The per-buffer reservation
                      ;; then closes the construction-to-publication gap with Stop
                      ;; and disposal, while the registry remains the innermost
                      ;; lock during exact publication.
                      (setf start-generation
                            (reserve-buffer-stream-start buffer))
                      (unless start-generation
                        (error
                         "Buffer ~A cannot start OAuth while its runtime is stopping"
                         (buffer-name buffer)))
                      (when (openai-oauth-login-pending-p)
                        (error
                         "An OpenAI Codex OAuth login is already in progress"))
                      (setf flow
                            (start-openai-codex-oauth-login :buffer buffer))
                      (unless
                          (publish-reserved-openai-oauth-pending-flow
                           buffer flow start-generation)
                        (error
                         "OAuth start lost ownership before publication"))
                      (setf published-p t))
                    :operation "OpenAI Codex OAuth login")
                   ;; Keep the start reservation through visible frame mutation.
                   ;; A teardown that claimed the published flow therefore cannot
                   ;; finish and expose :IDLE before this command unwinds.
                   (let* ((snapshot (openai-oauth-flow-snapshot flow))
                          (auth-url (getf snapshot :auth-url))
                          (redirect-uri (getf snapshot :redirect-uri)))
                     (buffer-insert-system-message
                      buffer
                      (format nil "[OpenAI Codex OAuth]~%~%A browser login was started for shared Codex auth.~%If the browser did not open, use this URL:~%~%  ~A~%~%The callback server is listening at:~%  ~A~%~%Press C-g to cancel."
                              auth-url redirect-uri))
                     (setf (buffer-status buffer) :oauth)
                     (notify-buffer-display-change buffer :status)))
               (error (condition)
                 ;; Runtime admission has unwound here, but the exact buffer
                 ;; reservation still makes this unregistered worker observable.
                 ;; Join under settlement admission before releasing it below.
                 (when (and flow (not published-p))
                   (unwind-protect
                        (ignore-errors
                          (cancel-openai-codex-oauth-login flow))
                     (join-openai-oauth-flow-worker flow))
                   (setf flow nil))
                 (error condition)))
          (when start-generation
            (release-buffer-stream-start buffer start-generation)))
      (error (e)
        (when published-p
          ;; Pending publication remains the reload-visible reservation until
          ;; the cancelled listener/client worker has completely unwound.
          (ignore-errors (cancel-openai-codex-oauth-login flow))
          (if (openai-oauth-flow-worker-settled-p-safe flow)
              (when (claim-openai-oauth-pending-flow flow)
                (setf retired-p t))
              (setf retired-p nil)))
        (when (and retired-p (eq (buffer-status buffer) :oauth))
          (setf (buffer-status buffer) :idle))
        (buffer-insert-system-message
         buffer
         (format nil "[OAuth error: ~A]" e))))))
(defcommand openai-codex-oauth-command)

;;; --------------------------------------------------------------------------
;;; Buffer Management Commands
;;; --------------------------------------------------------------------------

(defun list-buffers-command (buffer)
  "Open the visible minibuffer selector to switch between agent sessions."
  (setf *buffer-selector-active* nil)
  (minibuffer-select-buffer-command buffer))
(defcommand list-buffers-command)

;;; --------------------------------------------------------------------------
;;; Project Commands
;;; --------------------------------------------------------------------------

(defun ensure-projects-for-ui ()
  "Ensure project definitions are loaded before project UI commands run."
  (unless (project-definitions-loaded-p)
    (load-project-definitions))
  (list-projects))

(defun project-selector-items (&optional active-project-name)
  "Return minibuffer project selector items."
  (mapcar (lambda (project)
            (let* ((name (project-name project))
                   (active-p (and active-project-name
                                  (string= name active-project-name)))
                   (display (format nil "~A ~A  [~(~A~)] ~A"
                                    (if active-p "*" " ")
                                    name
                                    (or (project-source project) :unknown)
                                    (namestring (project-root project)))))
              (list :project project
                    :project-name name
                    :active-p active-p
                    :display display
                    :match-text (format nil "~A ~A ~A"
                                        name
                                        (or (project-description project) "")
                                        (namestring (project-root project))))))
          (ensure-projects-for-ui)))

(defun minibuffer-choose-project (buffer prompt callback)
  "Prompt for a project, then call CALLBACK with the selected project."
  (let ((items (project-selector-items (buffer-project-name buffer))))
    (if items
        (progn
          (minibuffer-activate prompt items
                               (lambda (item)
                                 (funcall callback (getf item :project))))
          (preselect-minibuffer-active-item items))
        (buffer-insert-system-message buffer "[No projects available.]"))))

(defun project-file-selector-items (project)
  "Return minibuffer file selector items for PROJECT."
  (mapcar (lambda (path)
            (list :project project
                  :path path
                  :display path
                  :match-text path))
          (project-list-files project)))

(defun minibuffer-open-project-file (buffer project)
  "Prompt for a file in PROJECT and open it."
  (let ((items (project-file-selector-items project)))
    (if items
        (minibuffer-activate
         (format nil "Open ~A" (project-name project))
         items
         (lambda (item)
           (handler-case
               (project-open-file (getf item :project) (getf item :path))
             (error (e)
               (buffer-insert-system-message
                buffer
                (format nil "[Open project file failed: ~A]" e))))))
        (buffer-insert-system-message
         buffer
         (format nil "[Project ~A has no files.]" (project-name project))))))

(defun current-buffer-project (buffer)
  "Return BUFFER's selected project, or NIL."
  (and (buffer-project-name buffer)
       (find-project (buffer-project-name buffer))))

(defun minibuffer-select-project-command (buffer)
  "Select the active project for the current buffer."
  (if (file-buffer-p buffer)
      (buffer-insert-system-message
       buffer
       "[File buffers keep the project of their backing resource.]")
      (minibuffer-choose-project
       buffer
       "Select Project"
       (lambda (project)
        (setf (buffer-project-name buffer) (project-name project)
              (buffer-working-directory buffer) (project-root project))
        (buffer-insert-system-message
         buffer
         (format nil "[Project changed to ~A]" (project-name project)))))))
(defcommand minibuffer-select-project-command)

(defun open-project-file-command (buffer)
  "Open a file from the current or selected project."
  (let ((project (current-buffer-project buffer)))
    (if project
        (minibuffer-open-project-file buffer project)
        (minibuffer-choose-project buffer
                                   "Select Project"
                                   (lambda (selected-project)
                                     (minibuffer-open-project-file
                                      buffer selected-project))))))
(defcommand open-project-file-command)

(defun create-project-file-command (buffer)
  "Create and open a new file in a selected project."
  (minibuffer-choose-project
   buffer
   "Select Project"
   (lambda (project)
     (minibuffer-prompt
      (format nil "Create in ~A" (project-name project))
      (lambda (path)
        (handler-case
            (progn
              (project-create-file project path)
              (project-open-file project path))
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Create project file failed: ~A]" e)))))))))
(defcommand create-project-file-command)

(defun search-project-command (buffer)
  "Search a selected project and insert the result list."
  (minibuffer-choose-project
   buffer
   "Select Project"
   (lambda (project)
     (minibuffer-prompt
      (format nil "Search ~A" (project-name project))
      (lambda (query)
        (handler-case
            (buffer-insert-system-message
             buffer
             (project-search-to-string project query))
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Search project failed: ~A]" e)))))))))
(defcommand search-project-command)

;;; --------------------------------------------------------------------------
;;; Skill Commands
;;; --------------------------------------------------------------------------

(defun skill-mention-text (skill)
  "Return the text inserted for selecting SKILL."
  (if (skill-path skill)
      (format nil "[$~A](skill://~A)"
              (skill-name skill)
              (namestring (skill-path skill)))
      (format nil "$~A" (skill-name skill))))

(defun make-skill-selector-item (skill &key include-enabled-marker)
  "Build one minibuffer item for SKILL."
  (let* ((enabled-p (skill-enabled-p skill))
         (marker (cond
                   ((not include-enabled-marker) "")
                   (enabled-p "[x] ")
                   (t "[ ] ")))
         (path (and (skill-path skill)
                    (namestring (skill-path skill))))
         (description (skill-display-description skill)))
    (list :skill skill
          :display (format nil "~A~A  [~(~A~)]~@[ ~A~]"
                           marker
                           (skill-name skill)
                           (or (skill-scope skill) :unknown)
                           description)
          :match-text (format nil "~A ~A ~A"
                              (skill-name skill)
                              description
                              (or path "")))))

(defun skill-selector-items (&key include-disabled include-enabled-marker)
  "Return minibuffer skill selector items."
  (mapcar (lambda (skill)
            (make-skill-selector-item
             skill
             :include-enabled-marker include-enabled-marker))
          (list-skills :include-disabled include-disabled)))

;;; --------------------------------------------------------------------------
;;; Automatic Slash Completion
;;; --------------------------------------------------------------------------

(defvar *automatic-slash-completion-enabled* t
  "When non-nil, typing /NAME in supported buffers opens slash completion.")

(defvar *slash-completion-enabled-buffer-kinds* '(:chat)
  "Buffer kinds where automatic slash completion is enabled.")

(defvar *slash-completion-max-height* 12
  "Maximum rows used by automatic slash completion, including the prompt row.")

(defun slash-completion-buffer-kind-enabled-p (kind)
  "Return true when KIND is configured for automatic slash completion."
  (or (eq *slash-completion-enabled-buffer-kinds* t)
      (member kind *slash-completion-enabled-buffer-kinds* :test #'eq)))

(defun slash-completion-enabled-for-buffer-p (buffer)
  "Return true when automatic slash completion should scan BUFFER."
  (and *automatic-slash-completion-enabled*
       buffer
       (slash-completion-buffer-kind-enabled-p (buffer-kind buffer))
       (not *minibuffer-active*)
       (not (openai-oauth-login-pending-p))))

(defun slash-completion-token-char-p (char)
  "Return true when CHAR can occur after / in an automatic slash token."
  (mention-name-char-p char))

(defun current-slash-command-token (message)
  "Return values QUERY START END TOKEN for the active /command token.
Slash completion only activates for the first non-whitespace token on the
first input line."
  (let* ((line (message-point-line message))
         (content (and line (line-content line)))
         (point (and content
                     (max 0 (min (message-point-offset message)
                                 (length content))))))
    (when (and content (eq line (message-first-line message)))
      (let* ((start (or (position-if-not #'slash-command-whitespace-char-p
                                         content)
                        0))
             (end (or (position-if #'slash-command-whitespace-char-p
                                   content
                                   :start start)
                      (length content))))
        (when (and (< start end)
                   (<= start point end)
                   (char= (char content start) #\/)
                   (loop :for idx :from (1+ start) :below end
                         :always (slash-completion-token-char-p
                                  (char content idx))))
          (let ((token (subseq content start end)))
            (values (subseq token 1) start end token)))))))

(defun slash-completion-update-filter ()
  "Recompute automatic slash completion candidates for the active query."
  (let ((query *slash-completion-query*))
    (cond
      ((zerop (length query))
       (setf *slash-completion-filtered-items*
             (copy-list *slash-completion-items*)
             *slash-completion-match-positions*
             (make-list (length *slash-completion-items*)
                        :initial-element nil)))
      (t
       (let* ((matched (remove-if-not
                        (lambda (item)
                          (fuzzy-match-p query
                                         (minibuffer-item-match-text item)))
                        *slash-completion-items*))
              (scored (mapcar (lambda (item)
                                (cons (or (fuzzy-score
                                           query
                                           (minibuffer-item-match-text item))
                                          0)
                                      item))
                              matched))
              (sorted (stable-sort scored #'> :key #'car))
              (sorted-items (mapcar #'cdr sorted)))
         (setf *slash-completion-filtered-items* sorted-items
               *slash-completion-match-positions*
               (mapcar (lambda (item)
                         (fuzzy-match-positions
                          query
                          (minibuffer-item-match-text item)))
                       sorted-items))))))
  (setf *slash-completion-selected-index*
        (max 0 (min *slash-completion-selected-index*
                    (1- (max 1 (length *slash-completion-filtered-items*))))))
  (setf *slash-completion-scroll-offset* 0)
  (slash-completion-ensure-visible)
  (touch-chat-interaction-state))

(defun slash-completion-visible-item-count ()
  "Return candidate rows visible in the automatic slash completion popup."
  (max 0
       (1- (min *slash-completion-max-height*
                (1+ (max 1 (length *slash-completion-filtered-items*)))))))

(defun slash-completion-ensure-visible ()
  "Adjust automatic slash completion scroll so the selection is visible."
  (let ((visible (slash-completion-visible-item-count)))
    (when (plusp visible)
      (when (< *slash-completion-selected-index*
               *slash-completion-scroll-offset*)
        (setf *slash-completion-scroll-offset*
              *slash-completion-selected-index*))
      (when (>= *slash-completion-selected-index*
                (+ *slash-completion-scroll-offset* visible))
        (setf *slash-completion-scroll-offset*
              (1+ (- *slash-completion-selected-index* visible)))))))

(defun slash-completion-next-item ()
  "Move automatic slash completion selection down one candidate."
  (when (< *slash-completion-selected-index*
           (1- (length *slash-completion-filtered-items*)))
    (incf *slash-completion-selected-index*)
    (slash-completion-ensure-visible)
    (touch-chat-interaction-state)))

(defun slash-completion-prev-item ()
  "Move automatic slash completion selection up one candidate."
  (when (plusp *slash-completion-selected-index*)
    (decf *slash-completion-selected-index*)
    (slash-completion-ensure-visible)
    (touch-chat-interaction-state)))

(defun deactivate-slash-completion (&key dismissed-token)
  "Hide automatic slash completion and optionally remember DISMISSED-TOKEN."
  (touch-chat-interaction-state)
  (setf *slash-completion-active* nil
        *slash-completion-buffer* nil
        *slash-completion-query* ""
        *slash-completion-token-start* 0
        *slash-completion-token-end* 0
        *slash-completion-token-text* nil
        *slash-completion-items* nil
        *slash-completion-filtered-items* nil
        *slash-completion-match-positions* nil
        *slash-completion-selected-index* 0
        *slash-completion-scroll-offset* 0
        *slash-completion-dismissed-token* dismissed-token))

(defun sync-slash-completion (buffer)
  "Synchronize automatic slash completion state with BUFFER's current input."
  (unless (slash-completion-enabled-for-buffer-p buffer)
    (deactivate-slash-completion)
    (return-from sync-slash-completion nil))
  (multiple-value-bind (query start end token)
      (current-slash-command-token (buffer-input-message buffer))
    (cond
      ((null token)
       (deactivate-slash-completion))
      ((and *slash-completion-dismissed-token*
            (string= token *slash-completion-dismissed-token*))
       (deactivate-slash-completion :dismissed-token token))
      (t
       (setf *slash-completion-dismissed-token* nil)
       (let ((items (slash-command-selector-items
                     :buffer buffer
                     :agent-name (buffer-agent-name buffer))))
         (if items
             (progn
               (setf *slash-completion-active* t
                     *slash-completion-buffer* buffer
                     *slash-completion-query* query
                     *slash-completion-token-start* start
                     *slash-completion-token-end* end
                     *slash-completion-token-text* token
                     *slash-completion-items* items)
               (slash-completion-update-filter))
             (deactivate-slash-completion)))))))

(defun insert-selected-slash-completion (buffer)
  "Replace the active /token in BUFFER with the selected slash command."
  (let ((item (when (plusp (length *slash-completion-filtered-items*))
                (nth *slash-completion-selected-index*
                     *slash-completion-filtered-items*))))
    (unless item
      (deactivate-slash-completion)
      (return-from insert-selected-slash-completion nil))
    (multiple-value-bind (query start end token)
        (current-slash-command-token (buffer-input-message buffer))
      (declare (ignore query))
      (if (or (null start)
              (not (eq buffer *slash-completion-buffer*))
              (null *slash-completion-token-text*)
              (not (string= token *slash-completion-token-text*)))
          (deactivate-slash-completion)
          (let* ((message (buffer-input-message buffer))
                 (line (message-point-line message))
                 (content (line-content line))
                 (inserted (format nil "/~A " (getf item :name)))
                 (replacement (concatenate 'string
                                           (subseq content 0 start)
                                           inserted
                                           (subseq content end))))
            (setf (line-content line) replacement
                  (message-point-offset message) (+ start (length inserted)))
            (mark-buffer-dirty buffer)
            (deactivate-slash-completion)
            t)))))

(defun handle-slash-completion-key (buffer key)
  "Handle KEY for the automatic slash completion popup."
  (unless (eq buffer *slash-completion-buffer*)
    (deactivate-slash-completion)
    (return-from handle-slash-completion-key nil))
  (let ((base-key (skill-completion-base-key key)))
    (cond
      ((or (eq base-key :escape)
           (and (characterp base-key)
                (or (char= base-key #\Esc)
                    (char= base-key (code-char 7)))))
       (deactivate-slash-completion
        :dismissed-token *slash-completion-token-text*)
       t)
      ((and (characterp base-key)
            (or (char= base-key #\Return)
                (char= base-key #\Newline)
                (char= base-key #\Tab)))
       (insert-selected-slash-completion buffer)
       t)
      ((eq base-key :tab)
       (insert-selected-slash-completion buffer)
       t)
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (slash-completion-next-item)
       t)
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (slash-completion-prev-item)
       t)
      (t nil))))

;;; --------------------------------------------------------------------------
;;; Automatic Skill Completion
;;; --------------------------------------------------------------------------

(defvar *automatic-skill-completion-enabled* t
  "When non-nil, typing $NAME in supported buffers opens skill completion.")

(defvar *skill-completion-enabled-buffer-kinds* '(:chat)
  "Buffer kinds where automatic skill completion is enabled.
Set this to T to enable automatic completion in every buffer kind, or NIL
to disable it without changing *AUTOMATIC-SKILL-COMPLETION-ENABLED*.")

(defvar *skill-completion-max-height* 12
  "Maximum rows used by automatic skill completion, including the prompt row.")

(defun skill-completion-buffer-kind-enabled-p (kind)
  "Return true when KIND is configured for automatic skill completion."
  (or (eq *skill-completion-enabled-buffer-kinds* t)
      (member kind *skill-completion-enabled-buffer-kinds* :test #'eq)))

(defun skill-completion-enabled-for-buffer-p (buffer)
  "Return true when automatic skill completion should scan BUFFER."
  (and *automatic-skill-completion-enabled*
       buffer
       (skill-completion-buffer-kind-enabled-p (buffer-kind buffer))
       (not *minibuffer-active*)
       (not (openai-oauth-login-pending-p))))

(defun skill-completion-whitespace-char-p (char)
  "Return true when CHAR separates input tokens for automatic completion."
  (member char '(#\Space #\Tab #\Newline #\Return) :test #'char=))

(defun skill-completion-token-char-p (char)
  "Return true when CHAR can occur after $ in an automatic skill token."
  (mention-name-char-p char))

(defun current-skill-mention-token (message)
  "Return values QUERY START END TOKEN for the $token at MESSAGE point.
Returns NIL values when point is not inside a whitespace-delimited token that
starts with $ and contains only skill mention characters after it."
  (let* ((line (message-point-line message))
         (content (and line (line-content line)))
         (point (and content
                     (max 0 (min (message-point-offset message)
                                 (length content))))))
    (when content
      (let ((start point)
            (end point))
        (loop :while (and (plusp start)
                          (not (skill-completion-whitespace-char-p
                                (char content (1- start)))))
              :do (decf start))
        (loop :while (and (< end (length content))
                          (not (skill-completion-whitespace-char-p
                                (char content end))))
              :do (incf end))
        (when (and (< start end)
                   (char= (char content start) #\$)
                   (loop :for idx :from (1+ start) :below end
                         :always (skill-completion-token-char-p
                                  (char content idx))))
          (let ((token (subseq content start end)))
            (values (subseq token 1) start end token)))))))

(defun skill-completion-update-filter ()
  "Recompute automatic skill completion candidates for the active query."
  (let ((query *skill-completion-query*))
    (cond
      ((zerop (length query))
       (setf *skill-completion-filtered-items*
             (copy-list *skill-completion-items*)
             *skill-completion-match-positions*
             (make-list (length *skill-completion-items*)
                        :initial-element nil)))
      (t
       (let* ((matched (remove-if-not
                        (lambda (item)
                          (fuzzy-match-p query
                                         (minibuffer-item-match-text item)))
                        *skill-completion-items*))
              (scored (mapcar (lambda (item)
                                (cons (or (fuzzy-score
                                           query
                                           (minibuffer-item-match-text item))
                                          0)
                                      item))
                              matched))
              (sorted (stable-sort scored #'> :key #'car))
              (sorted-items (mapcar #'cdr sorted)))
         (setf *skill-completion-filtered-items* sorted-items
               *skill-completion-match-positions*
               (mapcar (lambda (item)
                         (fuzzy-match-positions
                          query
                          (minibuffer-item-match-text item)))
                       sorted-items))))))
  (setf *skill-completion-selected-index*
        (max 0 (min *skill-completion-selected-index*
                    (1- (max 1 (length *skill-completion-filtered-items*))))))
  (setf *skill-completion-scroll-offset* 0)
  (skill-completion-ensure-visible)
  (touch-chat-interaction-state))

(defun skill-completion-visible-item-count ()
  "Return candidate rows visible in the automatic skill completion popup."
  (max 0
       (1- (min *skill-completion-max-height*
                (1+ (max 1 (length *skill-completion-filtered-items*)))))))

(defun skill-completion-ensure-visible ()
  "Adjust automatic skill completion scroll so the selection is visible."
  (let ((visible (skill-completion-visible-item-count)))
    (when (plusp visible)
      (when (< *skill-completion-selected-index*
               *skill-completion-scroll-offset*)
        (setf *skill-completion-scroll-offset*
              *skill-completion-selected-index*))
      (when (>= *skill-completion-selected-index*
                (+ *skill-completion-scroll-offset* visible))
        (setf *skill-completion-scroll-offset*
              (1+ (- *skill-completion-selected-index* visible)))))))

(defun skill-completion-next-item ()
  "Move automatic skill completion selection down one candidate."
  (when (< *skill-completion-selected-index*
           (1- (length *skill-completion-filtered-items*)))
    (incf *skill-completion-selected-index*)
    (skill-completion-ensure-visible)
    (touch-chat-interaction-state)))

(defun skill-completion-prev-item ()
  "Move automatic skill completion selection up one candidate."
  (when (plusp *skill-completion-selected-index*)
    (decf *skill-completion-selected-index*)
    (skill-completion-ensure-visible)
    (touch-chat-interaction-state)))

(defun deactivate-skill-completion (&key dismissed-token)
  "Hide automatic skill completion and optionally remember DISMISSED-TOKEN."
  (touch-chat-interaction-state)
  (setf *skill-completion-active* nil
        *skill-completion-buffer* nil
        *skill-completion-query* ""
        *skill-completion-token-start* 0
        *skill-completion-token-end* 0
        *skill-completion-token-text* nil
        *skill-completion-items* nil
        *skill-completion-filtered-items* nil
        *skill-completion-match-positions* nil
        *skill-completion-selected-index* 0
        *skill-completion-scroll-offset* 0
        *skill-completion-dismissed-token* dismissed-token))

(defun sync-skill-completion (buffer)
  "Synchronize automatic skill completion state with BUFFER's current input."
  (unless (skill-completion-enabled-for-buffer-p buffer)
    (deactivate-skill-completion)
    (return-from sync-skill-completion nil))
  (multiple-value-bind (query start end token)
      (current-skill-mention-token (buffer-input-message buffer))
    (cond
      ((null token)
       (deactivate-skill-completion))
      ((and *skill-completion-dismissed-token*
            (string= token *skill-completion-dismissed-token*))
       (deactivate-skill-completion :dismissed-token token))
      (t
       (setf *skill-completion-dismissed-token* nil)
       (let ((items (skill-selector-items)))
         (if items
             (progn
               (setf *skill-completion-active* t
                     *skill-completion-buffer* buffer
                     *skill-completion-query* query
                     *skill-completion-token-start* start
                     *skill-completion-token-end* end
                     *skill-completion-token-text* token
                     *skill-completion-items* items)
               (skill-completion-update-filter))
             (deactivate-skill-completion)))))))

(defun insert-selected-skill-completion (buffer)
  "Replace the active $token in BUFFER with the selected skill mention."
  (let ((item (when (plusp (length *skill-completion-filtered-items*))
                (nth *skill-completion-selected-index*
                     *skill-completion-filtered-items*))))
    (unless item
      (deactivate-skill-completion)
      (return-from insert-selected-skill-completion nil))
    (multiple-value-bind (query start end token)
        (current-skill-mention-token (buffer-input-message buffer))
      (declare (ignore query))
      (if (or (null start)
              (not (eq buffer *skill-completion-buffer*))
              (null *skill-completion-token-text*)
              (not (string= token *skill-completion-token-text*)))
          (deactivate-skill-completion)
          (let* ((message (buffer-input-message buffer))
                 (line (message-point-line message))
                 (content (line-content line))
                 (mention (skill-mention-text (getf item :skill)))
                 (inserted (concatenate 'string mention " "))
                 (replacement (concatenate 'string
                                           (subseq content 0 start)
                                           inserted
                                           (subseq content end))))
            (setf (line-content line) replacement
                  (message-point-offset message) (+ start (length inserted)))
            (mark-buffer-dirty buffer)
            (deactivate-skill-completion)
            t)))))

(defun skill-completion-base-key (key)
  "Return KEY without simple prefix wrappers used by completion handlers."
  (if (and (listp key) (= (length key) 2)
           (member (first key) '(:meta :alt :ctrl-x :ctrl-c)))
      (second key)
      key))

(defun handle-skill-completion-key (buffer key)
  "Handle KEY for the automatic skill completion popup.
Returns true when KEY was consumed by completion."
  (unless (eq buffer *skill-completion-buffer*)
    (deactivate-skill-completion)
    (return-from handle-skill-completion-key nil))
  (let ((base-key (skill-completion-base-key key)))
    (cond
      ((or (eq base-key :escape)
           (and (characterp base-key)
                (or (char= base-key #\Esc)
                    (char= base-key (code-char 7)))))
       (deactivate-skill-completion
        :dismissed-token *skill-completion-token-text*)
       t)
      ((and (characterp base-key)
            (or (char= base-key #\Return)
                (char= base-key #\Newline)
                (char= base-key #\Tab)))
       (insert-selected-skill-completion buffer)
       t)
      ((eq base-key :tab)
       (insert-selected-skill-completion buffer)
       t)
      ((or (eq base-key :down)
           (and (characterp base-key) (char= base-key (code-char 14))))
       (skill-completion-next-item)
       t)
      ((or (eq base-key :up)
           (and (characterp base-key) (char= base-key (code-char 16))))
       (skill-completion-prev-item)
       t)
      (t nil))))

(defun minibuffer-insert-skill-command (buffer)
  "Select a skill and insert an exact $skill mention into the input."
  (let ((items (skill-selector-items)))
    (if items
        (minibuffer-activate
         "Insert Skill" items
         (lambda (item)
           (message-insert-string
            (buffer-input-message buffer)
            (skill-mention-text (getf item :skill)))
           (mark-buffer-dirty buffer)))
        (buffer-insert-system-message buffer "[No enabled skills available.]"))))
(defcommand minibuffer-insert-skill-command)

(defun minibuffer-toggle-skill-command (buffer)
  "Select a skill and toggle whether it is enabled."
  (let ((items (skill-selector-items :include-disabled t
                                     :include-enabled-marker t)))
    (if items
        (minibuffer-activate
         "Toggle Skill" items
         (lambda (item)
           (let* ((skill (getf item :skill))
                  (enabled-p (not (skill-enabled-p skill))))
             (handler-case
                 (progn
                   (set-skill-enabled skill enabled-p)
                   (buffer-insert-system-message
                    buffer
                    (format nil "[Skill ~A ~A]"
                            (skill-name skill)
                            (if enabled-p "enabled" "disabled"))))
               (error (e)
                  (buffer-insert-system-message
                   buffer
                   (format nil "[Skill toggle failed: ~A]" e)))))))
        (buffer-insert-system-message buffer "[No skills available.]"))))
(defcommand minibuffer-toggle-skill-command)

(defun list-skills-command (buffer)
  "Open a help buffer listing loaded skills and skill load errors."
  (declare (ignore buffer))
  (reload-skills)
  (let* ((buf-name "*help:skills*")
         (existing (find-buffer-by-name buf-name))
         (content (list-skills-to-string :include-disabled t)))
    (if existing
        (progn
          (set-message-text (message-prev (buffer-input-message existing))
                            content)
          (switch-to-buffer existing))
        (switch-to-buffer (make-help-buffer buf-name content)))))
(defcommand list-skills-command)

;;; --------------------------------------------------------------------------
;;; Package Commands
;;; --------------------------------------------------------------------------

(defun make-package-selector-item (definition buffer)
  "Build one minibuffer item for package DEFINITION."
  (let* ((name (package-definition-name definition))
         (scope (package-enablement-scope name :buffer buffer))
         (description (package-display-description definition))
         (display (format nil "[~A] ~A - ~A"
                          (package-scope-label scope)
                          name
                          description)))
    (list :package definition
          :package-name name
          :scope scope
          :display display
          :match-text (format nil "~A ~A ~A"
                              name
                              (package-scope-label scope)
                              description))))

(defun installed-package-selector-items (buffer)
  "Return installed packages as minibuffer selector items."
  (mapcar (lambda (definition)
            (make-package-selector-item definition buffer))
          (sort (copy-list (list-installed-packages))
                #'string<
                :key #'package-definition-name)))

(defun select-package-selector-item (package-name)
  "Select PACKAGE-NAME in the active minibuffer when present."
  (let ((index (position package-name *minibuffer-filtered-items*
                         :key (lambda (item)
                                (getf item :package-name))
                         :test #'string=)))
    (when index
      (minibuffer-preselect-index index))))

(defun activate-package-toggle-minibuffer (buffer &optional selected-package-name)
  "Open the installed package enablement selector."
  (let ((items (installed-package-selector-items buffer)))
    (if items
        (progn
          (minibuffer-activate
           "Enable Package" items
           (lambda (item)
             (let* ((name (getf item :package-name))
                    (scope (cycle-package-enablement-scope name :buffer buffer)))
               (load-active-packages :buffer buffer)
               (buffer-insert-system-message
                buffer
                (format nil "[Package ~A ~A]"
                        name
                        (package-scope-message scope)))
               (activate-package-toggle-minibuffer buffer name))))
          (when selected-package-name
            (select-package-selector-item selected-package-name)))
        (buffer-insert-system-message buffer "[No installed packages available.]"))))

(defun minibuffer-toggle-package-command (buffer)
  "Select an installed package and cycle its enablement scope."
  (reload-package-channels)
  (activate-package-toggle-minibuffer buffer))
(defcommand minibuffer-toggle-package-command)

(defun describe-installed-package-command (buffer)
  "Select an installed package and open its help buffer."
  (reload-package-channels)
  (let ((items (installed-package-selector-items buffer)))
    (if items
        (minibuffer-activate
         "Describe Package" items
         (lambda (item)
           (let* ((definition (getf item :package))
                  (name (package-definition-name definition))
                  (content (describe-installed-package-to-string
                            definition buffer))
                  (buf-name (format nil "*help:package:~A*" name))
                  (existing (find-buffer-by-name buf-name)))
             (if existing
                 (progn
                   (set-message-text (message-prev (buffer-input-message existing))
                                     content)
                   (switch-to-buffer existing))
                 (switch-to-buffer (make-help-buffer buf-name content))))))
        (buffer-insert-system-message buffer "[No installed packages available.]"))))
(defcommand describe-installed-package-command)

(defun package-dashboard-command (buffer)
  "Open the installed package dashboard for BUFFER."
  (open-package-dashboard :buffer buffer))
(defcommand package-dashboard-command)

;;; --------------------------------------------------------------------------
;;; Model Selection Commands
;;; --------------------------------------------------------------------------

(defun model-selector-display (provider model)
  "Return the display string used for model selection history and UI."
  (format nil "~(~A~)/~A" provider model))

(defun build-model-selector-items (entries)
  "Convert selector ENTRIES into minibuffer items with display strings."
  (mapcar (lambda (entry)
            (let ((provider (getf entry :provider))
                  (model (getf entry :model)))
              (list :provider provider
                    :model model
                    :active-p (getf entry :active-p)
                    :display (model-selector-display provider model))))
          entries))

(defun model-selection-status-suffix (think-status think-level)
  "Return a short status suffix describing the resulting think level."
  (case think-status
    (:kept
     (format nil "; kept think ~A" think-level))
    (:reset
     "; think reset to default")
    (t
     (if think-level
         (format nil "; think ~A" think-level)
         "; think default"))))

(defun apply-buffer-model-selection (buffer provider model)
  "Apply PROVIDER and MODEL to BUFFER, reconcile think level, and report status."
  (set-buffer-provider-override buffer provider)
  (set-buffer-model-override buffer model)
  (multiple-value-bind (think-status think-level)
      (reconcile-buffer-think-level-override buffer
                                             :provider provider
                                             :model model)
    (when (buffer-session buffer)
      (record-session-model-change (buffer-session buffer)
                                   provider
                                   model
                                   :think-level think-level))
    (values think-status think-level)))

(defun record-model-selection-history (display)
  "Record DISPLAY as the most recently selected model."
  (setf *model-selection-history*
        (cons display
              (remove display *model-selection-history* :test #'string=))))

(defun insert-model-selection-message (buffer provider model think-status think-level)
  "Insert a confirmation message for a model selection."
  (buffer-insert-system-message
   buffer
   (format nil "[Model changed to ~A~A]"
           (model-selector-display provider model)
           (model-selection-status-suffix think-status think-level))))

(defun available-think-levels-for-selector (buffer)
  "Build think-level selector entries for BUFFER's active model."
  (multiple-value-bind (provider model current-think)
      (handler-case (resolve-buffer-provider-and-model buffer)
        (error () (values nil nil nil)))
    (let ((levels (and provider model
                       (provider-model-supported-think-levels provider model))))
      (when levels
        (cons (list :provider provider
                    :model model
                    :level nil
                    :default-p t
                    :active-p (null current-think)
                    :display "default")
              (mapcar (lambda (level)
                        (list :provider provider
                              :model model
                              :level level
                              :default-p nil
                              :active-p (and current-think
                                             (string= level current-think))
                              :display level))
                      levels))))))

(defun insert-think-selection-message (buffer provider model think-level)
  "Insert a confirmation message for a think-level selection."
  (buffer-insert-system-message
   buffer
   (if think-level
       (format nil "[Think level set to ~A for ~A]"
               think-level
               (model-selector-display provider model))
       (format nil "[Think level reset to default for ~A]"
               (model-selector-display provider model)))))

(defun apply-buffer-think-level-selection (buffer entry)
  "Apply think-level ENTRY to BUFFER and insert a confirmation message."
  (let ((provider (getf entry :provider))
        (model (getf entry :model))
        (level (getf entry :level)))
    (if level
        (set-buffer-think-level-override buffer level)
        (clear-buffer-think-level-override buffer))
    (when (buffer-session buffer)
      (record-session-think-level-change (buffer-session buffer) level))
    (insert-think-selection-message buffer provider model level)))

(defun preselect-minibuffer-active-item (items)
  "Move the minibuffer selection to the active item in ITEMS when present."
  (let ((active-idx (position-if (lambda (item) (getf item :active-p)) items)))
    (when active-idx
      (minibuffer-preselect-index active-idx))))

(defun resolve-agent-display-config (agent-name)
  "Return AGENT-NAME's effective provider, model, and think level for UI display."
  (let ((buf (make-buffer "agent-config-preview" :agent-name agent-name)))
    (resolve-buffer-provider-and-model buf)))

(defun format-agent-selection-message (agent-name)
  "Return a confirmation message after switching to AGENT-NAME."
  (handler-case
      (multiple-value-bind (provider model think-level)
          (resolve-agent-display-config agent-name)
        (format nil "[Agent changed to ~A (~(~A~)/~A~@[; think ~A~])]"
                agent-name provider model think-level))
    (error ()
      (format nil "[Agent changed to ~A]" agent-name))))

(defun switch-buffer-to-agent (buffer agent-name)
  "Switch BUFFER to AGENT-NAME, clear overrides, and confirm."
  (normalize-agent-name-key agent-name)
  (let* ((definition (find-agent-definition agent-name))
         (resolved-name (if definition
                            (agent-definition-name definition)
                            (string-trim '(#\Space #\Tab #\Newline #\Return) agent-name))))
    (setf (buffer-agent-name buffer) resolved-name)
    (clear-buffer-routing-overrides buffer)
    (sync-buffer-system-prompt-display buffer)
    (buffer-insert-system-message buffer (format-agent-selection-message resolved-name))
    buffer))

(defun make-agent-selector-item (agent-name active-agent-name)
  "Build one minibuffer item for AGENT-NAME."
  (let ((active-p (string= agent-name active-agent-name)))
    (handler-case
        (multiple-value-bind (provider model think-level)
            (resolve-agent-display-config agent-name)
          (list :agent-name agent-name
                :active-p active-p
                :display (format nil "~A ~A  [~(~A~)/~A~@[ think:~A~]]"
                                 (if active-p "*" " ")
                                 agent-name
                                 provider
                                 model
                                 think-level)
                :match-text (format nil "~A ~(~A~) ~A~@[ ~A~]"
                                    agent-name provider model think-level)))
      (error ()
        (list :agent-name agent-name
              :active-p active-p
              :display (format nil "~A ~A" (if active-p "*" " ") agent-name)
              :match-text agent-name)))))

(defun sort-agent-selector-items (items)
  "Sort agent selector ITEMS with the active agent first, then alphabetically."
  (stable-sort (copy-list items)
               (lambda (a b)
                 (cond
                   ((and (getf a :active-p) (not (getf b :active-p))) t)
                   ((and (getf b :active-p) (not (getf a :active-p))) nil)
                   (t (string< (getf a :agent-name)
                               (getf b :agent-name)))))))

(defun minibuffer-select-agent-command (buffer)
  "Open the minibuffer agent selector for the current buffer."
  (let* ((active-agent (buffer-agent-name buffer))
         (known-agents (list-known-agent-names))
         (items (sort-agent-selector-items
                 (mapcar (lambda (agent-name)
                           (make-agent-selector-item agent-name active-agent))
                         known-agents))))
    (cond
      ((null items)
       (buffer-insert-system-message buffer "[No known agents available.]"))
      (t
       (minibuffer-activate
        "Select Agent" items
        (lambda (item)
          (switch-buffer-to-agent buffer (getf item :agent-name))))
       (preselect-minibuffer-active-item items)))))
(defcommand minibuffer-select-agent-command)

(defun select-model-command (buffer)
  "Open the standard visible minibuffer model selector."
  (setf *model-selector-active* nil)
  (minibuffer-select-model-command buffer))
(defcommand select-model-command)

(defun minibuffer-select-model-command (buffer)
  "Open the minibuffer model selector with fuzzy search (helm/ivy/vertico style).
Activates the minibuffer with all available models as candidates, sorted by
recency then alphabetically. The user can type to fuzzy-filter and use C-n/C-p
to navigate."
  (let ((entries (available-models-for-selector buffer)))
    (cond
      ((null entries)
       (buffer-insert-system-message
        buffer "[No API keys configured. Cannot list models.]"))
      (t
       (let* ((items (build-model-selector-items entries))
              ;; Sort: by recency (from history), then active, then alphabetical
              (sorted (sort-models-by-recency items)))
         (minibuffer-activate
          "Select Model" sorted
          (lambda (item)
            (let ((provider (getf item :provider))
                  (model (getf item :model)))
              (multiple-value-bind (think-status think-level)
                  (apply-buffer-model-selection buffer provider model)
                (record-model-selection-history (getf item :display))
                (insert-model-selection-message buffer
                                                provider
                                                model
                                                think-status
                                                think-level))))))))))
(defcommand minibuffer-select-model-command)

(defun select-think-level-command (buffer)
  "Open the standard visible minibuffer think-level selector."
  (setf *think-selector-active* nil)
  (minibuffer-select-think-level-command buffer))
(defcommand select-think-level-command)

(defun minibuffer-select-think-level-command (buffer)
  "Open the minibuffer think-level selector for the active model."
  (let ((entries (available-think-levels-for-selector buffer)))
    (cond
      ((null entries)
       (multiple-value-bind (provider model)
           (handler-case (resolve-buffer-provider-and-model buffer)
             (error () (values nil nil)))
         (buffer-insert-system-message
          buffer
          (if (and provider model)
              (format nil "[Think levels not available for ~A.]"
                      (model-selector-display provider model))
              "[Think levels are not available for the active model.]"))))
      (t
       (minibuffer-activate
        "Select Think Level" entries
        (lambda (item)
          (apply-buffer-think-level-selection buffer item)))
       (preselect-minibuffer-active-item entries)))))
(defcommand minibuffer-select-think-level-command)

;;; --------------------------------------------------------------------------
;;; Buffer Management Commands (continued)
;;; --------------------------------------------------------------------------

(defun minibuffer-select-buffer-command (buffer)
  "Open the minibuffer buffer selector with fuzzy search (helm/ivy/vertico style).
Activates the minibuffer with all open buffers as candidates, sorted by
recorded and ring recency. The user can type to fuzzy-filter and use C-n/C-p
to navigate. Shows buffer name, agent, status, and message count."
  (declare (ignore buffer))
  (let* ((current (current-buffer))
         (items (mapcar (lambda (buf)
                          (let* ((name (buffer-name buf))
                                 (agent (buffer-agent-name buf))
                                 (status (string-downcase
                                          (symbol-name (buffer-status buf))))
                                 (msgs (max 0 (1- (buffer-message-count buf))))
                                 (current-p (eq buf current))
                                 (marker (if current-p "*" " "))
                                 (display (format nil "~A ~A  [~A] ~A  msgs:~D"
                                                  marker name agent status msgs)))
                            (list :buffer buf
                                  :name name
                                  :current-p current-p
                                  :display display
                                  :match-text name)))
                        *buffer-ring*))
         ;; Sort: explicit selector history, then the buffer ring's MRU order.
         (sorted (sort-buffers-by-recency items)))
    (minibuffer-activate
     "Switch Buffer" sorted
     (lambda (item)
       (let ((selected-buf (getf item :buffer))
             (name (getf item :name)))
         (when selected-buf
           (switch-to-buffer selected-buf)
           ;; Record in history for recency sorting
           (setf *buffer-selection-history*
                 (cons name
                       (remove name *buffer-selection-history*
                               :test #'string=)))))))
    ;; Return should perform a useful switch when the user has not typed a
    ;; query.  Keep the current buffer visible and marked, but select the most
    ;; recent non-current candidate when one exists.
    (let ((index (position-if-not (lambda (item) (getf item :current-p))
                                  sorted)))
      (when index
        (minibuffer-preselect-index index)))))
(defcommand minibuffer-select-buffer-command)

(defun new-buffer-command (buffer)
  "Create a new chat buffer and switch to it."
  (declare (ignore buffer))
  (let ((new-buf (make-chat-buffer (next-buffer-name)
                                   :agent-name *default-agent-name*
                                   :working-directory (truename ".")
                                   :add-to-ring-p t)))
    (autosave-session-snapshot new-buf)
    (switch-to-buffer new-buf)))
(defcommand new-buffer-command)

(defun new-listener-buffer-command (buffer)
  "Create or switch to the Common Lisp listener buffer."
  (declare (ignore buffer))
  (switch-to-buffer (ensure-listener-buffer)))
(defcommand new-listener-buffer-command)

(defun next-buffer-command (buffer)
  "Switch to the next buffer in the ring."
  (declare (ignore buffer))
  (when (cdr *buffer-ring*)
    ;; Rotate: move first to end
    (let ((current (pop *buffer-ring*)))
      (setf *buffer-ring* (append *buffer-ring* (list current))))))
(defcommand next-buffer-command)

(defun kill-buffer-command (buffer)
  "Kill the current buffer. Switches to the next buffer in the ring."
  (declare (ignore buffer))
  (when (cdr *buffer-ring*)  ; Don't kill the last buffer
    (kill-buffer-from-ring (current-buffer))))
(defcommand kill-buffer-command)

;;; --------------------------------------------------------------------------
;;; Session Commands
;;; --------------------------------------------------------------------------

(defun save-session-command (buffer)
  "Save the current buffer's persistent state."
  (cond
    ((file-buffer-p buffer)
     (let ((summary (project-save-buffer buffer)))
       (buffer-insert-system-message
        buffer
        (format nil "[Saved ~A:~A]"
                (getf summary :project)
                (getf summary :path)))))
    ((scratch-buffer-p buffer)
     (buffer-insert-system-message
      buffer
      "[Scratch buffer is not saved; it lasts only until RPLACA exits.]"))
    (t
     (let ((path (save-session buffer)))
       ;; Insert a system message confirming the save
        (buffer-insert-system-message
        buffer
        (format nil "[Session saved to ~A]" path))))))
(defcommand save-session-command)

(defun apply-session-branch-to-buffer
    (buffer leaf-id &key input-text (autosave-p t))
  "Move BUFFER's session to LEAF-ID and display that branch."
  (let ((session (or (buffer-session buffer)
                     (ensure-buffer-session buffer))))
    (set-session-current-leaf session leaf-id)
    (multiple-value-bind (provider model think-level)
        (session-branch-state session leaf-id)
      (setf (buffer-provider-override buffer) provider
            (buffer-model-override buffer) model
            (buffer-think-level-override buffer) think-level))
    (replace-buffer-history-with-serialized-messages
     buffer
     (session-active-branch-message-events session leaf-id)
     :input-text input-text
     :autosave-p autosave-p)
    buffer))

(defun session-branch-summary-source-text (buffer)
  "Return a text transcript of BUFFER's current visible branch."
  (with-output-to-string (out)
    (loop :for msg := (buffer-first-message buffer) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buffer))))
          :do (format out "~(~A~)> ~A~%~%"
                      (message-sender msg)
                      (message-text msg)))))

(defun generate-session-branch-summary (buffer &key custom-instructions)
  "Generate a branch summary for BUFFER using its active provider."
  (let* ((source (session-branch-summary-source-text buffer))
         (instructions
           (if (session-tree-blank-string-p custom-instructions)
               "Summarize this abandoned conversation branch. Preserve decisions, facts, files touched, and unresolved work. Keep it concise and useful as future context."
               custom-instructions))
         (prompt (format nil "~A~%~%Branch transcript:~%~A"
                         instructions source)))
    (multiple-value-bind (provider model think-level)
        (resolve-buffer-provider-and-model buffer)
      (let* ((state (provider-request-streaming
                     provider
                     (list `((:role . "user")
                             (:content . ,(coerce
                                           (canonicalize-message-content
                                            "user"
                                            prompt)
                                           'vector))))
                     (lambda (s) (declare (ignore s)))
                     :model model
                     :tools #()
                     :system-prompt "You summarize abandoned chat branches for future context."
                     :reasoning-effort think-level))
             (response (wait-for-compaction-stream-state state))
             (summary (content-text-blocks (response-content response))))
        (when (session-tree-blank-string-p summary)
          (error "Branch summary provider returned an empty summary"))
        summary))))

(defun complete-session-tree-navigation
    (buffer entry-id leaf-id input-text &key summarize custom-instructions)
  "Finish navigation to ENTRY-ID in BUFFER."
  (let* ((session (buffer-session buffer))
         (target-leaf leaf-id))
    (when summarize
      (let ((summary (generate-session-branch-summary
                      buffer
                      :custom-instructions custom-instructions)))
        (setf target-leaf
              (record-session-branch-summary session leaf-id summary))))
    (apply-session-branch-to-buffer buffer target-leaf :input-text input-text)
    (buffer-insert-system-message
     buffer
     (format nil "[Navigated session tree to ~A]" entry-id)
     :record-p nil)))

(defun session-tree-summary-choice-items ()
  "Return minibuffer choices for branch navigation summaries."
  (list (list :choice :none
              :display "No summary"
              :match-text "none no summary")
        (list :choice :summary
              :display "Summarize abandoned branch"
              :match-text "summarize abandoned branch")
        (list :choice :custom
              :display "Summarize with custom prompt"
              :match-text "summarize custom prompt")))

(defun prompt-session-tree-summary-choice (buffer entry-id leaf-id input-text)
  "Ask how to summarize before navigating the session tree."
  (minibuffer-activate
   "Branch Summary"
   (session-tree-summary-choice-items)
   (lambda (item)
     (case (getf item :choice)
       (:none
        (complete-session-tree-navigation
         buffer entry-id leaf-id input-text))
       (:summary
        (handler-case
            (complete-session-tree-navigation
             buffer entry-id leaf-id input-text :summarize t)
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Branch summary failed: ~A]" e)
             :record-p nil))))
       (:custom
        (minibuffer-prompt
         "Summary Prompt"
         (lambda (custom)
           (handler-case
               (complete-session-tree-navigation
                buffer entry-id leaf-id input-text
                :summarize t
                :custom-instructions custom)
             (error (e)
               (buffer-insert-system-message
                buffer
                (format nil "[Branch summary failed: ~A]" e)
                :record-p nil))))))))))

(defun session-tree-navigation-needs-summary-p (session leaf-id)
  "Return true when moving to LEAF-ID abandons the current leaf."
  (let ((current (session-effective-leaf-id session)))
    (and current
         (not (and leaf-id (string= current leaf-id))))))

(defun navigate-session-tree-item (buffer item)
  "Navigate BUFFER's session according to selected tree ITEM."
  (let* ((session (buffer-session buffer))
         (entry-id (getf item :id))
         (leaf-id (session-navigation-leaf-for-entry session entry-id))
         (input-text (session-entry-user-message-text session entry-id)))
    (if (session-tree-navigation-needs-summary-p session leaf-id)
        (prompt-session-tree-summary-choice buffer entry-id leaf-id input-text)
        (complete-session-tree-navigation buffer entry-id leaf-id input-text))))

(defun edit-session-tree-label (buffer item)
  "Prompt for a label for selected session tree ITEM."
  (let ((entry-id (getf item :id))
        (current-label (or (getf item :label) "")))
    (minibuffer-prompt
     "Entry Label"
     (lambda (label)
       (record-session-label-change (buffer-session buffer) entry-id label)
       (buffer-insert-system-message
        buffer
        (if (session-tree-blank-string-p label)
            (format nil "[Cleared label on ~A]" entry-id)
            (format nil "[Labeled ~A: ~A]" entry-id label))
        :record-p nil)
       (session-tree-selector-activate
        buffer
        (lambda (selected)
          (navigate-session-tree-item buffer selected))
        :label-callback
        (lambda (selected)
          (edit-session-tree-label buffer selected))
        :initial-entry-id entry-id))
     :initial-input current-label)))

(defun session-tree-command (buffer)
  "Open the current session's tree selector."
  (let ((session (ensure-buffer-session buffer)))
    (if (null (session-normalized-tree-events session))
        (buffer-insert-system-message
         buffer
         "[Current session has no tree entries yet.]"
         :record-p nil)
        (session-tree-selector-activate
         buffer
         (lambda (item)
           (navigate-session-tree-item buffer item))
         :label-callback
         (lambda (item)
           (edit-session-tree-label buffer item))))))
(defcommand session-tree-command)

(defun fork-session-from-tree-item (buffer item)
  "Fork BUFFER's session from selected tree ITEM into a new buffer."
  (let* ((session (buffer-session buffer))
         (entry-id (getf item :id))
         (new-buffer (fork-session-from-entry-id buffer entry-id)))
    new-buffer))

(defun fork-session-from-entry-id (buffer entry-id)
  "Fork BUFFER's session from ENTRY-ID into a new buffer."
  (let* ((session (buffer-session buffer))
         (leaf-id (session-navigation-leaf-for-entry session entry-id))
         (input-text (session-entry-user-message-text session entry-id))
         (new-session (create-branched-session session leaf-id))
         (new-buffer (make-buffer (session-name new-session)
                                  :agent-name (buffer-agent-name buffer)
                                  :working-directory (buffer-working-directory buffer)
                                  :session new-session)))
    (multiple-value-bind (provider model think-level)
        (session-branch-state new-session leaf-id)
      (setf (buffer-provider-override new-buffer) provider
            (buffer-model-override new-buffer) model
            (buffer-think-level-override new-buffer) think-level))
    (initialize-buffer-display-defaults new-buffer)
    (replace-buffer-history-with-serialized-messages
     new-buffer
     (session-active-branch-message-events new-session leaf-id)
     :input-text input-text)
    (add-buffer-to-ring new-buffer)
    (switch-to-buffer new-buffer)
    (buffer-insert-system-message
     new-buffer
     (format nil "[Forked from ~A]" entry-id)
     :record-p nil)
    new-buffer))

(defun fork-session-command (buffer)
  "Fork a selected session tree point into a new session buffer."
  (let ((session (ensure-buffer-session buffer)))
    (if (null (session-normalized-tree-events session))
        (buffer-insert-system-message
         buffer
         "[Current session has no tree entries to fork.]"
         :record-p nil)
        (session-tree-selector-activate
         buffer
         (lambda (item)
           (fork-session-from-tree-item buffer item))
         :label-callback
         (lambda (item)
           (edit-session-tree-label buffer item))))))
(defcommand fork-session-command)

(defun session-selector-display-text (record)
  "Return the display text for one saved-session RECORD."
  (let ((display-name (getf record :display-name))
        (session-name (getf record :session-name))
        (session-id (getf record :session-id)))
    (if (and display-name
             (stringp display-name)
             (not (string= display-name session-name)))
        (if (and session-id (plusp (length session-id)))
            (format nil "~A  [~A]  {~A}" display-name session-name session-id)
            (format nil "~A  [~A]" display-name session-name))
        (if (and session-id (plusp (length session-id)))
            (format nil "~A  {~A}" session-name session-id)
            session-name))))

(defun session-selector-match-text (record)
  "Return the fuzzy-match text for one saved-session RECORD."
  (with-output-to-string (stream)
    (format stream "~A " (or (getf record :session-name) ""))
    (format stream "~A " (or (getf record :display-name) ""))
    (format stream "~A " (or (getf record :session-id) ""))
    (let ((working-directory (getf record :working-directory)))
      (when working-directory
        (format stream "~A " working-directory)))
    (let ((path (getf record :path)))
      (when path
        (format stream "~A" path)))))

(defun record-session-selection (name)
  "Record NAME as the most recently selected session entry."
  (setf *buffer-selection-history*
        (cons name
              (remove name *buffer-selection-history* :test #'string=))))

(defun unique-loaded-buffer-name (base-name)
  "Return a unique buffer name based on BASE-NAME."
  (if (null (find-buffer-by-name base-name))
      base-name
      (loop :for suffix :from 2
            :for candidate := (format nil "~A<~D>" base-name suffix)
            :unless (find-buffer-by-name candidate)
              :return candidate)))

(defun find-open-session-buffer (session-name)
  "Return an open buffer already attached to SESSION-NAME, or NIL."
  (find session-name *buffer-ring*
        :test #'string=
        :key (lambda (candidate)
               (let ((session (buffer-session candidate)))
                 (and session
                      (session-name session))))))

(defun open-saved-session-record (source-buffer record &key (reuse-existing-p t))
  "Switch to RECORD's session buffer, loading it if needed.

When REUSE-EXISTING-P is NIL, always load a fresh buffer even if the session is
already open elsewhere."
  (let* ((session-name (getf record :session-name))
         (existing (and reuse-existing-p
                        (find-open-session-buffer session-name))))
    (if existing
        (progn
          (switch-to-buffer existing)
          (record-session-selection (buffer-name existing))
          existing)
        (let ((loaded (load-session session-name)))
          (if loaded
              (progn
                (setf (buffer-name loaded)
                      (unique-loaded-buffer-name (buffer-name loaded)))
                (initialize-buffer-display-defaults loaded)
                (add-buffer-to-ring loaded)
                (switch-to-buffer loaded)
                (record-session-selection (buffer-name loaded))
                loaded)
              (progn
                (buffer-insert-system-message
                 source-buffer
                 (format nil "[Saved session ~A is no longer available.]"
                         session-name))
                nil))))))

(defun load-session-command (buffer)
  "Load a saved chat session into a new buffer via minibuffer completion."
  (let* ((records (copy-list (or (list-saved-session-records) nil)))
         (recent (most-recent-saved-session-record
                  :working-directory (buffer-working-directory buffer)))
         (items (mapcar (lambda (record)
                          (let* ((session-name (getf record :session-name))
                                 (open-p (not (null (find-open-session-buffer
                                                     session-name))))
                                 (display (session-selector-display-text record)))
                            (list :session-name session-name
                                  :open-p open-p
                                  :display (if open-p
                                               (format nil "~A  [open]"
                                                       display)
                                               display)
                                  :match-text (session-selector-match-text record))))
                        records)))
    (if items
        (progn
          (minibuffer-activate
           "Load Session"
           items
           (lambda (item)
             (handler-case
                 (open-saved-session-record
                  buffer
                  (or (resolve-saved-session-record
                       (getf item :session-name))
                      item)
                  :reuse-existing-p nil)
               (error (e)
                 (buffer-insert-system-message
                  buffer
                  (format nil "[Load session failed: ~A]" e))))))
          (when recent
            (let ((index (position (getf recent :session-name)
                                   *minibuffer-filtered-items*
                                   :key (lambda (item)
                                          (getf item :session-name))
                                   :test #'string=)))
              (when index
                (minibuffer-preselect-index index)))))
        (buffer-insert-system-message
         buffer
         "[No saved sessions available.]"))))
(defcommand load-session-command)

(defun continue-session-command (buffer)
  "Continue the most recent saved session for BUFFER's working directory."
  (let ((record (most-recent-saved-session-record
                 :working-directory (buffer-working-directory buffer))))
    (if record
        (handler-case
            (open-saved-session-record buffer record)
          (error (e)
            (buffer-insert-system-message
             buffer
             (format nil "[Continue session failed: ~A]" e))))
        (buffer-insert-system-message
         buffer
         "[No saved session found for this working directory.]"))))
(defcommand continue-session-command)

(defun show-session-info-buffer (buffer)
  "Display BUFFER's session metadata in a reusable help buffer."
  (let ((session (ensure-buffer-session buffer)))
    (if (null session)
        (buffer-insert-system-message buffer "[Current buffer has no session.]"
                                      :record-p nil)
        (let ((name "*help:session*")
              (content (session-summary-string session :buffer buffer)))
          (let ((existing (find-buffer-by-name name)))
            (if existing
                (progn
                  (let ((message (message-prev (buffer-input-message existing))))
                    (if message
                        (set-message-text message content)
                        (buffer-insert-agent-message existing content)))
                  (setf (buffer-scroll-offset existing) most-positive-fixnum)
                  (notify-buffer-display-change existing :session-info)
                  (switch-to-buffer existing))
                (switch-to-buffer (make-help-buffer name content))))))))

(defun session-info-command (buffer)
  "Display the active session's identity and storage details."
  (show-session-info-buffer buffer))
(defcommand session-info-command)

(defun set-session-display-name-command (buffer display-name)
  "Persist DISPLAY-NAME as BUFFER's session label.
Blank input clears the stored display name."
  (let ((session (ensure-buffer-session buffer)))
    (set-session-display-name session display-name)
    (autosave-session-snapshot buffer)
    (notify-buffer-display-change buffer :session-display-name)
    (buffer-insert-system-message
     buffer
     (if (session-display-name session)
         (format nil "[Session display name: ~A]"
                 (session-display-name session))
         "[Session display name cleared.]")
     :record-p nil)))
(defcommand set-session-display-name-command
  :prompts ((display-name :prompt "Session display name")))

(defun execute-extended-command (buffer)
  "Select and run a command via the fuzzy minibuffer. Bound to M-x."
  (let ((items (make-command-selector-items :buffer buffer)))
    (if (null items)
        (buffer-insert-system-message buffer "[No commands available]")
        (minibuffer-activate
         "M-x"
         items
         (lambda (item)
           (invoke-command buffer (getf item :command)))))))
(defcommand execute-extended-command :keys ((:meta #\x)))

;;; --------------------------------------------------------------------------
;;; Display Toggle Commands
;;; --------------------------------------------------------------------------

(defun toggle-tool-results-command (buffer)
  "Toggle visibility of tool-result messages in the chat."
  (setf (buffer-show-tool-results-p buffer)
        (not (buffer-show-tool-results-p buffer)))
  (notify-buffer-display-change buffer :visibility))
(defcommand toggle-tool-results-command)

(defun toggle-reasoning-output-command (buffer)
  "Toggle visibility of provider-supplied reasoning blocks in the chat."
  (setf (buffer-show-reasoning-p buffer)
        (not (buffer-show-reasoning-p buffer)))
  (notify-buffer-display-change buffer :visibility))
(defcommand toggle-reasoning-output-command)

(defun toggle-metadata-output-command (buffer)
  "Toggle visibility of provider/response metadata in the chat."
  (setf (buffer-show-metadata-p buffer)
        (not (buffer-show-metadata-p buffer)))
  (notify-buffer-display-change buffer :visibility))
(defcommand toggle-metadata-output-command)

(defun toggle-debug-mode-command (buffer)
  "Toggle API debug mode on/off. When enabled, every outgoing API request
(provider, model, full messages/tools JSON) and every completed response
(stop-reason, content blocks) is echoed into the chat window as a debug
message, rendered in magenta so it stands out from normal system output.
Bound to C-c C-d."
  (setf *debug-mode* (not *debug-mode*))
  (buffer-insert-system-message
   buffer
   (if *debug-mode*
       "[Debug mode ON — API calls will be shown in chat]"
       "[Debug mode OFF]")))
(defcommand toggle-debug-mode-command)

(defun redraw-screen-command (buffer)
  "Request a full screen redraw. Bound to C-l."
  (declare (ignore buffer))
  :redraw)
(defcommand redraw-screen-command)

;;; --------------------------------------------------------------------------
;;; Customize Drawing Style
;;; --------------------------------------------------------------------------
;;; Introspection: list-functions & describe-function
;;; --------------------------------------------------------------------------

(defun list-functions ()
  "Return a sorted list of function symbols exported from the rplaca package.
Includes all exported symbols that have function bindings (functions, generic
functions, commands, macros)."
  (let ((functions nil))
    (do-external-symbols (sym :rplaca)
      (when (fboundp sym)
        (push sym functions)))
    (sort functions #'string< :key #'symbol-name)))

(defun find-keybindings-for-command (command-sym &optional (keymap *default-keymap*))
  "Return a list of key specifications bound to COMMAND-SYM in KEYMAP.
Walks only the direct keymap bindings (not the parent chain)."
  (let ((bindings nil))
    (when keymap
      (maphash (lambda (key cmd)
                 (when (eq cmd command-sym)
                   (push key bindings)))
               (keymap-bindings keymap)))
    bindings))

(defun format-key-binding (key)
  "Format a key binding specification as a human-readable string.
Converts raw characters, keywords, and prefix lists to standard Emacs notation."
  (cond
    ((characterp key)
     (let ((code (char-code key)))
       (cond
         ((= code 13) "RET")
         ((= code 10) "C-j")
         ((= code 27) "ESC")
         ((= code 127) "DEL")
         ((< code 32) (format nil "C-~A" (code-char (+ code 96))))
         ((char= key #\Space) "SPC")
         (t (string key)))))
    ((keywordp key)
     (string-downcase (symbol-name key)))
    ((and (listp key) (eq (first key) :ctrl-x))
     (format nil "C-x ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl-c))
     (format nil "C-c ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl-h))
     (format nil "C-h ~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :meta))
     (format nil "M-~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :alt))
     (format nil "A-~A" (format-key-binding (second key))))
    ((and (listp key) (eq (first key) :ctrl))
     (format nil "C-~A" (format-key-binding (second key))))
    (t (format nil "~S" key))))

(defun describe-function-to-string (fn-symbol)
  "Return a human-readable string describing FN-SYMBOL.
Includes: name, type, lambda list, docstring, and keybindings."
  (unless (and fn-symbol (fboundp fn-symbol))
    (return-from describe-function-to-string
      (format nil "~A is not a defined function." fn-symbol)))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" fn-symbol
            (make-string (min 60 (length (symbol-name fn-symbol)))
                         :initial-element #\-))
    ;; Type
    (let* ((cmd-meta (find-command-metadata fn-symbol))
           (fn-obj (fdefinition fn-symbol))
           (type-str (cond
                       ((macro-function fn-symbol) "Macro")
                       (cmd-meta "Command")
                       ((typep fn-obj 'generic-function) "Generic Function")
                       (t "Function"))))
      (format s "Type: ~A~%" type-str)
      ;; Lambda list
      (let ((lambda-list
              (handler-case
                  (cond
                    ((typep fn-obj 'generic-function)
                     #+sbcl (sb-mop:generic-function-lambda-list fn-obj)
                     #-sbcl nil)
                    (t
                     #+sbcl (sb-introspect:function-lambda-list fn-symbol)
                     #-sbcl nil))
                (error () nil))))
        (when lambda-list
          (format s "Arguments: (~{~A~^ ~})~%" lambda-list)))
      ;; Keybindings (from actual keymap scan)
      (let ((keybinds (find-keybindings-for-command fn-symbol)))
        (when keybinds
          (format s "Keybindings: ~{~A~^, ~}~%"
                  (mapcar #'format-key-binding keybinds))))
      ;; Docstring
      (let ((doc (or (documentation fn-symbol 'function) "")))
        (when (plusp (length doc))
          (format s "~%~A~%" doc)))
      ;; Extended documentation
      (let ((ext (extended-doc fn-symbol)))
        (when ext
          (let ((usage (getf ext :usage)))
            (when usage
              (format s "~%Usage:~%  ~A~%" usage)))
          (let ((returns (getf ext :returns)))
            (when returns
              (format s "~%Returns:~%  ~A~%" returns)))
          (let ((side-effects (getf ext :side-effects)))
            (when side-effects
              (format s "~%Side Effects:~%  ~A~%" side-effects)))
          (let ((see-also (getf ext :see-also)))
            (when see-also
              (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
          (let ((category (getf ext :category)))
            (when category
              (format s "~%Category: ~A~%" category))))))))

(defun describe-function-command (buffer)
  "Open a minibuffer selector listing all functions.
On selection, displays detailed function description in a help buffer.
Bound to C-h f."
  (declare (ignore buffer))
  (let* ((fn-list (list-functions))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (cmd-meta (find-command-metadata sym))
                                 (fn-obj (fdefinition sym))
                                 (type-str (cond
                                             ((macro-function sym) "macro")
                                             (cmd-meta "command")
                                             ((typep fn-obj 'generic-function)
                                              "generic")
                                             (t "function")))
                                 (keybinds (find-keybindings-for-command sym))
                                 (kb-str (if keybinds
                                             (format nil "  [~{~A~^, ~}]"
                                                     (mapcar #'format-key-binding keybinds))
                                             ""))
                                 (display (format nil "~A  (~A)~A" name type-str kb-str)))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        fn-list)))
    (minibuffer-activate
     "Describe Function" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-function-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this function if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))
(defcommand describe-function-command)

;;; --------------------------------------------------------------------------
;;; Introspection: list-variables & describe-variable
;;; --------------------------------------------------------------------------

(defun list-variables ()
  "Return a sorted list of variable symbols exported from the rplaca package.
Includes all exported symbols that have global variable bindings (special
variables, constants, parameters)."
  (let ((variables nil))
    (do-external-symbols (sym :rplaca)
      (when (boundp sym)
        (push sym variables)))
    (sort variables #'string< :key #'symbol-name)))

(defun variable-kind (sym)
  "Return a keyword describing the kind of variable SYM.
Returns :constant, :parameter, or :variable."
  (cond
    ((constantp sym) :constant)
    ;; Convention: *foo* with earmuffs is a special/dynamic variable.
    ;; defparameter defines a special variable with a default value — we call
    ;; those "parameter" to distinguish from plain defvar.  Since CL doesn't
    ;; store this distinction at runtime, we just rely on the earmuff naming.
    ((let ((name (symbol-name sym)))
       (and (> (length name) 2)
            (char= (char name 0) #\*)
            (char= (char name (1- (length name))) #\*)))
     :parameter)
    (t :variable)))

(defun truncate-value-string (value &optional (max-length 200))
  "Print VALUE to a string, truncating at MAX-LENGTH characters."
  (let* ((full (handler-case
                   (let ((*print-length* 20)
                         (*print-level* 3)
                         (*print-circle* t)
                         (*print-pretty* nil))
                     (prin1-to-string value))
                 (error (e)
                   (format nil "#<error printing value: ~A>" e))))
         (len (length full)))
    (if (> len max-length)
        (concatenate 'string (subseq full 0 max-length) "...")
        full)))

(defun describe-variable-to-string (var-symbol)
  "Return a human-readable string describing VAR-SYMBOL.
Includes: name, kind, type of current value, current value (truncated),
and docstring."
  (unless (and var-symbol (boundp var-symbol))
    (return-from describe-variable-to-string
      (format nil "~A is not a bound variable." var-symbol)))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" var-symbol
            (make-string (min 60 (length (symbol-name var-symbol)))
                         :initial-element #\-))
    ;; Kind
    (let ((kind (variable-kind var-symbol)))
      (format s "Kind: ~A~%"
              (ecase kind
                (:constant  "Constant (defconstant)")
                (:parameter "Special Variable (defvar/defparameter)")
                (:variable  "Variable"))))
    ;; Value type
    (let ((val (symbol-value var-symbol)))
      (format s "Value Type: ~A~%" (type-of val))
      ;; Current value (truncated)
      (format s "Current Value: ~A~%" (truncate-value-string val)))
    ;; Docstring
    (let ((doc (or (documentation var-symbol 'variable) "")))
      (when (plusp (length doc))
        (format s "~%~A~%" doc)))
    ;; Extended documentation
    (let ((ext (extended-doc var-symbol)))
      (when ext
        (let ((side-effects (getf ext :side-effects)))
          (when side-effects
            (format s "~%Side Effects:~%  ~A~%" side-effects)))
        (let ((see-also (getf ext :see-also)))
          (when see-also
            (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
        (let ((category (getf ext :category)))
          (when category
            (format s "~%Category: ~A~%" category)))))))

(defun describe-variable-command (buffer)
  "Open a minibuffer selector listing all exported variables.
On selection, displays detailed variable description in a help buffer.
Bound to C-h v."
  (declare (ignore buffer))
  (let* ((var-list (list-variables))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (kind (variable-kind sym))
                                 (kind-str (ecase kind
                                             (:constant "const")
                                             (:parameter "special")
                                             (:variable "var")))
                                 (val-preview
                                   (handler-case
                                       (let ((val (symbol-value sym)))
                                         (truncate-value-string val 40))
                                     (error () "#<unreadable>")))
                                 (display (format nil "~A  (~A)  = ~A"
                                                  name kind-str val-preview)))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        var-list)))
    (minibuffer-activate
     "Describe Variable" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-variable-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this variable if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))
(defcommand describe-variable-command)

;;; --------------------------------------------------------------------------
;;; Introspection: list-types & describe-type
;;; --------------------------------------------------------------------------

(defun list-types ()
  "Return a sorted list of type-name symbols exported from the rplaca package.
Includes CLOS classes, structures, and conditions — any exported symbol that
names a class (via find-class)."
  (let ((types nil))
    (do-external-symbols (sym :rplaca)
      (when (find-class sym nil)
        (push sym types)))
    (sort types #'string< :key #'symbol-name)))

(defun type-kind (sym)
  "Return a keyword describing the kind of type SYM names.
Returns :condition, :structure, :standard-class, or :class."
  (let ((class (find-class sym nil)))
    (cond
      ((null class) :unknown)
      ((subtypep sym 'condition) :condition)
      ((typep class 'structure-class) :structure)
      ((typep class 'standard-class) :standard-class)
      (t :class))))

(defun type-kind-label (kind)
  "Return a human-readable label for a type-kind keyword."
  (ecase kind
    (:condition "Condition")
    (:structure "Structure (defstruct)")
    (:standard-class "Class (defclass)")
    (:class "Built-in Class")
    (:unknown "Unknown")))

(defun type-slot-info (class)
  "Return a list of plists describing each slot in CLASS.
Each plist has :name, :type, :initform, :initargs, :readers, :writers,
:allocation, and :documentation."
  #+sbcl
  (handler-case
      (progn
        ;; Ensure the class is finalized so slots are available
        (unless (sb-mop:class-finalized-p class)
          (sb-mop:finalize-inheritance class))
        (mapcar
         (lambda (slot)
           (list :name (sb-mop:slot-definition-name slot)
                 :type (let ((ty (sb-mop:slot-definition-type slot)))
                         (if (eq ty t) nil ty))
                 :initform (if (sb-mop:slot-definition-initfunction slot)
                               (sb-mop:slot-definition-initform slot)
                               :no-initform)
                 :initargs (sb-mop:slot-definition-initargs slot)
                 :readers (when (typep slot 'sb-mop:direct-slot-definition)
                            (sb-mop:slot-definition-readers slot))
                 :writers (when (typep slot 'sb-mop:direct-slot-definition)
                            (sb-mop:slot-definition-writers slot))
                 :allocation (sb-mop:slot-definition-allocation slot)
                 :documentation (documentation slot t)))
         (sb-mop:class-direct-slots class)))
  (error () nil))
  #-sbcl nil)

(defun type-struct-slot-info (sym)
  "Return a list of plists describing each slot in structure type SYM.
Uses sb-kernel:dd-slots to get defstruct slot details."
  #+sbcl
  (handler-case
      (let* ((layout (sb-kernel:find-layout sym))
             (dd (when layout (sb-kernel:layout-info layout))))
        (when dd
          (mapcar
           (lambda (dsd)
             (list :name (sb-kernel:dsd-name dsd)
                   :type (let ((ty (sb-kernel:dsd-type dsd)))
                           (if (eq ty t) nil ty))
                   :read-only (sb-kernel:dsd-read-only dsd)
                   :accessor (let ((acc-name (sb-kernel:dsd-accessor-name dsd)))
                               (when (fboundp acc-name) acc-name))))
           (sb-kernel:dd-slots dd))))
    (error () nil))
  #-sbcl nil)

(defun describe-type-to-string (type-symbol)
  "Return a human-readable string describing the type named by TYPE-SYMBOL.
Includes: name, kind, superclasses, slots/fields with their types, initforms,
accessors, and documentation. Also shows class-level and extended documentation."
  (let ((class (find-class type-symbol nil)))
    (unless class
      (return-from describe-type-to-string
        (format nil "~A does not name a type." type-symbol))))
  (with-output-to-string (s)
    ;; Header
    (format s "~A~%~A~%~%" type-symbol
            (make-string (min 60 (length (symbol-name type-symbol)))
                         :initial-element #\-))
    (let* ((class (find-class type-symbol))
           (kind (type-kind type-symbol)))
      ;; Kind
      (format s "Kind: ~A~%" (type-kind-label kind))
      ;; Superclasses
      #+sbcl
      (handler-case
          (let* ((supers (sb-mop:class-direct-superclasses class))
                 (super-names (mapcar #'class-name supers)))
            (when (and super-names
                       (not (equal super-names '(structure-object)))
                       (not (equal super-names '(standard-object))))
              (format s "Superclasses: ~{~A~^, ~}~%" super-names)))
        (error () nil))
      ;; Class documentation
      (let ((doc (documentation class t)))
        (when (and doc (plusp (length doc)))
          (format s "~%~A~%" doc)))
      ;; Slots / Fields
      (cond
        ;; Structure type — use defstruct slot introspection
        ((eq kind :structure)
         (let ((slots (type-struct-slot-info type-symbol)))
           (when slots
             (format s "~%Fields:~%")
             (dolist (slot slots)
               (let ((name (getf slot :name))
                     (type (getf slot :type))
                     (read-only (getf slot :read-only))
                     (accessor (getf slot :accessor)))
                 (format s "  ~A" name)
                 (when type (format s " : ~A" type))
                 (when read-only (format s "  [read-only]"))
                 (when accessor (format s "  (accessor: ~A)" accessor))
                 (format s "~%"))))))
        ;; CLOS class or condition — use MOP
        ((member kind '(:standard-class :condition))
         (let ((slots (type-slot-info class)))
           (when slots
             (format s "~%Slots:~%")
             (dolist (slot slots)
               (let ((name (getf slot :name))
                     (type (getf slot :type))
                     (initform (getf slot :initform))
                     (initargs (getf slot :initargs))
                     (readers (getf slot :readers))
                     (writers (getf slot :writers))
                     (doc (getf slot :documentation)))
                 (format s "  ~A" name)
                 (when type (format s " : ~A" type))
                 (format s "~%")
                 (when initargs
                   (format s "    Initargs: ~{~S~^, ~}~%" initargs))
                 (unless (eq initform :no-initform)
                   (format s "    Default: ~S~%" initform))
                 (when readers
                   (format s "    Readers: ~{~A~^, ~}~%" readers))
                 (when writers
                   (format s "    Writers: ~{~A~^, ~}~%" writers))
                 (when (and doc (plusp (length doc)))
                   (format s "    ~A~%" doc))))))))
      ;; Extended documentation
      (let ((ext (extended-doc type-symbol)))
        (when ext
          (let ((usage (getf ext :usage)))
            (when usage
              (format s "~%Usage:~%  ~A~%" usage)))
          (let ((returns (getf ext :returns)))
            (when returns
              (format s "~%Returns:~%  ~A~%" returns)))
          (let ((side-effects (getf ext :side-effects)))
            (when side-effects
              (format s "~%Side Effects:~%  ~A~%" side-effects)))
          (let ((see-also (getf ext :see-also)))
            (when see-also
              (format s "~%See Also: ~{~(~A~)~^, ~}~%" see-also)))
          (let ((category (getf ext :category)))
            (when category
              (format s "~%Category: ~A~%" category))))))))

(defun undocumented-types ()
  "Return a list of exported type symbols that lack a defdoc entry.
Useful for finding types that still need extended documentation."
  (let ((missing nil))
    (dolist (sym (list-types))
      (unless (extended-doc sym)
        (push sym missing)))
    (nreverse missing)))

(defun describe-type-command (buffer)
  "Open a minibuffer selector listing all defined types.
On selection, displays detailed type description in a help buffer.
Bound to C-h T."
  (declare (ignore buffer))
  (let* ((type-list (list-types))
         (items (mapcar (lambda (sym)
                          (let* ((name (string-downcase (symbol-name sym)))
                                 (kind (type-kind sym))
                                 (kind-str (ecase kind
                                             (:condition "condition")
                                             (:structure "struct")
                                             (:standard-class "class")
                                             (:class "built-in")
                                             (:unknown "unknown")))
                                 (class (find-class sym nil))
                                 (doc-preview
                                   (let ((doc (when class (documentation class t))))
                                     (if (and doc (plusp (length doc)))
                                         (let ((first-line
                                                 (subseq doc 0
                                                         (or (position #\Newline doc)
                                                             (min 50 (length doc))))))
                                           (if (> (length first-line) 50)
                                               (concatenate 'string (subseq first-line 0 47) "...")
                                               first-line))
                                         "")))
                                 (display (if (plusp (length doc-preview))
                                              (format nil "~A  (~A)  ~A" name kind-str doc-preview)
                                              (format nil "~A  (~A)" name kind-str))))
                            (list :symbol sym
                                  :name name
                                  :display display)))
                        type-list)))
    (minibuffer-activate
     "Describe Type" items
     (lambda (item)
       (let* ((sym (getf item :symbol))
              (desc (describe-type-to-string sym))
              (buf-name (format nil "*help:~A*"
                                (string-downcase (symbol-name sym))))
              ;; Reuse existing help buffer for this type if one exists
              (existing (find-buffer-by-name buf-name)))
         (if existing
             (switch-to-buffer existing)
             (let ((help-buf (make-help-buffer buf-name desc)))
               (switch-to-buffer help-buf))))))))
(defcommand describe-type-command)

;;; --------------------------------------------------------------------------
;;; Describe Bindings (C-h b)
;;; --------------------------------------------------------------------------

(defun categorize-command (command-sym)
  "Return a category string for COMMAND-SYM based on its name.
Used to group keybindings in the describe-bindings listing."
  (let ((name (string-downcase (symbol-name command-sym))))
    (cond
      ((or (search "scroll" name) (search "forward-char" name)
           (search "backward-char" name) (search "forward-word" name)
           (search "backward-word" name) (search "beginning-of-line" name)
           (search "end-of-line" name) (search "next-line" name)
           (search "previous-line" name) (search "beginning-of-buffer" name)
           (search "end-of-buffer" name) (search "search" name))
       "Movement")
      ((or (search "kill" name) (search "delete" name)
           (search "yank" name) (search "insert-newline" name)
           (search "insert-tab" name) (search "self-insert" name)
           (search "mark" name) (search "copy-region" name))
       "Editing")
      ((or (search "buffer" name)
           (search "save-session" name)
           (search "load-session" name))
       "Buffers & Sessions")
      ((or (search "model" name) (search "select-model" name))
       "Model Selection")
      ((or (search "describe" name) (search "help" name)
           (search "info" name)
           (search "manual" name)
           (search "execute-extended" name))
       "Help & Introspection")
      ((or (search "debug" name) (search "toggle" name)
           (search "oauth" name))
       "Toggles & Misc")
      ((search "send-message" name)
       "Chat")
      (t "Other"))))

(defun describe-bindings-to-string (&optional (keymap *default-keymap*))
  "Return a formatted string listing all keybindings in KEYMAP, grouped by category.
Each binding shows the key notation and the command name."
  (let ((entries nil))
    ;; Collect all bindings as (key-string command-sym category) triples
    (maphash (lambda (key cmd)
               (let ((key-str (format-key-binding key)))
                 (push (list key-str cmd (categorize-command cmd)) entries)))
             (keymap-bindings keymap))
    ;; Group by category
    (let ((groups (make-hash-table :test #'equal)))
      (dolist (entry entries)
        (destructuring-bind (key-str cmd category) entry
          (declare (ignore cmd))
          (push (list key-str (third entry) (second entry)) (gethash category groups nil))))
      ;; Deduplicate: multiple keys may map to the same command.
      ;; For each category, collect unique (command → list-of-keys) then format.
      (with-output-to-string (s)
        (format s "Key Bindings~%")
        (format s "============~%~%")
        (format s "Keymap: ~A~%~%" (keymap-name keymap))
        ;; Define a stable category order
        (let ((category-order '("Chat" "Movement" "Editing" "Buffers & Sessions"
                                "Model Selection" "Help & Introspection"
                                "Toggles & Misc" "Other")))
          (dolist (category category-order)
            (let ((cat-entries (gethash category groups)))
              (when cat-entries
                (format s "~A~%" category)
                (format s "~A~%" (make-string (length category) :initial-element #\-))
                ;; Group by command symbol within the category
                (let ((cmd-keys (make-hash-table :test #'eq)))
                  (dolist (entry cat-entries)
                    (let ((key-str (first entry))
                          (cmd (third entry)))
                      (push key-str (gethash cmd cmd-keys nil))))
                  ;; Sort commands alphabetically and format
                  (let ((cmd-list nil))
                    (maphash (lambda (cmd keys)
                               (push (cons cmd (sort (copy-list keys) #'string<)) cmd-list))
                             cmd-keys)
                    (setf cmd-list (sort cmd-list #'string<
                                         :key (lambda (c)
                                                (string-downcase (symbol-name (car c))))))
                    (dolist (item cmd-list)
                      (let* ((cmd (car item))
                             (keys (cdr item))
                             (cmd-name (string-downcase (symbol-name cmd)))
                             (keys-str (format nil "~{~A~^, ~}" keys)))
                        (format s "  ~20A  ~A~%" keys-str cmd-name)))))
                (format s "~%")))))))))

(defun describe-bindings-command (buffer)
  "Open a help buffer listing all keybindings in the default keymap.
Bound to C-h b."
  (declare (ignore buffer))
  (let* ((buf-name "*help:keybindings*")
         (existing (find-buffer-by-name buf-name)))
    (if existing
        (switch-to-buffer existing)
        (let ((help-buf (make-help-buffer buf-name
                                          (describe-bindings-to-string))))
          (switch-to-buffer help-buf)))))
(defcommand describe-bindings-command)

;;; --------------------------------------------------------------------------
;;; Event Loop
;;; --------------------------------------------------------------------------

(defvar *alt-emulates-meta* t
  "When non-nil, physical Alt key events are treated as Meta in McCLIM.
Set this to NIL in user init to keep Alt and Meta separate when the backend
reports standalone Alt/Meta key events.")

;;; --------------------------------------------------------------------------
;;; Event Loop Dispatch
;;; --------------------------------------------------------------------------

(defun handle-key-event (buf key)
  "Dispatch a normalized key through the buffer's keymap.
Returns :QUIT if the application should exit, or nil otherwise.
KEY is already normalized by the interface before calling this."
  (flet ((redraw-key-p (candidate)
           (or (and (characterp candidate)
                    (char= candidate (code-char 12)))
               (equal candidate '(:ctrl #\l))
               (equal candidate '(:ctrl #\L))))
         (sync-current-composer-completion ()
           (let ((current (current-buffer)))
             (if (and current (not (eq current buf)))
                 (progn
                   (deactivate-slash-completion)
                   (deactivate-skill-completion))
                 (progn
                   (sync-slash-completion buf)
                   (if *slash-completion-active*
                       (deactivate-skill-completion)
                       (sync-skill-completion buf)))))))
  (let ((*current-caller* :user))
    (when (null key)
      (return-from handle-key-event nil))
    (cond
      ;; C-x C-c always quits (Emacs standard quit chord)
      ((equal key (list :ctrl-x #\Etx))
       (file-debug-event "key-command"
                         :key (format nil "~S" key)
                         :command 'quit-command)
       :quit)

      ;; C-l requests a full redraw in every mode.
      ((redraw-key-p key)
       (file-debug-event "key-command"
                         :key (format nil "~S" key)
                         :command 'redraw-screen-command)
       (redraw-screen-command buf))

      ;; Esc stops an active provider stream before any modal UI consumes it.
      ((and (characterp key)
            (char= key #\Esc)
            (buffer-llm-running-p buf))
       (stop-llm-command buf))

      ;; === MINIBUFFER MODE ===
      ;; When the minibuffer is active, it captures all input
      (*minibuffer-active*
       (handle-minibuffer-key key)
       nil)

      ;; === SESSION TREE SELECTOR MODE ===
      ;; Navigation, folding, filtering, and branch selection.
      (*session-tree-selector-active*
       (handle-session-tree-selector-key key)
       nil)

      ;; === HELP MODE ===
      ;; Help buffers are read-only views with a dedicated presentation.
      ((and buf (info-buffer-p buf))
       (handle-info-key buf key)
       nil)

      ;; === HELP MODE ===
      ;; Help buffers are read-only views with a dedicated presentation.
      ((and buf (help-buffer-p buf))
       (handle-help-key buf key)
       nil)

      ;; === FONT EDITOR MODE ===
      ;; CADR-style bitmap font editor buffers own their keyboard interaction,
      ;; but unhandled global prefixes still fall through to the default keymap.
      ((and buf (font-editor-buffer-p buf)
            (font-editor-handle-key buf key))
       :redraw)

      ;; === OPENAI OAUTH MODE ===
      ;; OAuth is pending in a background localhost callback server; only C-g cancels.
      ((openai-oauth-login-pending-p)
       (let ((flow (openai-oauth-pending-flow)))
         (cond
           ;; C-g: record cancellation; the frame process then claims and
           ;; applies that exact flow through the normal OAuth update path.
           ((and flow (characterp key) (char= key (code-char 7)))
            (cancel-openai-codex-oauth-login flow)
            (update-openai-oauth-login (openai-oauth-flow-buffer flow)))
           ;; Ignore other input while the browser flow is pending.
           (t nil)))
       nil)

      ;; === AUTOMATIC SLASH COMPLETION ===
      ((and *slash-completion-active*
            (handle-slash-completion-key buf key))
       nil)

      ;; === AUTOMATIC SKILL COMPLETION ===
      ;; Completion is non-modal for normal typing, but selected navigation and
      ;; confirmation keys are consumed before the chat keymap can send input.
      ((and *skill-completion-active*
            (handle-skill-completion-key buf key))
       nil)

      ;; === NORMAL MODE ===
      ;; Keymap lookup
      ((let ((command (keymap-lookup (buffer-keymap buf) key)))
         (when command
           (file-debug-event "key-command"
                             :key (format nil "~S" key)
                             :command command)
           (let ((result (invoke-command buf command)))
             (when (eq result :redraw)
               (return-from handle-key-event :redraw)))
           (when (and (characterp key)
                      (not (member command '(scroll-up-command scroll-down-command))))
             (setf (buffer-scroll-offset buf) 0))
           (sync-current-composer-completion)
           t)))
      ;; Self-insert
      ((and (characterp key) (graphic-char-p key))
       (let ((*self-insert-char* key))
       (self-insert-command buf))
       (setf (buffer-scroll-offset buf) 0)
       (sync-current-composer-completion)
       nil)
      (t nil)))))

(defparameter +default-user-init-directory+
  (merge-pathnames #P".rplaca.d/" (user-homedir-pathname))
  "Canonical directory for user Lisp configuration.")

(defparameter +legacy-user-init-directory+
  (merge-pathnames #P".clawmacs.d/" (user-homedir-pathname))
  "Legacy user configuration directory, never loaded automatically.")

(defvar *user-init-directory* +default-user-init-directory+
  "Directory for user Lisp configuration files.")

(defparameter +default-user-init-file+
  (merge-pathnames "init.lisp" +default-user-init-directory+))

(defparameter +legacy-user-init-file+
  (merge-pathnames "init.lisp" +legacy-user-init-directory+))

(defvar *user-init-file* +default-user-init-file+
  "Path to the user init file, loaded at startup if it exists.")

(defvar *inhibit-user-init* nil
  "When non-nil, skip loading the user init file at startup.")

(defun load-user-init-file ()
  "Load ~/.rplaca.d/init.lisp if it exists. Errors are caught and reported."
  (when *inhibit-user-init*
    (return-from load-user-init-file nil))
  (let* ((selected-path
           (configured-migration-read-path
            *user-init-file*
            +default-user-init-file+
            +legacy-user-init-file+
            :label "user init"
            :executable-p t))
         (init-path (and selected-path (probe-file selected-path))))
    (when init-path
      (handler-case
          (let ((*package* (find-package :rplaca)))
            (load init-path :verbose nil :print nil))
        (error (e)
          (format *error-output*
                  "~&;; Warning: error loading ~A:~%;; ~A~%"
                  init-path e)
          (file-debug-log "init" "error loading ~A: ~A" init-path e)
          nil)))))

(defun parse-rplaca-args ()
  "Parse command-line arguments and environment variables.
Recognized flags:
  --debug-log <path>   Enable file-based debug logging to <path>.
  --clean-build        Clear cached Lisp build artifacts before loading.
  --no-init            Skip loading the user init file.
Environment variables:
  RPLACA_DEBUG_LOG   Same as --debug-log (CLI flag takes precedence)."
  ;; CLI args (everything after SBCL's -- separator)
  (let ((args (uiop:command-line-arguments)))
    (setf *appearance-cli-selector*
          (parse-appearance-startup-arguments args))
    (loop :while args
          :for arg := (pop args)
          :do (cond
                ((string= arg "--debug-log")
                 (let ((path (pop args)))
                   (when path
                     (setf *debug-log-file* (pathname path)))))
                ((or (string= arg "--clean-build")
                     (string= arg "--force-clean-build"))
                 nil)
                ((string= arg "--no-init")
                 (setf *inhibit-user-init* t)))))
  ;; Environment variable fallback
  (unless *debug-log-file*
    (let ((env (uiop:getenv "RPLACA_DEBUG_LOG")))
      (when (and env (plusp (length env)))
        (setf *debug-log-file* (pathname env)))))
  ;; Log startup marker
  (when *debug-log-file*
    (file-debug-log "startup" "debug log enabled, writing to ~A" *debug-log-file*)))

(defun initialize-rplaca-runtime ()
  "Initialize shared runtime state before either UI or prompt execution."
  (init-default-keymap)
  (when (fboundp 'install-chat-frame-keybindings)
    (install-chat-frame-keybindings))
  (init-tools)
  ;; Load the configured personality prompt file before init.lisp so user init
  ;; may still override it directly or reload after changing the path.
  (load-personality-prompt-file)
  (load-user-init-file)
  (install-e2e-agent-definition)
  (when (fboundp 'install-chat-frame-keybindings)
    (install-chat-frame-keybindings))
  (reload-package-channels)
  (load-autoload-packages)
  (load-project-definitions)
  (run-hooks '*startup-hook*))

(defun reset-interaction-state ()
  "Reset buffer selectors, minibuffer state, OAuth state, and key prefixes."
  (setf *buffer-ring* nil *buffer-counter* 0)
  (setf *buffer-selector-active* nil
        *buffer-selector-index* 0
        *buffer-selector-scroll* 0)
  (setf *model-selector-active* nil
        *model-selector-index* 0
        *model-selector-scroll* 0
        *model-selector-entries* nil)
  (setf *think-selector-active* nil
        *think-selector-index* 0
        *think-selector-scroll* 0
        *think-selector-entries* nil)
  (clear-chat-interaction-state)
  (let ((flow (openai-oauth-pending-flow)))
    (when flow
      (unwind-protect
           (ignore-errors (cancel-openai-codex-oauth-login flow))
        (join-openai-oauth-flow-worker flow))
      (when (claim-openai-oauth-pending-flow flow)
        (let ((buffer (openai-oauth-flow-buffer flow)))
          (when buffer
            (setf (buffer-status buffer) :idle))))))
  nil)

(defun make-initial-chat-buffer (session-name agent-name
                                   &key (working-directory (truename ".")))
  "Create and register the eager sessionless conversation buffer.

The buffer is persistent-mode but sessionless: no session is attached until the
first say (todo 11), so autosave is a harmless no-op.  Display defaults, ring,
hooks, and the system-prompt header are initialized here."
  (let ((buf (make-buffer session-name
                          :agent-name agent-name
                          :working-directory working-directory
                          :kind :chat
                          :session-persistence-mode :persistent)))
    (initialize-buffer-display-defaults buf)
    (add-buffer-to-ring buf)
    (run-hook-with-args '*initial-buffer-hook* buf)
    (sync-buffer-system-prompt-display buf)
    (autosave-session-snapshot buf)
    buf))

(defun prompt-usage-string ()
  "Return command-line help for non-interactive prompt mode."
  (format nil "Usage: prompt.sh [options] PROMPT...

Options:
  --agent NAME              Use the named rplaca agent.
  --provider PROVIDER       Override provider: openai-codex, zai, openrouter.
                            Default without --agent: ~A.
  --model MODEL             Override the model name.
                            Default without --agent: ~A.
  --think LEVEL             Override reasoning effort when supported.
  --model-role ROLE         Apply a named model role for this run.
  --service-tier TIER       Prefer auto, default, flex, or priority transport.
  --show-tools              Print tool calls/results to stderr.
  --show-reasoning          Print provider-supplied reasoning blocks when present.
  --show-metadata           Print provider/model/iteration metadata to stderr.
  --json                    Emit a JSON result object to stdout.
  --jsonl                   Stream JSONL turn events to stdout.
  --output-schema SPEC      Validate the final response JSON against SPEC.
                            SPEC may be inline JSON or a path to a JSON file.
  --max-tool-iterations N   Stop after N tool-call turns (default: 20).
  --package NAME            Enable an installed package for this prompt run. May repeat.
  --skill-root PATH         Add a skill root for this prompt run. May repeat.
  --session NAME            Load/update a saved session instead of one-shot mode.
  --continue                Continue the most recent saved session for the current cwd.
  --ephemeral               Run without a saved session, transcript file, or autosave.
  --no-session              Alias for --ephemeral.
  --pipeline NAME           Run a deterministic pipeline defined in init.lisp.
  --debug-log PATH          Write low-level debug logs to PATH.
  --isolated                Use temporary prompt config/project/session dirs.
  --clean-build             Clear cached Lisp build artifacts before loading.
  --force-clean-build       Alias for --clean-build.
  --no-init                 Skip ~~/.rplaca.d/init.lisp.
  --help                    Show this help.

If PROMPT is omitted, non-interactive stdin is read as the prompt."
          +prompt-default-provider+
          +prompt-default-model+))

(defun default-session-prompt-session-name ()
  "Return the default saved session name for session-prompt.sh."
  (let ((name (uiop:getenv "RPLACA_SESSION_PROMPT_SESSION")))
    (if (and name (not (blank-string-p name)))
        name
        (or (most-recent-saved-session-name :working-directory (truename "."))
            +session-prompt-default-session-name+))))

(defun session-prompt-usage-string ()
  "Return command-line help for saved-session prompt mode."
  (format nil "Usage: session-prompt.sh [options] PROMPT...

Runs PROMPT against a saved prompt-mode session. The next invocation with the
same session name reloads the prior transcript before sending the new prompt.

Session options:
  --session NAME            Saved session to load/update. Accepts a session
                            name, unique id prefix, or saved-session path.
                            Default: most recent current-directory session
                            when available, otherwise ~A
  --continue                Continue the most recent saved session for the current cwd.
  --ephemeral               Run without a saved session, transcript file, or autosave.
  --no-session              Alias for --ephemeral.
  RPLACA_SESSION_PROMPT_SESSION
                            Environment default for --session.

All prompt.sh routing/output options are also supported.

Example:
  ./session-prompt.sh \"Reply with exactly: CACHE-PROBE-ONE\"
  ./session-prompt.sh \"Reply with exactly: CACHE-PROBE-TWO\""
          (default-session-prompt-session-name)))

(defun require-option-value (option args)
  "Pop and return OPTION's value from ARGS, or signal a clear error."
  (let ((value (pop args)))
    (unless value
      (error "~A requires a value" option))
    (values value args)))

(defun parse-positive-integer-option (option value)
  "Parse VALUE as a positive integer for OPTION."
  (let ((parsed (parse-integer value :junk-allowed nil)))
    (unless (plusp parsed)
      (error "~A must be a positive integer, got ~A" option value))
    parsed))

(defun read-stdin-to-string ()
  "Read all available standard input into a string."
  (let ((out (make-string-output-stream)))
    (loop :for char := (read-char *standard-input* nil nil)
          :while char
          :do (write-char char out))
    (get-output-stream-string out)))

(defun finalize-prompt-option-text (prompt-parts)
  "Return prompt text from PROMPT-PARTS or non-interactive stdin."
  (let ((from-args (and prompt-parts
                        (format nil "~{~A~^ ~}" prompt-parts))))
    (cond
      ((and from-args (not (blank-string-p from-args)))
       from-args)
      ((not (interactive-stream-p *standard-input*))
       (string-trim '(#\Space #\Tab #\Newline #\Return)
                    (read-stdin-to-string)))
      (t
       nil))))

(defun parse-rplaca-prompt-args (&optional (args (uiop:command-line-arguments)))
  "Parse ARGS for non-interactive prompt mode and return PROMPT-OPTIONS."
  (let ((options (make-prompt-options))
        (prompt-parts nil)
        (agent-supplied-p nil)
        (provider-supplied-p nil)
        (model-supplied-p nil)
        (remaining (copy-list args)))
    (loop :while remaining
          :for arg := (pop remaining)
          :do (cond
                ((string= arg "--")
                 (setf prompt-parts (append prompt-parts remaining)
                       remaining nil))
                ((or (string= arg "--help") (string= arg "-h"))
                 (setf (prompt-options-help-p options) t))
                ((string= arg "--agent")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-agent-name options) value
                         agent-supplied-p t
                         remaining rest)))
                ((string= arg "--provider")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-provider options) value
                         provider-supplied-p t
                         remaining rest)))
                ((string= arg "--model")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-model options) value
                         model-supplied-p t
                         remaining rest)))
                ((or (string= arg "--think")
                     (string= arg "--reasoning-effort"))
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-think-level options) value
                         remaining rest)))
                ((string= arg "--model-role")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-model-role options) value
                         remaining rest)))
                ((string= arg "--service-tier")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-service-tier options) value
                         remaining rest)))
                ((or (string= arg "--prompt") (string= arg "-p"))
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf prompt-parts (append prompt-parts (list value))
                         remaining rest)))
                ((or (string= arg "--show-tools")
                     (string= arg "--show-tool-calls"))
                 (setf (prompt-options-show-tools-p options) t))
                ((string= arg "--show-reasoning")
                 (setf (prompt-options-show-reasoning-p options) t))
                ((string= arg "--show-metadata")
                 (setf (prompt-options-show-metadata-p options) t))
                ((string= arg "--json")
                 (setf (prompt-options-json-p options) t))
                ((string= arg "--jsonl")
                 (setf (prompt-options-jsonl-p options) t))
                ((string= arg "--output-schema")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-output-schema options) value
                         remaining rest)))
                ((string= arg "--max-tool-iterations")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-max-tool-iterations options)
                         (parse-positive-integer-option arg value)
                         remaining rest)))
                ((string= arg "--skill-root")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-skill-roots options)
                         (append (prompt-options-skill-roots options)
                                 (list value))
                         remaining rest)))
                ((string= arg "--package")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-packages options)
                         (append (prompt-options-packages options)
                                 (list value))
                         remaining rest)))
                ((string= arg "--session")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-session-name options) value
                         remaining rest)))
                ((string= arg "--continue")
                 (setf (prompt-options-continue-session-p options) t))
                ((or (string= arg "--ephemeral")
                     (string= arg "--no-session"))
                 (setf (prompt-options-ephemeral-p options) t))
                ((string= arg "--pipeline")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-pipeline-name options) value
                         remaining rest)))
                ((string= arg "--debug-log")
                 (multiple-value-bind (value rest)
                     (require-option-value arg remaining)
                   (setf (prompt-options-debug-log-path options) value
                         remaining rest)))
                ((or (string= arg "--isolated")
                     (string= arg "--isolate"))
                 (setf (prompt-options-isolated-p options) t))
                ((or (string= arg "--clean-build")
                     (string= arg "--force-clean-build"))
                 nil)
                ((string= arg "--no-init")
                 (setf (prompt-options-inhibit-user-init-p options) t))
                ((and (plusp (length arg))
                      (char= #\- (char arg 0)))
                 (error "Unknown prompt option: ~A" arg))
                (t
                 (setf prompt-parts (append prompt-parts (cons arg remaining))
                       remaining nil))))
    (unless (prompt-options-help-p options)
      (unless (or agent-supplied-p provider-supplied-p)
        (setf (prompt-options-provider options) +prompt-default-provider+))
      (unless (or agent-supplied-p model-supplied-p provider-supplied-p)
        (setf (prompt-options-model options) +prompt-default-model+))
      (setf (prompt-options-prompt options)
            (finalize-prompt-option-text prompt-parts)))
    options))

(defun maybe-enable-prompt-debug-log (options)
  "Apply prompt-mode debug-log options and environment fallback."
  (let ((path (prompt-options-debug-log-path options)))
    (when path
      (setf *debug-log-file* (pathname path))))
  (unless *debug-log-file*
    (let ((env (uiop:getenv "RPLACA_DEBUG_LOG")))
      (when (and env (plusp (length env)))
        (setf *debug-log-file* (pathname env)))))
  (when *debug-log-file*
    (file-debug-log "startup" "prompt debug log enabled, writing to ~A"
                    *debug-log-file*)))

(defun prompt-isolation-root ()
  "Create and return a temporary root for isolated prompt execution."
  (let ((root (merge-pathnames
               (format nil "rplaca-prompt-isolated-~D-~D/"
                       (get-universal-time)
                       (get-internal-real-time))
               #P"/tmp/")))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    root))

(defun apply-prompt-isolation ()
  "Redirect prompt-mode mutable config paths into a temporary directory."
  (let* ((root (prompt-isolation-root))
         (config-dir (merge-pathnames #P".rplaca.d/" root)))
    (ensure-directories-exist (merge-pathnames #P".keep" config-dir))
    (setf *user-init-directory* config-dir
          *user-init-file* (merge-pathnames #P"init.lisp" config-dir)
          *project-definitions-directory*
          (merge-pathnames #P"projects.d/" root)
          *sessions-dir*
          (merge-pathnames #P"sessions/" root)
          *agent-defaults-path*
          (merge-pathnames #P"agent-defaults.json" root)
          *packages-directory*
          (merge-pathnames #P"packages/" root)
          *package-configuration-path*
          (merge-pathnames #P"packages.json" config-dir)
          *skill-user-directory*
          (merge-pathnames #P"skills/" config-dir)
          *skill-agents-directory*
          (merge-pathnames #P"agents-skills/" root)
          *skill-system-directory*
          (merge-pathnames #P"system-skills/" root)
          *skill-configuration-path*
          (merge-pathnames #P"skills.json" config-dir)
          *personality-prompt-path*
          (merge-pathnames #P"personality-prompt.txt" root))
    (when (boundp '*mcp-server-configuration-path*)
      (setf *mcp-server-configuration-path*
            (merge-pathnames #P"mcp-servers.json" config-dir)
            *mcp-server-registry* nil))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *project-definitions-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *sessions-dir*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *packages-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *skill-user-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *skill-agents-directory*))
    (ensure-directories-exist
     (merge-pathnames #P".keep" *skill-system-directory*))
    root))

(defparameter *prompt-workspace-project-name* "rplaca"
  "Project name used for the source tree mounted into prompt.sh runs.")

(defun prompt-workspace-project-root ()
  "Return the source root that prompt.sh should expose as a RPLACA project."
  (let ((root (uiop:getenv "RPLACA_PROMPT_PROJECT_ROOT")))
    (if (and root (plusp (length root)))
        (truename (uiop:ensure-directory-pathname root))
        (truename "."))))

(defun ensure-prompt-workspace-project ()
  "Expose the prompt workspace source tree as project \"rplaca\"."
  (define-project *prompt-workspace-project-name*
    :root (prompt-workspace-project-root)
    :description "RPLACA source tree mounted for prompt-mode analysis"
    :systems '(:rplaca :rplaca/tests)
    :source :builtin
    :replace nil))

(defun prompt-tool-event-json (event)
  "Return EVENT as a JSON-ready alist."
  `((:id . ,(prompt-tool-event-id event))
    (:name . ,(prompt-tool-event-name event))
    (:input . ,(prompt-tool-event-input event))
    (:result . ,(prompt-tool-event-result-text event))
    (:display . ,(prompt-tool-event-display event))
    (:denied . ,(prompt-tool-event-denied-p event))))

(defun prompt-run-result-json (result)
  "Return RESULT as a JSON-ready alist."
  `((:prompt . ,(prompt-run-result-prompt result))
    (:final--text . ,(prompt-run-result-final-text result))
    (:agent . ,(prompt-run-result-agent-name result))
    (:provider . ,(and (prompt-run-result-provider result)
                       (string-downcase
                        (symbol-name (prompt-run-result-provider result)))))
    (:model . ,(prompt-run-result-model result))
    (:reasoning--effort . ,(prompt-run-result-think-level result))
    (:service--tier . ,(prompt-run-result-service-tier result))
    (:iterations . ,(prompt-run-result-iterations result))
    (:stop--reason . ,(prompt-run-result-stop-reason result))
    (:session--name . ,(prompt-run-result-session-name result))
    (:session--id . ,(prompt-run-result-session-id result))
    ,@(when (prompt-run-result-usage result)
        `((:usage . ,(token-usage-json (prompt-run-result-usage result)))))
    ,@(when (prompt-run-result-structured-output result)
        `((:structured--output . ,(prompt-run-result-structured-output result))))
    (:tool--events . ,(coerce (mapcar #'prompt-tool-event-json
                                       (prompt-run-result-tool-events result))
                              'vector))
    (:reasoning . ,(coerce (prompt-run-result-reasoning-blocks result) 'vector))))

(defun write-string-with-final-newline (text stream)
  "Write TEXT to STREAM and ensure it is newline-terminated."
  (write-string (or text "") stream)
  (unless (and text
               (plusp (length text))
               (char= #\Newline (char text (1- (length text)))))
    (terpri stream)))

(defun write-prompt-metadata (result stream)
  "Write prompt metadata comments to STREAM."
  (format stream ";; agent: ~A~%" (prompt-run-result-agent-name result))
  (format stream ";; provider/model: ~(~A~)/~A~%"
          (prompt-run-result-provider result)
          (prompt-run-result-model result))
  (format stream ";; think: ~A~%"
          (or (prompt-run-result-think-level result) "default"))
  (format stream ";; service-tier: ~A~%"
          (or (prompt-run-result-service-tier result) "auto"))
  (format stream ";; iterations: ~D~%"
          (prompt-run-result-iterations result))
  (format stream ";; stop-reason: ~A~%"
          (or (prompt-run-result-stop-reason result) "nil"))
  (let ((usage-line (format-token-usage-summary
                     (prompt-run-result-usage result))))
    (when usage-line
      (format stream ";; ~A~%" usage-line))))

(defun write-prompt-tool-events (result stream)
  "Write prompt tool events to STREAM in Lisp-oriented display form."
  (loop :for event :in (prompt-run-result-tool-events result)
        :for index :from 1
        :do (format stream ";; tool ~D: ~A~%" index
                    (prompt-tool-event-name event))
            (format stream "~A~%"
                    (format-tool-call-sexpr
                     (prompt-tool-event-name event)
                     (prompt-tool-event-input event)))
            (format stream "~A~%~%" (prompt-tool-event-display event))))

(defun write-prompt-tool-event-list (events stream)
  "Write prompt tool EVENTS to STREAM."
  (loop :for event :in events
        :for index :from 1
        :do (format stream ";; partial tool ~D: ~A~%" index
                    (prompt-tool-event-name event))
            (format stream "~A~%"
                    (format-tool-call-sexpr
                     (prompt-tool-event-name event)
                     (prompt-tool-event-input event)))
            (format stream "~A~%~%" (prompt-tool-event-display event))))

(defun write-prompt-reasoning (result stream)
  "Write provider-supplied reasoning blocks to STREAM when present."
  (let ((blocks (prompt-run-result-reasoning-blocks result)))
    (if blocks
        (dolist (block blocks)
          (write-string-with-final-newline block stream))
        (format stream ";; no provider-supplied reasoning blocks captured~%"))))

(defun write-jsonl-record (record &optional (stream *standard-output*))
  "Write RECORD as one JSONL line to STREAM."
  (write-string-with-final-newline
   (api-json-encode (interop-json-ready record))
   stream))

(defun prompt-run-result-jsonl-record (result)
  "Return RESULT as a JSONL completion event."
  (cons '(:event . "turn.completed")
        (prompt-run-result-json result)))

(defun prompt-run-error-jsonl-record (condition)
  "Return CONDITION as a JSONL failure event."
  `((:event . "turn.failed")
    (:error . ,(prompt-run-error-message condition))
    (:iterations . ,(prompt-run-error-iterations condition))
    (:provider . ,(and (prompt-run-error-provider condition)
                       (string-downcase
                        (symbol-name
                         (prompt-run-error-provider condition)))))
    (:model . ,(prompt-run-error-model condition))
    (:reasoning--effort . ,(prompt-run-error-think-level condition))
    (:tool--events . ,(coerce (mapcar #'prompt-tool-event-json
                                      (prompt-run-error-tool-events condition))
                              'vector))))

(defun write-prompt-run-result (result options)
  "Write RESULT according to OPTIONS."
  (cond
    ((prompt-options-jsonl-p options)
     (write-jsonl-record (prompt-run-result-jsonl-record result)))
    ((prompt-options-json-p options)
     (write-string-with-final-newline
      (api-json-encode (prompt-run-result-json result))
      *standard-output*))
    (t
     (when (prompt-options-show-metadata-p options)
       (write-prompt-metadata result *error-output*))
     (when (prompt-options-show-tools-p options)
       (write-prompt-tool-events result *error-output*))
     (when (prompt-options-show-reasoning-p options)
       (write-prompt-reasoning result *error-output*))
     (write-string-with-final-newline
      (prompt-run-result-final-text result)
      *standard-output*))))

(defun run-prompt-options (options &key event-callback)
  "Run parsed prompt OPTIONS and return a PROMPT-RUN-RESULT."
  (let* ((ephemeral-p (prompt-options-ephemeral-p options))
         (session-name
           (unless ephemeral-p
             (or (prompt-options-session-name options)
                 (and (prompt-options-continue-session-p options)
                      (or (most-recent-saved-session-name
                           :working-directory (truename "."))
                          (error "No saved session exists for the current working directory."))))))
         (*default-buffer-session-persistence-mode*
           (if ephemeral-p :ephemeral :persistent)))
    (if (prompt-options-pipeline-name options)
        (run-pipeline-prompt
         (prompt-options-prompt options)
         (prompt-options-pipeline-name options)
         :session-name session-name
         :session-persistence-mode
         (if ephemeral-p :ephemeral :persistent)
         :agent-name (prompt-options-agent-name options)
         :provider (prompt-options-provider options)
         :model (prompt-options-model options)
         :think-level (prompt-options-think-level options)
         :model-role (prompt-options-model-role options)
         :service-tier (prompt-options-service-tier options)
         :output-schema (prompt-options-output-schema options)
         :max-tool-iterations
         (prompt-options-max-tool-iterations options)
         :package-names
         (prompt-options-packages options)
         :event-callback event-callback)
        (if session-name
            (run-session-prompt
             (prompt-options-prompt options)
             :session-name session-name
             :agent-name (prompt-options-agent-name options)
             :provider (prompt-options-provider options)
             :model (prompt-options-model options)
             :think-level (prompt-options-think-level options)
             :model-role (prompt-options-model-role options)
             :service-tier (prompt-options-service-tier options)
             :output-schema (prompt-options-output-schema options)
             :max-tool-iterations
             (prompt-options-max-tool-iterations options)
             :package-names
             (prompt-options-packages options)
             :event-callback event-callback)
            (run-single-prompt
             (prompt-options-prompt options)
             :session-persistence-mode
             (if ephemeral-p :ephemeral :persistent)
             :agent-name (prompt-options-agent-name options)
             :provider (prompt-options-provider options)
             :model (prompt-options-model options)
             :think-level (prompt-options-think-level options)
             :model-role (prompt-options-model-role options)
             :service-tier (prompt-options-service-tier options)
             :output-schema (prompt-options-output-schema options)
             :max-tool-iterations
             (prompt-options-max-tool-iterations options)
             :package-names
             (prompt-options-packages options)
             :event-callback event-callback)))))

(defun rplaca-prompt-main* (&key default-session-name usage-string-function)
  "Shared CLI entry point for one-shot and saved-session prompt modes."
  (let ((options nil))
    (handler-case
      (progn
        (setf options (parse-rplaca-prompt-args))
        (when (and default-session-name
                   (not (prompt-options-ephemeral-p options))
                   (not (prompt-options-session-name options))
                   (not (prompt-options-continue-session-p options)))
          (setf (prompt-options-session-name options) default-session-name))
        (when (prompt-options-help-p options)
          (write-string-with-final-newline (funcall usage-string-function)
                                           *standard-output*)
          (uiop:quit 0))
        (unless (prompt-options-prompt options)
          (error "No prompt supplied.~%~A" (funcall usage-string-function)))
        (maybe-enable-prompt-debug-log options)
        (when (prompt-options-isolated-p options)
          (let ((root (apply-prompt-isolation)))
            (when (prompt-options-show-metadata-p options)
              (format *error-output* ";; isolated-root: ~A~%" root))))
        (dolist (skill-root (prompt-options-skill-roots options))
          (register-skill-root skill-root :scope :user :source :cli))
        (let ((*inhibit-user-init* (or (prompt-options-isolated-p options)
                                       (prompt-options-inhibit-user-init-p
                                        options))))
          (initialize-rplaca-runtime)
          (reset-interaction-state)
          (ensure-prompt-workspace-project)
          (let* ((jsonl-lock (and (prompt-options-jsonl-p options)
                                  (bt:make-lock "prompt-jsonl-output")))
                 (event-callback
                   (and (prompt-options-jsonl-p options)
                        (lambda (event)
                          (bt:with-lock-held (jsonl-lock)
                            (write-jsonl-record event))))))
            (let ((result (run-prompt-options
                           options
                           :event-callback event-callback)))
              (write-prompt-run-result result options))))
        (uiop:quit 0))
      (prompt-run-error (e)
        (when (and options (prompt-options-jsonl-p options))
          (write-jsonl-record (prompt-run-error-jsonl-record e)))
        (format *error-output* "~&rplaca prompt error: ~A~%" e)
        (when options
          (when (prompt-options-show-metadata-p options)
            (format *error-output* ";; partial iterations: ~D~%"
                    (prompt-run-error-iterations e))
            (format *error-output* ";; partial provider/model: ~(~A~)/~A~%"
                    (or (prompt-run-error-provider e) :unknown)
                    (or (prompt-run-error-model e) "unknown"))
            (format *error-output* ";; partial think: ~A~%"
                    (or (prompt-run-error-think-level e) "default")))
          (when (prompt-run-error-tool-events e)
            (format *error-output* ";; partial tool trace follows~%")
            (write-prompt-tool-event-list
             (prompt-run-error-tool-events e)
             *error-output*)))
        (uiop:quit 1))
    (error (e)
      (format *error-output* "~&rplaca prompt error: ~A~%" e)
      (uiop:quit 1)))))

(defun rplaca-prompt-main ()
  "CLI entry point for one-shot prompt execution.
This function exits the Lisp image with status 0 on success and 1 on errors."
  (rplaca-prompt-main*
   :usage-string-function #'prompt-usage-string))

(defun rplaca-session-prompt-main ()
  "CLI entry point for saved-session prompt execution."
  (rplaca-prompt-main*
   :default-session-name (default-session-prompt-session-name)
   :usage-string-function #'session-prompt-usage-string))

(defun rplaca-main (&key (session-name "rplaca:session-01")
                           (agent-name *default-agent-name*)
                           (window-title "RPLACA")
                           (working-directory (truename "."))
                           (run-frame t))
  "Entry point for rplaca. Initializes state and starts the McCLIM frame."
  (call-with-installed-crash-reporter
   (lambda ()
     (publish-crash-report-runtime-snapshot :phase :startup)
     (parse-rplaca-args)
     (initialize-rplaca-runtime)
     (publish-crash-report-runtime-snapshot :phase :initialized)
     ;; Create initial buffer and initialize global state
     (reset-interaction-state)
     (let ((buf (make-initial-chat-buffer
                 session-name agent-name
                 :working-directory working-directory)))
       (publish-crash-report-runtime-snapshot :phase :buffer-ready)
       (ensure-scratch-buffer)
       (when run-frame
         ;; Appearance data is intentionally read and resolved only here:
         ;; package registration and init.lisp have completed, while prompt
         ;; entry points never reach this GUI-only boundary.
         (let ((appearance-profile (resolve-startup-appearance-profile)))
           (funcall (symbol-function 'run-rplaca-listener)
                    buf
                    :pending-session-name session-name
                    :window-title window-title
                    :appearance-profile appearance-profile)))
       (publish-crash-report-runtime-snapshot :phase :stopped)
       buf))))
