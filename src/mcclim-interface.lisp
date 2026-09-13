(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Fresh McCLIM Chat Interface
;;; --------------------------------------------------------------------------

(defparameter *rplaca-default-text-style*
  (clim:make-text-style :fix :roman :normal)
  "Portable fixed-width text style inherited by RPLACA's application panes.")

(clim:define-presentation-type chat-message ()
  :inherit-from 'message)

(defparameter +agent-lisp-button-metadata-key+ :agent-lisp-button)

(defun agent-lisp-button-spec (message)
  "Return MESSAGE's persisted agent-created Lisp button specification."
  (let ((metadata (message-metadata message)))
    (and metadata
         (message-metadata-value metadata
                                 +agent-lisp-button-metadata-key+))))

(defun agent-lisp-button-value (spec key)
  "Return KEY from persisted button SPEC without assuming keyword identity."
  (or (cdr (assoc key spec :test #'eq))
      (cdr (assoc (string-downcase (symbol-name key)) spec
                  :test (lambda (name candidate)
                          (string= name (string-downcase (string candidate))))))))

(defun execute-agent-lisp-button (frame spec)
  "Run arbitrary Lisp retained by SPEC after an explicit user activation."
  (let* ((buffer (chat-frame-buffer frame))
         (label (or (agent-lisp-button-value spec :label) "Lisp action"))
         (code (agent-lisp-button-value spec :code))
         (package (or (agent-lisp-button-value spec :package) "RPLACA"))
         (result
           (let ((*current-caller* :user)
                 (*current-tool-buffer* buffer))
             (execute-tool-safely
              "lisp_eval"
              `((:code . ,code)
                (:package . ,package)
                (:mode . "live"))
              :buffer buffer))))
    (buffer-insert-system-message
     buffer
     (format nil "[Button ~A returned]~%~A" label result))
    (request-chat-frame-redisplay frame)
    result))

(defun display-agent-lisp-button (frame stream spec)
  "Render SPEC as an ordinary CLIM push-button gadget in STREAM."
  (let ((label (or (agent-lisp-button-value spec :label) "Run Lisp"))
        (frame-manager (clim:frame-manager frame)))
    (clim:with-look-and-feel-realization (frame-manager frame)
      (clim:with-output-as-gadget (stream)
        (clim:make-pane
         'clim:push-button
         :label label
         :client frame
         :id spec
         :activate-callback
         (lambda (gadget)
           (execute-agent-lisp-button
            (clim:gadget-client gadget)
            (clim:gadget-id gadget))))))))

(clim:define-presentation-method clim:presentation-typep
    (object (type chat-message))
  (typep object 'message))

(clim:define-gesture-name
    :describe-presentation :pointer-button-press (:left :control :shift))

(clim:define-presentation-type tool-activity-summary ())
(clim:define-presentation-type chat-interaction-candidate ())
(clim:define-presentation-type package-dashboard-entry-ref ())
(clim:define-presentation-type appearance-theme-ref ())
(clim:define-presentation-type appearance-role-ref ())
(clim:define-presentation-type appearance-port-font-family-ref ())
(clim:define-presentation-type appearance-port-font-face-ref ())
(clim:define-presentation-type appearance-port-font-size-ref ())
(clim:define-presentation-type appearance-activation-ref ())
(clim:define-presentation-type appearance-diagnostic-ref ())

(defstruct (appearance-editor-ref
             (:constructor make-appearance-editor-ref (frame kind value)))
  "Semantic identity for one frame-local appearance editor value."
  frame
  kind
  value)

(defun appearance-editor-ref-kind-p (object kind)
  "Return true when OBJECT is an appearance editor reference of KIND."
  (and (typep object 'appearance-editor-ref)
       (eq kind (appearance-editor-ref-kind object))))

(macrolet ((define-appearance-ref-typep (type kind)
             `(clim:define-presentation-method clim:presentation-typep
                  (object (type ,type))
                (appearance-editor-ref-kind-p object ,kind))))
  (define-appearance-ref-typep appearance-theme-ref :theme)
  (define-appearance-ref-typep appearance-role-ref :role)
  (define-appearance-ref-typep appearance-port-font-family-ref :font-family)
  (define-appearance-ref-typep appearance-port-font-face-ref :font-face)
  (define-appearance-ref-typep appearance-port-font-size-ref :font-size)
  (define-appearance-ref-typep appearance-activation-ref :activation)
  (define-appearance-ref-typep appearance-diagnostic-ref :diagnostic))

(defun accept-appearance-editor-suggestions (stream kind values)
  "Accept one semantic appearance value from VALUES using CLIM completion."
  (let ((frame clim:*application-frame*))
    (values
     (clim:completing-from-suggestions (stream)
       (dolist (value values)
         (clim:suggest (if (member kind '(:theme :role) :test #'eq)
                           (appearance-editor-id-string value)
                           (format nil "~A" value))
                       (make-appearance-editor-ref frame kind value)))))))

(clim:define-presentation-method clim:accept
    ((type appearance-theme-ref) stream (view clim:textual-view) &key)
  (accept-appearance-editor-suggestions
   stream :theme
   (appearance-editor-theme-ids clim:*application-frame*)))

(clim:define-presentation-method clim:accept
    ((type appearance-role-ref) stream (view clim:textual-view) &key)
  (accept-appearance-editor-suggestions
   stream :role
   (appearance-editor-role-ids clim:*application-frame*)))

(clim:define-presentation-method clim:accept
    ((type appearance-port-font-family-ref) stream
     (view clim:textual-view) &key)
  (accept-appearance-editor-suggestions
   stream :font-family
   (appearance-editor-font-families clim:*application-frame*)))

(clim:define-presentation-method clim:accept
    ((type appearance-port-font-face-ref) stream
     (view clim:textual-view) &key)
  (let ((frame clim:*application-frame*))
    (accept-appearance-editor-suggestions
     stream :font-face
     (appearance-editor-font-faces
      frame (chat-frame-appearance-editor-font-family frame)))))

(clim:define-presentation-method clim:accept
    ((type appearance-port-font-size-ref) stream
     (view clim:textual-view) &key)
  (let ((frame clim:*application-frame*))
    (accept-appearance-editor-suggestions
     stream :font-size
     (appearance-editor-font-sizes
      frame
      (chat-frame-appearance-editor-font-family frame)
      (chat-frame-appearance-editor-font-face frame)))))

(clim:define-presentation-method clim:accept
    ((type appearance-activation-ref) stream
     (view clim:textual-view) &key)
  (accept-appearance-editor-suggestions
   stream :activation '(:apply :save :revert :reload :refresh-fonts)))

(clim:define-presentation-method clim:accept
    ((type appearance-diagnostic-ref) stream
     (view clim:textual-view) &key)
  (let ((frame clim:*application-frame*))
    (accept-appearance-editor-suggestions
     stream :diagnostic
     (append (chat-frame-appearance-activation-diagnostics frame)
             (chat-frame-appearance-font-refresh-diagnostics frame)))))

(defstruct (chat-interaction-candidate-ref
             (:constructor make-chat-interaction-candidate-ref
                 (state generation kind index item)))
  "Exact semantic identity for one visibly rendered interaction candidate."
  state
  generation
  kind
  index
  item)

(defparameter *buffer-presentation-default-columns* 100
  "Fallback display width passed to buffer presentation hooks.")

(defvar *buffer-input-presentation-text* nil
  "Compose text dynamically visible while rendering input presentation hooks.")

(defun buffer-input-presentation-text (buffer)
  "Return current compose text for BUFFER while rendering an input presentation."
  (or *buffer-input-presentation-text*
      (and buffer (message-text (buffer-input-message buffer)))
      ""))

(clim:define-presentation-method clim:presentation-typep
    (object (type chat-interaction-candidate))
  (typep object 'chat-interaction-candidate-ref))

(clim:define-presentation-method clim:presentation-typep
    (object (type package-dashboard-entry-ref))
  (and (listp object)
       (getf object :dashboard-buffer)
       (getf object :entry)))

(defclass rplaca-chat-redisplay-event (clim:window-manager-event)
  ())

(defclass rplaca-chat-appearance-activation-event (clim:window-manager-event)
  ((candidate :initarg :candidate :reader chat-appearance-activation-event-candidate))
  (:documentation
   "One immutable appearance request delivered to the owning CLIM frame process."))

(defclass rplaca-chat-appearance-catalog-event (clim:window-manager-event)
  ((reservation :initarg :reservation
                :reader chat-appearance-catalog-event-reservation)
   (token :initarg :token :reader chat-appearance-catalog-event-token))
  (:documentation "An admitted package appearance catalog transition."))

(defclass rplaca-chat-font-inventory-refresh-event (clim:window-manager-event)
  ()
  (:documentation
   "One explicit port-font inventory refresh delivered to the owning frame process."))

(defvar *suppress-chat-redisplay-requests* nil
  "When non-nil, buffer display hooks should not queue chat redisplay events.
This is bound while a chat frame is already applying provider stream state to
avoid recursive update→notify→redisplay loops in the CLIM event thread.")

(defvar *package-appearance-live-chat-frames* nil)
(defvar *package-appearance-live-chat-frames-lock*
  (bt:make-lock "package appearance live chat frames"))
(defvar *package-appearance-frame-batch-lock*
  (bt:make-lock "package appearance frame batch"))
(defvar *active-package-appearance-frame-batch* nil)
(defvar *package-appearance-user-edit-admitted-p* nil)
(defparameter *appearance-package-transition-timeout-seconds* 5)
(defvar *appearance-package-prepared-frame-resume-hook* (constantly nil)
  "Test/diagnostic seam run outside the barrier lock before target install.")

(defun call-with-package-appearance-user-edit (continuation)
  "Run CONTINUATION only when no package appearance batch owns admission.

The acquisition is deliberately nonblocking: a frame event loop reports busy
instead of parking behind Safe Reload and deadlocking a frame barrier."
  (if *package-appearance-user-edit-admitted-p*
      (funcall continuation)
      (progn
        (unless (bt:acquire-lock *package-appearance-frame-batch-lock* nil)
          (error "Appearance editing is busy during package reconciliation."))
        (unwind-protect
             (let ((*package-appearance-user-edit-admitted-p* t))
               (funcall continuation))
          (bt:release-lock *package-appearance-frame-batch-lock*)))))

(defun package-appearance-live-chat-frames ()
  (bt:with-lock-held (*package-appearance-live-chat-frames-lock*)
    (copy-list *package-appearance-live-chat-frames*)))

(defun register-package-appearance-live-chat-frame (frame)
  ;; Catalog publication already takes the catalog lock before enumerating the
  ;; live-frame registry.  Registration takes the same locks in the same order,
  ;; so a frame is either absent from the complete transaction or joins with
  ;; its exact committed catalog after that transaction finishes.
  (bt:with-lock-held (*package-appearance-frame-batch-lock*)
    (bt:with-lock-held (rplaca::*package-appearance-catalog-lock*)
      (bt:with-lock-held (*package-appearance-live-chat-frames-lock*)
        (setf (slot-value frame 'appearance-catalog)
              (rplaca::package-appearance-current-catalog-under-lock))
        (pushnew frame *package-appearance-live-chat-frames* :test #'eq))))
  frame)

(defun unregister-package-appearance-live-chat-frame (frame)
  (bt:with-lock-held (*package-appearance-frame-batch-lock*)
    (bt:with-lock-held (rplaca::*package-appearance-catalog-lock*)
      (bt:with-lock-held (*package-appearance-live-chat-frames-lock*)
        (setf *package-appearance-live-chat-frames*
              (remove frame *package-appearance-live-chat-frames* :test #'eq)))))
  frame)

(defparameter *chat-transcript-follow-tail* t
  "When non-nil, the chat transcript scrolls to the bottom after redisplay.")

(clim:define-command-table rplaca-chat-control-menu
  :menu (("Stop Response" :command com-chat-stop-response
          :documentation "Stop the active streaming response.")))

(clim:define-command-table rplaca-chat-view-menu
  :menu (("Toggle Tool Results"
          :command com-chat-toggle-tool-results
          :documentation "Toggle tool result messages.")
         ("Toggle Reasoning Output"
          :command com-chat-toggle-reasoning-output
          :documentation "Toggle provider reasoning blocks.")
         ("Toggle Metadata Output"
          :command com-chat-toggle-metadata-output
          :documentation "Toggle provider response metadata.")
         ("Toggle Debug Mode"
          :command com-chat-toggle-debug-mode
          :documentation "Toggle API debug messages.")))

(clim:define-command-table rplaca-chat-skills-menu
  :menu (("Toggle Skill..."
          :command com-chat-open-skill-selector
          :documentation
          "Enable or disable a skill through the presentation-based minibuffer.")))

(clim:define-command-table rplaca-chat-packages-menu
  :menu (("Open Package Dashboard..."
          :command com-chat-open-package-dashboard
          :documentation
          "Open the presentation-based package dashboard for this chat.")))

(clim:define-command-table rplaca-chat-effort-menu
  :menu (("Select Think Level..."
          :command com-chat-open-effort-selector
          :documentation
          "Choose reasoning effort through the presentation-based minibuffer.")))

(clim:define-command-table rplaca-chat-system-menu
  :menu (("Safe Reload"
          :command com-chat-safe-reload
          :documentation "Safely reload updated RPLACA source in place.")
         ("Recurse"
          :command com-chat-recurse
          :documentation "Open a fresh nested RPLACA frame in a new process.")))

(clim:define-command-table rplaca-chat-appearance-menu
  :menu (("Customize Appearance..."
          :command com-chat-customize-appearance
          :documentation "Open the frame-local staged appearance editor.")
         ("Switch Appearance Theme..."
          :command com-chat-switch-appearance-theme
          :documentation "Stage a theme selected through CLIM completion.")
         ("Describe Current Appearance"
          :command com-chat-describe-current-appearance
          :documentation "Describe active, staged, and persisted appearance state.")
         ("Apply Staged Appearance"
          :command com-chat-apply-staged-appearance
          :documentation "Queue the staged candidate for transactional activation.")
         ("Save Appearance"
          :command com-chat-save-appearance
          :documentation "Atomically persist the staged candidate.")
         ("Revert Staged Appearance"
          :command com-chat-revert-staged-appearance
          :documentation "Discard the staged candidate.")
         ("Reload Appearance File"
          :command com-chat-reload-appearance-file
          :documentation "Reload the appearance file without changing active state.")
         ("Refresh Font Inventory"
          :command com-chat-refresh-font-inventory
          :documentation "Queue a frame-local port font inventory refresh.")))

(clim:define-command-table rplaca-chat-menu-bar
  :menu (("Stop"
          :command com-chat-stop-response
          :documentation "Stop the active streaming response.")
         ("Buffers..."
          :command com-chat-open-buffer-selector
          :documentation "Switch between open buffers.")
         ("Model..."
          :command com-chat-open-model-selector
          :documentation "Select the provider model for this buffer.")
         ("Effort..."
          :command com-chat-open-effort-selector
          :documentation "Select the reasoning effort for this buffer.")
         ("Skills..."
          :command com-chat-open-skill-selector
          :documentation "Enable or disable a skill.")
         ("Packages..."
          :command com-chat-open-package-dashboard
          :documentation "Open the package dashboard.")
         ("Appearance..."
          :command com-chat-customize-appearance
          :documentation "Open the staged appearance editor.")
         ("Help"
          :command com-chat-open-manual
          :documentation "Open the RPLACA manual.")))

(defun chat-message-kind (msg)
  "Return MSG's high-level display kind."
  (case (message-sender msg)
    (:user :user)
    (:system :system)
    (:tool-result :tool)
    (t :agent)))

(defun chat-message-ink (msg)
  "Return the CLIM ink used for MSG."
  (ecase (chat-message-kind msg)
    (:user (clim:make-rgb-color 0.10 0.25 0.55))
    (:agent (clim:make-rgb-color 0.10 0.10 0.10))
    (:tool (clim:make-rgb-color 0.12 0.34 0.18))
    (:system (clim:make-rgb-color 0.36 0.36 0.36))))

(defun chat-message-label (msg)
  "Return the display label for MSG."
  (ecase (chat-message-kind msg)
    (:user "user")
    (:agent "agent")
    (:tool "tool")
    (:system "system")))

(defstruct (chat-tool-activity-summary
            (:constructor make-chat-tool-activity-summary
                (&key messages tool-counts result-count first-id last-id)))
  "Display-only summary for one consecutive run of tool calls/results."
  (messages nil :type list)
  (tool-counts nil :type list)
  (result-count 0 :type integer)
  first-id
  last-id)

(defun chat-message-tool-use-blocks (msg)
  "Return tool_use blocks recorded in MSG's raw content."
  (content-tool-use-blocks (or (message-raw-content msg) nil)))

(defun chat-message-tool-result-blocks (msg)
  "Return tool_result blocks recorded in MSG's raw content."
  (remove-if-not (lambda (block)
                   (string= "tool_result" (content-block-type block)))
                 (or (message-raw-content msg) nil)))

(defun chat-tool-activity-message-p (msg)
  "Return true when MSG is a tool call/result display message."
  (or (chat-message-tool-use-blocks msg)
      (eq (message-sender msg) :tool-result)
      (chat-message-tool-result-blocks msg)))

(defun chat-increment-tool-count (name counts)
  "Return COUNTS with NAME incremented once, preserving first-seen order."
  (let ((cell (assoc name counts :test #'string=)))
    (if cell
        (progn
          (incf (cdr cell))
          counts)
        (append counts (list (cons name 1))))))

(defun chat-tool-activity-summary-from-run (messages)
  "Return a collapsed tool activity summary for consecutive MESSAGES."
  (let ((counts nil)
        (result-count 0))
    (dolist (msg messages)
      (dolist (tool-use (chat-message-tool-use-blocks msg))
        (let ((name (or (cdr (assoc :name tool-use)) "unknown")))
          (setf counts (chat-increment-tool-count name counts))))
      (incf result-count (length (chat-message-tool-result-blocks msg))))
    (make-chat-tool-activity-summary
     :messages messages
     :tool-counts counts
     :result-count result-count
     :first-id (chat-message-output-id (first messages))
     :last-id (chat-message-output-id (car (last messages))))))

(defun chat-transcript-messages (buf)
  "Return finalized, non-ephemeral messages for BUF."
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :unless (buffer-ephemeral-display-message-p msg)
          :collect msg))

(defun chat-transcript-display-items (buf)
  "Return transcript display items, collapsing consecutive tool activity by default."
  (let ((messages (chat-transcript-messages buf)))
    (if (not (buffer-collapse-tool-activity-p buf))
        messages
        (let ((items nil)
              (tool-run nil))
          (labels ((flush-tool-run ()
                     (when tool-run
                       (push (chat-tool-activity-summary-from-run
                              (nreverse tool-run))
                             items)
                       (setf tool-run nil))))
            (dolist (msg messages)
              (cond
                ((chat-tool-activity-message-p msg)
                 (unless (and (eq (message-sender msg) :tool-result)
                              (not (buffer-show-tool-results-p buf)))
                   (push msg tool-run)))
                (t
                 (flush-tool-run)
                 (push msg items))))
            (flush-tool-run)
            (nreverse items))))))

(defun chat-message-output-id (msg)
  "Return a stable incremental-redisplay id for MSG."
  (or (message-entry-id msg) msg))

(defun chat-tool-activity-summary-output-id (summary)
  "Return a stable incremental-redisplay id for SUMMARY."
  (list :tool-activity-summary
        (chat-tool-activity-summary-first-id summary)
        (chat-tool-activity-summary-last-id summary)))

(defun chat-display-item-output-id (item)
  "Return a stable incremental-redisplay id for ITEM."
  (if (chat-tool-activity-summary-p item)
      (chat-tool-activity-summary-output-id item)
      (chat-message-output-id item)))

(defun chat-message-cache-value (msg)
  "Return a cache value covering visible MSG state."
  (list (message-sender msg)
        (message-timestamp msg)
        (message-text msg)
        (message-metadata msg)
        (message-entry-id msg)
        (message-parent-entry-id msg)))

(defun chat-tool-activity-summary-cache-value (summary)
  "Return a cache value covering visible SUMMARY state."
  (list (chat-tool-activity-summary-tool-counts summary)
        (chat-tool-activity-summary-result-count summary)
        (mapcar #'chat-message-cache-value
                (chat-tool-activity-summary-messages summary))))

(defun chat-display-item-cache-value (item)
  "Return a cache value covering visible ITEM state."
  (if (chat-tool-activity-summary-p item)
      (chat-tool-activity-summary-cache-value item)
      (chat-message-cache-value item)))

(defun message-metadata-help-string (msg)
  "Return help-window text describing MSG metadata."
  (with-output-to-string (stream)
    (format stream "Message~2%")
    (format stream "Sender: ~A~%" (message-sender msg))
    (format stream "Timestamp: ~A~%" (or (message-timestamp msg) "none"))
    (format stream "Entry id: ~A~%" (or (message-entry-id msg) "none"))
    (format stream "Parent entry id: ~A~%"
            (or (message-parent-entry-id msg) "none"))
    (format stream "Read only: ~:[no~;yes~]~%" (message-read-only-p msg))
    (format stream "Text length: ~D~%" (length (message-text msg)))
    (format stream "Line count: ~D~%" (message-line-count msg))
    (format stream "Raw content blocks: ~D~%"
            (length (or (message-raw-content msg) nil)))
    (format stream "~%Metadata:~%~S~%" (message-metadata msg))))

(defun handle-chat-compose-text (buf text)
  "Submit compose TEXT for BUF. Return true when TEXT was consumed."
  (cond
    ((buffer-user-input-pending buf)
     (set-message-text (buffer-input-message buf) text)
     (send-message buf)
     t)
    ((blank-string-p text)
     nil)
    (t
     (set-message-text (buffer-input-message buf) text)
     (send-message buf)
     t)))

(clim:define-application-frame rplaca-message-help-frame ()
  ((message :initarg :message
            :reader help-frame-message))
  (:panes
   (help :application
         :display-function 'display-message-help
         :display-time :command-loop
         :text-style *rplaca-default-text-style*
         :width 520
         :height 360))
  (:layouts
   (default help)))

(clim:define-application-frame rplaca-agent-window-frame ()
  ((content :initarg :content :reader agent-window-content))
  (:panes
   (content :application
            :display-function 'display-agent-window
            :display-time :command-loop
            :text-style *rplaca-default-text-style*
            :width 640
            :height 480))
  (:layouts
   (default content)))

(defun display-agent-window (frame stream)
  "Display the agent-supplied content owned by FRAME."
  (write-string (agent-window-content frame) stream))

(defun make-agent-window-worker-thread (function)
  "Create the worker that owns one independent agent-created frame."
  (bt:make-thread function :name "rplaca agent window"))

(defun make-agent-window-frame (title content)
  "Construct one independent application frame with TITLE and CONTENT."
  (clim:make-application-frame
   'rplaca-agent-window-frame
   :content content
   :pretty-name title))

(defun open-agent-window (title content)
  "Open a one-pane application frame and return it, or NIL on failure."
  (let ((reservation
          (handler-case
              (reserve-message-help-runtime)
            (error (condition)
              (file-debug-event
               "agent-window-admission-error"
               :condition (format nil "~A" condition))
              (return-from open-agent-window nil))))
        (frame nil))
    (setf frame
          (handler-case
              (make-agent-window-frame title content)
            (error (condition)
              (release-message-help-runtime reservation)
              (file-debug-event
               "agent-window-frame-construction-error"
               :condition (format nil "~A" condition))
              (return-from open-agent-window nil))))
    (handler-case
        (let ((worker-reservation reservation))
          (make-agent-window-worker-thread
           (lambda ()
             (unwind-protect
                  (handler-case
                      (clim:run-frame-top-level frame)
                    (error (condition)
                      (file-debug-event
                       "agent-window-frame-error"
                       :condition (format nil "~A" condition))))
               (release-message-help-runtime worker-reservation))))
          frame)
      (error (condition)
        (release-message-help-runtime reservation)
        (file-debug-event
         "agent-window-thread-start-error"
         :condition (format nil "~A" condition))
        nil))))

(defun display-message-help (frame stream)
  "Display FRAME's message metadata in STREAM."
  (write-string
   (message-metadata-help-string (help-frame-message frame))
   stream))

(defun make-message-help-worker-thread (function)
  "Create the worker that owns one independent message-help frame."
  (bt:make-thread function :name "rplaca message metadata"))

(defun make-message-help-frame (msg)
  "Construct one disowned message-help application frame for MSG."
  (clim:make-application-frame
   'rplaca-message-help-frame
   :message msg
   :pretty-name "Message Metadata"))

(defun open-message-help-window (msg)
  "Open a one-pane help frame for MSG metadata."
  (let ((reservation
          (handler-case
              (reserve-message-help-runtime)
            (runtime-admission-closed (condition)
              (file-debug-event
               "message-help-admission-refused"
               :condition (format nil "~A" condition))
              (return-from open-message-help-window nil))
            (error (condition)
              (file-debug-event
               "message-help-admission-error"
               :condition (format nil "~A" condition))
              (return-from open-message-help-window nil))))
        (frame nil))
    (setf frame
          (handler-case
              (make-message-help-frame msg)
            (error (condition)
              ;; Frame construction allocates panes and backend-independent
              ;; CLIM state at a user command boundary.  Resource errors must
              ;; not unwind the owning ESA frame.
              (release-message-help-runtime reservation)
              (file-debug-event
               "message-help-frame-construction-error"
               :condition (format nil "~A" condition))
              (return-from open-message-help-window nil))))
    (handler-case
        (let ((worker-reservation reservation))
          (make-message-help-worker-thread
           (lambda ()
             (unwind-protect
                  ;; An independent help frame must not drop its failure into
                  ;; SBCL's debugger or destabilize the main application frame.
                  (handler-case
                      (clim:run-frame-top-level frame)
                    (error (condition)
                      (file-debug-event
                       "message-help-frame-error"
                       :condition (format nil "~A" condition))))
               (release-message-help-runtime worker-reservation))))
          frame)
      (error (condition)
        ;; Thread resource exhaustion occurs at the user command boundary.
        ;; Contain it here so ESA's top level is never unwound.
        (release-message-help-runtime reservation)
        (file-debug-event
         "message-help-thread-start-error"
         :condition (format nil "~A" condition))
        nil))))

(clim:define-command-table rplaca-chat-compose-editing-table)

(defun chat-compose-drei-command-symbol (name)
  "Return Drei's built-in command named NAME.

McCLIM does not export its concrete editing-command symbols.  Keep that one
compatibility boundary here; command-table creation and pane integration use
the public ESA and DREI-SYNTAX protocols."
  (or (find-symbol name :drei-commands)
      (error "This McCLIM does not provide the Drei command ~A." name)))

(defun install-chat-compose-drei-keybindings ()
  "Install RPLACA compose bindings in its application-owned command table.

  Drei already binds the usual C-j representation, but some backends deliver it
  as Control-Newline.  The compose pane also preserves the RPLACA/Emacs-style
  C-w and C-Backspace editing bindings.  Do not modify Drei's process-global
  INDENT-TABLE or DELETION-TABLE: `additional-command-tables' scopes these
  bindings to `rplaca-chat-compose-pane'."
  (esa:set-key (chat-compose-drei-command-symbol
                "COM-NEWLINE-AND-INDENT")
               'rplaca-chat-compose-editing-table
               '((#\Newline :control)))
  (let ((backward-kill-word
          (chat-compose-drei-command-symbol "COM-BACKWARD-KILL-WORD")))
    (dolist (gesture '((#\w :control)
                       (#\Backspace :control)
                       (#\Rubout :control)))
      (esa:set-key (list backward-kill-word clim:*numeric-argument-marker*)
                   'rplaca-chat-compose-editing-table
                   (list gesture)))))

(install-chat-compose-drei-keybindings)

(defparameter *chat-compose-visible-rows* 5
  "Desired visible compose rows in the chat frame.")

(defparameter *chat-compose-line-height* 24
  "Approximate pixel height of one chat compose row.")

(defun chat-compose-desired-pixel-height ()
  "Return the preferred fixed pixel height for the chat compose pane."
  (* *chat-compose-visible-rows* *chat-compose-line-height*))

(defun configure-chat-compose-pane (pane)
  "Apply the compose pane's non-geometric compatibility configuration.

New chat panes declare this policy with their construction initargs.  Retain
the public helper for headless callers that construct a compose pane directly;
it never changes space requirements or requests layout."
  (when (and pane
             (compute-applicable-methods
              (fdefinition '(setf clim:stream-end-of-line-action))
              (list :wrap* pane)))
    (setf (clim:stream-end-of-line-action pane) :wrap*))
  pane)

(defun chat-compose-submit-event-p (event)
  "Return true when EVENT should submit the compose pane.
Drei may report the Enter key as either Return or Newline depending on the
backend.  Control-modified newline remains an editor gesture for inserting a
line break."
  (let ((modifiers (clim:event-modifier-state event)))
    (and (zerop (logand modifiers clim:+control-key+))
         (zerop (logand modifiers clim:+meta-key+))
         (let ((key-name (clim:keyboard-event-key-name event))
               (key-character (clim:keyboard-event-character event)))
           (or (eql key-character #\Return)
               (eql key-character #\Newline)
               (member key-name '(:return :newline :linefeed)
                       :test #'eq))))))

(defun chat-compose-modifier-key-name-p (key-name)
  "Return true when KEY-NAME names a modifier key by itself."
  (member key-name
          '(:shift :control :ctrl :meta :alt :super :hyper
            :lshift :lctrl :lmeta :lalt :lsuper :lhyper
            :rshift :rctrl :rmeta :ralt :rsuper :rhyper
            :shift-left :shift-right :control-left :control-right
            :ctrl-left :ctrl-right :meta-left :meta-right
            :alt-left :alt-right :super-left :super-right
            :hyper-left :hyper-right :caps-lock :num-lock
            :scroll-lock :mode-switch :iso-level3-shift)
          :test #'eq))

(defun chat-compose-encoded-control-character (event)
  "Return EVENT's control-encoded character, or NIL.

Some CLIM backends report C-b as character code 2 with no modifier state
instead of reporting #\\b plus the Control modifier.  Drei's command tables are
bound to the latter representation, so compose input normalizes these encoded
control characters before command lookup.  Editing keys that are commonly also
represented as ASCII control characters, such as Backspace and Tab, are left as
their normal key events so Drei can handle them directly."
  (let ((char (clim:keyboard-event-character event))
        (key-name (clim:keyboard-event-key-name event)))
    (and (characterp char)
         (not (member key-name '(:backspace :delete :rubout :tab
                                 :return :newline :linefeed)
                      :test #'eq))
         (zerop (clim:event-modifier-state event))
         (let ((code (char-code char)))
           (and (<= 1 code 26)
                ;; Leave Return/Newline activation alone.  When a backend sends
                ;; plain Enter as LF/CR, submit handling has already consumed it;
                ;; when it cannot distinguish C-j from Enter, there is no safe
                ;; way to infer the user's intent here.  Likewise preserve
                ;; plain Backspace and Tab as editing keys.
                (not (member char '(#\Backspace #\Tab #\Return #\Newline)
                             :test #'eql))
                (code-char (+ (char-code #\a) code -1)))))))

(defun chat-compose-modified-key-event-p (event)
  "Return true when EVENT is a key Drei should process directly.

Drei's gadget event bridge normally turns key events into gestures before the
Drei command processor sees them.  The compose pane uses a small CLIM gesture
normalizer for backend-specific Control encodings, but modifier keys by
themselves are still ignored and Shift-only character input remains ordinary
text input."
  (or (chat-compose-encoded-control-character event)
      (let ((modifiers (clim:event-modifier-state event))
            (key-name (clim:keyboard-event-key-name event)))
        (and (not (chat-compose-modifier-key-name-p key-name))
             (not (zerop modifiers))
             (not (eql modifiers clim:+shift-key+))
             (let ((key-character (clim:keyboard-event-character event)))
               (or key-character key-name))))))

(defun chat-compose-drei-control-editing-event-p (event)
  "Return true when EVENT is a modified key Drei should handle here.

This compatibility wrapper keeps older tests and callers meaningful after the
compose pane fix was broadened from a few control editing keys to every
modified key event."
  (chat-compose-modified-key-event-p event))

(defun chat-compose-drei-direct-command (event)
  "Return the native Drei command that must bypass ESA gesture parsing.

ESA consumes C-u as a universal numeric prefix before consulting any command
table, so an application-owned C-u binding cannot override it.  Dispatch the
equivalent Drei command through its public command executor instead.  Numeric
argument zero is Drei's documented kill-from-point-to-line-start operation."
  (when (eql (chat-compose-event-key event) (code-char 21))
    (list (chat-compose-drei-command-symbol "COM-KILL-LINE") 0 t)))

(defun execute-chat-compose-drei-command (pane command &key redisplay)
  "Execute native Drei COMMAND for PANE with undo and gadget propagation."
  (drei:with-bound-drei-special-variables
      (pane :prompt (format nil "~A " (first command)))
    (drei:execute-drei-command pane command)
    (when redisplay
      (drei:display-drei pane :redisplay-minibuffer t))
    ;; Match Drei gadget HANDLE-GESTURE's public value-callback contract after
    ;; using its exported direct command executor.
    (let ((view (drei:current-view pane)))
      (when (drei:modified-p view)
        (when (clim:gadget-value-changed-callback pane)
          (clim:value-changed-callback pane
                                       (clim:gadget-client pane)
                                       (clim:gadget-id pane)
                                       (clim:gadget-value pane)))
        (setf (drei:modified-p view) nil)))))

(defun chat-compose-drei-gesture (pane event)
  "Return the CLIM gesture the compose Drei command processor should see.

This is a narrow backend-normalization adapter for `drei-gadget-pane'.  It does
not decide which RPLACA command to run; it only preserves standard CLIM key
event information so Drei's command table machinery can resolve editor-table
and frame-table bindings."
  (let* ((key-name (clim:keyboard-event-key-name event))
         (key-character (clim:keyboard-event-character event))
         (modifiers (clim:event-modifier-state event))
         (encoded-control (chat-compose-encoded-control-character event))
         (replacement
           (cond
             ((chat-compose-modifier-key-name-p key-name) nil)
             (encoded-control encoded-control)
             ((and (null key-character)
                   (member key-name '(:escape :esc) :test #'eq))
              #\Esc)
             ((and (null key-character)
                   (member key-name '(:return) :test #'eq))
              #\Return)
             ((and (null key-character)
                   (member key-name '(:newline :linefeed) :test #'eq))
              #\Newline)
             ((and (null key-character)
                   (member key-name '(:backspace :delete :rubout) :test #'eq))
              #\Backspace))))
    (cond
      ((chat-compose-modifier-key-name-p key-name) nil)
      (replacement
       (make-instance 'clim:key-press-event
                      :sheet pane
                      :x 0
                      :y 0
                      :key-name nil
                      :key-character replacement
                      :modifier-state (if encoded-control
                                          clim:+control-key+
                                          modifiers)))
      ((and key-character
            (or (zerop modifiers)
                (eql modifiers clim:+shift-key+)))
       key-character)
      ((or key-character key-name)
       event)
      (t nil))))

(defun process-chat-compose-drei-event (pane event &key redisplay)
  "Process EVENT through Drei for compose PANE.
When REDISPLAY is nil, run just the command processor; this supports unit tests
without a grafted port. Live event handling uses Drei's normal handler so the
pane redraws and value callbacks propagate."
  (let ((direct-command (chat-compose-drei-direct-command event)))
    (if direct-command
        (execute-chat-compose-drei-command pane direct-command
                                           :redisplay redisplay)
        (let ((gesture (chat-compose-drei-gesture pane event)))
          (when (and gesture (esa:proper-gesture-p gesture))
            (drei:with-bound-drei-special-variables
                (pane :prompt (format nil "~A " (esa:gesture-name gesture)))
              (if redisplay
                  (drei:handle-gesture pane gesture)
                  (esa:process-gesture pane gesture))))))))

(defun chat-compose-application-input-active-p (&optional buffer)
  "Return true when RPLACA modal input should receive compose keystrokes."
  (or *minibuffer-active*
      *session-tree-selector-active*
      (openai-oauth-login-pending-p)
      (and buffer (buffer-user-input-pending buffer))))

(defun chat-key-name-keyword (key-name)
  "Return the RPLACA key keyword corresponding to CLIM KEY-NAME."
  (case key-name
    ((:left :right :up :down :home :end :page-up :page-down :tab :backspace)
     key-name)
    ((:backtab :shift-tab :iso-left-tab) :backtab)
    ((:prior) :page-up)
    ((:next) :page-down)
    ((:delete :rubout) :backspace)
    ((:return) #\Return)
    ((:newline :linefeed) #\Newline)
    (t key-name)))

(defun chat-control-key-character (char)
  "Return CHAR encoded as the control character expected by RPLACA keymaps."
  (cond
    ((null char) nil)
    ((char-equal char #\Space) (code-char 0))
    ((alpha-char-p char)
     (code-char (1+ (- (char-code (char-downcase char))
                      (char-code #\a)))))
    ((eql char #\Return) #\Return)
    ((eql char #\Newline) #\Newline)
    (t char)))

(defun chat-compose-event-key (event)
  "Return EVENT in the normalized key syntax used by `handle-key-event'."
  (let* ((modifiers (clim:event-modifier-state event))
         (key-name (clim:keyboard-event-key-name event))
         (key-character (clim:keyboard-event-character event))
         (encoded-control (and (chat-compose-encoded-control-character event)
                               key-character))
         (shift-tab-p (and (not (zerop (logand modifiers clim:+shift-key+)))
                           (or (eql key-character #\Tab)
                               (eq key-name :tab))))
         (base (cond
                 (encoded-control encoded-control)
                 (shift-tab-p :backtab)
                 ((member key-name '(:backtab :shift-tab :iso-left-tab)
                          :test #'eq)
                  :backtab)
                 (key-character key-character)
                 (key-name (chat-key-name-keyword key-name))))
         (control-p (or encoded-control
                        (not (zerop (logand modifiers clim:+control-key+)))))
         (meta-p (not (zerop (logand modifiers clim:+meta-key+))))
         (base-key (if (and control-p (characterp base))
                       (chat-control-key-character base)
                       base)))
    (cond
      ((and meta-p base-key) (list :meta base-key))
      (base-key base-key)
      (t nil))))

(defun chat-compose-escape-event-p (event)
  "Return true when EVENT is a plain Escape key press."
  (let ((modifiers (clim:event-modifier-state event))
        (key-name (clim:keyboard-event-key-name event))
        (key-character (clim:keyboard-event-character event)))
    (and (zerop (logand modifiers clim:+control-key+))
         (zerop (logand modifiers clim:+meta-key+))
         (or (eql key-character #\Esc)
             (eq key-name :escape)))))

(defun chat-compose-pane-point-offset (pane)
  "Return PANE's Drei point as a flat character offset."
  (drei-buffer:offset (drei:point (drei:current-view pane))))

(defun set-chat-compose-pane-point-offset (pane offset)
  "Set PANE's Drei point to OFFSET when its public view is available."
  (when offset
    (setf (drei-buffer:offset (drei:point (drei:current-view pane)))
          offset)))

(defun chat-compose-pane-mark-offset (pane)
  "Return PANE's Drei mark as a flat character offset."
  (drei-buffer:offset (drei:mark (drei:current-view pane))))

(defun set-chat-compose-pane-mark-offset (pane offset)
  "Set PANE's Drei mark to OFFSET."
  (setf (drei-buffer:offset (drei:mark (drei:current-view pane))) offset))

(defun sync-chat-buffer-input-from-compose-pane (pane buffer)
  "Reflect compose PANE's text, point, and meaningful mark into BUFFER."
  (when (and pane buffer)
    (let* ((value (clim:gadget-value pane))
           (offset (chat-compose-pane-point-offset pane))
           (message (buffer-input-message buffer))
           (mark-offset (chat-compose-pane-mark-offset pane)))
      (unless (stringp value)
        (error "Compose pane returned a non-string draft: ~S" value))
      ;; Rebuilding either representation is destructive: SET-MESSAGE-TEXT
      ;; clears point/mark, while Drei's GADGET-VALUE setter clears undo.
      ;; Only replace text when the editor actually changed it.
      (unless (string= value (message-text message))
        (if (file-buffer-p buffer)
            (setf (file-buffer-text buffer) value)
            (set-message-text message value)))
      (set-message-point-from-absolute-offset message offset)
      ;; Drei always owns a mark, while the message model uses NIL for no
      ;; meaningful region.  Persist only a non-empty Drei region so a real
      ;; C-SPC selection survives switching without manufacturing one.
      (if (= mark-offset offset)
          (message-clear-mark message)
          (set-message-mark-from-absolute-offset message mark-offset)))))

(defun sync-chat-compose-pane-from-buffer (pane buffer &key force)
  "Reflect BUFFER's input editor text and point in compose PANE."
  (when (and pane buffer)
    (let* ((message (buffer-input-message buffer))
           (text (message-text message))
           (offset (message-point-absolute-offset message))
           (mark-offset (or (message-mark-absolute-offset message) offset)))
      (if force
          ;; A real buffer change must replace the single Drei buffer and its
          ;; undo history.  Do not first allocate the outgoing large draft.
          (setf (clim:gadget-value pane) text)
          (let ((visible-text (clim:gadget-value pane)))
            (unless (stringp visible-text)
              (error "Compose pane returned a non-string draft: ~S"
                     visible-text))
            (unless (string= visible-text text)
              (setf (clim:gadget-value pane) text))))
      (set-chat-compose-pane-mark-offset pane mark-offset)
      (set-chat-compose-pane-point-offset pane offset))))

(defun initialize-chat-frame-compose-pane (frame pane)
  "Hydrate construction-configured PANE from FRAME's initial buffer before input."
  (when pane
    (sync-chat-compose-pane-from-buffer
     pane (chat-frame-buffer frame) :force t)
    (setf (chat-frame-compose-synchronized-buffer frame)
          (chat-frame-buffer frame)))
  pane)

(defvar *chat-frame-transition-compose-pane* nil
  "Dynamically selected compose pane for one frame buffer transition.")

(defvar *chat-frame-transition-synchronized-buffer* nil
  "Source buffer already saved by the active frame transition, or NIL.")

(defun call-with-chat-frame-buffer-transition
    (frame function &key compose-pane focus-compose state-only)
  "Call FUNCTION as one FRAME buffer/compose-state transition.

FRAME's buffer is authoritative at entry.  Save its Drei draft and point,
establish it as the process-level current buffer for legacy commands, then run
FUNCTION with that source buffer.  On normal return, or before re-signalling an
ordinary application error, adopt the buffer selected by the command and load
that buffer's draft, mark, and point back into Drei.  CLIM control transfers
such as frame exit are not intercepted.  STATE-ONLY promises that FUNCTION
changes only frame interaction state unless it selects another buffer; this
lets modal navigation avoid materializing a large compose draft on every key."
  (let* ((source (chat-frame-buffer frame))
         (compose (or compose-pane
                      (clim:find-pane-named frame 'compose))))
    (let ((*chat-frame-transition-compose-pane* compose)
          (*chat-frame-transition-synchronized-buffer* nil))
      (unless (eq source (chat-frame-compose-synchronized-buffer frame))
        (sync-chat-buffer-input-from-compose-pane compose source)
        (setf (chat-frame-compose-synchronized-buffer frame) source))
      (setf *chat-frame-transition-synchronized-buffer* source)
      (unless (member source *buffer-ring* :test #'eq)
        (add-buffer-to-ring source))
      (unless (eq source (current-buffer))
        (switch-to-buffer source))
      (labels ((finish-transition ()
                 (let ((target (or (current-buffer) source)))
                   (if (and state-only
                            (eq target source)
                            (eq source
                                (chat-frame-compose-synchronized-buffer frame)))
                       (request-chat-frame-redisplay frame)
                       (setf (esa:esa-current-buffer frame) target))
                   (when focus-compose
                     (focus-chat-compose-pane frame)))))
        (multiple-value-prog1
            (handler-case
                (funcall function source)
              (error (condition)
                (handler-case
                    (finish-transition)
                  (error (reconciliation-error)
                    (error "Application command failed (~A); buffer reconciliation also failed (~A)."
                           condition reconciliation-error)))
                (error condition)))
          (finish-transition))))))

(defun chat-compose-state-only-modal-key-p (key)
  "Return true when modal KEY cannot invoke a buffer-changing callback."
  (let ((base-key (minibuffer-base-key key)))
    (and (or *minibuffer-active* *session-tree-selector-active*)
         (not (and (characterp base-key)
                   (or (char= base-key #\Return)
                       (char= base-key #\Newline)))))))

(defun dispatch-chat-compose-event-to-buffer (pane event)
  "Dispatch EVENT through RPLACA's buffer key handler and refresh the frame."
  (let* ((frame (clim:pane-frame pane))
         (raw-key (chat-compose-event-key event))
         (key (cond
                ((and raw-key *meta-pending*)
                 (setf *meta-pending* nil)
                 (list :meta raw-key))
                (t raw-key))))
    (when key
      (call-with-chat-frame-buffer-transition
       frame
       (lambda (source)
         (let ((result (handle-key-event source key)))
           (when (eq result :quit)
             (clim:frame-exit frame))
           t))
       :compose-pane pane
       :state-only (chat-compose-state-only-modal-key-p key)))))

(defclass rplaca-chat-compose-pane (drei:drei-gadget-pane)
  ()
  (:metaclass esa-utils:modual-class)
  (:documentation "Drei-backed chat compose pane with RPLACA frame commands."))

(defmethod drei-syntax:additional-command-tables append
    ((pane rplaca-chat-compose-pane) (table clim:command-table))
  "Prefer FRAME's application commands, then compose-specific editing keys.

Drei gadgets put `exclusive-gadget-table' before their inherited frame table.
That table owns M-x for Drei's blocking `accept' workflow, while RPLACA uses
its frame command to activate the application's non-blocking minibuffer.  Use
Drei's public application-extension hook to put the frame-local table first;
the inherited copy remains harmless and ordinary editing keys stay in Drei
because they are intentionally absent from the frame table."
  (declare (ignore table))
  (let ((frame (ignore-errors (clim:pane-frame pane))))
    (append (when frame
              (list (clim:frame-command-table frame)))
            '(rplaca-chat-compose-editing-table))))

(defun chat-compose-meta-prefix-event (pane event)
  "Return EVENT as a Meta-modified key press for an ESC prefix."
  (make-instance 'clim:key-press-event
                 :sheet pane
                 :x 0
                 :y 0
                 :key-name (clim:keyboard-event-key-name event)
                 :key-character (clim:keyboard-event-character event)
                 :modifier-state (logior (clim:event-modifier-state event)
                                          clim:+meta-key+)))

(defmethod clim:handle-event :before
    ((pane rplaca-chat-compose-pane) (event clim:pointer-event))
  "Invalidate compose/model synchronization before Drei handles a pointer."
  (declare (ignore event))
  (let ((frame (clim:pane-frame pane)))
    (when (typep frame 'rplaca-chat-frame)
      (setf (chat-frame-compose-synchronized-buffer frame) nil))))

(defmethod clim:handle-event :around
    ((pane rplaca-chat-compose-pane) (event clim:key-press-event))
  "Normalize only key forms that upstream Drei cannot process directly.

Ordinary editing and application keys go through Drei's own HANDLE-EVENT and
its inherited frame command table.  Direct dispatch remains only for RPLACA's
modal input overlays, ESC-as-Meta, and modified key events that
ESA:CONVERT-TO-GESTURE currently drops.  Standalone modifier presses are
consumed explicitly so a retained prefix never depends on backend-specific
modifier-event conversion."
  (let* ((frame (ignore-errors (clim:pane-frame pane)))
         (dispatch
           (lambda ()
             (let ((buf (and frame (chat-frame-buffer frame))))
               ;; A non-modal Drei gesture may edit text or move point/mark.
               ;; Modal keys are consumed by RPLACA and leave Drei intact.
               (when (and buf
                          (not (chat-compose-application-input-active-p buf)))
                 (setf (chat-frame-compose-synchronized-buffer frame) nil))
               (cond
                 ((chat-compose-modifier-key-name-p
                   (clim:keyboard-event-key-name event))
                  ;; A modifier by itself is not a semantic editor gesture.
                  ;; Keep it out of ESA's accumulated prefix sequence.
                  t)
                 ((and buf (chat-compose-escape-event-p event)
                       (buffer-llm-running-p buf))
                  ;; Escape always means "stop the active stream" before it can
                  ;; extend or consume a pending ESC-as-Meta prefix.
                  (setf *meta-pending* nil)
                  (dispatch-chat-compose-event-to-buffer pane event))
                 ((and buf (chat-compose-application-input-active-p buf)
                       (chat-compose-escape-event-p event)
                       (not *meta-pending*))
                  (setf *meta-pending* t)
                  t)
                 ((and buf (chat-compose-application-input-active-p buf))
                  (dispatch-chat-compose-event-to-buffer pane event))
                 ((and (chat-compose-escape-event-p event)
                       (not *meta-pending*))
                  (setf *meta-pending* t)
                  t)
                 (*meta-pending*
                  (let ((gesture-event
                          (prog1 (chat-compose-meta-prefix-event pane event)
                            (setf *meta-pending* nil))))
                    (process-chat-compose-drei-event pane gesture-event
                                                     :redisplay t)))
                 ((chat-compose-modified-key-event-p event)
                  (process-chat-compose-drei-event pane event :redisplay t))
                 (t (call-next-method))))))
         (result
           (if (typep frame 'rplaca-chat-frame)
               (call-chat-frame-ui-action-safely
                frame "compose key dispatch" dispatch)
               (funcall dispatch))))
    (when frame
      (emit-chat-frame-e2e-snapshot frame :reason "compose-key" :pane "compose"))
    result))

(defclass rplaca-transcript-pane (esa:esa-pane-mixin clim:application-pane)
  ()
  (:documentation "ESA window pane that displays the current chat transcript."))

(defclass rplaca-chat-info-pane (esa:info-pane)
  ()
  (:documentation "Emacs-style status line for the RPLACA chat frame.")
  (:default-initargs
   :height 22
   :min-height 22
   :max-height 22))

(defparameter *chat-minibuffer-line-height* 24
  "Minimum fallback pixel height of one chat minibuffer row.")

(defparameter *chat-minibuffer-max-pixel-height* nil
  "Maximum pixel height for the expanded chat minibuffer pane.
When NIL, derive it from `*minibuffer-max-height*' and
`*chat-minibuffer-line-height*' so logical rows and reserved space agree.")

(defclass rplaca-chat-minibuffer-pane (esa:minibuffer-pane)
  ()
  (:documentation "ESA minibuffer used for messages, command arguments, and M-x.")
  (:default-initargs
   :height 24
   :min-height 24
   :max-height 24
   :incremental-redisplay nil))

(defmethod clim:compose-space ((pane rplaca-chat-minibuffer-pane)
                               &key width height)
  "Return a compact minibuffer space requirement without probing a basic medium.

Some native McCLIM builds compute `esa:minibuffer-pane' height by asking a
fresh `clim:basic-medium' for font metrics before the pane has a backend
medium.  Quicklisp/Ultralisp McCLIM may not define those metric methods, so
native startup fails before the frame is adopted.  RPLACA owns the chat
minibuffer height explicitly, so this pane-specific method keeps layout stable
and avoids the backend-independent metric probe."
  (declare (ignore pane height))
  (clim:make-space-requirement
   :width (or width 900)
   :height *chat-minibuffer-line-height*))

(defun chat-frame-e2e-effective-frame (frame)
  "Return FRAME when it is a chat frame, otherwise the current application frame."
  (cond
    ((typep frame 'rplaca-chat-frame) frame)
    ((typep clim:*application-frame* 'rplaca-chat-frame)
     clim:*application-frame*)
    (t nil)))

(defun chat-frame-e2e-compose-text (frame)
  "Return semantic compose text for FRAME without inspecting pixels."
  (let* ((compose (and frame (ignore-errors (clim:find-pane-named frame 'compose))))
         (value (and compose (ignore-errors (clim:gadget-value compose))))
         (buf (and frame (chat-frame-buffer frame)))
         (fallback (and buf (message-text (buffer-input-message buf)))))
    (or (and (stringp value) value)
        fallback
        "")))

(defun chat-frame-e2e-compose-point (frame)
  "Return FRAME's visible Drei compose point as a flat character offset."
  (let ((compose (and frame
                      (ignore-errors (clim:find-pane-named frame 'compose)))))
    (or (and compose (chat-compose-pane-point-offset compose)) 0)))

(defun chat-frame-e2e-compose-mark (frame)
  "Return FRAME's visible Drei compose mark as a flat character offset."
  (let ((compose (and frame
                      (ignore-errors (clim:find-pane-named frame 'compose)))))
    (or (and compose (chat-compose-pane-mark-offset compose)) 0)))

(defun chat-frame-e2e-sheet-bounds (frame sheet prefix)
  "Return flat E2E fields for SHEET's bounds in FRAME top-level coordinates.

This uses the portable CLIM sheet geometry protocol.  It observes the layout
selected by CLIM without inspecting output records, pixels, mirrors, or private
rendering state."
  (let ((top-level (and frame
                        (ignore-errors (clim:frame-top-level-sheet frame)))))
    (when (and top-level sheet)
      (ignore-errors
        (let* ((transformation
                 (clim:sheet-delta-transformation sheet top-level))
               (region
                 (clim:transform-region transformation
                                        (clim:sheet-region sheet))))
          (multiple-value-bind (left top right bottom)
              (clim:bounding-rectangle* region)
            (flet ((field (suffix)
                     (intern (format nil "~A-~A" prefix suffix) :keyword)))
              (list (field "LEFT") left
                    (field "TOP") top
                    (field "RIGHT") right
                    (field "BOTTOM") bottom))))))))

(defun chat-frame-e2e-input-focus-pane (frame)
  "Return the semantic FRAME pane that currently owns CLIM keyboard input."
  (let* ((top-level (and frame
                         (ignore-errors (clim:frame-top-level-sheet frame))))
         (port (and top-level (ignore-errors (clim:port top-level))))
         (focus (and port
                     (ignore-errors (clim:port-keyboard-input-focus port))))
         (standard-input
           (and frame (ignore-errors (clim:frame-standard-input frame))))
         (compose (and frame
                       (ignore-errors (clim:find-pane-named frame 'compose))))
         (transcript
           (and frame
                (ignore-errors (clim:find-pane-named frame 'transcript))))
         (pointer-documentation
           (and frame
                (ignore-errors
                  (clim:frame-pointer-documentation-output frame)))))
    (cond
      ((null focus) nil)
      ((eq focus compose) :compose)
      ((eq focus transcript) :transcript)
      ((eq focus standard-input) :standard-input)
      ((eq focus pointer-documentation) :pointer-documentation)
      (t :other))))

(defun chat-frame-e2e-layout-fields (frame)
  "Return E2E-only semantic and CLIM layout measurements for FRAME."
  (let* ((top-level (and frame
                         (ignore-errors (clim:frame-top-level-sheet frame))))
         (minibuffer
           (and frame
                (ignore-errors (clim:find-pane-named frame 'minibuffer))))
         (pointer-documentation
           (and frame
                (ignore-errors
                  (clim:frame-pointer-documentation-output frame))))
         (kind (chat-interaction-pane-kind))
         (visible-count (length (chat-interaction-pane-rows kind)))
         (desired-rows (chat-minibuffer-desired-row-count))
         (required-height
           (and minibuffer
                (chat-minibuffer-content-pixel-height minibuffer desired-rows))))
    (append
     (list :input-focus-pane (chat-frame-e2e-input-focus-pane frame)
           :compose-point (chat-frame-e2e-compose-point frame)
           :compose-mark (chat-frame-e2e-compose-mark frame)
           :minibuffer-filtered-count
           (if (and *minibuffer-active*
                    (eq *minibuffer-mode* :completion))
               (length *minibuffer-filtered-items*)
               0)
           :minibuffer-visible-count visible-count
           :minibuffer-desired-rows desired-rows
           :minibuffer-row-height
           (and minibuffer (chat-minibuffer-row-pixel-height minibuffer))
           :minibuffer-required-height required-height
           :top-level-grafted
           (and top-level (ignore-errors (clim:sheet-grafted-p top-level)))
           :pointer-documentation-grafted
           (and pointer-documentation
                (ignore-errors
                  (clim:sheet-grafted-p pointer-documentation))))
     (chat-frame-e2e-sheet-bounds frame top-level "TOP-LEVEL")
     (chat-frame-e2e-sheet-bounds frame minibuffer "MINIBUFFER")
     (chat-frame-e2e-sheet-bounds
      frame pointer-documentation "POINTER-DOCUMENTATION"))))

(defun minibuffer-selection-count-text ()
  "Return a compact selected/total completion count string, or NIL."
  (let ((count (length *minibuffer-filtered-items*)))
    (when (plusp count)
      (format nil "~D/~D" (1+ *minibuffer-selected-index*) count))))

(defun chat-minibuffer-display-input ()
  "Return minibuffer input with a visible point marker for display."
  (let ((point (max 0 (min *minibuffer-point* (length *minibuffer-input*)))))
    (concatenate 'string
                 (subseq *minibuffer-input* 0 point)
                 "|"
                 (subseq *minibuffer-input* point))))

(defun write-minibuffer-prompt-line (stream &key (display-cursor-p nil))
  "Write the minibuffer prompt line to STREAM."
  (format stream "~A: ~A"
          *minibuffer-prompt*
          (if display-cursor-p
              (chat-minibuffer-display-input)
              *minibuffer-input*))
  (when (and (eq *minibuffer-mode* :completion)
             *minibuffer-filtered-items*)
    (let ((item (nth *minibuffer-selected-index*
                     *minibuffer-filtered-items*))
          (count-text (minibuffer-selection-count-text)))
      (when item
        (format stream "  [~A]" (minibuffer-item-display item)))
      (when count-text
        (format stream "  (~A)" count-text)))))

(defun chat-completion-visible-candidate-rows
    (items scroll-offset selected-index visible-count)
  "Return semantic rows for one automatic completion view."
  (let* ((count (length items))
         (start (max 0 (min scroll-offset count)))
         (end (min count (+ start visible-count))))
    (loop :for item :in (subseq items start end)
          :for index :from start
          :collect (list :index index
                         :item item
                         :display (minibuffer-item-display item)
                         :selected-p (= index selected-index)))))

(defun chat-interaction-pane-kind ()
  "Return the visible semantic interaction hosted by the minibuffer pane."
  (cond
    (*minibuffer-active* :minibuffer)
    (*session-tree-selector-active* :session-tree)
    (*slash-completion-active* :slash)
    (*skill-completion-active* :skill)
    (t nil)))

(defun chat-interaction-pane-rows (&optional (kind (chat-interaction-pane-kind)))
  "Return visible presentation rows for KIND."
  (case kind
    (:minibuffer (minibuffer-visible-candidate-rows))
    (:session-tree (session-tree-selector-visible-candidate-rows))
    (:slash
     (chat-completion-visible-candidate-rows
      *slash-completion-filtered-items*
      *slash-completion-scroll-offset*
      *slash-completion-selected-index*
      (slash-completion-visible-item-count)))
    (:skill
     (chat-completion-visible-candidate-rows
      *skill-completion-filtered-items*
      *skill-completion-scroll-offset*
      *skill-completion-selected-index*
      (skill-completion-visible-item-count)))
    (otherwise nil)))

(defun write-chat-interaction-prompt-line
    (stream kind &key (display-cursor-p nil))
  "Write the visible prompt for semantic interaction KIND."
  (case kind
    (:minibuffer
     (write-minibuffer-prompt-line stream :display-cursor-p display-cursor-p))
    (:session-tree
     (format stream "Session Tree [~(~A~)] search: ~A"
             *session-tree-selector-filter-mode*
             *session-tree-selector-search*))
    (:slash
     (format stream "Slash command: /~A" *slash-completion-query*))
    (:skill
     (format stream "Skill mention: $~A" *skill-completion-query*))))

(defun chat-frame-e2e-minibuffer-text ()
  "Return semantic text for the interaction declared in the minibuffer pane."
  (let ((kind (chat-interaction-pane-kind)))
    (if kind
      (with-output-to-string (stream)
          (write-chat-interaction-prompt-line stream kind)
          (let ((rows (chat-interaction-pane-rows kind)))
            (if rows
                (dolist (row rows)
                  (format stream "~%~A ~A"
                          (if (getf row :selected-p) ">" " ")
                          (getf row :display)))
                (unless (and (eq kind :minibuffer)
                             (eq *minibuffer-mode* :prompt))
                  (format stream "~%  No matches")))))
        "")))

(defun chat-frame-buffer-status-label (buffer)
  "Return BUFFER's visible status label for the chat info line."
  (cond
    (*safe-reload-running-p* "reloading")
    (buffer (string-downcase (symbol-name (buffer-status buffer))))
    (t "")))

(defun chat-frame-e2e-info-line (frame)
  "Return the status/model line represented by FRAME."
  (let ((buf (and frame (chat-frame-buffer frame))))
    (if buf
        (multiple-value-bind (provider model)
            (handler-case (resolve-buffer-provider-and-model buf)
              (error () (values nil nil)))
          (format nil "~A  ~A  ~A  agent ~A  ~A"
                  (buffer-name buf)
                  (buffer-major-mode buf)
                  (chat-frame-buffer-status-label buf)
                  (buffer-agent-name buf)
                  (if (and provider model)
                      (model-selector-display provider model)
                      "no model")))
        "")))

(defun chat-display-item-e2e-text (item)
  "Return semantic transcript text for one displayed ITEM."
  (if (chat-tool-activity-summary-p item)
      (chat-tool-activity-summary-text item)
      (format nil "~A>~%~A" (chat-message-label item) (message-text item))))

(defun call-buffer-presentation-function (function buffer columns)
  "Return entries from FUNCTION for BUFFER and COLUMNS."
  (when function
    (handler-case
        (or (funcall function buffer columns) nil)
      (error (condition)
        (list (list :text (format nil "[Presentation error: ~A]" condition)
                    :face :error
                    :unique-id (list :presentation-error function)))))))

(defun buffer-presentation-entries-e2e-text (entries)
  "Return semantic text for generic presentation ENTRIES."
  (format nil "~{~A~^~%~}"
          (mapcar (lambda (entry) (getf entry :text "")) entries)))

(defun buffer-presentation-function-e2e-text (buffer function)
  "Return semantic text produced by BUFFER's presentation FUNCTION."
  (buffer-presentation-entries-e2e-text
   (call-buffer-presentation-function
    function
    buffer
    *buffer-presentation-default-columns*)))

(defun buffer-presentation-feedback-items (buffer)
  "Return transcript feedback messages to append after custom presentations."
  (remove-if-not (lambda (item)
                   (and (typep item 'message)
                        (eq (message-sender item) :system)
                        (not (buffer-system-prompt-display-message-p item))))
                 (chat-transcript-display-items buffer)))

(defun buffer-presentation-feedback-e2e-text (buffer)
  "Return semantic text for BUFFER feedback messages."
  (let ((items (and buffer (buffer-presentation-feedback-items buffer))))
    (and items
         (format nil "~{~A~^~%~%~}"
                 (mapcar #'chat-display-item-e2e-text items)))))

(defun chat-frame-e2e-transcript-text (buf)
  "Return semantic transcript text for BUF using the normal display item path."
  (let ((presentation-function (and buf (buffer-presentation-function buf))))
    (cond
      (presentation-function
       (format nil "~{~A~^~%~%~}"
               (remove-if #'blank-string-p
                          (list (buffer-presentation-function-e2e-text
                                 buf presentation-function)
                                (buffer-presentation-feedback-e2e-text buf)))))
      (t
       (let ((items (and buf (chat-transcript-display-items buf))))
         (if items
             (format nil "~{~A~^~%~%~}" (mapcar #'chat-display-item-e2e-text items))
             "No messages yet."))))))

(defun chat-frame-e2e-input-presentation-text (buf &optional input-text)
  "Return semantic text for BUF's input presentation overlay, if any."
  (let ((input-functions (and buf (buffer-input-presentation-functions buf)))
        (*buffer-input-presentation-text* input-text))
    (and input-functions
         (format nil "~{~A~^~%~}"
                 (mapcar (lambda (function)
                           (buffer-presentation-function-e2e-text buf function))
                         input-functions)))))

(defun chat-frame-e2e-screen-text (frame)
  "Return a semantic screen-text snapshot for FRAME."
  (let ((*chat-interaction-state*
          (if (typep frame 'rplaca-chat-frame)
              (chat-frame-interaction-state frame)
              *chat-interaction-state*)))
    (let* ((buf (and frame (chat-frame-buffer frame)))
           (transcript (chat-frame-e2e-transcript-text buf))
           (input-panel (chat-frame-e2e-input-presentation-text
                         buf
                         (chat-frame-e2e-compose-text frame)))
           (info (chat-frame-e2e-info-line frame))
           (minibuffer (chat-frame-e2e-minibuffer-text))
           (parts (remove-if #'blank-string-p
                             (list transcript
                                   input-panel
                                   info
                                   minibuffer))))
      (format nil "~{~A~%~}" parts))))

(defun chat-frame-e2e-profile-theme (profile)
  "Return PROFILE's externally stable theme spelling for E2E observations."
  (and profile
       (appearance-id-external-string
        (appearance-profile-selected-theme profile))))

(defun chat-frame-e2e-bundle-surface (bundle role)
  "Return ROLE's portable surface value from BUNDLE, or NIL when unspecified."
  (when bundle
    (let* ((entry (assoc role (resolved-appearance-bundle-surface-defaults bundle)
                         :test #'equal))
           (surface (and entry (appearance-role-style-surface (cdr entry)))))
      (unless (or (null surface) (appearance-unspecified-p surface))
        (let ((background (appearance-surface-spec-background surface)))
          (unless (appearance-unspecified-p background)
            (copy-appearance-value background)))))))

(defun chat-frame-e2e-appearance-fields (frame)
  "Return data-only appearance lifecycle state owned by FRAME.

These fields deliberately expose no CLIM port, medium, pane, or mutable
resolver object.  The GUI harness uses them to prove the frame-owned
active/staged/persisted distinction and generation coherence without treating
pixels or private McCLIM state as the contract."
  (when (typep frame 'rplaca-chat-frame)
    (let* ((catalog (chat-frame-appearance-catalog frame))
           (staged (chat-frame-appearance-staged-candidate frame))
           (bundle (chat-frame-appearance-active-bundle frame))
           (result (chat-frame-appearance-last-activation-result frame)))
      (list
       :appearance-active-theme
       (chat-frame-e2e-profile-theme (chat-frame-appearance-profile frame))
       :appearance-staged-theme
       (and staged
            (chat-frame-e2e-profile-theme
             (appearance-candidate-profile staged)))
       :appearance-persisted-theme
       (chat-frame-e2e-profile-theme
        (chat-frame-appearance-persisted-profile frame))
       :appearance-catalog-generation
       (appearance-catalog-generation catalog)
       :appearance-profile-revision
       (chat-frame-appearance-revision frame)
       :appearance-font-inventory-generation
       (chat-frame-appearance-font-inventory-generation frame)
       :appearance-font-choice-count
       (length (appearance-editor-font-choices frame))
       :appearance-bundle-catalog-generation
       (and bundle (resolved-appearance-bundle-catalog-generation bundle))
       :appearance-bundle-profile-revision
       (and bundle (resolved-appearance-bundle-profile-revision bundle))
       :appearance-bundle-font-inventory-generation
       (and bundle
            (resolved-appearance-bundle-font-inventory-generation bundle))
       :appearance-bundle-theme
       (and bundle
            (chat-frame-e2e-profile-theme
             (resolved-appearance-bundle-profile bundle)))
       :appearance-transcript-surface
       (chat-frame-e2e-bundle-surface bundle :transcript-pane)
       :appearance-compose-surface
       (chat-frame-e2e-bundle-surface bundle :compose-pane)
       :appearance-activation-status
       (and result
            (string-downcase
             (symbol-name (appearance-activation-result-status result))))
       :appearance-activation-classification
       (let ((classification
               (and result (appearance-activation-result-classification result))))
         (and classification
              (string-downcase
               (symbol-name
                (appearance-activation-classification-status classification)))))))))

(defun chat-frame-e2e-snapshot (frame)
  "Return semantic GUI state for FRAME as a plist for tests and E2E logs."
  (let* ((frame (chat-frame-e2e-effective-frame frame))
         (buf (and frame (chat-frame-buffer frame)))
         (compose-text (chat-frame-e2e-compose-text frame))
         (*chat-interaction-state*
           (if frame
               (chat-frame-interaction-state frame)
               *chat-interaction-state*)))
    (multiple-value-bind (provider model)
        (if buf
            (handler-case (resolve-buffer-provider-and-model buf)
              (error () (values nil nil)))
            (values nil nil))
      (append
       (list :buffer-name (and buf (buffer-name buf))
             :agent (and buf (buffer-agent-name buf))
             :status (and buf (chat-frame-buffer-status-label buf))
             :major-mode (and buf (buffer-major-mode buf))
             :provider (and provider (string-downcase (symbol-name provider)))
             :model model
             :message-count (and buf (buffer-message-count buf))
             :show-tool-results (and buf (buffer-show-tool-results-p buf))
             :show-reasoning (and buf (buffer-show-reasoning-p buf))
             :show-metadata (and buf (buffer-show-metadata-p buf))
             :debug-mode *debug-mode*
             :minibuffer-active *minibuffer-active*
             :buffer-selector-active nil
             :model-selector-active nil
             :think-selector-active nil
             :session-tree-selector-active *session-tree-selector-active*
             :interaction-kind (chat-interaction-pane-kind)
             :compose-text compose-text
             :compose-length (length compose-text)
             :compose-fingerprint
             (file-checkpoint-content-hash compose-text)
             :minibuffer-text (chat-frame-e2e-minibuffer-text)
             :info-text (chat-frame-e2e-info-line frame)
             :screen-text (chat-frame-e2e-screen-text frame))
       (chat-frame-e2e-appearance-fields frame)
       (chat-frame-e2e-layout-fields frame)))))

(defun emit-chat-frame-e2e-snapshot
    (frame &key reason pane (repeat nil repeat-supplied-p))
  "Emit a structured semantic GUI snapshot for FRAME when E2E logging is enabled."
  ;; Gate before constructing the snapshot.  Its semantic transcript mirrors
  ;; the visible CLIM view for the external E2E driver and may invoke package
  ;; presentation functions; production redisplay must never evaluate that
  ;; secondary test representation.
  (when (and *debug-log-file* (e2e-events-enabled-p))
    (let ((frame (chat-frame-e2e-effective-frame frame)))
      (when frame
        (ignore-errors
          (apply #'file-debug-event
                 "ui-snapshot"
                 (append (list :reason reason :pane pane)
                         (when repeat-supplied-p
                           (list :repeat repeat))
                         (chat-frame-e2e-snapshot frame))))))))

(defun emit-chat-pane-rendered (frame pane-name &rest payload)
  "Emit E2E pane render and snapshot events for FRAME."
  (when (and *debug-log-file* (e2e-events-enabled-p))
    (let ((frame (chat-frame-e2e-effective-frame frame)))
      (when frame
        (ignore-errors
          (apply #'file-debug-event
                 "pane-rendered"
                 :pane pane-name
                 payload))
        (emit-chat-frame-e2e-snapshot
         frame :reason "pane-rendered" :pane pane-name)))))

(defun display-minibuffer-candidate-row (frame stream row kind)
  "Display one semantic interaction ROW on STREAM as a presentation."
  (let* ((item (getf row :item))
         (index (getf row :index))
         (display (getf row :display))
         (selected-p (getf row :selected-p))
         (marker (if selected-p ">" " "))
         (ref (make-chat-interaction-candidate-ref
               *chat-interaction-state*
               (chat-interaction-state-generation *chat-interaction-state*)
               kind index item)))
    (flet ((emit-row ()
             (format stream " ~A " marker)
             (call-with-chat-appearance-role
              frame stream
              (if selected-p
                  '(:minibuffer-pane :selector-entry :selector-selection
                    :minibuffer-selection-emphasis)
                  '(:minibuffer-pane :selector-entry))
              (lambda () (format stream "~A" display)))))
      (clim:with-output-as-presentation
          (stream ref 'chat-interaction-candidate :single-box t)
        (emit-row)))))

(defun display-chat-minibuffer-pane (frame stream)
  "Display frame-owned semantic interaction state in the minibuffer pane."
  (let ((*chat-interaction-state* (chat-frame-interaction-state frame)))
    (let ((kind (chat-interaction-pane-kind)))
      (when kind
        (call-with-chat-appearance-role
         frame stream '(:minibuffer-pane :default-text)
         (lambda ()
           (write-char #\Space stream)
           (write-chat-interaction-prompt-line
            stream kind :display-cursor-p t)))
        (let ((rows (chat-interaction-pane-rows kind)))
          (if rows
              (dolist (row rows)
                (terpri stream)
                (display-minibuffer-candidate-row frame stream row kind))
              (unless (and (eq kind :minibuffer)
                           (eq *minibuffer-mode* :prompt))
                (call-with-chat-appearance-role
                 frame stream '(:minibuffer-pane :selector-separator)
                 (lambda () (format stream "~%   No matches"))))))
      (emit-chat-pane-rendered frame "minibuffer"
                               :active (not (null kind))
                               :kind kind
                               :text (chat-frame-e2e-minibuffer-text))))))

(defun display-chat-info-pane (frame stream)
  "Display an Emacs-style status line for FRAME."
  (let* ((pane (and (typep stream 'esa:info-pane) stream))
         (master (and pane (ignore-errors (esa:master-pane pane))))
         (frame (or (and master (ignore-errors (clim:pane-frame master)))
                    clim:*application-frame*))
         (buf (and (typep frame 'rplaca-chat-frame)
                   (chat-frame-buffer frame))))
    (when buf
      (multiple-value-bind (provider model)
          (handler-case (resolve-buffer-provider-and-model buf)
            (error () (values nil nil)))
        (call-with-chat-appearance-role
         frame stream '(:info-pane :modeline)
         (lambda ()
           (format stream " ~A  ~A  ~A  ~A"
                   (buffer-name buf)
                   (buffer-major-mode buf)
                   (chat-frame-buffer-status-label buf)
                   (if (and provider model)
                       (model-selector-display provider model)
                       "no model"))))
      (emit-chat-pane-rendered frame "info"
                               :text (chat-frame-e2e-info-line frame))))))

(clim:define-application-frame rplaca-chat-frame
    (esa:esa-frame-mixin clim:standard-application-frame)
  ((buffer :initarg :buffer
           :accessor chat-frame-buffer)
   (appearance-profile :initarg :appearance-profile
                       :initform (make-appearance-profile)
                       :accessor chat-frame-appearance-profile
                       :documentation
                       "Immutable profile selected for this frame at construction.")
   (appearance-catalog :initarg :appearance-catalog
                       :initform (current-package-appearance-catalog)
                       :reader chat-frame-appearance-catalog
                       :documentation
                       "Immutable appearance declarations resolved by this frame.")
   (appearance-revision :initform 0
                        :reader chat-frame-appearance-revision
                        :documentation
                        "Frame-local appearance state revision; it is not a render key.")
   (appearance-font-inventory-generation :initform 0
                                         :reader chat-frame-appearance-font-inventory-generation
                                         :documentation
                                         "Frame-local supplied font inventory generation; enumeration is deferred.")
   (appearance-font-inventory :initform nil
                              :reader chat-frame-appearance-font-inventory
                              :documentation
                              "Private, frame-local McCLIM font protocol inventory after adoption.")
   (appearance-font-refresh-diagnostics :initform nil
                                        :reader chat-frame-appearance-font-refresh-diagnostics
                                        :documentation
                                        "Structured failures from this frame's explicit font refreshes.")
   (appearance-active-bundle :initform nil
                             :reader chat-frame-appearance-active-bundle
                             :documentation
                             "Last fully resolved bundle atomically published by this frame.")
   (appearance-staged-candidate :initform nil
                                :reader chat-frame-appearance-staged-candidate
                                :documentation
                                "Last requested immutable candidate, distinct from active state.")
   (appearance-editor-buffer :initform nil
                             :accessor chat-frame-appearance-editor-buffer
                             :documentation
                             "Dedicated presentation buffer for this frame's staged editor.")
   (appearance-editor-status :initform nil
                             :accessor chat-frame-appearance-editor-status
                             :documentation
                             "Last structured editor operation result.")
   (appearance-persisted-profile :initform nil
                                 :accessor chat-frame-appearance-persisted-profile
                                 :documentation
                                 "Last profile successfully read from or written to disk.")
   (appearance-editor-role :initform :default-text
                           :accessor chat-frame-appearance-editor-role)
   (appearance-editor-font-family :initform nil
                                  :accessor chat-frame-appearance-editor-font-family)
   (appearance-editor-font-face :initform nil
                                :accessor chat-frame-appearance-editor-font-face)
   (appearance-editor-font-styles :initform (make-hash-table :test #'equal)
                                  :reader chat-frame-appearance-editor-font-styles)
   (appearance-last-activation-result :initform nil
                                      :reader chat-frame-appearance-last-activation-result)
   (appearance-activation-diagnostics :initform nil
                                      :reader chat-frame-appearance-activation-diagnostics)
   (appearance-resolved-roles :initform (make-hash-table :test #'equal)
                              :reader chat-frame-appearance-resolved-roles
                              :documentation
                              "Frame-local cache from role stacks to resolved styles.")
   (appearance-role-keys :initform (make-hash-table :test #'equal)
                         :reader chat-frame-appearance-role-keys
                         :documentation
                         "Frame-local structural render keys, one per role stack.")
   (appearance-runtime-diagnostic-keys :initform (make-hash-table :test #'equal)
                                       :reader chat-frame-appearance-runtime-diagnostic-keys)
   (appearance-runtime-diagnostics :initform nil
                                   :accessor chat-frame-appearance-runtime-diagnostics)
   (interaction-state :initform (make-chat-interaction-state)
                      :reader chat-frame-interaction-state)
   (compose-synchronized-buffer
    :initform nil
    :accessor chat-frame-compose-synchronized-buffer
    :documentation
    "Frame buffer whose draft/point/mark currently match the Drei pane.")
   (lifecycle-lock :initform (bt:make-lock "rplaca chat lifecycle")
                   :reader chat-frame-lifecycle-lock)
   (lifecycle-state :initform :created
                    :accessor chat-frame-lifecycle-state))
  (:command-table (rplaca-chat-frame
                   :inherit-from (esa:global-esa-table
                                  esa:keyboard-macro-table)
                   :menu (("Chat" :menu rplaca-chat-control-menu
                           :documentation "Chat controls.")
                          ("View" :menu rplaca-chat-view-menu
                           :documentation "Transcript display controls.")
                          ("Skills" :menu rplaca-chat-skills-menu
                           :documentation "Enable or disable skills.")
                          ("Packages" :menu rplaca-chat-packages-menu
                           :documentation "Manage packages for this chat.")
                          ("Effort" :menu rplaca-chat-effort-menu
                           :documentation
                           "Select model reasoning effort for this chat.")
                          ("Appearance" :menu rplaca-chat-appearance-menu
                           :documentation
                           "Stage, preview, apply, and save appearance profiles.")
                          ("System" :menu rplaca-chat-system-menu
                           :documentation
                           "Launch nested frames and system actions."))))
  (:pointer-documentation t)
  ;; Pinned McCLIM/CLX can retain pointer events for a transient submenu after
  ;; switching top-level categories disowns that submenu frame.  Keep the full
  ;; hierarchical command table above for M-x and keys, but attach a separate
  ;; public-CLIM leaf-only table to the visible bar so ordinary pointer motion
  ;; never creates or disowns transient submenu frames.
  (:menu-bar rplaca-chat-menu-bar)
  (:panes
   (transcript
    (let ((pane (clim:make-pane
                 'rplaca-transcript-pane
                 :display-function 'display-chat-transcript
                 :display-time :command-loop
                 :incremental-redisplay t
                 :end-of-page-action :allow
                 :text-style *rplaca-default-text-style*
                 :width 900
                 :height 640
                 :command-table 'rplaca-chat-frame)))
      (setf (esa:windows clim:*application-frame*) (list pane))
      pane))
   (info
    (clim:make-pane
     'rplaca-chat-info-pane
     :master-pane nil
     :display-function 'display-chat-info-pane
     :text-style *rplaca-default-text-style*
     :width 900))
   (compose
    (clim:make-pane
     'rplaca-chat-compose-pane
     :initial-contents ""
     :text-style *rplaca-default-text-style*
     :ncolumns 90
     :nlines 5
     ;; Keep the normal compose geometry in the pane's original CLIM space
     ;; requirement.  Runtime geometry changes during Drei repaint can reenter
     ;; layout with a partially updated displayed-line vector.
     :height (chat-compose-desired-pixel-height)
     :min-height (chat-compose-desired-pixel-height)
     :max-height (chat-compose-desired-pixel-height)
     :end-of-line-action :wrap*
     :minibuffer nil
     :scroll-bars nil
     :border-width 0
     :activation-gestures '(:return)
     :activate-callback #'compose-pane-activated))
   (minibuffer
    (clim:make-pane 'rplaca-chat-minibuffer-pane
                    :display-function 'display-chat-minibuffer-pane
                    :display-time :command-loop
                    :text-style *rplaca-default-text-style*
                    :width 900)))
  (:layouts
   (default
    (clim:vertically ()
      (clim:scrolling ()
        transcript)
      compose
      info
      minibuffer)))
  (:top-level (run-rplaca-chat-top-level)))

(defparameter +appearance-editor-buffer-name+ "*Appearance*")

(defparameter +appearance-editor-preview-roles+
  '(:default-text :transcript-user :transcript-agent :transcript-tool
    :transcript-system :modeline :selector-title :selector-header
    :selector-entry :selector-selection :system :disabled :error)
  "Core semantic roles sampled by the appearance editor.")

(defvar *customize-face-deprecation-reported-p* nil
  "Whether the compatibility command has emitted its sole process diagnostic.")

(defun appearance-command-frame (context)
  "Return the active chat frame for frame- or buffer-originated CONTEXT."
  (cond
    ((typep context 'rplaca-chat-frame) context)
    ((typep clim:*application-frame* 'rplaca-chat-frame)
     clim:*application-frame*)
    (t
     (error "Appearance commands require a running RPLACA chat frame."))))

(defun appearance-editor-profile (frame)
  "Return FRAME's staged profile, falling back to its active profile."
  (let ((candidate (chat-frame-appearance-staged-candidate frame)))
    (if candidate
        (appearance-candidate-profile candidate)
        (chat-frame-appearance-profile frame))))

(defun appearance-editor-staged-profile (frame)
  "Return FRAME's staged profile, or NIL when no candidate is staged."
  (let ((candidate (chat-frame-appearance-staged-candidate frame)))
    (and candidate (appearance-candidate-profile candidate))))

(defun appearance-editor-id-string (id)
  "Return the stable external spelling for a core or package appearance ID."
  (appearance-id-external-string id))

(defun appearance-editor-record-status (frame operation status &rest details)
  "Record and return one bounded structured editor operation result."
  (setf (chat-frame-appearance-editor-status frame)
        (list* :operation operation :status status details)))

(defun appearance-editor-theme-ids (frame)
  "Return FRAME's theme IDs in deterministic display order."
  (sort (mapcar #'appearance-theme-definition-id
                (appearance-catalog-theme-definitions
                 (chat-frame-appearance-catalog frame)))
        #'string< :key #'appearance-editor-id-string))

(defun appearance-editor-role-ids (frame)
  "Return FRAME's role IDs in deterministic display order."
  (sort (mapcar #'appearance-role-definition-id
                (appearance-catalog-role-definitions
                 (chat-frame-appearance-catalog frame)))
        #'string< :key #'appearance-editor-id-string))

(defun appearance-editor-font-choices (frame)
  "Return copied choices from FRAME's adopted font inventory."
  (let ((inventory (chat-frame-appearance-font-inventory frame)))
    (if inventory (appearance-font-inventory-choices inventory) nil)))

(defun appearance-editor-font-families (frame)
  "Return unique port font family names available to FRAME."
  (sort (remove-duplicates
         (mapcar #'enumerated-font-choice-family-display
                 (appearance-editor-font-choices frame))
         :test #'string=)
        #'string<))

(defun appearance-editor-font-faces (frame family)
  "Return unique port font faces available under FAMILY."
  (if (not (stringp family))
      nil
      (sort (remove-duplicates
             (loop :for choice :in (appearance-editor-font-choices frame)
                   :when (string= family
                                  (enumerated-font-choice-family-display choice))
                     :collect (enumerated-font-choice-face-display choice))
             :test #'string=)
            #'string<)))

(defun appearance-editor-font-sizes (frame family face)
  "Return unique sorted port font sizes available under FAMILY and FACE."
  (if (not (and (stringp family) (stringp face)))
      nil
      (sort (remove-duplicates
             (loop :for choice :in (appearance-editor-font-choices frame)
                   :when
                   (and (string=
                         family
                         (enumerated-font-choice-family-display choice))
                        (string=
                         face
                         (enumerated-font-choice-face-display choice)))
                     :collect (enumerated-font-choice-size choice))
             :test #'equal)
            #'<)))

(defun appearance-editor-stage-profile (frame profile operation)
  "Install an immutable candidate for PROFILE without changing active state."
  (call-with-package-appearance-user-edit
   (lambda ()
     (setf (slot-value frame 'appearance-staged-candidate)
           (make-appearance-candidate profile))
     (appearance-editor-record-status frame operation :staged)
     (when (chat-frame-appearance-editor-buffer frame)
       (notify-buffer-display-change
        (chat-frame-appearance-editor-buffer frame) :appearance-staged))
     (chat-frame-appearance-staged-candidate frame))))

(defun appearance-editor-resolved-font-style (frame profile role)
  "Return ROLE's already validated port text style for PROFILE, or NIL."
  (let ((entry (gethash role
                        (chat-frame-appearance-editor-font-styles frame))))
    (and entry
         (equal (getf entry :profile-key)
                (appearance-profile-structural-key profile))
         (getf entry :text-style))))

(defun switch-appearance-theme-command (frame theme)
  "Stage THEME for FRAME without activating it."
  (setf frame (appearance-command-frame frame))
  (let* ((theme (if (typep theme 'appearance-editor-ref)
                    (appearance-editor-ref-value theme)
                    theme))
         (profile (appearance-editor-profile frame)))
    (unless (find theme (appearance-editor-theme-ids frame) :test #'equal)
      (error-appearance-condition 'missing-appearance-parent
                                  :origin :appearance-editor :value theme))
    (appearance-editor-stage-profile
     frame
     (make-appearance-profile
      :selected-theme theme
      :strict-contrast (appearance-profile-strict-contrast profile)
      :role-overrides (appearance-profile-role-overrides profile))
     :switch-theme)))

(defun appearance-editor-stage-role-font (frame size)
  "Stage the selected role's named port font at SIZE."
  (call-with-package-appearance-user-edit
   (lambda ()
    (let* ((profile (appearance-editor-profile frame))
         (role (chat-frame-appearance-editor-role frame))
         (family (chat-frame-appearance-editor-font-family frame))
         (face (chat-frame-appearance-editor-font-face frame))
         (overrides (appearance-profile-role-overrides profile))
         (old-style (cdr (assoc role overrides :test #'equal)))
         (inventory (chat-frame-appearance-font-inventory frame)))
    (unless (and family face
                 (member size (appearance-editor-font-sizes frame family face)
                         :test #'equal))
      (error-appearance-condition
       'invalid-appearance-component :origin :appearance-editor
       :axis :font-choice :value (list family face size)))
    (let ((resolved-text-style
            (resolve-enumerated-font-choice
             inventory
             (make-enumerated-font-choice
              :family-display family :face-display face :size size)
             :medium (chat-frame-font-metric-medium frame)
             :scope role))
          (style
            (make-appearance-role-style
             :typography (make-appearance-typography-spec
                          :family family :face face :size size)
             :foreground-ink
             (if old-style
                 (appearance-role-style-foreground-ink old-style)
                 *appearance-unspecified*)
             :surface
             (if old-style
                 (appearance-role-style-surface old-style)
                 *appearance-unspecified*)
             :decoration
             (if old-style
                 (appearance-role-style-decoration old-style)
                 *appearance-unspecified*))))
      (let ((candidate
              (appearance-editor-stage-profile
               frame
               (make-appearance-profile
                :selected-theme (appearance-profile-selected-theme profile)
                :strict-contrast (appearance-profile-strict-contrast profile)
                :role-overrides
                (acons role style
                       (remove role overrides :key #'car :test #'equal)))
               :choose-font)))
        (setf (gethash role
                       (chat-frame-appearance-editor-font-styles frame))
              (list :profile-key
                    (appearance-profile-structural-key
                     (appearance-candidate-profile candidate))
                    :text-style resolved-text-style))
        candidate))))))

(defun appearance-editor-open-buffer (frame origin)
  "Build or focus FRAME's dedicated appearance presentation buffer."
  (let ((editor (chat-frame-appearance-editor-buffer frame)))
    (unless (and editor (member editor *buffer-ring* :test #'eq))
      (setf editor
            (make-buffer +appearance-editor-buffer-name+
                         :agent-name "appearance"
                         :kind :appearance-editor
                         :working-directory (buffer-working-directory origin))
            (chat-frame-appearance-editor-buffer frame) editor)
      (initialize-buffer-display-defaults editor)
      (setf (buffer-major-mode editor) "appearance-editor")
      (add-buffer-to-ring editor))
    (switch-to-buffer editor)
    editor))

(defun customize-appearance-command (buffer)
  "Open the owning frame's staged CLIM appearance editor."
  (let ((frame clim:*application-frame*))
    (unless (typep frame 'rplaca-chat-frame)
      (error "Customize Appearance requires a running RPLACA chat frame."))
    (unless (chat-frame-appearance-staged-candidate frame)
      (appearance-editor-stage-profile
       frame (chat-frame-appearance-profile frame) :customize))
    (appearance-editor-open-buffer frame buffer)))

(defun customize-face-command (buffer)
  "Deprecated compatibility forwarder for CUSTOMIZE-APPEARANCE-COMMAND."
  (unless *customize-face-deprecation-reported-p*
    (setf *customize-face-deprecation-reported-p* t)
    (file-debug-event
     "deprecated-command"
     :command 'customize-face-command
     :replacement 'customize-appearance-command))
  (customize-appearance-command buffer))

(defun apply-staged-appearance-command (frame)
  "Queue FRAME's complete staged candidate for transactional activation."
  (setf frame (appearance-command-frame frame))
  (let ((candidate (chat-frame-appearance-staged-candidate frame)))
    (cond
      ((null candidate)
       (appearance-editor-record-status frame :apply :no-staged-candidate))
      ((request-chat-frame-appearance-activation frame candidate)
       (appearance-editor-record-status frame :apply :queued))
      (t
       (appearance-editor-record-status frame :apply :not-running)))))

(defun save-appearance-command (frame)
  "Atomically persist FRAME's staged profile without activating it."
  (setf frame (appearance-command-frame frame))
  (call-with-package-appearance-user-edit
   (lambda ()
    (let ((candidate (chat-frame-appearance-staged-candidate frame)))
    (if (null candidate)
        (appearance-editor-record-status frame :save :no-staged-candidate)
        (handler-case
            (let ((profile (appearance-candidate-profile candidate)))
              (save-staged-appearance-profile profile)
              (setf (chat-frame-appearance-persisted-profile frame) profile)
              (appearance-editor-record-status frame :save :saved
                                               :path (appearance-config-pathname)))
          (error (condition)
            (appearance-editor-record-status
             frame :save :failed :diagnostic
             (princ-to-string condition)))))))))

(defun revert-staged-appearance-command (frame)
  "Discard FRAME's staged candidate while retaining active state."
  (setf frame (appearance-command-frame frame))
  (call-with-package-appearance-user-edit
   (lambda ()
     (setf (slot-value frame 'appearance-staged-candidate) nil)
     (appearance-editor-record-status frame :revert :reverted))))

(defun reload-appearance-file-command (frame)
  "Reload a valid profile into staging, retaining active and staged state on failure."
  (setf frame (appearance-command-frame frame))
  (call-with-package-appearance-user-edit
   (lambda ()
    (let ((active (chat-frame-appearance-profile frame))
        (old-staged (chat-frame-appearance-staged-candidate frame)))
    (multiple-value-bind (profile loaded-p)
        (reload-appearance-file-profile active)
      (if loaded-p
          (progn
            (setf (chat-frame-appearance-persisted-profile frame) profile)
            (appearance-editor-stage-profile frame profile :reload)
            (appearance-editor-record-status frame :reload :loaded))
          (progn
            (setf (slot-value frame 'appearance-staged-candidate) old-staged)
            (appearance-editor-record-status
             frame :reload :retained-active))))))))

(defun describe-current-appearance-command (frame)
  "Return a structured distinction between active, staged, and persisted state."
  (setf frame (appearance-command-frame frame))
  (handler-case
      (multiple-value-bind (disk-profile disk-status)
          (read-appearance-profile-file)
        (when (eq disk-status :valid)
          (call-with-package-appearance-user-edit
           (lambda ()
             (setf (chat-frame-appearance-persisted-profile frame)
                   disk-profile))))
        (let ((description
                (list :active (chat-frame-appearance-profile frame)
                      :staged (let ((candidate
                                      (chat-frame-appearance-staged-candidate frame)))
                                (and candidate
                                     (appearance-candidate-profile candidate)))
                      :persisted (or disk-profile
                                     (chat-frame-appearance-persisted-profile frame))
                      :persisted-status disk-status
                      :activation
                      (chat-frame-appearance-last-activation-result frame)
                      :diagnostics
                      (append
                       (chat-frame-appearance-activation-diagnostics frame)
                       (chat-frame-appearance-font-refresh-diagnostics frame)))))
          (appearance-editor-record-status frame :describe :described
                                           :description description)
          description))
    (error (condition)
      (let ((description
              (list :active (chat-frame-appearance-profile frame)
                    :staged (let ((candidate
                                    (chat-frame-appearance-staged-candidate frame)))
                              (and candidate
                                   (appearance-candidate-profile candidate)))
                    :persisted (chat-frame-appearance-persisted-profile frame)
                    :persisted-status :invalid
                    :diagnostic (princ-to-string condition))))
        (appearance-editor-record-status frame :describe :invalid-file
                                         :description description)
        description))))

(defun appearance-editor-profile-label (profile)
  "Return a concise stable label for PROFILE."
  (if profile
      (format nil "~A~:[~; (strict contrast)~]"
              (appearance-editor-id-string
               (appearance-profile-selected-theme profile))
              (appearance-profile-strict-contrast profile))
      "none"))

(defun appearance-editor-diagnostic-text (diagnostic)
  "Return bounded display text for a structured appearance DIAGNOSTIC."
  (format nil "~(~A~): ~A"
          (type-of diagnostic)
          (or (appearance-condition-value diagnostic)
              (appearance-condition-axis diagnostic)
              "no details")))

(defun appearance-editor-display-entries (buffer &optional columns)
  "Return ordinary CLIM presentation entries for BUFFER's owning frame."
  (declare (ignore columns))
  (let ((frame clim:*application-frame*))
    (unless (and (typep frame 'rplaca-chat-frame)
                 (eq buffer (chat-frame-appearance-editor-buffer frame)))
      (return-from appearance-editor-display-entries
        (list (list :text "[Appearance editor is not attached to this frame.]"
                    :face :error))))
    (let* ((active (chat-frame-appearance-profile frame))
           (preview-profile (appearance-editor-profile frame))
           (staged (appearance-editor-staged-profile frame))
           (persisted (chat-frame-appearance-persisted-profile frame))
           (status (chat-frame-appearance-editor-status frame))
           (activation (chat-frame-appearance-last-activation-result frame))
           (role (chat-frame-appearance-editor-role frame))
           (family (chat-frame-appearance-editor-font-family frame))
           (face (chat-frame-appearance-editor-font-face frame))
           (entries
             (list
              (list :text "Appearance"
                    :face :selector-title
                    :unique-id :appearance-title)
              (list :text "Select a theme or role. Apply is transactional; Save persists staging."
                    :face :selector-footer
                    :unique-id :appearance-help)
              (list :text (format nil "Active: ~A"
                                  (appearance-editor-profile-label active))
                    :face :selector-header
                    :unique-id :appearance-active)
              (list :text (format nil "Staged: ~A"
                                  (appearance-editor-profile-label staged))
                    :face :selector-header
                    :unique-id :appearance-staged)
              (list :text (format nil "Persisted: ~A"
                                  (appearance-editor-profile-label persisted))
                    :face :selector-header
                    :unique-id :appearance-persisted)
              (list :text (format nil "Last operation: ~S" status)
                    :face :system
                    :unique-id :appearance-status)
              (list :text
                    (format nil "Activation: ~(~A~)"
                            (if activation
                                (appearance-activation-result-status activation)
                                :not-requested))
                    :face
                    (if (and activation
                             (eq :failed
                                 (appearance-activation-result-status activation)))
                        :error
                        :system)
                    :unique-id :appearance-activation-status)
              (list :text "Actions"
                    :face :selector-title
                    :unique-id :appearance-actions))))
      (dolist (action '((:apply . "Apply staged appearance")
                        (:save . "Save staged appearance")
                        (:revert . "Revert staged appearance")
                        (:reload . "Reload appearance file")
                        (:refresh-fonts . "Refresh port font inventory")))
        (setf entries
              (nconc entries
                     (list
                      (list :text (cdr action)
                            :face :selector-entry
                            :object
                            (make-appearance-editor-ref frame :activation
                                                        (car action))
                            :presentation-type 'appearance-activation-ref
                            :unique-id (list :appearance-action (car action)))))))
      (setf entries
            (nconc entries
                   (list (list :text "Themes" :face :selector-title
                               :unique-id :appearance-themes))))
      (dolist (theme (appearance-editor-theme-ids frame))
        (setf entries
              (nconc entries
                     (list
                      (list :text
                            (format nil "~:[  ~;> ~]~A"
                                    (equal theme
                                           (appearance-profile-selected-theme
                                            preview-profile))
                                    (appearance-editor-id-string theme))
                            :face (if (equal theme
                                             (appearance-profile-selected-theme
                                              preview-profile))
                                      :selector-selection
                                      :selector-entry)
                            :object (make-appearance-editor-ref frame :theme theme)
                            :presentation-type 'appearance-theme-ref
                            :unique-id (list :appearance-theme theme))))))
      (setf entries
            (nconc entries
                   (list (list :text "Preview roles"
                               :face :selector-title
                               :unique-id :appearance-preview))))
      (dolist (preview-role +appearance-editor-preview-roles+)
        (setf entries
              (nconc entries
                     (list
                      (list :text (format nil "~:[  ~;> ~]~(~A~): The quick brown fox 0123"
                                          (equal preview-role role) preview-role)
                            :face preview-role
                            :appearance-profile preview-profile
                            :role-stack (list :transcript-pane preview-role)
                            :resolved-text-style
                            (appearance-editor-resolved-font-style
                             frame preview-profile preview-role)
                            :object
                            (make-appearance-editor-ref frame :role preview-role)
                            :presentation-type 'appearance-role-ref
                            :unique-id (list :appearance-role preview-role))))))
      (setf entries
            (nconc entries
                   (list
                    (list :text
                          (format nil "Port fonts for ~(~A~)~@[ — ~A~]~@[ / ~A~]"
                                  role family face)
                          :face :selector-title
                          :unique-id :appearance-fonts))))
      (cond
        ((null (appearance-editor-font-choices frame))
         (setf entries
               (nconc entries
                      (list (list :text "Refresh the port font inventory to choose a named font."
                                  :face :disabled
                                  :unique-id :appearance-no-fonts)))))
        ((null family)
         (dolist (choice (appearance-editor-font-families frame))
           (setf entries
                 (nconc entries
                        (list
                         (list :text choice :face :selector-entry
                               :object
                               (make-appearance-editor-ref
                                frame :font-family choice)
                               :presentation-type
                               'appearance-port-font-family-ref
                               :unique-id (list :appearance-font-family choice)))))))
        ((null face)
         (dolist (choice (appearance-editor-font-faces frame family))
           (setf entries
                 (nconc entries
                        (list
                         (list :text choice :face :selector-entry
                               :object
                               (make-appearance-editor-ref frame :font-face choice)
                               :presentation-type 'appearance-port-font-face-ref
                               :unique-id
                               (list :appearance-font-face family choice)))))))
        (t
         (dolist (choice (appearance-editor-font-sizes frame family face))
           (setf entries
                 (nconc entries
                        (list
                         (list :text (format nil "~A" choice)
                               :face :selector-entry
                               :object
                               (make-appearance-editor-ref frame :font-size choice)
                               :presentation-type 'appearance-port-font-size-ref
                               :unique-id
                               (list :appearance-font-size
                                     family face choice))))))))
      (let ((diagnostics
              (append (chat-frame-appearance-activation-diagnostics frame)
                      (chat-frame-appearance-font-refresh-diagnostics frame))))
        (when diagnostics
          (setf entries
                (nconc entries
                       (list (list :text "Diagnostics" :face :selector-title
                                   :unique-id :appearance-diagnostics))))
          (loop :for diagnostic :in diagnostics
                :for index :from 0
                :do
                   (setf entries
                         (nconc entries
                                (list
                                 (list
                                  :text
                                  (appearance-editor-diagnostic-text diagnostic)
                                  :face :error
                                  :object
                                  (make-appearance-editor-ref
                                   frame :diagnostic diagnostic)
                                  :presentation-type
                                  'appearance-diagnostic-ref
                                  :unique-id
                                  (list :appearance-diagnostic index))))))))
      entries)))

(register-buffer-type
 :appearance-editor
 :description "Frame-local staged CLIM appearance editor."
 :major-mode "appearance-editor"
 :presentation-function 'appearance-editor-display-entries)

(register-command-metadata 'customize-appearance-command)
(register-command-metadata 'customize-face-command)
(register-command-metadata
 'switch-appearance-theme-command
 :prompts '((theme :prompt "Appearance theme"
                   :reader parse-appearance-theme-selector)))
(register-command-metadata 'describe-current-appearance-command)
(register-command-metadata 'apply-staged-appearance-command)
(register-command-metadata 'save-appearance-command)
(register-command-metadata 'revert-staged-appearance-command)
(register-command-metadata 'reload-appearance-file-command)

(defvar *chat-graphical-debugger-enabled-p* t
  "Offer McCLIM's live graphical debugger for errors in a running chat frame.
Set to NIL for unattended embedding; ordinary error containment remains active.")

(defvar *chat-graphical-debugger-active-p* nil
  "Prevent a debugger failure from recursively opening another debugger.")

(defun chat-frame-debugger-available-p (frame)
  "Return true when FRAME has a usable, owning graphical event process."
  (and *chat-graphical-debugger-enabled-p*
       (not *chat-graphical-debugger-active-p*)
       (eq clim:*application-frame* frame)
       ;; McCLIM's process ownership is stronger than a dynamic frame binding,
       ;; which an embedding caller could also establish on a worker thread.
       (eq (climi::frame-process frame) (clim-sys:current-process))
       (eq (chat-frame-lifecycle-state frame) :running)
       (chat-frame-grafted-top-level-sheet frame)))

(defun report-chat-frame-ui-error (frame action condition &key (notify-p t))
  "Record an abandoned action without letting reporting cause another failure."
  (let* ((action-text (handler-case (format nil "~(~A~)" action)
                        (error () "unknown UI action")))
         (condition-text
           (let ((*print-circle* t)
                 (*print-level* 6)
                 (*print-length* 20))
             (handler-case (format nil "~A" condition)
               (error () "unprintable error"))))
         (diagnostic-text (if (> (length condition-text) 240)
                              (concatenate 'string (subseq condition-text 0 240) "...")
                              condition-text)))
    (ignore-errors
      (file-debug-event "ui-action-error" :action action-text
                        :condition condition-text))
    (ignore-errors
      (let ((buffer (chat-frame-buffer frame)))
        (when buffer
          (setf (buffer-status buffer) :error)
          ;; A display error must not notify its own failing display forever.
          (let ((*suppress-chat-redisplay-requests*
                  (or *suppress-chat-redisplay-requests* (not notify-p))))
            (buffer-insert-read-only-message
             buffer :system (format nil "[UI action failed (~A): ~A]"
                            action-text diagnostic-text)
             :record-p nil :run-hook-p nil :notify-p notify-p)))))
    (when notify-p
      (ignore-errors (request-chat-frame-redisplay frame)))
    nil))

(defun call-chat-frame-ui-action-safely (frame action function &key (notify-p t))
  "Run ACTION with live graphical restarts and a nonrecursive error fallback.

HANDLER-BIND preserves the failing stack and CLIM's pane recovery restarts.
The native debugger runs on that same thread. Closing it invokes the local
ABORT restart, returning to the application without retrying domain effects.
Headless callers and failures of the debugger use ordinary error containment."
  (let ((*chat-interaction-state* (chat-frame-interaction-state frame))
        (failure nil))
    (flet ((report-failure (condition)
             (report-chat-frame-ui-error frame action condition :notify-p notify-p)))
      (handler-case
          (restart-case
              (handler-bind
                  ((error
                     (lambda (condition)
                       (setf failure condition)
                       (when (chat-frame-debugger-available-p frame)
                         (let ((*chat-graphical-debugger-active-p* t))
                           (ignore-errors
                             (file-debug-event "graphical-debugger-entered"
                                               :action action
                                               :condition-type (type-of condition)))
                           (handler-case
                               (clim-debugger:debugger condition nil)
                             (error ()
                               ;; Preserve the original condition if the
                               ;; graphical debugger itself cannot display it.
                               (ignore-errors
                                 (file-debug-event "graphical-debugger-failed"
                                                   :action action))
                               nil)))))))
                (funcall function))
            (abort ()
              :report "Return to RPLACA without retrying the failed action."
              (when failure (report-failure failure))))
        (error (condition) (report-failure condition))))))

(defmethod clim:redisplay-frame-pane :around
    ((frame rplaca-chat-frame) pane &key force-p)
  "Keep CLIM's retry/skip restarts live for graphical pane error recovery."
  (declare (ignore force-p))
  (call-chat-frame-ui-action-safely
   frame (format nil "display ~A" (if (symbolp pane) pane (clim:pane-name pane)))
   (lambda () (call-next-method)) :notify-p nil))

(defmethod clim:execute-frame-command :around
    ((frame rplaca-chat-frame) command)
  "Keep an ordinary command error inside FRAME's running CLIM command loop."
  (call-chat-frame-ui-action-safely
   frame
   (if (and (consp command) (symbolp (car command)))
       (format nil "frame command ~A" (car command))
       "frame command")
   (lambda () (call-next-method))))

(defmethod (setf clim:frame-command-table) :around
    (new-table (frame rplaca-chat-frame))
  "Avoid rebuilding FRAME's live menu bar for an identical command table.

Pinned ESA assigns its applicable command table on every command-loop turn.
McCLIM treats even an EQ assignment as a request to disown and recreate every
menu gadget.  Besides doing unnecessary work, that can leave already queued
pointer events referring to the disowned gadgets.  A genuinely fresh
frame-local table still goes through the standard CLIM setter and menu update."
  (if (eq new-table (clim:frame-command-table frame))
      new-table
      (call-next-method)))

(defmethod initialize-instance :after ((frame rplaca-chat-frame) &key)
  "Keep ESA frame slots safely initialized before panes are generated."
  (unless (slot-boundp frame 'esa:windows)
    (setf (esa:windows frame) nil)))

(defmethod clim:frame-standard-input ((frame rplaca-chat-frame))
  "Use the ESA minibuffer as FRAME's standard input stream when it exists."
  (or (ignore-errors (clim:find-pane-named frame 'minibuffer))
      (call-next-method)))

(defmethod esa:buffers ((frame rplaca-chat-frame))
  "Return the RPLACA buffers visible to the ESA command processor."
  (remove-duplicates
   (remove nil (cons (chat-frame-buffer frame) *buffer-ring*))
   :test #'eq))

(defmethod esa:esa-current-buffer ((frame rplaca-chat-frame))
  "Return FRAME's current RPLACA buffer."
  (chat-frame-buffer frame))

(defun restore-chat-frame-source-after-load-error
    (frame compose source load-error)
  "Restore FRAME, ring, and COMPOSE to SOURCE, then re-signal LOAD-ERROR."
  (let ((restore-error nil))
    (when source
      (unless (member source *buffer-ring* :test #'eq)
        (add-buffer-to-ring source))
      (switch-to-buffer source)
      (setf (chat-frame-buffer frame) source)
      (handler-case
          (progn
            (sync-chat-compose-pane-from-buffer compose source :force t)
            (setf (chat-frame-compose-synchronized-buffer frame) source))
        (error (condition)
          (setf restore-error condition
                (chat-frame-compose-synchronized-buffer frame) nil))))
    (request-chat-frame-redisplay frame)
    (if restore-error
        (error "Target compose load failed (~A); restoring the source compose state also failed (~A)."
               load-error restore-error)
        (error load-error))))

(defmethod (setf esa:esa-current-buffer) ((new-buffer buffer)
                                          (frame rplaca-chat-frame))
  "Synchronize FRAME's compose editor and switch it to NEW-BUFFER.

This is the canonical frame-level transition contract for built-in commands
and extensions.  It saves the old frame buffer, installs the new draft in
Drei, adopts the new buffer into the process-level ring, and requests CLIM
redisplay.  Loading a genuinely different buffer deliberately resets the
single shared Drei gadget's undo history so undo cannot cross buffers."
  (let* ((old-buffer (chat-frame-buffer frame))
         (changed-p (not (eq old-buffer new-buffer)))
         (helper-synchronized-p
           (eq old-buffer *chat-frame-transition-synchronized-buffer*))
         (compose
           (or *chat-frame-transition-compose-pane*
               (clim:find-pane-named frame 'compose))))
    (cond
      ;; A direct same-buffer assignment means "adopt the live editor".  Save
      ;; it and do not invoke Drei's destructive replacement setter.
      ((and (not changed-p) (not helper-synchronized-p))
       (sync-chat-buffer-input-from-compose-pane compose old-buffer)
       (setf (chat-frame-compose-synchronized-buffer frame) old-buffer))
      (t
       (when (and changed-p old-buffer (not helper-synchronized-p))
         (sync-chat-buffer-input-from-compose-pane compose old-buffer)
         (setf (chat-frame-compose-synchronized-buffer frame) old-buffer))
       ;; Prepare the visible editor before publishing NEW-BUFFER as frame or
       ;; ring state.  A failed or partially applied Drei replacement rolls
       ;; every application-owned current-buffer representation back to OLD.
       (handler-case
           (progn
             (sync-chat-compose-pane-from-buffer
              compose new-buffer :force changed-p)
             (setf (chat-frame-compose-synchronized-buffer frame) new-buffer))
         (error (condition)
           (restore-chat-frame-source-after-load-error
            frame compose old-buffer condition)))))
    (unless (member new-buffer *buffer-ring* :test #'eq)
      (add-buffer-to-ring new-buffer))
    (switch-to-buffer new-buffer)
    (setf (chat-frame-buffer frame) new-buffer)
    (request-chat-frame-redisplay frame))
  new-buffer)

(defmethod esa:esa-current-window ((frame rplaca-chat-frame))
  "Return the current ESA window, falling back to FRAME before panes exist."
  (or (first (ignore-errors (esa:windows frame)))
      (ignore-errors (clim:find-pane-named frame 'transcript))
      frame))

(defmethod (setf esa:previous-command) (command (frame rplaca-chat-frame))
  "Accept ESA's previous-command update before concrete window panes exist."
  command)

(defmethod esa:find-applicable-command-table ((frame rplaca-chat-frame))
  "Use FRAME's stable application command table for ESA lookup and M-x."
  (clim:frame-command-table frame))

(defun focus-chat-compose-pane (frame)
  "Give keyboard focus to FRAME's compose pane when it is available."
  (let ((compose (ignore-errors (clim:find-pane-named frame 'compose))))
    (when compose
      (ignore-errors
        (clim:stream-set-input-focus compose))
      compose)))

(defun focus-chat-frame-initial-input-pane (frame)
  "Focus FRAME's normal compose pane or an explicitly E2E-requested CLIM stream.

The override is accepted only while structured E2E events are enabled.  It lets
the external GUI harness start from ESA's standard-input ownership and prove
that opening a non-blocking selector transfers ownership back to Drei, without
changing ordinary application startup."
  (let* ((requested
           (and (e2e-events-enabled-p)
                (uiop:getenv "RPLACA_GUI_E2E_INITIAL_INPUT_FOCUS")))
         (pane
           (cond
             ((and requested (string-equal requested "standard-input"))
              (ignore-errors (clim:frame-standard-input frame)))
             ((and requested (string-equal requested "transcript"))
              (ignore-errors (clim:find-pane-named frame 'transcript)))
             (t nil))))
    (if pane
        (progn
          (ignore-errors (clim:stream-set-input-focus pane))
          pane)
        (focus-chat-compose-pane frame))))

(defmethod clim:execute-frame-command :after
    ((frame rplaca-chat-frame) command)
  "Keep non-blocking RPLACA interaction input on FRAME's compose pane.

The visible minibuffer pane is an application pane: it displays semantic
completion presentations, while the Drei compose pane deliberately owns the
non-blocking keyboard event adapter.  Commands may be invoked through ESA's
standard input stream, menus, or presentations with some other pane focused.
When such a command activates modal interaction, return focus to the pane that
implements that input contract before the next gesture is delivered."
  (declare (ignore command))
  ;; ESA redisplays frame panes immediately after the effective command method
  ;; returns.  Apply interaction layout here, before that redisplay, rather than
  ;; waiting for the separately queued asynchronous refresh.
  (update-chat-minibuffer-space-requirements frame)
  (when (chat-compose-application-input-active-p (chat-frame-buffer frame))
    (focus-chat-compose-pane frame)))

(defun initialize-chat-frame-top-level-panes (frame)
  "Initialize FRAME's panes after CLIM has generated and adopted them."
  ;; CLIM generates and adopts the declared panes before invoking the
  ;; application top-level.  Hydrate Drei only now: FIND-PANE-NAMED returns NIL
  ;; in the outer RUN-FRAME-TOP-LEVEL :AROUND method, before the primary CLIM
  ;; lifecycle has created the panes.
  (let ((transcript (clim:find-pane-named frame 'transcript)))
    (setf (esa:windows frame) (and transcript (list transcript))))
  (initialize-chat-frame-compose-pane
   frame (clim:find-pane-named frame 'compose)))

(defun run-rplaca-chat-top-level (frame)
  "Run FRAME with ESA command processing and compose focused initially."
  (unless (eq (clim:frame-state frame) :enabled)
    (clim:enable-frame frame)
    (file-debug-event "frame-enabled"
                      :buffer-name (buffer-name (chat-frame-buffer frame))
                      :state (clim:frame-state frame)))
  ;; Ordinary MAKE-APPLICATION-FRAME construction is profile-only.  The
  ;; top-level sheet is now adopted and grafted, so this is the first legal
  ;; point to obtain the selected port through the public frame-manager path.
  ;; The bundle is frame-local data only; it does not change panes or mappings.
  (refresh-chat-frame-appearance-port-bundle frame)
  (initialize-chat-frame-top-level-panes frame)
  (bt:with-lock-held ((chat-frame-lifecycle-lock frame))
    (setf (chat-frame-lifecycle-state frame) :running))
  ;; Construction-time changes already live in the buffer. Queue their first
  ;; application update only after CLIM has adopted the panes.
  (request-chat-frame-redisplay frame)
  (focus-chat-frame-initial-input-pane frame)
  (file-debug-event "frame-ready"
                    :buffer-name (buffer-name (chat-frame-buffer frame))
                    :state (clim:frame-state frame))
  (emit-chat-frame-e2e-snapshot frame :reason "frame-ready")
  (esa:esa-top-level frame))

(defun chat-appearance-wire-role-stack (face)
  "Return the exact appearance role stack for legacy presentation FACE.

Unknown runtime faces deliberately remain in the returned stack.  The pure
runtime resolver maps them to :DEFAULT-TEXT and produces a catalog-generation
keyed diagnostic that this frame records once."
  (case face
    (:default-text '(:default-text))
    (:selector-title '(:selector-title))
    (:selector-header '(:selector-header))
    (:selector-entry '(:selector-entry))
    (:selector-selected '(:selector-entry :selector-selection))
    (:selector-separator '(:selector-separator))
    (:selector-footer '(:selector-footer))
    (:tool-result '(:tool-result))
    (:system '(:system))
    (:disabled '(:default-text :disabled))
    (:error '(:error))
    (t (list face))))

(defun chat-frame-resolve-appearance-role (frame role-stack)
  "Resolve ROLE-STACK through FRAME's immutable appearance state.

The cache and all diagnostic de-duplication are frame-local.  Cache entries
are keyed by semantic roles, while callers use the resolver's structural key
for incremental redisplay so an unrelated role never invalidates this output."
  (let* ((roles (copy-list role-stack))
         (cache (chat-frame-appearance-resolved-roles frame))
         (missing (gensym "MISSING"))
         (cached (gethash roles cache missing)))
    (if (not (eq cached missing))
        cached
        (let* ((profile (chat-frame-appearance-profile frame))
               (resolved
                 (resolve-runtime-appearance-role-stack
                  (chat-frame-appearance-catalog frame)
                  (appearance-profile-selected-theme profile)
                  roles
                  :unsaved-overrides
                  (appearance-profile-role-overrides profile))))
          (setf (gethash roles cache) resolved
                (gethash roles (chat-frame-appearance-role-keys frame))
                (resolved-appearance-role-structural-key resolved))
          (dolist (diagnostic (resolved-appearance-role-diagnostics resolved))
            (let ((key (appearance-diagnostic-deduplication-key diagnostic)))
              (unless (gethash key (chat-frame-appearance-runtime-diagnostic-keys frame))
                (setf (gethash key
                               (chat-frame-appearance-runtime-diagnostic-keys frame))
                      t)
                (push diagnostic (chat-frame-appearance-runtime-diagnostics frame)))))
          resolved))))

(defun chat-frame-appearance-role-key (frame role-stack)
  "Return FRAME's structural incremental-redisplay key for ROLE-STACK."
  (let ((roles (copy-list role-stack)))
    (chat-frame-resolve-appearance-role frame roles)
    (copy-tree (gethash roles (chat-frame-appearance-role-keys frame)))))

(defun appearance-foreground-ink (foreground)
  "Translate the persisted portable RGB representation to a CLIM ink."
  (cond
    ((and (listp foreground)
          (= (length foreground) 3)
          (every #'realp foreground))
     (apply #'clim:make-rgb-color foreground))
    ((and (listp foreground)
          (eq (first foreground) :rgb)
          (= (length foreground) 4)
          (every #'realp (rest foreground)))
     (apply #'clim:make-rgb-color (rest foreground)))
    (t foreground)))

(defun resolved-appearance-text-style (resolved)
  "Return a portable partial CLIM text style for RESOLVED, or NIL.

NIL components intentionally inherit the target stream's existing text style;
this is the standard CLIM composition used by WITH-TEXT-STYLE."
  (let ((typography
          (appearance-role-style-typography
           (resolved-appearance-role-style resolved))))
    (unless (appearance-unspecified-p typography)
      (let ((family (appearance-typography-spec-family typography))
            (face (appearance-typography-spec-face typography))
            (size (appearance-typography-spec-size typography)))
        (unless (and (appearance-unspecified-p family)
                     (appearance-unspecified-p face)
                     (appearance-unspecified-p size))
          (list (if (appearance-unspecified-p family) nil family)
                (if (appearance-unspecified-p face) nil face)
                (if (appearance-unspecified-p size) nil size)))))))

(defun call-with-chat-appearance-role (frame stream role-stack function)
  "Call FUNCTION with FRAME's resolved ROLE-STACK bound at STREAM's output edge."
  (let* ((resolved (chat-frame-resolve-appearance-role frame role-stack))
         (style (resolved-appearance-role-style resolved))
         (ink-spec (appearance-role-style-foreground-ink style))
         (foreground (and (not (appearance-unspecified-p ink-spec))
                          (appearance-ink-spec-foreground ink-spec)))
         (text-style (resolved-appearance-text-style resolved)))
    (labels ((call-with-text-style ()
               (if text-style
                   (clim:with-text-style (stream text-style)
                     (funcall function))
                   (funcall function))))
      (if (appearance-unspecified-p foreground)
          (call-with-text-style)
          (clim:with-drawing-options
              (stream :ink (appearance-foreground-ink foreground))
            (call-with-text-style))))))

(defun call-with-appearance-profile-role
    (frame stream profile role-stack function &optional resolved-text-style)
  "Call FUNCTION using PROFILE's resolved role without publishing frame state."
  (let* ((resolved
           (resolve-runtime-appearance-role-stack
            (chat-frame-appearance-catalog frame)
            (appearance-profile-selected-theme profile)
            role-stack
            :unsaved-overrides (appearance-profile-role-overrides profile)))
         (style (resolved-appearance-role-style resolved))
         (ink-spec (appearance-role-style-foreground-ink style))
         (foreground (and (not (appearance-unspecified-p ink-spec))
                          (appearance-ink-spec-foreground ink-spec)))
         (typography (appearance-role-style-typography style))
         (text-style
           (or resolved-text-style
               (unless (appearance-unspecified-p typography)
             (let ((family (appearance-typography-spec-family typography))
                   (face (appearance-typography-spec-face typography))
                   (size (appearance-typography-spec-size typography)))
               ;; Named font display strings must cross the explicit
               ;; port-inventory resolver before reaching a rendering call.
               (unless (or (stringp family)
                           (stringp face)
                           (and (appearance-unspecified-p family)
                                (appearance-unspecified-p face)
                                (appearance-unspecified-p size)))
                 (list (if (appearance-unspecified-p family) nil family)
                       (if (appearance-unspecified-p face) nil face)
                       (if (appearance-unspecified-p size) nil size))))))))
    (labels ((emit ()
               (if text-style
                   (clim:with-text-style (stream text-style)
                     (funcall function))
                   (funcall function))))
      (if (appearance-unspecified-p foreground)
          (emit)
          (clim:with-drawing-options
              (stream :ink (appearance-foreground-ink foreground))
            (emit))))))

(defun chat-message-appearance-role-stack (msg)
  "Return the transcript role stack for MSG."
  (list :transcript-pane
        (ecase (chat-message-kind msg)
          (:user :transcript-user)
          (:agent :transcript-agent)
          (:tool :transcript-tool)
          (:system :transcript-system))))

(defun display-chat-message (frame stream msg)
  "Display MSG as one chat-message presentation on STREAM."
  (let ((sender (chat-message-label msg))
        (text (message-text msg))
        (button (agent-lisp-button-spec msg)))
    (clim:with-output-as-presentation
        (stream msg 'chat-message :single-box t)
      (call-with-chat-appearance-role
       frame stream (chat-message-appearance-role-stack msg)
       (lambda ()
         (format stream "~A>~%" sender)
         (write-string text stream)
         (when button
           (terpri stream)
           (display-agent-lisp-button frame stream button)))))
    (terpri stream)
    (terpri stream)))

(defun attach-agent-lisp-button-tool (args)
  "Attach an agent-created Lisp button to the current chat transcript."
  (let ((buffer *current-tool-buffer*)
        (label (tool-arg args :label "label"))
        (code (tool-arg args :code "code"))
        (package (or (tool-arg args :package "package") "RPLACA")))
    (unless buffer
      (error "attach_lisp_button requires an active chat buffer"))
    (unless (and (stringp label) (plusp (length label)))
      (error "attach_lisp_button requires a non-empty label"))
    (unless (and (stringp code) (plusp (length code)))
      (error "attach_lisp_button requires non-empty Lisp code"))
    (unless (and (stringp package) (find-package package))
      (error "Unknown Lisp package ~S" package))
    (buffer-insert-system-message
     buffer
     "Agent action available:"
     :metadata
     (list
      (cons +agent-lisp-button-metadata-key+
            `((:label . ,(copy-seq label))
              (:code . ,(copy-seq code))
              (:package . ,(copy-seq package))))))
    (lisp-data-string
     (list :status :attached :label label :package package))))

(deftool attach-agent-lisp-button-tool
  :name "attach_lisp_button"
  :description "Attach a real clickable CLIM push button to the transcript. When the user clicks it, RPLACA evaluates the supplied arbitrary Common Lisp code in the live UI process. The RPLACA container is the authority boundary; no approval or Lisp allowlist is applied."
  :call-style :raw-args
  :execution :frame
  :args ((label :type "string"
                :description "Visible button label.")
         (code :type "string"
               :description "Arbitrary Common Lisp code evaluated only when the user clicks the button.")
         (package :type "string"
                  :required nil
                  :description "Package used to read and evaluate code. Default: RPLACA.")))

(defun open-agent-window-tool (args)
  "Open an independent text window requested by the current agent."
  (let ((title (tool-arg args :title "title"))
        (content (tool-arg args :content "content")))
    (unless (and (stringp title) (plusp (length title)))
      (error "open_window requires a non-empty title"))
    (unless (stringp content)
      (error "open_window requires string content"))
    (unless (open-agent-window title content)
      (error "Unable to open agent window ~S" title))
    (lisp-data-string
     (list :status :opened :title title))))

(deftool open-agent-window-tool
  :name "open_window"
  :description "Open a new independent RPLACA/McCLIM application window with an agent-supplied title and text content. The window is owned by its own frame top-level and remains independent of the chat frame."
  :call-style :raw-args
  :execution :frame
  :args ((title :type "string"
                :description "Window title.")
         (content :type "string"
                  :description "Text displayed in the new window.")))

(defun chat-tool-activity-summary-text (summary)
  "Return the collapsed display text for SUMMARY."
  (with-output-to-string (stream)
    (format stream "tools> ~D tool message~:P collapsed"
            (length (chat-tool-activity-summary-messages summary)))
    (let ((counts (chat-tool-activity-summary-tool-counts summary)))
      (if counts
          (dolist (entry counts)
            (format stream "~%  ~A × ~D" (car entry) (cdr entry)))
          (format stream "~%  no tool calls recorded")))
    (when (plusp (chat-tool-activity-summary-result-count summary))
      (format stream "~%  ~D tool result~:P"
              (chat-tool-activity-summary-result-count summary)))))

(defun display-chat-tool-activity-summary (frame stream summary)
  "Display SUMMARY as one collapsed tool-activity presentation."
  (clim:with-output-as-presentation
      (stream summary 'tool-activity-summary :single-box t)
    (call-with-chat-appearance-role
     frame stream '(:transcript-pane :transcript-tool)
     (lambda ()
       (write-string (chat-tool-activity-summary-text summary) stream))))
  (terpri stream)
  (terpri stream))

(defun display-chat-display-item (frame stream item)
  "Display one transcript ITEM."
  (if (chat-tool-activity-summary-p item)
      (display-chat-tool-activity-summary frame stream item)
      (display-chat-message frame stream item)))

(defun display-buffer-presentation-entry (frame stream entry)
  "Display one generic buffer presentation ENTRY on STREAM."
  (let ((text (getf entry :text ""))
        (object (getf entry :object))
        (presentation-type (getf entry :presentation-type)))
    (flet ((emit ()
             (let ((profile (getf entry :appearance-profile))
                   (role-stack (getf entry :role-stack)))
               (if (and profile role-stack)
                   (call-with-appearance-profile-role
                    frame stream profile role-stack
                    (lambda () (write-string text stream))
                    (getf entry :resolved-text-style))
                   (call-with-chat-appearance-role
                    frame stream
                    (append '(:transcript-pane)
                            (chat-appearance-wire-role-stack (getf entry :face)))
                    (lambda () (write-string text stream)))))))
      (if (and object presentation-type)
          (clim:with-output-as-presentation
              (stream object presentation-type :single-box t)
            (emit))
          (emit))))
  (terpri stream))

(defun buffer-presentation-entry-cache-value (entry)
  "Return ENTRY's structural redisplay value without semantic object identity."
  (or (getf entry :cache-value)
      (loop :for (key value) :on entry :by #'cddr
            :unless (eq key :object)
              :append
              (list key
                    (if (and (eq key :appearance-profile)
                             (typep value 'appearance-profile))
                        (appearance-profile-structural-key value)
                        value)))))

(defun display-buffer-presentation-entries (frame stream entries &key (namespace :buffer))
  "Display generic presentation ENTRIES on STREAM with incremental redisplay."
  (loop :for entry :in entries
        :for index :from 0
        :do
           (clim:updating-output
               (stream
                :unique-id (or (getf entry :unique-id)
                               (list namespace index (getf entry :text "")))
                :id-test #'equal
                :cache-value
                (list (buffer-presentation-entry-cache-value entry)
                      (chat-frame-appearance-role-key
                       frame
                       (append '(:transcript-pane)
                               (chat-appearance-wire-role-stack
                                (getf entry :face)))))
                :cache-test #'equal)
             (display-buffer-presentation-entry frame stream entry))))

(defun stream-buffer-presentation-columns (stream)
  "Return an approximate text column count available on STREAM."
  (or (ignore-errors
        (let* ((width (clim:bounding-rectangle-width stream))
               (char-width (max 1 (or (ignore-errors
                                         (clim:text-style-width
                                          (clim:medium-text-style stream)
                                          stream))
                                       8))))
          (max 20 (floor width char-width))))
      *buffer-presentation-default-columns*))

(defun display-buffer-presentation-function
    (frame stream buffer function &key (namespace :buffer) columns)
  "Display BUFFER entries produced by FUNCTION and return the entry count."
  (let ((entries (call-buffer-presentation-function
                  function
                  buffer
                  (or columns *buffer-presentation-default-columns*))))
    (display-buffer-presentation-entries frame stream entries :namespace namespace)
    (length entries)))

(defun display-chat-transcript-feedback (frame stream buffer)
  "Display presentation-buffer feedback messages and return count."
  (let ((items (buffer-presentation-feedback-items buffer))
        (count 0))
    (dolist (item items count)
      (incf count)
      (clim:updating-output
          (stream
           :unique-id (chat-display-item-output-id item)
           :id-test #'equal
           :cache-value
           (list (chat-display-item-cache-value item)
                 (chat-frame-appearance-role-key
                  frame (if (chat-tool-activity-summary-p item)
                            '(:transcript-pane :transcript-tool)
                            (chat-message-appearance-role-stack item))))
           :cache-test #'equal)
        (display-chat-display-item frame stream item)))))

(defun display-chat-transcript (frame stream)
  "Display FRAME's transcript on STREAM."
  (let* ((buf (chat-frame-buffer frame))
         (columns (stream-buffer-presentation-columns stream))
         (presentation-function (and buf (buffer-presentation-function buf)))
         (input-functions (and buf (buffer-input-presentation-functions buf)))
         (items (and (not presentation-function)
                     (chat-transcript-display-items buf)))
         (item-count 0))
    (cond
      (presentation-function
       (incf item-count
             (display-buffer-presentation-function
              frame stream buf presentation-function
              :namespace :buffer-presentation
              :columns columns))
       (incf item-count (display-chat-transcript-feedback frame stream buf)))
      (items
       (dolist (item items)
         (incf item-count)
         (clim:updating-output
             (stream
              :unique-id (chat-display-item-output-id item)
              :id-test #'equal
              :cache-value
              (list (chat-display-item-cache-value item)
                    (chat-frame-appearance-role-key
                     frame (if (chat-tool-activity-summary-p item)
                               '(:transcript-pane :transcript-tool)
                               (chat-message-appearance-role-stack item))))
              :cache-test #'equal)
           (display-chat-display-item frame stream item))))
      (t
       (call-with-chat-appearance-role
        frame stream '(:transcript-pane :transcript-empty)
        (lambda () (format stream "No messages yet.~%")))))
    (when input-functions
      (let ((*buffer-input-presentation-text* (chat-frame-e2e-compose-text frame)))
        (dolist (input-function input-functions)
          (incf item-count
                (display-buffer-presentation-function
                 frame stream buf input-function
                 :namespace :buffer-input-presentation
                 :columns columns)))))
    (emit-chat-pane-rendered frame "transcript"
                             :item-count item-count)))

(defun chat-transcript-pane (frame)
  "Return FRAME's transcript pane, or NIL when unavailable."
  (ignore-errors
    (clim:find-pane-named frame 'transcript)))

(defun chat-transcript-bottom-scroll-y-from-heights (content-height viewport-height)
  "Return the Y displacement that places CONTENT-HEIGHT's bottom in view."
  (max 0 (- (or content-height 0)
            (or viewport-height 0))))

(defun chat-transcript-output-height (pane)
  "Return PANE's recorded output height, or NIL when unavailable."
  (let ((history (ignore-errors (clim:stream-output-history pane))))
    (and history
         (ignore-errors
           (clim:bounding-rectangle-height history)))))

(defun chat-transcript-viewport-height (pane)
  "Return PANE's visible viewport height, or NIL when unavailable."
  (let ((viewport (or (ignore-errors (clim:pane-viewport pane)) pane)))
    (and viewport
         (ignore-errors
           (clim:bounding-rectangle-height viewport)))))

(defun chat-transcript-bottom-scroll-y (pane)
  "Return the Y displacement for keeping PANE scrolled to transcript tail."
  (let ((content-height (chat-transcript-output-height pane))
        (viewport-height (chat-transcript-viewport-height pane)))
    (and content-height
         viewport-height
         (chat-transcript-bottom-scroll-y-from-heights content-height viewport-height))))

(defun chat-transcript-scroll-to-bottom (pane)
  "Scroll transcript PANE to its bottom when McCLIM has viewport geometry."
  (let ((bottom-y (and pane (chat-transcript-bottom-scroll-y pane))))
    (when bottom-y
      (ignore-errors
        (clim:scroll-extent pane 0 bottom-y))
      bottom-y)))

(defun chat-frame-follow-transcript-tail (frame)
  "Scroll FRAME's transcript to the newest visible output when enabled."
  (when *chat-transcript-follow-tail*
    (chat-transcript-scroll-to-bottom (chat-transcript-pane frame))))

(defun submit-chat-compose-pane (frame compose-pane)
  "Submit COMPOSE-PANE through FRAME's chat command path."
  (let ((text (clim:gadget-value compose-pane))
        (buf (chat-frame-buffer frame)))
    (file-debug-event "compose-submitted"
                      :buffer-name (buffer-name buf)
                      :text text)
    (when (handle-chat-compose-text buf text)
      (setf (clim:gadget-value compose-pane) "")
      (request-chat-frame-redisplay frame)
      (emit-chat-frame-e2e-snapshot frame :reason "compose-submitted" :pane "compose")
      t)))

(defun compose-pane-activated (gadget)
  "Dispatch compose activation as a frame command."
  (declare (ignore gadget))
  (clim:with-application-frame (frame)
    (clim:execute-frame-command frame '(com-chat-submit-compose))))

(defun queue-chat-frame-redisplay-event (frame)
  "Queue one redisplay wakeup on FRAME's grafted top-level sheet.

The event is only a cross-thread handoff into the CLIM event process.  Its
EVENT-SHEET is the actual top-level sheet, as required by the event protocol;
all rendering remains in the pane display functions and
CLIM:REDISPLAY-FRAME-PANE."
  (let ((sheet (chat-frame-grafted-top-level-sheet frame)))
    (when sheet
      (handler-case
          (progn
            (clim:queue-event
             sheet
             (make-instance 'rplaca-chat-redisplay-event :sheet sheet))
            t)
        (error (condition)
          (file-debug-event "redisplay-queue-failed"
                            :condition (format nil "~A" condition))
          nil)))))

(defun queue-chat-frame-appearance-activation-event (frame candidate)
  "Queue CANDIDATE for activation on FRAME's owning CLIM event process.

The caller performs no frame mutation and does not resolve the candidate.  An
ungrafted frame simply cannot accept an interactive activation yet; normal
frame construction remains the profile-application path for that case."
  (let ((sheet (chat-frame-grafted-top-level-sheet frame)))
    (when sheet
      (handler-case
          (progn
            (clim:queue-event
             sheet
             (make-instance 'rplaca-chat-appearance-activation-event
                            :sheet sheet :candidate candidate))
            t)
        (error (condition)
          (file-debug-event "appearance-activation-queue-failed"
                            :condition (format nil "~A" condition))
          nil)))))

(defun queue-chat-frame-font-inventory-refresh-event (frame)
  "Queue FRAME's explicit font refresh on its canonical CLIM event process."
  (let ((sheet (chat-frame-grafted-top-level-sheet frame)))
    (when sheet
      (handler-case
          (progn
            (clim:queue-event
             sheet
             (make-instance 'rplaca-chat-font-inventory-refresh-event :sheet sheet))
            t)
        (error (condition)
          (file-debug-event "font-inventory-refresh-queue-failed"
                            :condition (format nil "~A" condition))
          nil)))))

(defun refresh-font-inventory-command (frame)
  "Request a frame-local named-font inventory refresh without caller mutation."
  (setf frame (appearance-command-frame frame))
  (queue-chat-frame-font-inventory-refresh-event frame))

(register-command-metadata 'refresh-font-inventory-command)

(defun request-chat-frame-appearance-activation (frame candidate)
  "Request immutable CANDIDATE activation without mutating FRAME on the caller.

Publication is exclusively performed by the appearance activation event
handler, which is delivered by the frame's normal CLIM event process."
  (unless (typep candidate 'appearance-candidate)
    (error-appearance-condition 'invalid-appearance-component
                                :axis :appearance-candidate :value candidate))
  (call-with-package-appearance-user-edit
   (lambda ()
     (queue-chat-frame-appearance-activation-event frame candidate))))

(defun package-appearance-frame-transition-plan (frame catalog)
  "Admit FRAME's complete replacement bundle without changing frame state."
  (let* ((expected-catalog (chat-frame-appearance-catalog frame))
         (current (chat-frame-appearance-profile frame))
         (expected-bundle (chat-frame-appearance-active-bundle frame))
         (expected-revision (chat-frame-appearance-revision frame))
         (expected-staged (chat-frame-appearance-staged-candidate frame))
         (expected-persisted
           (chat-frame-appearance-persisted-profile frame))
         (expected-font-inventory
           (chat-frame-appearance-font-inventory frame))
         (expected-font-generation
           (chat-frame-appearance-font-inventory-generation frame))
         (theme (appearance-profile-selected-theme current))
         (profile (if (find-appearance-theme-definition catalog theme)
                      current
                      (make-appearance-profile
                       :selected-theme :classic
                       :strict-contrast (appearance-profile-strict-contrast current)
                       :role-overrides (appearance-profile-role-overrides current))))
         (port (chat-frame-appearance-live-port frame))
         (staged expected-staged)
         (persisted expected-persisted))
    (handler-case
        (handler-bind
            ((appearance-contrast-warning
               (lambda (condition)
                 (let ((restart (find-restart 'muffle-warning condition)))
                   (when restart (invoke-restart restart))))))
          (progn
          (unless port
            (error "A running frame without an adopted port cannot classify appearance."))
          ;; This validates all roles/overlays and thus rejects a fallback
          ;; which would leave a persisted or staged profile dangling.
          (let* ((active
                   (or expected-bundle
                       (resolve-appearance-profile-bundle
                        expected-catalog current
                        :profile-revision expected-revision
                        :font-inventory-generation
                        expected-font-generation
                        :port-identity port)))
                 (bundle
                   (resolve-appearance-profile-bundle
                    catalog profile
                    :profile-revision
                    (1+ expected-revision)
                    :font-inventory-generation
                    expected-font-generation
                    :port-identity port))
                 (classification
                   (classify-appearance-bundle-delta catalog active bundle))
                 (classification-status
                   (appearance-activation-classification-status
                    classification)))
            (validate-appearance-profile-contrast catalog profile)
            ;; Safe Reload/catalog reconciliation must never discard a valid
            ;; unsaved candidate or persisted profile.  Validate both against
            ;; the proposed catalog before admitting any frame transition; a
            ;; removed package theme in either is a refusal, not silent loss.
            (dolist (candidate-profile
                     (remove nil
                             (list (and staged
                                        (appearance-candidate-profile staged))
                                   persisted)))
              (validate-appearance-profile-contrast catalog candidate-profile)
              (resolve-appearance-profile-bundle
               catalog candidate-profile
               :profile-revision expected-revision
               :font-inventory-generation
               expected-font-generation
               :port-identity port))
            (case classification-status
              ((:no-op :render-boundary-live)
               (list :status (if (eq classification-status :no-op)
                                 :no-op
                                 :ready)
                     :profile profile
                     :bundle bundle
                     :classification classification
                     :staged-candidate staged
                     :persisted-profile persisted
                     :expected-catalog expected-catalog
                     :expected-profile current
                     :expected-bundle expected-bundle
                     :expected-revision expected-revision
                     :expected-staged expected-staged
                     :expected-persisted expected-persisted
                     :expected-font-inventory expected-font-inventory
                     :expected-font-generation expected-font-generation))
              ((:restart-required :unsupported)
               (list :status :failed
                     :condition
                     (make-appearance-condition
                      'appearance-live-update-unsupported
                      :axis :package-catalog-transition
                      :value classification-status
                      :suggested-repairs '(:restart-rplaca))))
              (otherwise
               (list :status :failed :condition classification-status))))))
      (condition (condition)
        (list :status :failed :condition condition)))))

(defun reserve-package-appearance-frame-transition (frame plan catalog token)
  "Return a side-effect-free event reservation for one admitted transition."
  (let ((sheet (chat-frame-grafted-top-level-sheet frame))
        (profile (getf plan :profile))
        (bundle (getf plan :bundle)))
    (let ((reservation
            (and sheet profile bundle
                 (list :sheet sheet
               :frame frame
               :catalog catalog
               :profile profile
               :bundle bundle
               :target-revision
               (resolved-appearance-bundle-profile-revision bundle)
               :token token
               :expected-catalog (getf plan :expected-catalog)
               :expected-profile (getf plan :expected-profile)
               :expected-bundle (getf plan :expected-bundle)
               :expected-revision (getf plan :expected-revision)
               :expected-staged (getf plan :expected-staged)
               :expected-persisted (getf plan :expected-persisted)
               :expected-font-inventory
               (getf plan :expected-font-inventory)
               :expected-font-generation
               (getf plan :expected-font-generation)
               :expected-resolved-roles
               (copy-package-runtime-hash-table
                (chat-frame-appearance-resolved-roles frame))
               :expected-role-keys
               (copy-package-runtime-hash-table
                (chat-frame-appearance-role-keys frame))
               :origin-frame-p
               (eq frame clim:*application-frame*)))))
      (and reservation
           (package-appearance-reservation-current-p reservation)
           reservation))))

(defun release-package-appearance-frame-transition (reservation)
  "Queue RESERVATION only after the catalog transaction is committed."
  (when (getf reservation :origin-frame-p)
    (return-from release-package-appearance-frame-transition t))
  (handler-case
      (let ((sheet (getf reservation :sheet)))
        (clim:queue-event
         sheet
         (make-instance 'rplaca-chat-appearance-catalog-event
                        :sheet sheet
                        :reservation reservation
                        :token (getf reservation :token)))
        t)
    (error () nil)))

(defun appearance-package-transition-notify-all (token)
  #+sbcl
  (sb-thread:condition-broadcast
   (rplaca::appearance-package-transition-token-condition token))
  #-sbcl
  (bt:condition-notify
   (rplaca::appearance-package-transition-token-condition token)))

(defun wait-for-appearance-package-transition-count
    (token count-reader expected deadline &key (stop-on-failure-p t))
  "Wait with TOKEN locked until COUNT-READER reaches EXPECTED or fails."
  (loop
    (when (and stop-on-failure-p
               (rplaca::appearance-package-transition-token-failure token))
      (return nil))
    (when (>= (funcall count-reader token) expected)
      (return t))
    (when (>= (get-internal-real-time) deadline)
      (setf (rplaca::appearance-package-transition-token-failure token)
            :frame-transition-timeout)
      (return nil))
    (bt:condition-wait
     (rplaca::appearance-package-transition-token-condition token)
     (rplaca::appearance-package-transition-token-lock token)
     :timeout 0.05)))

(defun finalize-package-appearance-frame-transition
    (token reservations commit-function rollback-function)
  "Run a two-phase owning-frame barrier around global catalog publication."
  (declare (ignore rollback-function))
  (let ((deadline (+ (get-internal-real-time)
                     (* *appearance-package-transition-timeout-seconds*
                        internal-time-units-per-second))))
    (bt:with-lock-held
        ((rplaca::appearance-package-transition-token-lock token))
      (let ((expected
              (rplaca::appearance-package-transition-token-expected-count
               token))
            (origin
              (find-if (lambda (reservation)
                         (getf reservation :origin-frame-p))
                       reservations)))
        ;; A command may initiate reload on its own CLIM event thread.  That
        ;; frame cannot consume a queued barrier event while this coordinator
        ;; is waiting, so stage and commit its exact reservation synchronously
        ;; on the already-correct owning process.
        (when origin
          (unless (package-appearance-reservation-current-p origin)
            (setf
             (rplaca::appearance-package-transition-token-failure token)
             :origin-frame-state-changed))
          (incf
           (rplaca::appearance-package-transition-token-ready-count token)))
        (unless
            (wait-for-appearance-package-transition-count
             token
             #'rplaca::appearance-package-transition-token-ready-count
             expected deadline)
          (setf (rplaca::appearance-package-transition-token-state token)
                :aborted)
          (appearance-package-transition-notify-all token)
          (error "Appearance frame transition preparation failed: ~A"
                 (rplaca::appearance-package-transition-token-failure token)))
        (funcall commit-function)
        (setf (rplaca::appearance-package-transition-token-state token)
              :committing)
        (appearance-package-transition-notify-all token)
        ;; Every non-origin owning process is now parked in its event handler,
        ;; and the origin is already executing on its owner.  Nothing can
        ;; change the captured slots between prepare and this bounded install,
        ;; so commit contains no fallible resolution or compensating rollback.
        (when origin
          (apply-package-appearance-frame-reservation-target origin)
          (incf
           (rplaca::appearance-package-transition-token-applied-count token)))
        (unless
            (wait-for-appearance-package-transition-count
             token
             #'rplaca::appearance-package-transition-token-applied-count
             expected
             (+ (get-internal-real-time)
                (* *appearance-package-transition-timeout-seconds*
                   internal-time-units-per-second))
             :stop-on-failure-p nil)
          ;; Global state is already committed.  Never manufacture split-brain
          ;; by unwinding it after any frame has installed the same commit.
          ;; Prepared owners retain their immutable reservation and complete
          ;; recovery-forward when their event process resumes.
          (ignore-errors
            (warn
             "Committed appearance transition is still settling on ~D frame(s)."
             (- expected
                (rplaca::appearance-package-transition-token-applied-count
                 token)))))
        (setf (rplaca::appearance-package-transition-token-state token)
              :committed)
        (appearance-package-transition-notify-all token)
        t))))

(defun chat-frame-appearance-live-port (frame)
  "Return FRAME's public frame-manager port only after engraftment.

The frame manager owns the selected port.  Requiring its top-level sheet to be
grafted prevents construction-time profile resolution from manufacturing a
runtime bundle before ordinary CLIM adoption has completed."
  (when (chat-frame-grafted-top-level-sheet frame)
    (ignore-errors
      (clim:port (clim:frame-manager frame)))))

(defun chat-frame-font-metric-medium (frame)
  "Return an existing adopted pane's public medium for font metric queries.

The pinned McCLIM exposes sheet methods for basic metrics, but its public
TEXT-STYLE-FIXED-WIDTH-P method is defined on mediums.  Read the existing
pane's public SHEET-MEDIUM; never create a pane, initialize Drei, or mutate
the medium."
  (when (chat-frame-grafted-top-level-sheet frame)
    (loop :for name :in '(transcript info minibuffer)
          :for pane := (ignore-errors (clim:find-pane-named frame name))
          :for medium := (and pane (ignore-errors (clim:sheet-medium pane)))
          :when medium :return medium)))

(defun appearance-bundles-same-typography-p (left right)
  "Return true when LEFT and RIGHT differ only outside effective typography."
  (labels ((same-typography-p (left-style right-style)
             (let ((left (appearance-role-style-typography left-style))
                   (right (appearance-role-style-typography right-style)))
               ;; Do not compare typography structs by identity: each complete
               ;; bundle owns independently immutable declarations.
               (and (equal (appearance-typography-spec-family left)
                           (appearance-typography-spec-family right))
                    (equal (appearance-typography-spec-face left)
                           (appearance-typography-spec-face right))
                    (equal (appearance-typography-spec-size left)
                           (appearance-typography-spec-size right))))))
    (and left right
       (let ((left-table (%resolved-appearance-bundle-role-table left))
             (right-table (%resolved-appearance-bundle-role-table right)))
         (and (= (length left-table) (length right-table))
              (every
               (lambda (left-entry)
                 (let ((right-entry (assoc (car left-entry) right-table :test #'equal)))
                   (and right-entry
                        (same-typography-p
                         (resolved-appearance-role-style (cdr left-entry))
                         (resolved-appearance-role-style (cdr right-entry))))))
               left-table))))))

(defun record-chat-frame-font-refresh-diagnostic (frame condition)
  "Store one copied refresh failure without changing active appearance state."
  (push (make-appearance-condition
         (type-of condition)
         :origin (appearance-condition-origin condition)
         :role (appearance-condition-role condition)
         :axis (appearance-condition-axis condition)
         :value (appearance-condition-value condition)
         :path (appearance-condition-path condition)
         :port (appearance-condition-port condition)
         :available-choices (appearance-condition-available-choices condition)
         :fatal-p (appearance-condition-fatal-p condition)
         :suggested-repairs (appearance-condition-suggested-repairs condition))
        (slot-value frame 'appearance-font-refresh-diagnostics))
  condition)

(defun bounded-appearance-debug-text (value &optional (limit 240))
  "Return a bounded printable diagnostic value for the debug event stream."
  (let ((text (handler-case (princ-to-string value)
                (error () "<unprintable>"))))
    (if (> (length text) limit) (subseq text 0 limit) text)))

(defun file-debug-font-refresh-diagnostic
    (phase status condition diagnostic)
  "Emit one bounded structured font failure without retaining backend objects."
  (ignore-errors
    (file-debug-event
     "font-inventory-refresh-diagnostic"
     :phase phase
     :status status
     :condition-class (bounded-appearance-debug-text (type-of condition))
     :condition-text (bounded-appearance-debug-text condition)
     :diagnostic-axis
     (and (typep diagnostic 'appearance-condition)
          (bounded-appearance-debug-text
           (appearance-condition-axis diagnostic)))
     :diagnostic-value
     (and (typep diagnostic 'appearance-condition)
          (bounded-appearance-debug-text
           (appearance-condition-value diagnostic))))))

(defun refresh-chat-frame-font-inventory
    (frame &key (invalidate-cache t) (phase :explicit-refresh))
  "Atomically refresh FRAME's own post-adoption McCLIM font inventory.

INVALIDATE-CACHE advances the frame-local inventory generation.  It does not
forward cache invalidation to an adopted live McCLIM port: pinned McCLIM 1.0.0
closes TrueType streams that remain mapped by live pane mediums.  Newly
installed system fonts therefore require a new frame/application process.

The refresh first builds the next inventory and complete bundle off-frame.  A
candidate whose effective role typography differs is restart-required and is
not partly published.  Any error retains the old inventory, generation, bundle,
profile, and render keys while recording a copied structured diagnostic."
  (let ((port (chat-frame-appearance-live-port frame)))
    (unless port
      (return-from refresh-chat-frame-font-inventory nil))
    (let* ((old-inventory (chat-frame-appearance-font-inventory frame))
           (old-bundle (chat-frame-appearance-active-bundle frame))
           (old-generation (chat-frame-appearance-font-inventory-generation frame))
           (next-generation (if invalidate-cache (1+ old-generation) old-generation)))
      (handler-case
          (let* ((metric-medium (chat-frame-font-metric-medium frame))
                 (inventory (enumerate-port-font-inventory
                             port :invalidate-cache nil
                             :generation next-generation
                             :metric-medium metric-medium))
                 (bundle (resolve-appearance-profile-bundle
                          (chat-frame-appearance-catalog frame)
                          (chat-frame-appearance-profile frame)
                          :profile-revision (chat-frame-appearance-revision frame)
                          :font-inventory-generation next-generation
                          :port-identity port)))
            ;; Current v1 profiles only contain portable typography, but keep
            ;; this guard so a future named-font profile cannot publish a new
            ;; inventory generation while changing an active text style.
            (if (and old-bundle
                     (not (appearance-bundles-same-typography-p old-bundle bundle)))
                (%make-appearance-activation-result
                 :status :restart-required
                 :diagnostics
                 (list (make-appearance-condition
                        'appearance-live-update-unsupported
                        :axis :typography :value :font-inventory-refresh
                        :port port
                        :suggested-repairs '(:restart-rplaca))))
                (progn
                  (setf (slot-value frame 'appearance-font-inventory) inventory
                        (slot-value frame 'appearance-font-inventory-generation)
                        next-generation
                        (slot-value frame 'appearance-active-bundle) bundle)
                  (%make-appearance-activation-result :status :ready :bundle bundle))))
        (appearance-condition (condition)
          (record-chat-frame-font-refresh-diagnostic frame condition)
          (file-debug-font-refresh-diagnostic
           phase :failed condition condition)
          (%make-appearance-activation-result :status :failed
                                               :diagnostics (list condition)))
        (error (condition)
          (let ((diagnostic
                  (make-appearance-condition
                   'appearance-activation-failed
                   :axis :font-inventory-refresh
                   :value (format nil "~A" condition)
                   :port port
                   :suggested-repairs '(:refresh-font-inventory))))
            (record-chat-frame-font-refresh-diagnostic frame diagnostic)
            (file-debug-font-refresh-diagnostic
             phase :failed condition diagnostic)
            (%make-appearance-activation-result :status :failed
                                                 :diagnostics (list diagnostic))))))))

(defun real-chat-frame-appearance-port-p (port)
  "Return true when PORT is an adopted public CLIM port."
  (typep port 'clim:port))

(defun publish-chat-frame-portable-appearance-bundle (frame port)
  "Publish FRAME's portable generation-zero bundle independently of fonts."
  (let ((bundle
          (resolve-appearance-profile-bundle
           (chat-frame-appearance-catalog frame)
           (chat-frame-appearance-profile frame)
           :profile-revision (chat-frame-appearance-revision frame)
           :font-inventory-generation
           (chat-frame-appearance-font-inventory-generation frame)
           :port-identity port)))
    (unless (and (chat-frame-appearance-active-bundle frame)
                 (equal (resolved-appearance-bundle-bundle-key bundle)
                        (resolved-appearance-bundle-bundle-key
                         (chat-frame-appearance-active-bundle frame))))
      (setf (slot-value frame 'appearance-active-bundle) bundle))
    bundle))

(defun refresh-chat-frame-appearance-port-bundle (frame)
  "Build or replace FRAME's local bundle after adoption on its event process.

For a real adopted CLIM port this also creates the initial noninvalidating
frame-local inventory.  Test-only opaque ports retain the old bundle seam."
  (let ((port (chat-frame-appearance-live-port frame)))
    (when port
      ;; Portable CLIM typography and inks are sufficient for first paint.
      ;; Optional named-font discovery may replace this complete bundle, but a
      ;; stale backend inventory can never leave an adopted frame bundle-less.
      (let ((portable
              (publish-chat-frame-portable-appearance-bundle frame port)))
        (when (real-chat-frame-appearance-port-p port)
          (refresh-chat-frame-font-inventory
           frame :invalidate-cache nil :phase :startup-inventory))
        (or (chat-frame-appearance-active-bundle frame) portable)))))

(defun publish-chat-frame-appearance-bundle (frame result)
  "Atomically publish one already validated live RESULT on FRAME.

This function is called only by HANDLE-CHAT-FRAME-APPEARANCE-ACTIVATION, the
CLIM event-process boundary.  It touches frame-owned semantic state and cache
keys only; pane construction and low-level rendering objects are untouched."
  (let ((bundle (appearance-activation-result-bundle result))
        (candidate (appearance-activation-result-candidate result)))
    (unless (and bundle candidate)
      (error "A live appearance result requires a bundle and candidate."))
    (setf (chat-frame-appearance-profile frame)
          (appearance-candidate-profile candidate)
          (slot-value frame 'appearance-active-bundle) bundle)
    ;; A newer staged edit must survive completion of an older direct request.
    (when (eq candidate (chat-frame-appearance-staged-candidate frame))
      (setf (slot-value frame 'appearance-staged-candidate) nil))
    (incf (slot-value frame 'appearance-revision))
    ;; Structural keys belong to resolved output.  Clearing only after the
    ;; atomic publish lets ordinary display functions acquire the new keys on
    ;; the next CLIM redisplay; no direct output-record operation is needed.
    (clrhash (chat-frame-appearance-resolved-roles frame))
    (clrhash (chat-frame-appearance-role-keys frame))
    result))

(defun record-chat-frame-appearance-result (frame result)
  "Retain RESULT and its structured diagnostics in FRAME-local state."
  (setf (slot-value frame 'appearance-last-activation-result) result)
  (dolist (diagnostic (appearance-activation-result-diagnostics result))
    (push diagnostic (slot-value frame 'appearance-activation-diagnostics)))
  result)

(defun %handle-chat-frame-appearance-activation (frame candidate)
  "Resolve and conditionally publish CANDIDATE on FRAME's CLIM event process."
  (let ((port (chat-frame-appearance-live-port frame)))
    (unless port
      (return-from %handle-chat-frame-appearance-activation
        (record-chat-frame-appearance-result
         frame
         (%make-appearance-activation-result
          :status :failed :candidate candidate
          :diagnostics
          (list (make-appearance-condition
                 'appearance-activation-failed
                 :axis :port-identity :value :frame-not-engrafted))))))
    (let ((result
            (prepare-appearance-activation
             (chat-frame-appearance-catalog frame)
             (chat-frame-appearance-profile frame)
             candidate
             :profile-revision (chat-frame-appearance-revision frame)
             :font-inventory-generation
             (chat-frame-appearance-font-inventory-generation frame)
             :port-identity port)))
    ;; A candidate is staged only after whole-profile resolution succeeded.
    ;; In particular, a failed candidate never overwrites the last valid
    ;; staged profile, while active profile/bundle/keys remain untouched.
    (case (appearance-activation-result-status result)
      (:ready
       (publish-chat-frame-appearance-bundle frame result)
       (ignore-errors (request-chat-frame-redisplay frame)))
      ((:restart-required :unsupported)
       (setf (slot-value frame 'appearance-staged-candidate) candidate))
      ((:failed :no-op) nil))
      (record-chat-frame-appearance-result frame result))))

(defun handle-chat-frame-appearance-activation (frame candidate)
  "Admit one owning-process appearance activation without blocking on reload."
  (call-with-package-appearance-user-edit
   (lambda ()
     (%handle-chat-frame-appearance-activation frame candidate))))

(defun handle-queued-chat-frame-appearance-activation (frame candidate)
  "Apply CANDIDATE only while it remains FRAME's exact staged request."
  (if (eq candidate (chat-frame-appearance-staged-candidate frame))
      (handle-chat-frame-appearance-activation frame candidate)
      (record-chat-frame-appearance-result
       frame
       (%make-appearance-activation-result
        :status :no-op
        :candidate candidate
        :diagnostics
        (list (make-appearance-condition
               'appearance-activation-failed
               :axis :activation
               :value :superseded-staged-candidate
               :fatal-p nil))))))

(defun chat-frame-grafted-top-level-sheet (frame)
  "Return FRAME's grafted top-level sheet, or NIL before FRAME is running."
  (let ((sheet (ignore-errors (clim:frame-top-level-sheet frame))))
    (and sheet
         (ignore-errors (clim:sheet-grafted-p sheet))
         sheet)))

(defun request-chat-frame-redisplay (frame)
  "Deliver an application update through McCLIM's native event queue.

Each notification has its own wakeup. There is no application dirty/pending
reservation which can consume the last update when a display fails. Startup
queues an update after adoption; stopped frames accept no new work."
  (when (bt:with-lock-held ((chat-frame-lifecycle-lock frame))
          (eq (chat-frame-lifecycle-state frame) :running))
    (queue-chat-frame-redisplay-event frame))
  frame)

(defun chat-minibuffer-desired-row-count ()
  "Return rows reserved for the frame's visible semantic interaction."
  (let ((kind (chat-interaction-pane-kind)))
    (case kind
      (:minibuffer
       (if (eq *minibuffer-mode* :completion)
           (+ 1 (max 1 (minibuffer-visible-item-count)))
           1))
      (:session-tree
       (+ 1 (max 1 (session-tree-selector-visible-item-count))))
      (:slash (+ 1 (max 1 (slash-completion-visible-item-count))))
      (:skill (+ 1 (max 1 (skill-completion-visible-item-count))))
      (otherwise 1))))

(defun chat-minibuffer-row-pixel-height (pane)
  "Return one CLIM-managed interaction row height for PANE in pixels.

Once PANE has a medium, `clim:stream-line-height' reflects the active backend,
font, and vertical spacing.  The fixed fallback remains necessary while an
ungrafted pane is being composed during startup, where some McCLIM builds
cannot yet answer font metric queries."
  (max *chat-minibuffer-line-height*
       (or (and pane
                (ignore-errors
                  (clim:stream-line-height pane)))
           *chat-minibuffer-line-height*)))

(defun chat-minibuffer-baseline-pixel-inset (pane)
  "Return PANE's non-negative CLIM text baseline inset in pixels."
  (max 0
       (or (and pane
                (ignore-errors (clim:stream-baseline pane)))
           0)))

(defun chat-minibuffer-content-pixel-height (pane rows)
  "Return the pixel height required for ROWS complete text lines in PANE."
  (ceiling (+ (chat-minibuffer-baseline-pixel-inset pane)
              (* (chat-minibuffer-row-pixel-height pane) rows))))

(defun chat-minibuffer-max-pixel-height (&optional pane)
  "Return the maximum expanded minibuffer PANE height in pixels."
  (or *chat-minibuffer-max-pixel-height*
      (chat-minibuffer-content-pixel-height
       pane *minibuffer-max-height*)))

(defun update-chat-minibuffer-space-requirements (frame)
  "Resize FRAME's minibuffer pane for its frame-owned completion rows.

Propagate the change through the layout hierarchy so the top-level mirror also
contains the expanded pane and the pointer-documentation pane below it."
  (let ((*chat-interaction-state* (chat-frame-interaction-state frame)))
    (let* ((pane (ignore-errors (clim:find-pane-named frame 'minibuffer)))
           (kind (chat-interaction-pane-kind))
           (height
             (if kind
                 (min (chat-minibuffer-max-pixel-height pane)
                      (chat-minibuffer-content-pixel-height
                       pane (chat-minibuffer-desired-row-count)))
                 *chat-minibuffer-line-height*))
           (space (and pane (ignore-errors (clim:compose-space pane))))
           (current-height
             (and space
                  (ignore-errors
                    (clim:space-requirement-height space))))
           (current-min-height
             (and space
                  (ignore-errors
                    (clim:space-requirement-min-height space))))
           (current-max-height
             (and space
                  (ignore-errors
                    (clim:space-requirement-max-height space)))))
      (when (and pane
                 (not (and (numberp current-height)
                           (numberp current-min-height)
                           (numberp current-max-height)
                           (= height current-height)
                           (= height current-min-height)
                           (= height current-max-height))))
        (ignore-errors
          (clim:change-space-requirements pane
                                          :resize-frame t
                                          :height height
                                          :min-height height
                                          :max-height height))))))

(defun apply-chat-frame-runtime-updates (frame)
  "Apply worker-published results on FRAME's owning event process."
  (let ((buf (chat-frame-buffer frame))
        (*suppress-chat-redisplay-requests* t))
    (when buf
      (deliver-buffer-runtime-stopped-notification buf)
      (update-openai-oauth-login buf)
      (update-interactive-tool-execution buf)
      (update-interactive-buffer-operation buf)
      (when (buffer-pending-stream buf)
        (update-streaming-response buf)))))

(defun handle-chat-frame-redisplay (frame)
  "Apply one queued update and let CLIM redisplay its application panes."
  (when (eq (chat-frame-lifecycle-state frame) :running)
    (apply-chat-frame-runtime-updates frame)
    (update-chat-minibuffer-space-requirements frame)
    ;; Use the pane protocol directly: ESA:REDISPLAY-FRAME-PANES defers while
    ;; a key prefix is pending, but background messages must remain visible.
    ;; CLIM owns incremental records, clearing, buffering, and repainting.
    (clim:redisplay-frame-pane frame 'transcript :force-p nil)
    (clim:redisplay-frame-pane frame 'info :force-p t)
    (clim:redisplay-frame-pane frame 'minibuffer :force-p t)
    (chat-frame-follow-transcript-tail frame)
    (file-debug-event "redisplay-handled"
                      :buffer-name (buffer-name (chat-frame-buffer frame))
                      :repeat nil)
    (emit-chat-frame-e2e-snapshot frame :reason "redisplay-handled" :repeat nil))
  frame)

(defun handle-chat-frame-redisplay-safely (frame)
  "Contain an asynchronous application update at its frame event boundary."
  (call-chat-frame-ui-action-safely
   frame "background update" (lambda () (handle-chat-frame-redisplay frame))
   :notify-p nil)
  frame)

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event rplaca-chat-redisplay-event))
  (declare (ignore event))
  (let ((frame (ignore-errors (clim:pane-frame sheet))))
    (when (typep frame 'rplaca-chat-frame)
      (handle-chat-frame-redisplay-safely frame))))

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event rplaca-chat-appearance-activation-event))
  "Deliver appearance publication through the same canonical CLIM event loop."
  (let ((frame (ignore-errors (clim:pane-frame sheet))))
    (when (typep frame 'rplaca-chat-frame)
      (handler-case
          (handle-queued-chat-frame-appearance-activation
           frame (chat-appearance-activation-event-candidate event))
        (error (condition)
          ;; PREPARE normally turns appearance failures into a structured
          ;; result.  This final containment protects the event loop from an
          ;; unexpected implementation failure without mutating panes.
          (let ((result
                  (%make-appearance-activation-result
                   :status :failed
                   :candidate (chat-appearance-activation-event-candidate event)
                   :diagnostics
                   (list (make-appearance-condition
                          'appearance-activation-failed
                          :axis :activation :value (format nil "~A" condition))))))
            (record-chat-frame-appearance-result frame result)))))))

(defun handle-chat-frame-font-inventory-refresh (frame)
  "Refresh FRAME's fonts only when package reconciliation is not active."
  (handler-case
      (call-with-package-appearance-user-edit
       (lambda ()
         (let ((result
                 (refresh-chat-frame-font-inventory
                  frame :invalidate-cache t)))
           (when result
             (record-chat-frame-appearance-result frame result)
             ;; E2E consumes this data-only acknowledgement after the public
             ;; port enumeration has completed on the owning frame process.
             ;; It deliberately exposes neither the port nor McCLIM cache
             ;; objects, and it is useful only when debug logging is enabled.
             (file-debug-event
              "font-inventory-refresh"
              :status (string-downcase
                       (symbol-name (appearance-activation-result-status result)))
              :generation (chat-frame-appearance-font-inventory-generation frame)
              :choice-count (length (appearance-editor-font-choices frame)))))))
    (error (condition)
      (record-chat-frame-appearance-result
       frame
       (%make-appearance-activation-result
        :status :failed
        :diagnostics
        (list
         (make-appearance-condition
          'appearance-activation-failed
          :axis :font-inventory-refresh
          :value (format nil "~A" condition))))))))

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event rplaca-chat-font-inventory-refresh-event))
  "Run the explicit font refresh through the owning frame's event process."
  (declare (ignore event))
  (let ((frame (ignore-errors (clim:pane-frame sheet))))
    (when (typep frame 'rplaca-chat-frame)
      (handle-chat-frame-font-inventory-refresh frame))))

(defun publish-admitted-package-appearance-frame-state
    (frame catalog profile bundle &key revision)
  "Atomically install one preclassified package appearance transition.

Only the owning CLIM event handler calls this in production.  Keeping the
bounded frame-state commit separate makes the no-skew invariant deterministic
to test without manufacturing a backend top-level sheet."
  (setf (slot-value frame 'appearance-catalog) catalog
        (chat-frame-appearance-profile frame) profile
        (slot-value frame 'appearance-active-bundle) bundle
        (slot-value frame 'appearance-revision)
        (or revision
            (resolved-appearance-bundle-profile-revision bundle)))
  (clrhash (chat-frame-appearance-resolved-roles frame))
  (clrhash (chat-frame-appearance-role-keys frame))
  (ignore-errors (request-chat-frame-redisplay frame))
  bundle)

(defun apply-package-appearance-frame-reservation-target (reservation)
  "Install RESERVATION's admitted target on its owning frame."
  (let ((frame (getf reservation :frame)))
    (publish-admitted-package-appearance-frame-state
     frame
     (getf reservation :catalog)
     (getf reservation :profile)
     (getf reservation :bundle)
     :revision (getf reservation :target-revision))
    (when (getf reservation :target-state-p)
      (setf (slot-value frame 'appearance-staged-candidate)
            (getf reservation :target-staged)
            (chat-frame-appearance-persisted-profile frame)
            (getf reservation :target-persisted)))
    (when (getf reservation :target-font-state-p)
      (setf (slot-value frame 'appearance-font-inventory)
            (getf reservation :target-font-inventory)
            (slot-value frame 'appearance-font-inventory-generation)
            (getf reservation :target-font-generation)))
    (when (getf reservation :target-cache-p)
      (clrhash (chat-frame-appearance-resolved-roles frame))
      (maphash
       (lambda (key value)
         (setf (gethash key (chat-frame-appearance-resolved-roles frame))
               value))
       (getf reservation :target-resolved-roles))
      (clrhash (chat-frame-appearance-role-keys frame))
      (maphash
       (lambda (key value)
         (setf (gethash key (chat-frame-appearance-role-keys frame)) value))
       (getf reservation :target-role-keys)))
    frame))

(defun package-appearance-reservation-current-p (reservation)
  "Return true when RESERVATION still names the exact captured frame state."
  (let ((frame (getf reservation :frame)))
    (and (eq (getf reservation :expected-catalog)
             (chat-frame-appearance-catalog frame))
         (eq (getf reservation :expected-profile)
             (chat-frame-appearance-profile frame))
         (eq (getf reservation :expected-bundle)
             (chat-frame-appearance-active-bundle frame))
         (= (getf reservation :expected-revision)
            (chat-frame-appearance-revision frame))
         (eq (getf reservation :expected-staged)
             (chat-frame-appearance-staged-candidate frame))
         (eq (getf reservation :expected-persisted)
             (chat-frame-appearance-persisted-profile frame))
         (eq (getf reservation :expected-font-inventory)
             (chat-frame-appearance-font-inventory frame))
         (= (getf reservation :expected-font-generation)
            (chat-frame-appearance-font-inventory-generation frame)))))

(defun handle-package-appearance-frame-reservation
    (frame reservation token)
  "Participate in TOKEN from FRAME's owning event process."
  (let ((commit-p nil))
    (bt:with-lock-held
        ((rplaca::appearance-package-transition-token-lock token))
      (unless (and (typep frame 'rplaca-chat-frame)
                   (eq frame (getf reservation :frame))
                   (package-appearance-reservation-current-p reservation))
        (setf (rplaca::appearance-package-transition-token-failure token)
              :frame-state-changed))
      (incf (rplaca::appearance-package-transition-token-ready-count token))
      (appearance-package-transition-notify-all token)
      (loop :while
              (eq :preparing
                  (rplaca::appearance-package-transition-token-state token))
            :do
               (bt:condition-wait
                (rplaca::appearance-package-transition-token-condition token)
                (rplaca::appearance-package-transition-token-lock token)))
      (setf commit-p
            (member
             (rplaca::appearance-package-transition-token-state token)
             '(:committing :committed)
             :test #'eq)))
    (when commit-p
      ;; Preparation parked this owning event loop after its only CAS.  The
      ;; target install contains no resolution, I/O, or extension callback.
      ;; The diagnostic hook is outside the token lock so a stalled owner can
      ;; never prevent the coordinator from publishing recovery-forward.
      (funcall *appearance-package-prepared-frame-resume-hook*
               frame reservation)
      (apply-package-appearance-frame-reservation-target reservation)
      (bt:with-lock-held
          ((rplaca::appearance-package-transition-token-lock token))
        (incf
         (rplaca::appearance-package-transition-token-applied-count token))
        (appearance-package-transition-notify-all token))))
  (when (eq :committed
            (rplaca::appearance-package-transition-token-state token))
    (ignore-errors (request-chat-frame-redisplay frame)))
  nil)

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event rplaca-chat-appearance-catalog-event))
  "Publish an already-admitted catalog transition on FRAME's event process."
  (handle-package-appearance-frame-reservation
   (ignore-errors (clim:pane-frame sheet))
   (chat-appearance-catalog-event-reservation event)
   (chat-appearance-catalog-event-token event)))

(defun run-chat-frame-buffer-command (frame command)
  "Run COMMAND on FRAME's buffer and refresh frame UI state."
  (funcall command (chat-frame-buffer frame))
  (request-chat-frame-redisplay frame))

(defun select-chat-effort-for-buffer (buffer level)
  "Set BUFFER's reasoning effort LEVEL and report the new selection."
  (let ((entries (available-think-levels-for-selector buffer)))
    (unless entries
      (error "Think levels are not available for the active model."))
    (let ((entry (find level
                       entries
                       :key (lambda (item)
                              (or (getf item :level) ""))
                       :test #'string=)))
      (unless entry
        (error "Unknown think level selection: ~S" level))
      (apply-buffer-think-level-selection buffer entry)
      entry)))

(defun toggle-chat-skill-for-buffer (buffer skill-key)
  "Toggle SKILL-KEY and record feedback in BUFFER."
  (let ((skill (find-skill-by-path skill-key :include-disabled t)))
    (unless skill
      (error "Unknown skill: ~A" skill-key))
    (let ((enabled-p (not (skill-enabled-p skill))))
      (set-skill-enabled skill enabled-p)
      (buffer-insert-system-message
       buffer
       (format nil "[Skill ~A ~A]"
               (skill-name skill)
               (if enabled-p "enabled" "disabled")))
      enabled-p)))

(defun toggle-chat-package-for-buffer (buffer package-name)
  "Toggle explicit BUFFER enablement for PACKAGE-NAME."
  (unless buffer
    (error "Package toolbar toggles require a chat buffer."))
  (let* ((name (manifest-package-name package-name))
         (agent (buffer-agent-name buffer))
         (definition (find-installed-package name :buffer buffer))
         (previous-scope (package-enablement-scope
                          name
                          :buffer buffer
                          :agent-name agent))
         (had-context-p (buffer-has-conversation-context-p buffer))
         (enabled-p (not (buffer-package-name-enabled-p buffer name))))
    (unless definition
      (error "Unknown package: ~A" package-name))
    (set-buffer-package-name-enabled buffer name enabled-p)
    (if enabled-p
        (maybe-insert-enabled-package-context
         buffer definition previous-scope :buffer had-context-p)
        (remove-package-context-messages buffer name))
    (sync-buffer-system-prompt-display buffer)
    (load-active-packages :buffer buffer)
    (maybe-run-hook-with-args
     '*package-enablement-changed-hook*
     name
     (package-enablement-scope name :buffer buffer :agent-name agent)
     buffer
     agent)
    (buffer-insert-system-message
     buffer
     (format nil "[Package ~A ~A for this buffer]"
             name
             (if enabled-p "enabled" "disabled")))
    enabled-p))

(defun chat-recurse-readable-form (form)
  "Return FORM printed safely for a child Lisp process command line."
  (let ((*print-readably* nil)
        (*print-escape* t)
        (*print-array* nil)
        (*print-pretty* nil))
    (prin1-to-string form)))

(defun chat-recurse-source-root ()
  "Return the RPLACA source root used for recurse launches."
  (uiop:ensure-directory-pathname
   (or (ignore-errors (asdf:system-source-directory :rplaca))
       (truename "."))))

(defun chat-recurse-quicklisp-setup ()
  "Return the Quicklisp setup file used for recurse launches."
  (or (let ((env (uiop:getenv "RPLACA_QUICKLISP_SETUP")))
        (and env
             (plusp (length env))
             (probe-file env)))
      (probe-file
       (merge-pathnames #P"quicklisp/setup.lisp"
                        (user-homedir-pathname)))
      (error 'simple-error
             :format-control
             "Cannot recurse without Quicklisp setup. Set RPLACA_QUICKLISP_SETUP or install ~/quicklisp/setup.lisp."
             :format-arguments nil)))

(defun chat-recurse-session-name (buffer)
  "Return a unique session name for BUFFER's recurse child."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time (get-universal-time))
    (format nil "~A recurse ~4,'0D~2,'0D~2,'0D-~2,'0D~2,'0D~2,'0D-~D"
            (buffer-name buffer)
            year
            month
            date
            hour
            minute
            second
            (get-internal-real-time))))

(defun chat-recurse-window-title (buffer)
  "Return the window title for BUFFER's recurse child."
  (format nil "RPLACA Recurse - ~A" (buffer-name buffer)))

(defun chat-recurse-startup-form
    (buffer &key session-name window-title working-directory)
  "Return the child-process startup form for BUFFER's recurse launch."
  (let ((session-name (or session-name (chat-recurse-session-name buffer)))
        (window-title (or window-title (chat-recurse-window-title buffer)))
        (working-directory
          (normalize-buffer-working-directory
           (or working-directory
               (buffer-working-directory buffer)))))
    (format nil
            "(rplaca:rplaca-main :session-name ~A :agent-name ~A :window-title ~A :working-directory ~A)"
            (chat-recurse-readable-form session-name)
            (chat-recurse-readable-form (buffer-agent-name buffer))
            (chat-recurse-readable-form window-title)
            (chat-recurse-readable-form (namestring working-directory)))))

(defun chat-recurse-launch-spec
    (buffer &key repo-root quicklisp-setup session-name window-title
                  working-directory)
  "Return a launch plist for a fresh child RPLACA process for BUFFER."
  (let* ((source-root
           (uiop:ensure-directory-pathname
            (or repo-root (chat-recurse-source-root))))
         (quicklisp-setup
           (or quicklisp-setup (chat-recurse-quicklisp-setup)))
         (session-name (or session-name (chat-recurse-session-name buffer)))
         (window-title (or window-title (chat-recurse-window-title buffer)))
         (working-directory
           (normalize-buffer-working-directory
            (or working-directory
                (buffer-working-directory buffer))))
         (build-cache-script
           (merge-pathnames #P"scripts/build-cache.lisp" source-root))
         (argv
           (list "sbcl"
                 "--noinform"
                 "--eval" "(require :asdf)"
                 "--load" (namestring build-cache-script)
                 "--load" (namestring quicklisp-setup)
                 "--eval"
                 (format nil
                         "(rplaca/build-cache:maybe-clean-build-cache :environment-variable ~S)"
                         "RPLACA_RUN_CLEAN_BUILD")
                 "--eval"
                 (format nil
                         "(push (truename ~S) asdf:*central-registry*)"
                         (namestring source-root))
                 "--eval" "(ql:quickload :rplaca)"
                 "--eval" "(asdf:load-system :rplaca :force t)"
                 "--eval"
                 (chat-recurse-startup-form
                  buffer
                  :session-name session-name
                  :window-title window-title
                  :working-directory working-directory)
                 "--eval" "(uiop:quit)")))
    (list :directory source-root
          :argv argv
          :session-name session-name
          :window-title window-title
          :working-directory working-directory)))

(defun launch-chat-recurse (buffer)
  "Spawn a fresh child RPLACA process for BUFFER and return its launch plist."
  (let* ((spec (chat-recurse-launch-spec buffer))
         (process
           (uiop:launch-program
            (getf spec :argv)
            :directory (getf spec :directory)
            :input nil
            :output :interactive
            :error-output :interactive
            :ignore-error-status t)))
    (setf (getf spec :process) process)
    spec))

(define-rplaca-chat-frame-command
    (com-show-message-metadata :name nil)
    ((msg 'chat-message))
  (unless (open-message-help-window msg)
    (clim:with-application-frame (frame)
      (buffer-insert-system-message
       (chat-frame-buffer frame)
       "[Unable to open message metadata: help-frame worker unavailable]")
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-submit-compose :name nil)
    ()
  (clim:with-application-frame (frame)
    (submit-chat-compose-pane
     frame
     (clim:find-pane-named frame 'compose))))

(defun chat-interaction-candidate-current-p (frame ref)
  "Return true only when REF exactly names a current FRAME candidate."
  (let* ((state (chat-frame-interaction-state frame))
         (*chat-interaction-state* state))
    (when (typep ref 'chat-interaction-candidate-ref)
      (let* ((kind (chat-interaction-candidate-ref-kind ref))
             (index (chat-interaction-candidate-ref-index ref))
             (item (chat-interaction-candidate-ref-item ref))
             (items
               (case kind
                 (:minibuffer *minibuffer-filtered-items*)
                 (:session-tree *session-tree-selector-filtered-items*)
                 (:slash *slash-completion-filtered-items*)
                 (:skill *skill-completion-filtered-items*)
                 (otherwise nil)))
             (active-p
               (case kind
                 (:minibuffer *minibuffer-active*)
                 (:session-tree
                  (and *session-tree-selector-active*
                       (eq *session-tree-selector-buffer*
                           (chat-frame-buffer frame))))
                 (:slash
                  (and *slash-completion-active*
                       (eq *slash-completion-buffer*
                           (chat-frame-buffer frame))))
                 (:skill
                  (and *skill-completion-active*
                       (eq *skill-completion-buffer*
                           (chat-frame-buffer frame))))
                 (otherwise nil))))
        (and (eq state (chat-interaction-candidate-ref-state ref))
             (= (chat-interaction-state-generation state)
                (chat-interaction-candidate-ref-generation ref))
             active-p
             (integerp index)
             (<= 0 index)
             (< index (length items))
             (eq item (nth index items)))))))

(defun choose-chat-interaction-candidate (frame ref)
  "Choose current semantic REF and reject stale presentation records."
  (let ((*chat-interaction-state* (chat-frame-interaction-state frame)))
    (unless (chat-interaction-candidate-current-p frame ref)
      (file-debug-event
       "stale-interaction-candidate"
       :kind (and (typep ref 'chat-interaction-candidate-ref)
                  (chat-interaction-candidate-ref-kind ref)))
      (request-chat-frame-redisplay frame)
      (return-from choose-chat-interaction-candidate nil))
    (let ((kind (chat-interaction-candidate-ref-kind ref))
          (index (chat-interaction-candidate-ref-index ref))
          (item (chat-interaction-candidate-ref-item ref)))
      (call-with-chat-frame-buffer-transition
       frame
       (lambda (buffer)
         (case kind
           (:minibuffer
            (setf *minibuffer-selected-index* index)
            (minibuffer-confirm))
           (:session-tree
            (let ((callback *session-tree-selector-callback*))
              (session-tree-selector-deactivate)
              (when callback
                (funcall callback item))))
           (:slash
            (setf *slash-completion-selected-index* index)
            (insert-selected-slash-completion buffer))
           (:skill
            (setf *skill-completion-selected-index* index)
            (insert-selected-skill-completion buffer)))
         t)
       :focus-compose t))))

(define-rplaca-chat-frame-command
    (com-chat-select-interaction-candidate :name nil)
    ((ref 'chat-interaction-candidate))
  (clim:with-application-frame (frame)
    (choose-chat-interaction-candidate frame ref)))

(dolist (gesture '((#\Return) (#\Newline)))
  (clim:add-keystroke-to-command-table
   'rplaca-chat-frame
   gesture
   :command '(com-chat-submit-compose)
   :errorp nil))

(defparameter *chat-compose-drei-owned-commands*
  '(send-message
    self-insert-command
    insert-newline-command
    insert-tab-command
    beginning-of-line-command
    end-of-line-command
    forward-char-command
    backward-char-command
    forward-word-command
    backward-word-command
    beginning-of-buffer-command
    end-of-buffer-command
    previous-line-command
    next-line-command
    set-mark-command
    exchange-point-and-mark-command
    mark-whole-buffer-command
    kill-region-command
    copy-region-command
    kill-line-command
    kill-backward-line-command
    kill-word-command
    backward-kill-word-command
    yank-command
    yank-pop-command
    yank-previous-command-first-arg-command
    yank-previous-command-last-arg-command
    delete-char-backward-command
    delete-char-forward-command
    search-forward-command
    search-backward-command)
  "RPLACA keymap commands that the Drei compose editor should handle itself.

The compose pane inherits the chat frame command table before Drei's editor
command tables, so binding these in the frame table would steal ordinary text
editing gestures such as C-a, C-k, and M-f from Drei.  Application-level
bindings such as M-x, C-x b, C-h b, C-c t, and scrolling commands are installed
in the frame table and remain available while focus is in compose.")

(defun chat-compose-application-command-p (command)
  "Return true when COMMAND should be dispatched by the chat frame in compose."
  (and command
       (not (member command *chat-compose-drei-owned-commands* :test #'eq))))

(defun chat-control-character-gesture (char)
  "Return the CLIM gesture for RPLACA control character CHAR."
  (let ((code (char-code char)))
    (cond
      ((= code 0) '(#\Space :control))
      ((or (eql char #\Return) (eql char #\Newline)) (list char :control))
      ((and (>= code 1) (<= code 26))
       (list (code-char (+ (char-code #\a) code -1)) :control))
      ((= code 27) '(#\Esc))
      ((= code 127) '(#\Rubout))
      (t (list char)))))

(defun chat-key-component-gesture (component &optional modifier)
  "Return a one-gesture CLIM key spec for one RPLACA key COMPONENT."
  (let ((gesture
          (cond
            ((characterp component)
             (cond
               ((< (char-code component) 32)
                (chat-control-character-gesture component))
               ((upper-case-p component)
                (list component :shift))
               (t
                (list component))))
            ((keywordp component)
             (list component))
            (t
             (list component)))))
    (if modifier
        (append gesture (list modifier))
        gesture)))

(defun chat-keyspec-gestures (key)
  "Translate one RPLACA keymap KEY into ESA/CLIM gesture sequence syntax."
  (cond
    ((characterp key)
     (list (chat-key-component-gesture key)))
    ((keywordp key)
     (list (chat-key-component-gesture key)))
    ((and (consp key) (eq (first key) :meta))
     (list (chat-key-component-gesture (second key) :meta)))
    ((and (consp key) (eq (first key) :alt))
     (list (chat-key-component-gesture (second key) :meta)))
    ((and (consp key) (eq (first key) :ctrl))
     (list (chat-key-component-gesture (second key) :control)))
    ((and (consp key) (eq (first key) :ctrl-x))
     (list (chat-key-component-gesture #\x :control)
           (chat-key-component-gesture (second key))))
    ((and (consp key) (eq (first key) :ctrl-c))
     (list (chat-key-component-gesture #\c :control)
           (chat-key-component-gesture (second key))))
    ((and (consp key) (eq (first key) :ctrl-h))
     (list (chat-key-component-gesture #\h :control)
           (chat-key-component-gesture (second key))))
    (t nil)))

(defun chat-keyspec-uppercase-aliases (gestures)
  "Return backend-compatible aliases for uppercase character gestures.

McCLIM backends differ in how a typed uppercase letter is represented in key
events: uppercase character plus :SHIFT, uppercase character without :SHIFT,
or lowercase character plus :SHIFT.  Install all three spellings for
application command keys so prefix bindings such as C-c V work consistently
from the Drei compose pane."
  (labels ((uppercase-shift-gesture-p (gesture)
             (and (consp gesture)
                  (characterp (first gesture))
                  (upper-case-p (first gesture))
                  (member :shift (rest gesture))))
           (without-shift (gesture)
             (if (uppercase-shift-gesture-p gesture)
                 (remove :shift gesture)
                 gesture))
           (lowercase-with-shift (gesture)
             (if (uppercase-shift-gesture-p gesture)
                 (cons (char-downcase (first gesture))
                       (rest gesture))
                 gesture))
           (maybe-alias (transform)
             (let ((alias (mapcar transform gestures)))
               (unless (equal alias gestures)
                 alias))))
    (remove nil
            (list (maybe-alias #'without-shift)
                  (maybe-alias #'lowercase-with-shift))
            :test #'equal)))

(defun install-chat-frame-keybindings (&optional (keymap *default-keymap*))
  "Install application-level RPLACA key bindings into the chat command table.

Drei gadgets already inherit the frame command table.  Installing only
application-level bindings there lets global RPLACA/ESA keys work from the
compose pane while leaving text editing keys to Drei's editor tables."
  (when keymap
    (maphash
     (lambda (key command)
       (when (chat-compose-application-command-p command)
         (let ((gestures (chat-keyspec-gestures key)))
           (when gestures
             (dolist (key-gestures
                      (cons gestures
                            (chat-keyspec-uppercase-aliases gestures)))
               (handler-case
                   ;; ESA's partial command parser evaluates supplied argument
                   ;; forms before executing the command.  Quote list-valued
                   ;; keys so (:META #\x) remains data instead of a function
                   ;; call whose operator is :META.
                   (esa:set-key (if (eq command 'customize-appearance-command)
                                    '(com-chat-customize-appearance)
                                    `(com-chat-dispatch-key ',key))
                                'rplaca-chat-frame
                                key-gestures)
                 (clim:command-already-present () nil)))))))
     (keymap-bindings keymap)))
  (dolist (gesture '((#\Return) (#\Newline)))
    (clim:add-keystroke-to-command-table
     'rplaca-chat-frame
     gesture
     :command '(com-chat-submit-compose)
     :errorp nil)))

(define-rplaca-chat-frame-command
    (com-chat-dispatch-key :name nil)
    ((key 't))
  (clim:with-application-frame (frame)
    (call-with-chat-frame-buffer-transition
     frame
     (lambda (buffer)
       (let ((result (handle-key-event buffer key)))
         (when (eq result :quit)
           (clim:frame-exit frame))
         result)))))

(install-chat-frame-keybindings)

(define-rplaca-chat-frame-command
    (com-chat-stop-response :name "Stop Response")
    ()
  (clim:with-application-frame (frame)
    (when (stop-streaming-response (chat-frame-buffer frame))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-open-buffer-selector :name "Switch Buffer")
    ()
  (clim:with-application-frame (frame)
    (call-with-chat-frame-buffer-transition
     frame
     #'minibuffer-select-buffer-command
     :state-only t)))

(define-rplaca-chat-frame-command
    (com-chat-open-model-selector :name "Select Model")
    ()
  (clim:with-application-frame (frame)
    (call-with-chat-frame-buffer-transition
     frame
     #'minibuffer-select-model-command
     :state-only t)))

(define-rplaca-chat-frame-command
    (com-chat-open-manual :name "RPLACA Manual")
    ()
  (clim:with-application-frame (frame)
    (call-with-chat-frame-buffer-transition
     frame
     #'rplaca-manual-command
     :focus-compose t)))

(define-rplaca-chat-frame-command
    (com-chat-toggle-tool-results :name "Toggle Tool Results")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-tool-results-command)))

(define-rplaca-chat-frame-command
    (com-chat-toggle-reasoning-output :name "Toggle Reasoning Output")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-reasoning-output-command)))

(define-rplaca-chat-frame-command
    (com-chat-toggle-metadata-output :name "Toggle Metadata Output")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-metadata-output-command)))

(define-rplaca-chat-frame-command
    (com-chat-toggle-debug-mode :name "Toggle Debug Mode")
    ()
  (clim:with-application-frame (frame)
    (run-chat-frame-buffer-command frame #'toggle-debug-mode-command)))

(define-rplaca-chat-frame-command
    (com-chat-open-package-dashboard :name "Open Package Dashboard")
    ()
  (clim:with-application-frame (frame)
    (call-with-chat-frame-buffer-transition
     frame #'package-dashboard-command
     :focus-compose t)))

(define-rplaca-chat-frame-command
    (com-chat-open-skill-selector :name "Toggle Skill")
    ()
  (clim:with-application-frame (frame)
    (minibuffer-toggle-skill-command (chat-frame-buffer frame))
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-open-effort-selector :name "Select Think Level")
    ()
  (clim:with-application-frame (frame)
    ;; The minibuffer renders each choice as a semantic CLIM presentation and
    ;; validates the selected candidate against frame-owned interaction state.
    (select-think-level-command (chat-frame-buffer frame))
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-toggle-skill :name nil)
    ((skill-key 'string))
  (clim:with-application-frame (frame)
    (toggle-chat-skill-for-buffer (chat-frame-buffer frame) skill-key)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-toggle-package :name nil)
    ((package-name 'string))
  (clim:with-application-frame (frame)
    (toggle-chat-package-for-buffer (chat-frame-buffer frame) package-name)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-select-effort :name nil)
    ((level 'string))
  (clim:with-application-frame (frame)
    (select-chat-effort-for-buffer (chat-frame-buffer frame) level)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-safe-reload :name "Safe Reload")
    ()
  (clim:with-application-frame (frame)
    (safe-reload-rplaca-command (chat-frame-buffer frame))
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-refresh-font-inventory :name "Refresh Font Inventory")
    ()
  (clim:with-application-frame (frame)
    (refresh-font-inventory-command frame)
    (appearance-editor-record-status frame :refresh-fonts :queued)))

(define-rplaca-chat-frame-command
    (com-chat-customize-appearance :name "Customize Appearance")
    ()
  (clim:with-application-frame (frame)
    (call-with-chat-frame-buffer-transition
     frame #'customize-appearance-command
     :focus-compose t)))

(define-rplaca-chat-frame-command
    (com-chat-switch-appearance-theme :name "Switch Appearance Theme")
    ((theme 'appearance-theme-ref))
  (clim:with-application-frame (frame)
    (switch-appearance-theme-command frame theme)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-describe-current-appearance :name "Describe Current Appearance")
    ()
  (clim:with-application-frame (frame)
    (describe-current-appearance-command frame)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-apply-staged-appearance :name "Apply Staged Appearance")
    ()
  (clim:with-application-frame (frame)
    (apply-staged-appearance-command frame)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-save-appearance :name "Save Appearance")
    ()
  (clim:with-application-frame (frame)
    (save-appearance-command frame)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-revert-staged-appearance :name "Revert Staged Appearance")
    ()
  (clim:with-application-frame (frame)
    (revert-staged-appearance-command frame)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-reload-appearance-file :name "Reload Appearance File")
    ()
  (clim:with-application-frame (frame)
    (reload-appearance-file-command frame)
    (request-chat-frame-redisplay frame)))

(define-rplaca-chat-frame-command
    (com-chat-activate-appearance-editor-ref :name nil)
    ((ref 'appearance-activation-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (case (appearance-editor-ref-value ref)
        (:apply (apply-staged-appearance-command frame))
        (:save (save-appearance-command frame))
        (:revert (revert-staged-appearance-command frame))
        (:reload (reload-appearance-file-command frame))
        (:refresh-fonts
         (refresh-font-inventory-command frame)
         (appearance-editor-record-status frame :refresh-fonts :queued)))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-select-appearance-theme :name nil)
    ((ref 'appearance-theme-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (switch-appearance-theme-command frame ref)
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-select-appearance-role :name nil)
    ((ref 'appearance-role-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (setf (chat-frame-appearance-editor-role frame)
            (appearance-editor-ref-value ref)
            (chat-frame-appearance-editor-font-family frame) nil
            (chat-frame-appearance-editor-font-face frame) nil)
      (appearance-editor-record-status frame :select-role :selected
                                       :role (appearance-editor-ref-value ref))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-select-appearance-font-family :name nil)
    ((ref 'appearance-port-font-family-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (setf (chat-frame-appearance-editor-font-family frame)
            (appearance-editor-ref-value ref)
            (chat-frame-appearance-editor-font-face frame) nil)
      (appearance-editor-record-status frame :select-font-family :selected
                                       :family (appearance-editor-ref-value ref))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-select-appearance-font-face :name nil)
    ((ref 'appearance-port-font-face-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (setf (chat-frame-appearance-editor-font-face frame)
            (appearance-editor-ref-value ref))
      (appearance-editor-record-status frame :select-font-face :selected
                                       :face (appearance-editor-ref-value ref))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-select-appearance-font-size :name nil)
    ((ref 'appearance-port-font-size-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (appearance-editor-stage-role-font
       frame (appearance-editor-ref-value ref))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-describe-appearance-diagnostic :name nil)
    ((ref 'appearance-diagnostic-ref))
  (let ((frame (appearance-editor-ref-frame ref)))
    (when (eq frame clim:*application-frame*)
      (appearance-editor-record-status
       frame :diagnostic :described
       :diagnostic (appearance-editor-ref-value ref))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-chat-recurse :name "Recurse")
    ()
  (clim:with-application-frame (frame)
    (let ((buffer (chat-frame-buffer frame)))
      (handler-case
          (let ((spec (launch-chat-recurse buffer)))
            (buffer-insert-system-message
             buffer
             (format nil
                     "[Opened recurse frame ~A for session ~A in ~A]"
                     (getf spec :window-title)
                     (getf spec :session-name)
                     (namestring (getf spec :working-directory)))))
        (error (condition)
          ;; Process creation is an application action, not part of CLIM's
          ;; frame lifecycle.  Report it without unwinding ESA's top level.
          (file-debug-event
           "chat-recurse-launch-error"
           :buffer-name (buffer-name buffer)
           :condition (format nil "~A" condition))
          (buffer-insert-system-message
           buffer
           (format nil "[Unable to open recurse frame: ~A]" condition))))
      (request-chat-frame-redisplay frame))))

(define-rplaca-chat-frame-command
    (com-toggle-package-dashboard-entry :name nil)
    ((ref 'package-dashboard-entry-ref))
  (let ((dashboard (getf ref :dashboard-buffer))
        (origin (getf ref :origin-buffer))
        (entry (getf ref :entry)))
    (when (and dashboard entry)
      (package-dashboard-toggle-entry dashboard entry :origin-buffer origin))))

(clim:define-presentation-to-command-translator describe-chat-message
    (chat-message com-show-message-metadata rplaca-chat-frame
     :gesture :describe
     :documentation "View message metadata"
     :menu nil)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-chat-interaction-candidate
    (chat-interaction-candidate com-chat-select-interaction-candidate
     rplaca-chat-frame
     :gesture :select
     :documentation "Choose candidate"
     :menu nil)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-package-dashboard-entry
    (package-dashboard-entry-ref com-toggle-package-dashboard-entry
     rplaca-chat-frame
     :gesture :select
     :documentation "Toggle package scope"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator activate-appearance-editor-action
    (appearance-activation-ref com-chat-activate-appearance-editor-ref
     rplaca-chat-frame
     :gesture :select
     :documentation "Run appearance action"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-appearance-theme
    (appearance-theme-ref com-chat-select-appearance-theme
     rplaca-chat-frame
     :gesture :select
     :documentation "Stage theme"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-appearance-role
    (appearance-role-ref com-chat-select-appearance-role
     rplaca-chat-frame
     :gesture :select
     :documentation "Customize role"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-appearance-font-family
    (appearance-port-font-family-ref com-chat-select-appearance-font-family
     rplaca-chat-frame
     :gesture :select
     :documentation "Choose font family"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-appearance-font-face
    (appearance-port-font-face-ref com-chat-select-appearance-font-face
     rplaca-chat-frame
     :gesture :select
     :documentation "Choose font face"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator select-appearance-font-size
    (appearance-port-font-size-ref com-chat-select-appearance-font-size
     rplaca-chat-frame
     :gesture :select
     :documentation "Stage font size"
     :menu t)
    (object)
  (list object))

(clim:define-presentation-to-command-translator describe-appearance-diagnostic
    (appearance-diagnostic-ref com-chat-describe-appearance-diagnostic
     rplaca-chat-frame
     :gesture :describe-presentation
     :documentation "Describe diagnostic"
     :menu t)
    (object)
  (list object))

(defun report-chat-frame-cleanup-error (frame phase condition &optional buffer)
  "Record a teardown CONDITION without allowing diagnostics to abort cleanup."
  (ignore-errors
    (file-debug-event "frame-cleanup-error"
                      :phase phase
                      :buffer-name
                      (and buffer (buffer-name buffer))
                      :frame-buffer-name
                      (buffer-name (chat-frame-buffer frame))
                      :condition (princ-to-string condition))))

(defun cleanup-chat-frame-runtime (frame hook)
  "Release FRAME-owned runtime resources and always retire its display hook.

Every buffer cancellation has its own error boundary.  A broken provider or
tool cleanup therefore cannot retain the dead frame through HOOK, prevent the
remaining buffers from being cancelled, or leave the frame marked running."
  (handler-case
      (bt:with-lock-held ((chat-frame-lifecycle-lock frame))
        (setf (chat-frame-lifecycle-state frame) :stopping))
    (error (condition)
      (report-chat-frame-cleanup-error frame :mark-stopping condition)))
  (unwind-protect
       ;; The chat frame is the application object for the in-process buffer
       ;; ring.  Closing it tears down operations from every buffer that could
       ;; otherwise retain the dead frame through display callbacks.
       (dolist (buffer
                (remove-duplicates
                 (cons (chat-frame-buffer frame) *buffer-ring*)
                 :test #'eq))
         (when buffer
           (handler-case
               (progn
                 ;; Closing the application frame is permanent disposal, not a
                 ;; temporary Stop.  Mark that ownership before cancellation so
                 ;; a late reaper silently protocol-completes and releases its
                 ;; exact teardown after this frame's wake hook is gone.
                 (dispose-buffer buffer)
                 ;; Apply any already-ready legacy/live delivery before retiring
                 ;; the hook.  A still-running disposal needs no future frame:
                 ;; its reaper follows the silent disposal branch above.
                 (deliver-buffer-runtime-stopped-notification buffer))
             (error (condition)
               (report-chat-frame-cleanup-error
                frame :cancel-buffer condition buffer)))))
    (handler-case
        (clear-chat-interaction-state (chat-frame-interaction-state frame))
      (error (condition)
        (report-chat-frame-cleanup-error
         frame :clear-interaction-state condition)))
    (when hook
      (handler-case
          (remove-hook '*buffer-display-wakeup-hook* hook)
        (error (condition)
          (report-chat-frame-cleanup-error frame :remove-hook condition))))
    (handler-case
        (bt:with-lock-held ((chat-frame-lifecycle-lock frame))
          (setf (chat-frame-lifecycle-state frame) :stopped))
      (error (condition)
        (report-chat-frame-cleanup-error frame :mark-stopped condition)))
    (ignore-errors
      (file-debug-event "frame-stopped"
                        :buffer-name
                        (buffer-name (chat-frame-buffer frame)))))
  (unregister-package-appearance-live-chat-frame frame)
  frame)

(defun call-with-chat-frame-runtime (frame continuation)
  "Run CONTINUATION with FRAME's top-level runtime installed and protected."
  (let ((hook nil)
        (*chat-interaction-state* (chat-frame-interaction-state frame)))
    ;; Pane configuration and hook registration can invoke extension code.
    ;; Establish cleanup first so no startup failure can strand :STARTING or a
    ;; partially installed display hook.
    (unwind-protect
         (progn
           (bt:with-lock-held ((chat-frame-lifecycle-lock frame))
             (setf (chat-frame-lifecycle-state frame) :starting))
           (register-package-appearance-live-chat-frame frame)
           (setf hook
                 (lambda (buf reason)
                   (when (and (or (eq reason :runtime-stopped-pending)
                                  (not *suppress-chat-redisplay-requests*))
                              (eq buf (chat-frame-buffer frame)))
                     (request-chat-frame-redisplay frame))))
           (add-hook '*buffer-display-wakeup-hook* hook :append t)
           (funcall continuation))
      (cleanup-chat-frame-runtime frame hook))))

(defmethod clim:run-frame-top-level :around ((frame rplaca-chat-frame) &key)
  (call-with-chat-frame-runtime frame (lambda () (call-next-method))))

(defun snapshot-package-appearance-frame-state (frame)
  "Capture FRAME's exact package-reconciliation state."
  (list :frame frame
        :sheet (chat-frame-grafted-top-level-sheet frame)
        :catalog (chat-frame-appearance-catalog frame)
        :profile (chat-frame-appearance-profile frame)
        :bundle (chat-frame-appearance-active-bundle frame)
        :revision (chat-frame-appearance-revision frame)
        :staged (chat-frame-appearance-staged-candidate frame)
        :persisted (chat-frame-appearance-persisted-profile frame)
        :font-inventory (chat-frame-appearance-font-inventory frame)
        :font-generation
        (chat-frame-appearance-font-inventory-generation frame)
        :resolved-roles
        (copy-package-runtime-hash-table
         (chat-frame-appearance-resolved-roles frame))
        :role-keys
        (copy-package-runtime-hash-table
         (chat-frame-appearance-role-keys frame))))

(defun copy-package-appearance-declaration-registry ()
  "Return a private owner/declaration registry snapshot."
  (copy-package-runtime-hash-table
   rplaca::*package-appearance-declarations*))

(defun restore-package-appearance-declaration-registry (snapshot)
  "Atomically publish a private declaration-registry copy."
  (setf rplaca::*package-appearance-declarations*
        (copy-package-runtime-hash-table snapshot)))

(defun begin-package-appearance-frame-batch ()
  "Exclude frame registration and snapshot the whole appearance transaction."
  (bt:acquire-lock *package-appearance-frame-batch-lock*)
  (handler-case
      (bt:with-lock-held (rplaca::*package-appearance-catalog-lock*)
        (bt:with-lock-held (*package-appearance-live-chat-frames-lock*)
          (let ((frames
                  (mapcar #'snapshot-package-appearance-frame-state
                          *package-appearance-live-chat-frames*)))
            (setf *active-package-appearance-frame-batch*
                  (list
                   :lock-held-p t
                   :catalog
                   (rplaca::package-appearance-current-catalog-under-lock)
                   :declarations (copy-package-appearance-declaration-registry)
                   :frames frames
                   :expected-frames (copy-list frames)
                   :concurrent-change-p nil)))))
    (error (condition)
      (bt:release-lock *package-appearance-frame-batch-lock*)
      (error condition))))

(defun package-appearance-reservation-matches-frame-snapshot-p
    (reservation snapshot)
  "Return true when RESERVATION began at the last batch checkpoint."
  (and (eq (getf reservation :frame) (getf snapshot :frame))
       (eq (getf reservation :expected-catalog) (getf snapshot :catalog))
       (eq (getf reservation :expected-profile) (getf snapshot :profile))
       (eq (getf reservation :expected-bundle) (getf snapshot :bundle))
       (= (getf reservation :expected-revision) (getf snapshot :revision))
       (eq (getf reservation :expected-staged) (getf snapshot :staged))
       (eq (getf reservation :expected-persisted)
           (getf snapshot :persisted))
       (eq (getf reservation :expected-font-inventory)
           (getf snapshot :font-inventory))
       (= (getf reservation :expected-font-generation)
          (getf snapshot :font-generation))))

(defun package-appearance-reservation-target-snapshot (reservation)
  "Return the logical committed frame state of RESERVATION."
  (list
   :frame (getf reservation :frame)
   :sheet (getf reservation :sheet)
   :catalog (getf reservation :catalog)
   :profile (getf reservation :profile)
   :bundle (getf reservation :bundle)
   :revision (getf reservation :target-revision)
   :staged (if (getf reservation :target-state-p)
               (getf reservation :target-staged)
               (getf reservation :expected-staged))
   :persisted (if (getf reservation :target-state-p)
                  (getf reservation :target-persisted)
                  (getf reservation :expected-persisted))
   :font-inventory
   (if (getf reservation :target-font-state-p)
       (getf reservation :target-font-inventory)
       (getf reservation :expected-font-inventory))
   :font-generation
   (if (getf reservation :target-font-state-p)
       (getf reservation :target-font-generation)
       (getf reservation :expected-font-generation))
   :resolved-roles
   (if (getf reservation :target-cache-p)
       (getf reservation :target-resolved-roles)
       (make-hash-table :test #'equal))
   :role-keys
   (if (getf reservation :target-cache-p)
       (getf reservation :target-role-keys)
       (make-hash-table :test #'equal))))

(defun checkpoint-package-appearance-frame-batch (reservations)
  "Advance the batch CAS checkpoint after one package-owned commit."
  (let ((batch *active-package-appearance-frame-batch*))
    (when batch
      (dolist (reservation reservations)
        (let ((expected
                (find (getf reservation :frame)
                      (getf batch :expected-frames)
                      :key (lambda (snapshot) (getf snapshot :frame))
                      :test #'eq)))
          (unless (and expected
                       (package-appearance-reservation-matches-frame-snapshot-p
                        reservation expected))
            (setf (getf batch :concurrent-change-p) t))))
      (setf (getf batch :expected-frames)
            (mapcar
             (lambda (snapshot)
               (let ((reservation
                       (find (getf snapshot :frame) reservations
                             :key (lambda (entry)
                                    (getf entry :frame))
                             :test #'eq)))
                 (if reservation
                     (package-appearance-reservation-target-snapshot
                      reservation)
                     snapshot)))
             (getf batch :frames)))))
  nil)

(defun make-package-appearance-batch-restoration-reservation
    (frame-snapshot expected-snapshot token)
  "Make a CAS reservation restoring one FRAME-SNAPSHOT."
  (let ((frame (getf frame-snapshot :frame)))
    (list
     :sheet (getf frame-snapshot :sheet)
     :frame frame
     :catalog (getf frame-snapshot :catalog)
     :profile (getf frame-snapshot :profile)
     :bundle (getf frame-snapshot :bundle)
     :target-revision (getf frame-snapshot :revision)
     :target-state-p t
     :target-staged (getf frame-snapshot :staged)
     :target-persisted (getf frame-snapshot :persisted)
     :target-font-state-p t
     :target-font-inventory (getf frame-snapshot :font-inventory)
     :target-font-generation (getf frame-snapshot :font-generation)
     :target-cache-p t
     :target-resolved-roles (getf frame-snapshot :resolved-roles)
     :target-role-keys (getf frame-snapshot :role-keys)
     :token token
     :expected-catalog (getf expected-snapshot :catalog)
     :expected-profile (getf expected-snapshot :profile)
     :expected-bundle (getf expected-snapshot :bundle)
     :expected-revision (getf expected-snapshot :revision)
     :expected-staged (getf expected-snapshot :staged)
     :expected-persisted (getf expected-snapshot :persisted)
     :expected-font-inventory (getf expected-snapshot :font-inventory)
     :expected-font-generation (getf expected-snapshot :font-generation)
     :expected-resolved-roles (getf expected-snapshot :resolved-roles)
     :expected-role-keys (getf expected-snapshot :role-keys)
     :origin-frame-p (eq frame clim:*application-frame*))))

(defun restore-package-appearance-frame-batch (snapshot)
  "Restore all catalog, declaration, and owning-frame state in SNAPSHOT."
  (when (getf snapshot :concurrent-change-p)
    (error "Appearance batch rollback refused: a frame changed outside package reconciliation."))
  (let ((token (make-appearance-package-transition-token))
        (reservations nil))
    (bt:with-lock-held (rplaca::*package-appearance-catalog-lock*)
      (setf reservations
            (mapcar
             (lambda (frame-snapshot)
               (let ((expected
                       (find (getf frame-snapshot :frame)
                             (getf snapshot :expected-frames)
                             :key (lambda (state) (getf state :frame))
                             :test #'eq)))
                 (make-package-appearance-batch-restoration-reservation
                  frame-snapshot expected token)))
             (getf snapshot :frames))
            (appearance-package-transition-token-expected-count token)
            (length reservations))
      (unwind-protect
           (progn
             (dolist (reservation reservations)
               (unless (release-package-appearance-frame-transition reservation)
                 (error "Appearance batch restoration release failed.")))
             (let ((current-catalog *package-appearance-catalog*)
                   (current-declarations
                     (copy-package-appearance-declaration-registry)))
               (finalize-package-appearance-frame-transition
                token reservations
                (lambda ()
                  (setf *package-appearance-catalog*
                        (getf snapshot :catalog))
                  (restore-package-appearance-declaration-registry
                   (getf snapshot :declarations)))
                (lambda ()
                  (setf *package-appearance-catalog* current-catalog)
                  (restore-package-appearance-declaration-registry
                   current-declarations)))))
        (unless
            (member (appearance-package-transition-token-state token)
                    '(:committing :committed)
                    :test #'eq)
          (abort-appearance-package-transition token)))))
  t)

(defun end-package-appearance-frame-batch (snapshot)
  "Release the registration exclusion held by SNAPSHOT."
  (when (getf snapshot :lock-held-p)
    (when (eq snapshot *active-package-appearance-frame-batch*)
      (setf *active-package-appearance-frame-batch* nil))
    (setf (getf snapshot :lock-held-p) nil)
    (bt:release-lock *package-appearance-frame-batch-lock*))
  nil)

(defun run-rplaca-chat-frame
    (buffer &key window-title (appearance-profile (make-appearance-profile)))
  "Run the fresh McCLIM chat frame for BUFFER using an immutable startup profile.

The initial :CLASSIC profile is deliberately passed as frame construction data.
It does not select a port, install named fonts, or alter the fixed-width pane
default shared by the Drei/ESA pane declarations."
  (let ((frame (clim:make-application-frame
                'rplaca-chat-frame
                :buffer buffer
                :appearance-profile appearance-profile
                :pretty-name (or window-title "RPLACA"))))
    (let ((*crash-report-frame* frame))
      (publish-crash-report-runtime-snapshot :phase :frame-created)
      (file-debug-event "frame-created"
                        :buffer-name (buffer-name buffer)
                        :window-title (or window-title "RPLACA"))
      (unwind-protect
           (clim:run-frame-top-level frame)
        (publish-crash-report-runtime-snapshot :phase :frame-stopped)))))

;; The declaration layer remains CLIM-free; this is its concrete McCLIM
;; adapter.  It plans before catalog publication and queues the actual frame
;; change through the regular window-manager event loop.
(setf *appearance-package-live-frame-provider* #'package-appearance-live-chat-frames
      *appearance-package-frame-transition-planner*
      #'package-appearance-frame-transition-plan
      *appearance-package-frame-transition-reserver*
      #'reserve-package-appearance-frame-transition
      *appearance-package-frame-transition-publisher*
      #'release-package-appearance-frame-transition
      *appearance-package-frame-transition-finalizer*
      #'finalize-package-appearance-frame-transition
      *appearance-package-batch-checkpoint-function*
      #'checkpoint-package-appearance-frame-batch
      *package-appearance-batch-begin-function*
      #'begin-package-appearance-frame-batch
      *package-appearance-batch-restore-function*
      #'restore-package-appearance-frame-batch
      *package-appearance-batch-end-function*
      #'end-package-appearance-frame-batch)
