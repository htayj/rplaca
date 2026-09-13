(in-package :rplaca/tests)

(in-suite llm-suite)

(defclass unprintable-eval-value () ())

(defmethod print-object ((object unprintable-eval-value) stream)
  (declare (ignore object stream))
  (error "cannot print eval value"))

(defun make-unprintable-eval-value ()
  "Return an object whose printer signals for lisp_eval tests."
  (make-instance 'unprintable-eval-value))

(defun temp-test-token-path (provider)
  (let* ((base (make-pathname :directory (list :absolute "tmp"
                                               (format nil "rplaca-llm-tests-~A"
                                                       (list (get-universal-time)
                                                             (get-internal-real-time)
                                                             (gensym))))))
         (filename (ecase provider
                     (:openai-codex "openai-codex-token")
                     (:zai "zai-api-key")
                     (:openrouter "openrouter-api-key"))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames filename base)))

(defun wait-for-llm-test (predicate &key (timeout 3.0))
  "Wait boundedly for asynchronous LLM test state and return its value."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop :for value := (funcall predicate)
          :when value :return value
          :when (>= (get-internal-real-time) deadline) :return nil
          :do (sleep 0.005))))

(defmacro with-provider-token-path-overrides ((_removed-provider-path openai-codex-path &optional zai-path) &body body)
  (declare (ignore _removed-provider-path))
  `(let ((original-provider-token-path
           (symbol-function 'rplaca::provider-token-path)))
     (unwind-protect
          (progn
            (setf (symbol-function 'rplaca::provider-token-path)
                  (lambda (provider)
                    (case provider
                      (:openai-codex ,openai-codex-path)
                      (:zai ,(or zai-path '(funcall original-provider-token-path provider)))
                      (otherwise
                       (funcall original-provider-token-path provider)))))
            ,@body)
       (setf (symbol-function 'rplaca::provider-token-path)
             original-provider-token-path))))

(defun temp-agent-defaults-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "rplaca-agent-defaults-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "agent-defaults.json" base)))

(defmacro with-agent-defaults-path-override ((path) &body body)
  `(let ((rplaca::*agent-defaults-path* ,path)
         (rplaca::*agent-defaults-registry* nil))
     ,@body))

(defun temp-package-test-directory (label)
  (make-pathname :directory (list :absolute "tmp"
                                  (format nil "rplaca-package-tests-~A-~36R-~36R-~A"
                                          label
                                          (get-universal-time)
                                          (get-internal-real-time)
                                          (gensym)))))

(defun default-package-test-channels ()
  (list (rplaca:make-package-channel
         :name "default"
         :root rplaca:*default-package-channel-directory*
         :description "Bundled RPLACA packages"
         :source :builtin)))

(defmacro with-agent-definition-registry-override (() &body body)
  `(let ((rplaca::*agent-definition-registry* (make-hash-table :test #'equal)))
     ,@body))

(defmacro with-subagent-registry-override (() &body body)
  `(let ((rplaca::*subagent-handle-counter* 0)
         (rplaca::*subagent-handles* (make-hash-table :test #'equal))
         (rplaca::*subagent-registry-lock*
           (bt:make-lock "test-subagent-registry")))
     ,@body))

(defmacro with-pipeline-definition-registry-override (() &body body)
  `(let ((rplaca::*pipeline-definition-registry*
           (make-hash-table :test #'equal))
         (rplaca::*pipeline-test-profile-registry*
           (make-hash-table :test #'equal))
         (rplaca:*default-pipeline-name* nil))
     ,@body))

(defun temp-codex-auth-path ()
  (let ((base (make-pathname :directory (list :absolute "tmp"
                                              (format nil "rplaca-codex-auth-~A"
                                                      (list (get-universal-time)
                                                            (get-internal-real-time)
                                                            (gensym)))))))
    (ensure-directories-exist (merge-pathnames #P".keep" base))
    (merge-pathnames "auth.json" base)))

(defmacro with-codex-auth-path-override ((path) &body body)
  `(let ((rplaca::*codex-auth-path* ,path))
     ,@body))

(defun write-agent-defaults-file (path json)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string json stream)))

(defmacro with-function-override ((name lambda-list &body implementation) &body body)
  `(let ((original-function (symbol-function ',name)))
     (unwind-protect
          (progn
            (setf (symbol-function ',name)
                  (lambda ,lambda-list
                    ,@implementation))
            ,@body)
       (setf (symbol-function ',name) original-function))))

(defclass controlled-character-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((contents
    :initarg :contents
    :initform ""
    :reader controlled-stream-contents)
   (position
    :initform 0
    :accessor controlled-stream-position)
   (block-first-read-p
    :initarg :block-first-read-p
    :initform nil
    :reader controlled-stream-block-first-read-p)
   (first-read-started-p
    :initform nil
    :accessor controlled-stream-first-read-started-p)
   (read-entered
    :initform (bt:make-semaphore :name "test-stream-read-entered")
    :reader controlled-stream-read-entered)
   (read-release
    :initform (bt:make-semaphore :name "test-stream-read-release")
    :reader controlled-stream-read-release)
   (closed-p
    :initform nil
    :accessor controlled-stream-closed-p)
   (close-count
    :initform 0
    :accessor controlled-stream-close-count)
   (lock
    :initform (bt:make-lock "test-controlled-stream")
    :reader controlled-stream-lock)))

(defmethod trivial-gray-streams:stream-read-char
    ((stream controlled-character-input-stream))
  (let ((block-p nil))
    (bt:with-lock-held ((controlled-stream-lock stream))
      (when (and (controlled-stream-block-first-read-p stream)
                 (not (controlled-stream-first-read-started-p stream)))
        (setf (controlled-stream-first-read-started-p stream) t
              block-p t)))
    (when block-p
      (bt:signal-semaphore (controlled-stream-read-entered stream))
      (bt:wait-on-semaphore (controlled-stream-read-release stream)
                            :timeout 2))
    (bt:with-lock-held ((controlled-stream-lock stream))
      (if (controlled-stream-closed-p stream)
          :eof
          (let ((position (controlled-stream-position stream))
                (contents (controlled-stream-contents stream)))
            (if (< position (length contents))
                (prog1 (char contents position)
                  (incf (controlled-stream-position stream)))
                :eof))))))

(defmethod close :around ((stream controlled-character-input-stream)
                          &key abort)
  (declare (ignore abort))
  (bt:with-lock-held ((controlled-stream-lock stream))
    (incf (controlled-stream-close-count stream))
    (setf (controlled-stream-closed-p stream) t))
  (bt:signal-semaphore (controlled-stream-read-release stream))
  (call-next-method))

(defmethod rplaca::interrupt-provider-stream-read
    ((stream controlled-character-input-stream))
  "Wake the controlled test read while leaving close ownership to its reader."
  (bt:signal-semaphore (controlled-stream-read-release stream))
  t)

#+sbcl
(defclass observed-provider-character-input-stream
    (trivial-gray-streams:fundamental-character-input-stream)
  ((underlying-stream
    :initarg :underlying-stream
    :reader observed-provider-underlying-stream)
   (read-entered
    :initform (bt:make-semaphore :name "test-provider-read-entered")
    :reader observed-provider-read-entered)
   (read-observed-p
    :initform nil
    :accessor observed-provider-read-observed-p)
   (lock
    :initform (bt:make-lock "test-observed-provider-stream")
    :reader observed-provider-stream-lock)))

#+sbcl
(defmethod trivial-gray-streams:stream-read-char
    ((stream observed-provider-character-input-stream))
  (let ((signal-p nil))
    (bt:with-lock-held ((observed-provider-stream-lock stream))
      (unless (observed-provider-read-observed-p stream)
        (setf (observed-provider-read-observed-p stream) t
              signal-p t)))
    (when signal-p
      (bt:signal-semaphore (observed-provider-read-entered stream)))
    (read-char (observed-provider-underlying-stream stream) nil :eof)))

#+sbcl
(defmethod trivial-gray-streams:stream-listen
    ((stream observed-provider-character-input-stream))
  (listen (observed-provider-underlying-stream stream)))

#+sbcl
(defmethod close :around ((stream observed-provider-character-input-stream)
                          &key abort)
  (unwind-protect
       (call-next-method)
    (ignore-errors
      (close (observed-provider-underlying-stream stream) :abort abort))))

#+sbcl
(defmethod rplaca::interrupt-provider-stream-read
    ((stream observed-provider-character-input-stream))
  (rplaca::interrupt-provider-stream-read
   (observed-provider-underlying-stream stream)))

(defun stream-state-reader-thread-snapshot (state)
  (bt:with-lock-held ((rplaca::stream-state-lock state))
    (rplaca::stream-state-reader-thread state)))

(defun join-test-stream-reader (state)
  (let ((thread (stream-state-reader-thread-snapshot state)))
    (when thread
      (rplaca::settle-stream-state-reader state))
    thread))

(defun write-test-file (path contents)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream)))

(defmacro with-tool-table-restored (&body body)
  `(let* ((snapshot (make-hash-table :test (hash-table-test rplaca::*tool-table*)))
          (agent-tool-snapshot
            (make-hash-table
             :test (hash-table-test rplaca::*agent-tool-metadata-table*)))
          (agent-tool-name-snapshot
            (make-hash-table
             :test (hash-table-test rplaca::*agent-tool-name-table*)))
          (package-test-root (temp-package-test-directory "llm-package-config"))
          (rplaca::*package-configuration-path*
           (merge-pathnames "packages.json"
                            (uiop:ensure-directory-pathname package-test-root)))
          (rplaca::*package-configuration* nil)
          (rplaca::*package-channels* (default-package-test-channels))
          (rplaca::*available-packages* nil)
          (rplaca::*package-registry-loaded-p* nil)
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*package-prompt-sections* nil)
          (*sessions-dir* (temp-session-test-directory "llm-sessions"))
          (rplaca::*buffer-ring* nil)
          (rplaca::*buffer-counter* 0)
          (rplaca::*startup-hook* nil)
          (rplaca::*initial-buffer-hook* nil)
          (rplaca::*before-command-hook* nil)
          (rplaca::*after-command-hook* nil)
          (rplaca::*before-tool-hook* nil)
          (rplaca::*after-tool-hook* nil)
          (rplaca::*before-send-message-hook* nil)
          (rplaca::*after-send-message-hook* nil)
          (rplaca::*after-buffer-create-hook* nil)
          (rplaca::*after-provider-response-hook* nil))
     (maphash (lambda (key value)
                (setf (gethash key snapshot) value))
              rplaca::*tool-table*)
     (maphash (lambda (key value)
                (setf (gethash key agent-tool-snapshot) value))
              rplaca::*agent-tool-metadata-table*)
     (maphash (lambda (key value)
                (setf (gethash key agent-tool-name-snapshot) value))
              rplaca::*agent-tool-name-table*)
     (unwind-protect
          (progn
            ,@body)
       (clrhash rplaca::*tool-table*)
       (maphash (lambda (key value)
                  (setf (gethash key rplaca::*tool-table*) value))
                snapshot)
       (clrhash rplaca::*agent-tool-metadata-table*)
       (maphash (lambda (key value)
                  (setf (gethash key rplaca::*agent-tool-metadata-table*)
                        value))
                agent-tool-snapshot)
       (clrhash rplaca::*agent-tool-name-table*)
       (maphash (lambda (key value)
                  (setf (gethash key rplaca::*agent-tool-name-table*) value))
                agent-tool-name-snapshot))))

(defun initialize-test-tools ()
  "Initialize the tool table with the bundled lispi package enabled."
  (rplaca::init-tools)
  (rplaca:set-package-enablement-scope "lispi" :global)
  (rplaca:load-active-packages))

(defun append-test-user-message (buf text)
  (rplaca::set-message-text (buffer-input-message buf) text)
  (buffer-finalize-input buf)
  (message-prev (buffer-input-message buf)))

(defun test-buffer-history-messages (buf)
  (loop :for msg := (buffer-first-message buf) :then (message-next msg)
        :while (and msg (not (eq msg (buffer-input-message buf))))
        :unless (rplaca::buffer-ephemeral-display-message-p msg)
        :collect msg))

(defun test-buffer-history-senders (buf)
  (mapcar #'message-sender (test-buffer-history-messages buf)))

(defun make-completed-stream-state-response (stop-reason content-blocks
                                             &optional usage)
  (let ((state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-stop-reason state) stop-reason
            (rplaca::stream-state-content-blocks state) (reverse content-blocks)
            (rplaca::stream-state-usage state) usage
            (rplaca::stream-state-done-p state) t))
    state))

(defun write-codex-auth-json (path payload)
  (ensure-directories-exist path)
  (with-open-file (stream path
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string (rplaca::api-json-encode payload) stream)))

(defun make-codex-chatgpt-auth-payload (&key
                                          (access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig")
                                          (refresh-token "chatgpt-refresh")
                                          (account-id "acct_123")
                                          (id-token "id-token-placeholder")
                                          openai-api-key
                                          (last-refresh "2026-04-08T12:00:00Z"))
  `((:auth--mode . "chatgpt")
    (:openai--api--key . ,openai-api-key)
    (:tokens . ((:id--token . ,id-token)
                (:access--token . ,access-token)
                (:refresh--token . ,refresh-token)
                (:account--id . ,account-id)))
    (:last--refresh . ,last-refresh)))

(defun make-codex-api-key-auth-payload (&key
                                          (api-key "sk-test-api-key")
                                          (last-refresh "2026-04-08T12:00:00Z"))
  `((:openai--api--key . ,api-key)
    (:last--refresh . ,last-refresh)))

(test provider-token-paths
  "Provider token paths are provider-specific."
  (let ((home (user-homedir-pathname)))
    (is (equal (merge-pathnames #P".config/rplaca/openai-codex-token" home)
               (rplaca::provider-token-path :openai-codex)))
    (is (equal (merge-pathnames #P".config/rplaca/zai-api-key" home)
               (rplaca::provider-token-path :zai)))
    (is (equal (merge-pathnames #P".config/rplaca/openrouter-api-key" home)
               (rplaca::provider-token-path :openrouter)))))

(test provider-token-path-unknown-provider
  "Unknown providers signal a clear error."
  (signals error
    (rplaca::provider-token-path :unknown-provider)))

(test init-tools-registers-pi-style-tools-by-default
  "Enabling the bundled lispi package exposes file tools beside core tools."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (initialize-test-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)
                             #'string<)))
      (is (equal '("edit" "find" "grep" "lisp_eval" "live_lisp_eval"
                   "read" "recovery_list" "write")
                 tool-names))
      (is (string= "RPLACA" rplaca:*lisp-eval-default-package*))
      (dolist (name '("read" "find" "grep" "write" "edit" "lisp_eval"))
        (is (not (null (gethash name rplaca::*tool-table*)))))
      (is (null (gethash "http_fetch" rplaca::*tool-table*)))
      (is (null (gethash "file_read" rplaca::*tool-table*)))
      (is (null (gethash "file_write" rplaca::*tool-table*)))
      (is (null (gethash "file_edit" rplaca::*tool-table*)))
      (is (null (gethash "shell_exec" rplaca::*tool-table*))))))

(test tool-definitions-for-api-returns-stable-name-order
  "Provider tool definitions are sorted by name for deterministic prompts."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (initialize-test-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (mapcar (lambda (tool)
                                 (cdr (assoc :name tool)))
                               tools)))
      (is (equal '("edit" "find" "grep" "lisp_eval" "live_lisp_eval"
                   "read" "recovery_list" "write")
                 tool-names)))))

(test mcclim-provider-live-lisp-eval-refusal-continues-tool-loop
  "A refused provider live eval produces a result and continues automatically."
  (with-tool-table-restored
    (initialize-test-tools)
    (let ((request-count 0))
      (with-function-override (rplaca::resolve-buffer-provider-and-model
                               (buffer)
                               (declare (ignore buffer))
                               (values :zai "glm-test" nil))
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                             reasoning-effort system-prompt
                                             service-tier)
                                 (declare (ignore provider messages callback
                                                  model max-tokens tools
                                                  reasoning-effort
                                                  system-prompt service-tier))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (rplaca::canonical-tool-use-block
                                        "call-1"
                                        "lisp_eval"
                                        '((:code . "(+ 1 1)")
                                          (:package . "RPLACA")))))
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list
                                       (rplaca::canonical-text-block
                                        "the result is 2")))))
          (let ((buf (make-buffer "mcclim-live-eval" :agent-name "agent")))
            (rplaca::start-streaming-response buf)
            (is (= 1 request-count))
            (is-true (rplaca::update-streaming-response buf))
            (is (null (buffer-pending-tool-execution buf)))
            (is (= 2 request-count))
            (is-false (rplaca::update-streaming-response buf))
            (let ((messages (test-buffer-history-messages buf)))
              (is (member :tool-result
                          (mapcar #'message-sender messages)))
              (is (some (lambda (text)
                          (and (search "lisp_eval REFUSED" text)
                               (search "trusted user command" text)))
                        (mapcar #'message-text messages)))
              (is (some (lambda (text)
                          (search "the result is 2" text))
                        (mapcar #'message-text messages))))))))))

(defun test-command-table-key-event (key-character &key key-name modifiers)
  "Return a synthetic key event for command-table lookup tests."
  (make-instance 'clim:key-press-event
                 :sheet nil
                 :x 0
                 :y 0
                 :key-name key-name
                 :key-character key-character
                 :modifier-state
                 (apply #'clim:make-modifier-state modifiers)))

(defun test-command-table-key-command
    (table key-character &key key-name modifiers)
  "Return the inherited command bound to KEY-CHARACTER in command TABLE."
  (let ((item (esa::find-gestures-with-inheritance
               (list (test-command-table-key-event key-character
                                                   :key-name key-name
                                                   :modifiers modifiers))
               table)))
    (and item (clim:command-menu-item-value item))))

(defun test-command-table-key-sequence-command (table event-specs)
  "Return the inherited command bound to EVENT-SPECS in command TABLE."
  (let ((item (esa::find-gestures-with-inheritance
               (mapcar (lambda (spec)
                         (apply #'test-command-table-key-event spec))
                       event-specs)
               table)))
    (and item (clim:command-menu-item-value item))))

(test mcclim-compose-return-keystrokes-submit-message
  "The McCLIM chat frame binds Return/Newline to submit the message."
  (let* ((buf (make-buffer "mcclim-ret-submit" :agent-name "agent"))
         (frame (clim:make-application-frame
                 'rplaca::rplaca-chat-frame
                 :buffer buf))
         (table (clim:find-command-table 'rplaca::rplaca-chat-frame))
         (menu-table (clim:frame-command-table frame))
         (drei-order-table
           (clim:make-command-table
            nil
            :inherit-from (list menu-table 'drei:editor-table))))
    (is (clim:command-accessible-in-command-table-p
         'rplaca::com-chat-submit-compose
         table))
    (is (equal '(rplaca::com-chat-submit-compose)
               (test-command-table-key-command table #\Return)))
    (is (equal '(rplaca::com-chat-submit-compose)
               (test-command-table-key-command table #\Newline)))
    (is (equal '(rplaca::com-chat-submit-compose)
               (test-command-table-key-command menu-table #\Return)))
    (is (equal '(rplaca::com-chat-submit-compose)
               (test-command-table-key-command drei-order-table #\Return)))
    (is (equal '(rplaca::com-chat-submit-compose)
               (test-command-table-key-command drei-order-table #\Newline)))
    (rplaca::init-default-keymap)
    (rplaca::install-chat-frame-keybindings)
    (is (equal '(rplaca::com-chat-dispatch-key '(:meta #\x))
               (test-command-table-key-command table #\x
                                               :modifiers '(:meta))))
    (is (equal '(rplaca::com-chat-dispatch-key '(:ctrl-x #\b))
               (test-command-table-key-sequence-command
                table
                `((#\x :modifiers (:control))
                  (#\b)))))
    (is (equal '(rplaca::com-chat-dispatch-key '(:ctrl-h #\b))
               (test-command-table-key-sequence-command
                drei-order-table
                `((#\h :modifiers (:control))
                  (#\b)))))
    (dolist (second-gesture
             (list #\V
                   (test-command-table-key-event #\V)
                   (test-command-table-key-event #\V :modifiers '(:shift))
                   (test-command-table-key-event #\v :modifiers '(:shift))))
      (is (equal '(rplaca::com-chat-dispatch-key '(:ctrl-c #\V))
                 (let ((item
                         (esa::find-gestures-with-inheritance
                          (list (test-command-table-key-event
                                 #\c :modifiers '(:control))
                                second-gesture)
                          table)))
                   (and item (clim:command-menu-item-value item))))))
    (is-false (equal '(rplaca::com-chat-dispatch-key #\Soh)
                     (test-command-table-key-command table #\a
                                                     :modifiers '(:control))))
    (is-true
     (rplaca::chat-compose-submit-event-p
      (make-instance 'clim:key-press-event
                     :sheet nil
                     :x 0
                     :y 0
                     :key-name nil
                     :key-character #\Newline
                     :modifier-state (clim:make-modifier-state))))
    (is-false
     (rplaca::chat-compose-submit-event-p
      (make-instance 'clim:key-press-event
                     :sheet nil
                     :x 0
                     :y 0
                     :key-name nil
                     :key-character #\Newline
                     :modifier-state (clim:make-modifier-state :control))))))

(test mcclim-compose-pane-is-drei-gadget
  "The chat compose pane uses a Drei gadget editor with RPLACA command tables."
  (let ((compose (make-instance 'rplaca::rplaca-chat-compose-pane)))
    (is (typep compose 'drei:drei-gadget-pane))
    (is (typep compose 'rplaca::rplaca-chat-compose-pane))
    (setf (clim:gadget-value compose) "hello")
    (is (string= "hello" (clim:gadget-value compose)))
    (rplaca::configure-chat-compose-pane compose)
    (is (eq :wrap* (clim:stream-end-of-line-action compose)))))

(test mcclim-compose-drei-control-editing-gestures
  "Compose-specific control edits remain native, undoable Drei gestures."
  (let* ((editor-table (clim:find-command-table 'drei:editor-table))
         (compose-table
           (clim:find-command-table
            'rplaca::rplaca-chat-compose-editing-table))
         (compose (make-instance 'rplaca::rplaca-chat-compose-pane
                                 :initial-contents "hello world"))
         (plain-drei (make-instance 'drei:drei-gadget-pane))
         (control-j (make-instance 'clim:key-press-event
                                   :sheet compose
                                   :x 0
                                   :y 0
                                   :key-name nil
                                   :key-character #\Newline
                                   :modifier-state
                                   (clim:make-modifier-state :control)))
         (control-b (make-instance 'clim:key-press-event
                                    :sheet compose
                                    :x 0
                                    :y 0
                                    :key-name nil
                                    :key-character #\b
                                    :modifier-state
                                    (clim:make-modifier-state :control)))
         (control-u (make-instance 'clim:key-press-event
                                   :sheet compose
                                   :x 0
                                   :y 0
                                   :key-name nil
                                   :key-character #\u
                                   :modifier-state
                                   (clim:make-modifier-state :control)))
         (control-w (make-instance 'clim:key-press-event
                                   :sheet compose
                                   :x 0
                                   :y 0
                                   :key-name nil
                                   :key-character #\w
                                   :modifier-state
                                   (clim:make-modifier-state :control)))
         (control-underscore
           (make-instance 'clim:key-press-event
                          :sheet compose
                          :x 0
                          :y 0
                          :key-name nil
                          :key-character #\_
                          :modifier-state
                          (clim:make-modifier-state :control)))
         (encoded-control-b (make-instance 'clim:key-press-event
                                            :sheet compose
                                            :x 0
                                            :y 0
                                            :key-name nil
                                            :key-character (code-char 2)
                                            :modifier-state
                                            (clim:make-modifier-state)))
         (meta-f (make-instance 'clim:key-press-event
                                :sheet compose
                                :x 0
                                :y 0
                                :key-name nil
                                :key-character #\f
                                :modifier-state
                                (clim:make-modifier-state :meta)))
         (control-backspace (make-instance 'clim:key-press-event
                                           :sheet compose
                                           :x 0
                                           :y 0
                                           :key-name nil
                                           :key-character #\Backspace
                                           :modifier-state
                                           (clim:make-modifier-state
                                            :control)))
         (control-named-backspace (make-instance 'clim:key-press-event
                                                 :sheet compose
                                                 :x 0
                                                 :y 0
                                                 :key-name :backspace
                                                 :key-character nil
                                                 :modifier-state
                                                 (clim:make-modifier-state
                                                  :control)))
         (plain-backspace (make-instance 'clim:key-press-event
                                         :sheet compose
                                         :x 0
                                         :y 0
                                         :key-name nil
                                         :key-character #\Backspace
                                         :modifier-state
                                         (clim:make-modifier-state)))
         (named-plain-backspace (make-instance 'clim:key-press-event
                                               :sheet compose
                                               :x 0
                                               :y 0
                                               :key-name :backspace
                                               :key-character #\Backspace
                                               :modifier-state
                                               (clim:make-modifier-state)))
         (modifier-only (make-instance 'clim:key-press-event
                                       :sheet compose
                                       :x 0
                                       :y 0
                                       :key-name :control
                                       :key-character nil
                                       :modifier-state
                                       (clim:make-modifier-state :control))))
    (is (equal '(drei-commands::com-newline-and-indent)
               (test-command-table-key-command editor-table #\j
                                               :modifiers '(:control))))
    (is (equal '(drei-commands::com-newline-and-indent)
               (test-command-table-key-command compose-table #\Newline
                                               :modifiers '(:control))))
    (is (equal `(drei-commands::com-backward-object
                 ,clim:*numeric-argument-marker*)
               (test-command-table-key-command editor-table #\b
                                               :modifiers '(:control))))
    (is (equal `(drei-commands::com-forward-word
                 ,clim:*numeric-argument-marker*)
               (test-command-table-key-command editor-table #\f
                                               :modifiers '(:meta))))
    (is (equal `(drei-commands::com-backward-kill-word
                 ,clim:*numeric-argument-marker*)
               (test-command-table-key-command compose-table #\Backspace
                                               :modifiers '(:control))))
    (is (equal `(drei-commands::com-backward-kill-word
                 ,clim:*numeric-argument-marker*)
               (test-command-table-key-command compose-table #\w
                                               :modifiers '(:control))))
    (is (equal '(drei-commands::com-kill-line 0 t)
               (rplaca::chat-compose-drei-direct-command control-u)))
    (is (equal `(drei-commands::com-backward-delete-object
                 ,clim:*numeric-argument-marker*
                 ,clim:*numeric-argument-marker*)
               (test-command-table-key-command editor-table #\Backspace)))
    (is-false
     (test-command-table-key-command editor-table #\Newline
                                     :modifiers '(:control)))
    (is-false
     (test-command-table-key-command editor-table #\Backspace
                                     :modifiers '(:control)))
    (is (member 'rplaca::rplaca-chat-compose-editing-table
                (drei-syntax:additional-command-tables compose editor-table)))
    (is-false
     (member 'rplaca::rplaca-chat-compose-editing-table
             (drei-syntax:additional-command-tables plain-drei editor-table)))
    (is-true (rplaca::chat-compose-drei-control-editing-event-p control-j))
    (is-false (rplaca::chat-compose-encoded-control-character
               plain-backspace))
    (is-false (rplaca::chat-compose-encoded-control-character
               named-plain-backspace))
    (is-false (rplaca::chat-compose-modified-key-event-p plain-backspace))
    (is-false (rplaca::chat-compose-modified-key-event-p modifier-only))
    (rplaca::process-chat-compose-drei-event compose control-j)
    (is (string= (format nil "~%hello world")
                 (clim:gadget-value compose)))
    (setf (clim:gadget-value compose) "hello world")
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (is-true (rplaca::chat-compose-modified-key-event-p control-b))
    (rplaca::process-chat-compose-drei-event compose control-b)
    (is (= (1- (length (clim:gadget-value compose)))
           (drei-buffer:offset (drei:point (drei:current-view compose)))))
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (is (eql #\b (rplaca::chat-compose-encoded-control-character
                  encoded-control-b)))
    (is-true (rplaca::chat-compose-modified-key-event-p encoded-control-b))
    (rplaca::process-chat-compose-drei-event compose encoded-control-b)
    (is (= (1- (length (clim:gadget-value compose)))
           (drei-buffer:offset (drei:point (drei:current-view compose)))))
    (setf (drei-buffer:offset (drei:point (drei:current-view compose))) 0)
    (is-true (rplaca::chat-compose-modified-key-event-p meta-f))
    (rplaca::process-chat-compose-drei-event compose meta-f)
    (is (= 5 (drei-buffer:offset (drei:point (drei:current-view compose)))))
    (setf (clim:gadget-value compose) "hello world")
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (rplaca::process-chat-compose-drei-event compose plain-backspace)
    (is (string= "hello worl" (clim:gadget-value compose)))
    (setf (clim:gadget-value compose) "hello world")
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (is-true (rplaca::chat-compose-drei-control-editing-event-p
              control-backspace))
    (rplaca::process-chat-compose-drei-event compose control-backspace)
    (is (string= "hello " (clim:gadget-value compose)))
    (setf (clim:gadget-value compose) "hello world")
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (is-true (rplaca::chat-compose-drei-control-editing-event-p
              control-named-backspace))
    (rplaca::process-chat-compose-drei-event compose control-named-backspace)
    (is (string= "hello " (clim:gadget-value compose)))
    (setf (clim:gadget-value compose) "hello world")
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (rplaca::process-chat-compose-drei-event compose control-w)
    (is (string= "hello " (clim:gadget-value compose)))
    (rplaca::process-chat-compose-drei-event compose control-underscore)
    (is (string= "hello world" (clim:gadget-value compose)))
    (setf (clim:gadget-value compose) (format nil "alpha~%beta"))
    (setf (drei-buffer:offset (drei:point (drei:current-view compose)))
          (length (clim:gadget-value compose)))
    (rplaca::process-chat-compose-drei-event compose control-u)
    (is (string= (format nil "alpha~%") (clim:gadget-value compose)))
    (rplaca::process-chat-compose-drei-event compose control-underscore)
    (is (string= (format nil "alpha~%beta")
                 (clim:gadget-value compose)))))

(test pinned-mcclim-word-kill-can-be-yanked
  "Pinned McCLIM's Edward word kill/yank works without application overrides."
  (let* ((compose (make-instance 'clim:text-editor-pane :value "button "))
         (buffer (climi::input-editor-buffer compose)))
    (let ((climi::*killring-uses-clipboard* nil))
      (is-true
       (handler-case
           (progn
             (climi::ie-erase-word compose buffer nil 1)
             t)
         (error () nil)))
      (is (string= "" (clim:gadget-value compose)))
      (is-true
       (handler-case
           (progn
             (climi::ie-yank-kill-ring compose buffer nil 1)
             t)
         (error () nil)))
      (is (string= "button " (clim:gadget-value compose))))))

(defclass soft-wrap-test-pane ()
  ((end-of-line-action :initform :scroll
                       :accessor soft-wrap-test-pane-end-of-line-action)))

(defmethod clim:stream-end-of-line-action ((pane soft-wrap-test-pane))
  (soft-wrap-test-pane-end-of-line-action pane))

(defmethod (setf clim:stream-end-of-line-action) (action (pane soft-wrap-test-pane))
  (setf (soft-wrap-test-pane-end-of-line-action pane) action))

(defclass no-soft-wrap-test-pane () ())

(test mcclim-compose-pane-uses-mcclim-soft-wrap-when-supported
  "The chat compose configuration uses CLIM stream soft wrapping when supported."
  (let ((pane (make-instance 'soft-wrap-test-pane)))
    (is (eq :scroll (clim:stream-end-of-line-action pane)))
    (rplaca::configure-chat-compose-pane pane)
    (is (eq :wrap* (clim:stream-end-of-line-action pane)))))

(test mcclim-compose-pane-skips-soft-wrap-when-unsupported
  "The compatibility helper leaves panes without the CLIM writer untouched."
  (let ((pane (make-instance 'no-soft-wrap-test-pane)))
    (is (eq pane (rplaca::configure-chat-compose-pane pane)))))

(test mcclim-compose-pane-does-not-hard-wrap-text-editor-value
  "Current McCLIM text-editor gadgets do not get approximate hard newlines inserted."
  (let ((pane (make-instance 'clim:text-editor-pane :value "hello world")))
    (rplaca::configure-chat-compose-pane pane)
    (is (string= "hello world" (clim:gadget-value pane)))))

(defclass transcript-scroll-test-region ()
  ((height :initarg :height
           :reader transcript-scroll-test-region-height)))

(defmethod clim:bounding-rectangle-height ((region transcript-scroll-test-region))
  (transcript-scroll-test-region-height region))

(defclass transcript-scroll-test-pane ()
  ((history :initarg :history
            :reader transcript-scroll-test-pane-history)
   (viewport :initarg :viewport
             :reader transcript-scroll-test-pane-viewport)
   (scroll-x :initform nil
             :accessor transcript-scroll-test-pane-scroll-x)
   (scroll-y :initform nil
             :accessor transcript-scroll-test-pane-scroll-y)))

(defmethod clim:stream-output-history ((pane transcript-scroll-test-pane))
  (transcript-scroll-test-pane-history pane))

(defmethod clim:pane-viewport ((pane transcript-scroll-test-pane))
  (transcript-scroll-test-pane-viewport pane))

(defmethod clim:scroll-extent ((pane transcript-scroll-test-pane) x y)
  (setf (transcript-scroll-test-pane-scroll-x pane) x
        (transcript-scroll-test-pane-scroll-y pane) y))

(test mcclim-transcript-bottom-scroll-uses-output-and-viewport-heights
  "Transcript tail following computes Listener-style bottom scroll offsets."
  (is (= 800
         (rplaca::chat-transcript-bottom-scroll-y-from-heights 1200 400)))
  (is (= 0
         (rplaca::chat-transcript-bottom-scroll-y-from-heights 300 400)))
  (let* ((history (make-instance 'transcript-scroll-test-region :height 1200))
         (viewport (make-instance 'transcript-scroll-test-region :height 400))
         (pane (make-instance 'transcript-scroll-test-pane
                              :history history
                              :viewport viewport)))
    (is (= 800 (rplaca::chat-transcript-scroll-to-bottom pane)))
    (is (= 0 (transcript-scroll-test-pane-scroll-x pane)))
    (is (= 800 (transcript-scroll-test-pane-scroll-y pane)))))

(test mcclim-chat-menu-bar-exposes-toolbar-commands
  "The McCLIM chat frame exposes its MVP toolbar through command-table menus."
  (let* ((menu-table (clim:find-command-table
                      'rplaca::rplaca-chat-frame))
         (chat-menu (clim:find-menu-item "Chat" menu-table :errorp nil))
         (view-menu (clim:find-menu-item "View" menu-table :errorp nil))
         (skills-menu (clim:find-menu-item "Skills" menu-table :errorp nil))
         (packages-menu (clim:find-menu-item "Packages" menu-table :errorp nil))
         (effort-menu (clim:find-menu-item "Effort" menu-table :errorp nil))
         (system-menu (clim:find-menu-item "System" menu-table :errorp nil)))
    (is (not (null chat-menu)))
    (is (not (null view-menu)))
    (is (not (null skills-menu)))
    (is (not (null packages-menu)))
    (is (not (null effort-menu)))
    (is (not (null system-menu)))
    (is (eq :menu (clim:command-menu-item-type chat-menu)))
    (is (eq :menu (clim:command-menu-item-type view-menu)))
    (is (eq :menu (clim:command-menu-item-type skills-menu)))
    (is (eq :menu (clim:command-menu-item-type packages-menu)))
    (is (eq :menu (clim:command-menu-item-type effort-menu)))
    (is (eq :menu (clim:command-menu-item-type system-menu)))
    (let ((chat-table (clim:command-menu-item-value chat-menu))
          (view-table (clim:command-menu-item-value view-menu))
          (effort-table (clim:command-menu-item-value effort-menu))
          (system-table (clim:command-menu-item-value system-menu)))
      (flet ((menu-command (name table)
               (let ((item (clim:find-menu-item name table :errorp nil)))
                 (and item (clim:command-menu-item-value item)))))
        (is (eq 'rplaca::com-chat-stop-response
                (menu-command "Stop Response" chat-table)))
        (is (eq 'rplaca::com-chat-toggle-tool-results
                (menu-command "Toggle Tool Results" view-table)))
        (is (eq 'rplaca::com-chat-toggle-reasoning-output
                (menu-command "Toggle Reasoning Output" view-table)))
        (is (eq 'rplaca::com-chat-toggle-metadata-output
                (menu-command "Toggle Metadata Output" view-table)))
        (is (eq 'rplaca::com-chat-toggle-debug-mode
                (menu-command "Toggle Debug Mode" view-table)))
        (is (eq 'rplaca::com-chat-open-effort-selector
                (menu-command "Select Think Level..." effort-table)))
        (is (eq 'rplaca::com-chat-safe-reload
                (menu-command "Safe Reload" system-table)))
        (is (eq 'rplaca::com-chat-recurse
                (menu-command "Recurse" system-table)))))
    (dolist (command '(rplaca::com-chat-stop-response
                       rplaca::com-chat-toggle-tool-results
                       rplaca::com-chat-toggle-reasoning-output
                       rplaca::com-chat-toggle-metadata-output
                       rplaca::com-chat-toggle-debug-mode
                       rplaca::com-chat-open-skill-selector
                       rplaca::com-chat-open-package-dashboard
                       rplaca::com-chat-open-effort-selector
                       rplaca::com-chat-submit-compose
                       rplaca::com-chat-toggle-skill
                       rplaca::com-chat-toggle-package
                       rplaca::com-chat-select-effort
                       rplaca::com-chat-safe-reload
                       rplaca::com-chat-recurse))
      (is (clim:command-accessible-in-command-table-p
           command menu-table)))))

(defun test-command-table-menu-labels (table)
  "Return menu labels from TABLE in display order."
  (let ((labels nil))
    (clim:map-over-command-table-menu-items
     (lambda (name keystroke item)
       (declare (ignore keystroke item))
       (push name labels))
     table
     :inherited nil)
    (nreverse labels)))

(defun test-chat-menu-submenu (table name)
  "Return the submenu table named NAME from TABLE."
  (clim:command-menu-item-value
   (clim:find-menu-item name table :errorp t)))

(test mcclim-chat-system-menu-exposes-recurse-command
  "The McCLIM system menu exposes safe reload and recurse frame commands."
  (let* ((menu-table (clim:find-command-table
                      'rplaca::rplaca-chat-frame))
         (system-table (test-chat-menu-submenu menu-table "System")))
    (is (equal '("Safe Reload" "Recurse")
               (test-command-table-menu-labels system-table)))
    (is (eq 'rplaca::com-chat-safe-reload
            (clim:command-menu-item-value
             (clim:find-menu-item "Safe Reload" system-table :errorp t))))
    (is (eq 'rplaca::com-chat-recurse
            (clim:command-menu-item-value
             (clim:find-menu-item "Recurse" system-table :errorp t))))))

(test mcclim-chat-view-menu-is-stable-while-commands-toggle-frame-state
  "View commands mutate state without replacing the live menu gadget tree."
  (let* ((buf (make-buffer "view-toolbar" :agent-name "agent"))
         (frame (clim:make-application-frame
                 'rplaca::rplaca-chat-frame
                 :buffer buf)))
    (let ((*debug-mode* nil))
      (let* ((before (clim:frame-command-table frame))
             (view-menu (test-chat-menu-submenu before "View"))
             (labels (test-command-table-menu-labels view-menu)))
        (is (equal '("Toggle Tool Results"
                     "Toggle Reasoning Output"
                     "Toggle Metadata Output"
                     "Toggle Debug Mode")
                   labels))
        (rplaca::run-chat-frame-buffer-command
         frame
         #'rplaca::toggle-reasoning-output-command)
        (rplaca::run-chat-frame-buffer-command
         frame
         #'rplaca::toggle-metadata-output-command)
        (rplaca::run-chat-frame-buffer-command
         frame
         #'rplaca::toggle-debug-mode-command)
        (is (eq before (clim:frame-command-table frame)))
        (is (equal labels
                   (test-command-table-menu-labels
                    (test-chat-menu-submenu
                     (clim:frame-command-table frame) "View"))))))))

(test mcclim-chat-effort-menu-dispatches-semantic-selector
  "The stable effort menu delegates changing state to the CLIM minibuffer."
  (with-agent-definition-registry-override ()
    (let* ((buf (make-buffer "effort-toolbar" :agent-name "agent"))
           (frame (clim:make-application-frame
                   'rplaca::rplaca-chat-frame
                   :buffer buf)))
      (set-buffer-provider-override buf :openai-codex)
      (set-buffer-model-override buf "gpt-5.4")
      (let* ((menu-table (clim:frame-command-table frame))
             (effort-menu-item (clim:find-menu-item "Effort"
                                                    menu-table :errorp nil))
             (effort-menu (clim:command-menu-item-value effort-menu-item))
             (selector-item
               (clim:find-menu-item "Select Think Level..."
                                    effort-menu :errorp nil)))
        (is (not (null effort-menu-item)))
        (is (eq 'rplaca::com-chat-open-effort-selector
                (clim:command-menu-item-value selector-item))))
      (rplaca::select-chat-effort-for-buffer buf "high")
      (is (string= "high" (buffer-think-level-override buf))))))

(test mcclim-chat-skills-menu-opens-selector-and-helper-toggles-state
  "The stable Skills menu delegates selection while its helper persists state."
  (with-isolated-skills (root)
    (write-demo-skill root :name "demo")
    (register-skill-root root)
    (let* ((buf (make-buffer "skill-toolbar"))
           (skill (first (list-skills :include-disabled t)))
           (key (rplaca::skill-path-key skill)))
      (let ((menu-table (clim:find-command-table
                         'rplaca::rplaca-chat-frame)))
        (let* ((skills (test-chat-menu-submenu menu-table "Skills"))
               (item (clim:find-menu-item "Toggle Skill..."
                                          skills :errorp nil)))
          (is (eq 'rplaca::com-chat-open-skill-selector
                  (clim:command-menu-item-value item)))))
      (is-false (rplaca::toggle-chat-skill-for-buffer buf key))
      (is-false (skill-enabled-p
                 (rplaca::find-skill-by-path key :include-disabled t)))
      (is (search "[Skill demo disabled]"
                  (message-text (message-prev (buffer-input-message buf))))))))

(test mcclim-chat-frames-share-state-independent-menu-table
  "Frames share the immutable menu table while commands obtain frame state."
  (with-package-state-override ((default-package-test-channels))
    (let* ((enabled-buffer (make-buffer "enabled-package-toolbar"
                                        :agent-name "agent"))
           (default-buffer (make-buffer "default-package-toolbar"
                                        :agent-name "agent"))
           (enabled-frame (clim:make-application-frame
                           'rplaca::rplaca-chat-frame
                           :buffer enabled-buffer))
           (default-frame (clim:make-application-frame
                           'rplaca::rplaca-chat-frame
                           :buffer default-buffer)))
      (is (eq (clim:frame-command-table enabled-frame)
              (clim:frame-command-table default-frame)))
      (dolist (frame (list enabled-frame default-frame))
        (let* ((packages (test-chat-menu-submenu
                          (clim:frame-command-table frame) "Packages"))
               (item (clim:find-menu-item "Open Package Dashboard..."
                                          packages :errorp nil)))
          (is (eq 'rplaca::com-chat-open-package-dashboard
                  (clim:command-menu-item-value item))))))))

(test mcclim-chat-package-helper-toggles-state-without-changing-menu
  "Package state changes while the stable menu continues to open its dashboard."
  (with-package-state-override ((default-package-test-channels))
    (let ((buf (make-buffer "package-toolbar" :agent-name "agent")))
      (let ((menu-table (clim:find-command-table
                         'rplaca::rplaca-chat-frame)))
        (is-true (rplaca::toggle-chat-package-for-buffer buf "sexed"))
        (is (member "sexed" (buffer-enabled-packages buf) :test #'string=))
        (is-false (rplaca::package-enabled-globally-p "sexed"))
        (is-false (rplaca::toggle-chat-package-for-buffer buf "sexed"))
        (is-false (member "sexed" (buffer-enabled-packages buf)
                          :test #'string=))
        (let* ((packages (test-chat-menu-submenu menu-table "Packages"))
               (item (clim:find-menu-item "Open Package Dashboard..."
                                          packages :errorp nil)))
          (is (eq 'rplaca::com-chat-open-package-dashboard
                  (clim:command-menu-item-value item))))))))

(test chat-recurse-launch-spec-uses-current-buffer-state
  "Recurse launch specs inherit the current buffer agent and working directory."
  (let* ((working-directory
           (uiop:ensure-directory-pathname #P"/tmp/rplaca-recurse-spec/"))
         (buffer (make-buffer "spec-buffer"
                              :agent-name "tester"
                              :working-directory working-directory))
         (spec (rplaca::chat-recurse-launch-spec
                buffer
                :repo-root #P"/workspace/"
                :quicklisp-setup #P"/tmp/fake-quicklisp/setup.lisp"
                :session-name "recursive-session"
                :window-title "Recursive Window")))
    (is (equal #P"/workspace/" (getf spec :directory)))
    (is (equal "recursive-session" (getf spec :session-name)))
    (is (equal "Recursive Window" (getf spec :window-title)))
    (is (equal working-directory (getf spec :working-directory)))
    (is (equal "sbcl" (first (getf spec :argv))))
    (is (member "(ql:quickload :rplaca)" (getf spec :argv) :test #'string=))
    (is (member "(asdf:load-system :rplaca :force t)"
                (getf spec :argv)
                :test #'string=))
    (let* ((argv (getf spec :argv))
           (startup-form (nth (+ 2 (position "(asdf:load-system :rplaca :force t)"
                                            argv
                                            :test #'string=
                                            :from-end t))
                              argv)))
      (is (search ":session-name \"recursive-session\"" startup-form))
      (is (search ":agent-name \"tester\"" startup-form))
      (is (search ":window-title \"Recursive Window\"" startup-form))
      (is (search ":working-directory \"/tmp/rplaca-recurse-spec/\""
                  startup-form)))))

(test chat-recurse-command-records-child-launch-message
  "Running recurse inserts a status message describing the child launch."
  (let* ((working-directory
           (uiop:ensure-directory-pathname #P"/tmp/rplaca-recurse-command/"))
         (buf (make-buffer "recurse-buffer"
                           :agent-name "tester"
                           :working-directory working-directory))
         (launch-calls nil)
         (original-launch (symbol-function 'rplaca::launch-chat-recurse)))
    (unwind-protect
         (progn
           (setf (symbol-function 'rplaca::launch-chat-recurse)
                 (lambda (buffer)
                   (push buffer launch-calls)
                   (list :window-title "RPLACA Recurse - recurse-buffer"
                         :session-name "recurse-session"
                         :working-directory working-directory)))
           (let ((frame (clim:make-application-frame
                         'rplaca::rplaca-chat-frame
                         :buffer buf)))
             (clim:execute-frame-command frame '(rplaca::com-chat-recurse)))
           (is (equal (list buf) launch-calls))
           (let ((status (message-prev (buffer-input-message buf))))
             (is (eq :system (message-sender status)))
             (is (search "Opened recurse frame RPLACA Recurse - recurse-buffer"
                         (message-text status)))
             (is (search "session recurse-session"
                         (message-text status)))
             (is (search "/tmp/rplaca-recurse-command/"
                         (message-text status)))))
      (setf (symbol-function 'rplaca::launch-chat-recurse)
            original-launch))))

(test rplaca-main-honors-working-directory-argument
  "rplaca-main seeds the initial chat buffer from the supplied working directory."
  (let* ((working-directory
           (uiop:ensure-directory-pathname #P"/tmp/rplaca-main-working-directory/"))
         (*sessions-dir* (temp-session-test-directory "main-working-directory"))
         (rplaca::*buffer-ring* nil)
         (rplaca::*buffer-counter* 0)
         (rplaca::*startup-hook* nil)
         (rplaca::*initial-buffer-hook* nil)
         (original-parse (symbol-function 'rplaca::parse-rplaca-args))
         (original-init (symbol-function 'rplaca::initialize-rplaca-runtime))
         (original-scratch (symbol-function 'rplaca::ensure-scratch-buffer)))
    (unwind-protect
         (progn
           (setf (symbol-function 'rplaca::parse-rplaca-args)
                 (lambda () nil)
                 (symbol-function 'rplaca::initialize-rplaca-runtime)
                 (lambda () nil)
                 (symbol-function 'rplaca::ensure-scratch-buffer)
                 (lambda () nil))
           (let ((buffer (rplaca:rplaca-main
                          :session-name "main-working-directory"
                          :agent-name "tester"
                          :working-directory working-directory
                          :run-frame nil)))
             (is (equal working-directory
                        (buffer-working-directory buffer)))
             (is (equal "main-working-directory"
                        (buffer-name buffer)))))
      (setf (symbol-function 'rplaca::parse-rplaca-args) original-parse
            (symbol-function 'rplaca::initialize-rplaca-runtime) original-init
            (symbol-function 'rplaca::ensure-scratch-buffer) original-scratch))))

(test init-tools-hides-lispi-tools-until-package-enabled
  "init-tools exposes built-in core tools without lispi package tools."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (equal '("lisp_eval" "live_lisp_eval" "recovery_list")
                 tool-names))
      (is (not (null (gethash "lisp_eval" rplaca::*tool-table*))))
      (is-false (member "read" tool-names :test #'string=)))))

(test init-tools-only-reserves-core-tools
  "User tools may use Lispi names when Lispi is not active."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca::register-tool
     "read"
     "User read tool."
     '((:type . "object")
       (:properties . nil))
     (lambda (args)
       (declare (ignore args))
       "user-read"))
    (rplaca::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (equal '("lisp_eval" "live_lisp_eval" "read" "recovery_list")
                 tool-names))
      (is (string= "user-read"
                   (rplaca:execute-tool "read" nil))))))

(test direct-tools-with-lispi-names-are-not-package-scoped
  "A direct user tool named like a Lispi tool remains visible without Lispi."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca::init-tools)
    (rplaca:set-package-enablement-scope "lispi" :global)
    (rplaca:load-active-packages)
    (rplaca:set-package-enablement-scope "lispi" :default)
    (rplaca::register-tool
     "read"
     "User read tool."
     '((:type . "object")
       (:properties . nil))
     (lambda (args)
       (declare (ignore args))
       "user-read"))
    (rplaca::init-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (equal '("lisp_eval" "live_lisp_eval" "read" "recovery_list")
                 tool-names))
      (is (string= "user-read"
                   (rplaca:execute-tool "read" nil))))))

(defun tool-execution-test-events (buf)
  "Return durable tool-execution events recorded for BUF."
  (remove-if-not (lambda (event)
                   (string= "tool-execution" (event-value event :event)))
                 (session-current-events (buffer-session buf))))

(test execute-tool-safely-journals-tool-errors
  "Safe tool execution records start and error result events before returning."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca:register-tool
     "journal_fail"
     "Tool that fails for journaling tests."
     '((:type . "object") (:properties . nil))
     (lambda (args)
       (declare (ignore args))
       (error "boom from journal_fail")))
    (let* ((buf (make-chat-buffer "tool-journal-error"))
           (*current-caller* :coder)
           (*current-tool-buffer* buf)
           (result (rplaca::execute-tool-safely
                    "journal_fail" '(:value "x")
                    :buffer buf
                    :tool-id "toolu-journal-1"))
           (events (tool-execution-test-events buf)))
      (is (search ":error" result))
      (is (= 2 (length events)))
      (is (string= "start" (event-value (first events) :phase)))
      (is (string= "result" (event-value (second events) :phase)))
      (is (string= "error" (event-value (second events) :status)))
      (is (string= "journal_fail" (event-value (second events) :tool-name)))
      (is (string= "toolu-journal-1" (event-value (second events) :tool-id)))
      (is (search "boom from journal_fail"
                  (event-value (second events) :condition-message))))))

(test execute-prompt-tool-call-journals-tool-errors
  "Prompt-mode tool execution uses the safe journaling wrapper."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca:register-tool
     "prompt_journal_fail"
     "Tool that fails in prompt-mode journaling tests."
     '((:type . "object") (:properties . nil))
     (lambda (args)
       (declare (ignore args))
       (error "prompt boom")))
    (let* ((buf (make-chat-buffer "prompt-tool-journal-error"))
           (tool-use '((:type . "tool_use")
                       (:id . "toolu-prompt-journal-1")
                       (:name . "prompt_journal_fail")
                       (:input . ((:value . "x"))))))
      (multiple-value-bind (result event)
          (rplaca::execute-prompt-tool-call buf tool-use :coder)
        (is (search ":error" (cdr (assoc :result result))))
        (is (string= "prompt_journal_fail"
                     (rplaca:prompt-tool-event-name event))))
      (let ((events (tool-execution-test-events buf)))
        (is (= 2 (length events)))
        (is (string= "start" (event-value (first events) :phase)))
        (is (string= "error" (event-value (second events) :status)))
        (is (search "prompt boom"
                    (event-value (second events) :condition-message)))))))

(test execute-tool-safely-journals-unavailable-tool-calls
  "Unavailable tool calls are journaled as ordinary execution errors."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (let* ((buf (make-chat-buffer "tool-journal-unavailable"))
           (*current-caller* :coder)
           (*current-tool-buffer* buf)
           (result (rplaca::execute-tool-safely
                    "not_registered" '(:value "x")
                    :buffer buf
                    :tool-id "toolu-unavailable-1"))
           (events (tool-execution-test-events buf)))
      (is (search ":error" result))
      (is (= 2 (length events)))
      (is (string= "error" (event-value (second events) :status)))
      (is (null (event-value (second events) :reason)))
      (is (search "Unknown tool"
                  (event-value (second events) :condition-message)))
      (is (search "Unknown tool" (event-value (second events) :result))))))

(defun file-checkpoint-test-events (buf)
  "Return durable file-checkpoint events recorded for BUF."
  (remove-if-not (lambda (event)
                   (string= "file-checkpoint" (event-value event :event)))
                 (session-current-events (buffer-session buf))))

(defun lisp-eval-checkpoint-test-events (buf)
  "Return durable lisp-eval-checkpoint events recorded for BUF."
  (remove-if-not (lambda (event)
                   (string= "lisp-eval-checkpoint" (event-value event :event)))
                 (session-current-events (buffer-session buf))))

(test execute-tool-safely-checkpoints-live-lisp-eval
  "Safe lisp_eval execution records before/after recovery checkpoints."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((buf (make-chat-buffer "lisp-eval-checkpoint-live"))
           (*current-caller* :coder)
           (*current-tool-buffer* buf)
           (result (rplaca::execute-tool-safely
                    "lisp_eval"
                    '(:code "(+ 20 22)" :package "CL-USER")
                    :buffer buf
                    :tool-id "toolu-eval-checkpoint"))
           (events (lisp-eval-checkpoint-test-events buf)))
      (is (search "42" result))
      (is (= 2 (length events)))
      (is (string= "before" (event-value (first events) :phase)))
      (is (string= "after" (event-value (second events) :phase)))
      (is (string= "live" (event-value (first events) :mode)))
      (is (string= "CL-USER" (event-value (first events) :package)))
      (is (string= "ok" (event-value (second events) :status)))
      (is (search "42" (event-value (second events) :result))))))

(test live-lisp-eval-runs-in-current-image-and-is-checkpointed
  "The explicit live tool sees current-image state and records recovery data."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((buf (make-chat-buffer "live-lisp-eval-checkpoint"))
           (*current-caller* :coder)
           (*current-tool-buffer* buf)
           (marker (gensym "LIVE-MARKER-"))
           (result (rplaca::execute-tool-safely
                    "live_lisp_eval"
                    `(:code ,(format nil "(quote ~S)" marker)
                      :package "RPLACA/TESTS")
                    :buffer buf
                    :tool-id "toolu-live-eval-checkpoint"))
           (events (lisp-eval-checkpoint-test-events buf)))
      (is (search (symbol-name marker) result))
      (is (= 2 (length events)))
      (is (string= "live" (event-value (first events) :mode)))
      (is (string= "ok" (event-value (second events) :status)))
      (is (eq :frame
              (rplaca::interactive-tool-execution-policy
               "live_lisp_eval" `((:code . ,(format nil "(quote ~S)" marker)))))))))

(test live-lisp-eval-description-carries-prominent-risk-warning
  "Provider discovery makes the live evaluator's session risk explicit."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((definition (rplaca::effective-tool-definition "live_lisp_eval"))
           (description (rplaca::tool-definition-description definition)))
      (is (search "DANGER" description))
      (is (search "no timeout or isolation" description))
      (is (search "lose the user's unsaved session" description)))))

(test execute-tool-safely-checkpoints-lisp-eval-tool-errors
  "lisp_eval recovery checkpoints retain tool wrapper errors for repair."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (let* ((buf (make-chat-buffer "lisp-eval-checkpoint-error"))
           (*current-caller* :coder)
           (*current-tool-buffer* buf)
           (result (rplaca::execute-tool-safely
                    "lisp_eval"
                    '(:code "(+ 1 2)")
                    :buffer buf
                    :tool-id "toolu-eval-missing"))
           (events (lisp-eval-checkpoint-test-events buf)))
      (is (search ":error" result))
      (is (= 2 (length events)))
      (is (string= "error" (event-value (second events) :status)))
      (is (search "Unknown tool"
                  (event-value (second events) :condition-message))))))

(test recovery-list-reports-recent-session-checkpoints
  "The recovery_list tool summarizes durable recovery events for agents."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((buf (make-chat-buffer "recovery-list"))
           (*current-caller* :coder)
           (*current-tool-buffer* buf))
      (rplaca::execute-tool-safely
       "lisp_eval"
       '(:code "(+ 3 4)" :package "CL-USER")
       :buffer buf
       :tool-id "toolu-recovery-eval")
      (let* ((data (rplaca::execute-tool-safely
                    "recovery_list"
                    '(:kind "lisp-eval" :limit 5)
                    :buffer buf
                    :tool-id "toolu-recovery-list"))
             (decoded (rplaca::lisp-data-read data))
             (events (getf decoded :events)))
        (is (string= "lisp-eval" (getf decoded :kind)))
        (is (>= (getf decoded :event-count) 2))
        (is (null (getf decoded :pending-lisp-evals)))
        (is (search "(+ 3 4)" (getf (first events) :code)))))))

(test execute-tool-safely-checkpoints-write-tools
  "Safe write execution records before and after file checkpoints."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((root (temp-package-test-directory "checkpoint-write"))
           (*tool-working-directory* root)
           (buf (make-chat-buffer "file-checkpoint-write"
                                  :working-directory root))
           (*current-caller* :coder)
           (*current-tool-buffer* buf)
           (result (rplaca::execute-tool-safely
                    "write"
                    '((:path . "notes.txt")
                      (:content . "hello\n"))
                    :buffer buf
                    :tool-id "toolu-write-checkpoint"))
           (events (file-checkpoint-test-events buf)))
      (is (search "Successfully wrote" result))
      (is (= 2 (length events)))
      (is (string= "before" (event-value (first events) :phase)))
      (is (string= "after" (event-value (second events) :phase)))
      (is (equal nil (event-value (first events) :before-exists-p)))
      (is (equal t (event-value (second events) :after-exists-p)))
      (is (string= "notes.txt" (event-value (second events) :path)))
      (is (search "+hello" (event-value (second events) :diff))))))

(test execute-tool-safely-checkpoints-edit-tools
  "Safe edit execution records before/after hashes and diffs."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((root (temp-package-test-directory "checkpoint-edit"))
           (*tool-working-directory* root)
           (target (merge-pathnames "notes.txt" root))
           (buf (make-chat-buffer "file-checkpoint-edit"
                                  :working-directory root)))
      (write-test-file target "hello\n")
      (let* ((*current-caller* :coder)
             (*current-tool-buffer* buf)
             (result (rplaca::execute-tool-safely
                      "edit"
                      '((:path . "notes.txt")
                        (:old-text . "hello")
                        (:new-text . "goodbye"))
                      :buffer buf
                      :tool-id "toolu-edit-checkpoint"))
             (events (file-checkpoint-test-events buf))
             (after-event (second events))
             (before-hash (event-value after-event :before-hash))
             (after-hash (event-value after-event :after-hash)))
        (is (search "Successfully replaced" result))
        (is (= 2 (length events)))
        (is (string= "ok" (event-value after-event :status)))
        (is-false (string= before-hash after-hash))
        (is (search "-hello" (event-value after-event :diff)))
        (is (search "+goodbye" (event-value after-event :diff)))))))

(test tool-definitions->responses-tools-encodes-empty-properties-as-object
  "Zero-arg tool schemas encode JSON object properties as `{}`, not `null`."
  (let* ((tools (vector
                 '((:name . "artifactum_list")
                   (:description . "List artifacts.")
                   (:input--schema
                    (:type . "object")
                    (:properties)
                    (:required . #())))))
         (json (rplaca:api-json-encode
                (rplaca::tool-definitions->responses-tools tools))))
    (is (search "\"properties\":{}" json))
    (is-false (search "\"properties\":null" json))))

(test tool-definitions->openai-tools-encodes-empty-properties-as-object
  "Chat-completions tool schemas also encode empty properties as `{}`."
  (let* ((tools (vector
                 '((:name . "artifactum_list")
                   (:description . "List artifacts.")
                   (:input--schema
                    (:type . "object")
                    (:properties)
                    (:required . #())))))
         (json (rplaca:api-json-encode
                (rplaca::tool-definitions->openai-tools tools))))
    (is (search "\"properties\":{}" json))
    (is-false (search "\"properties\":null" json))))

(test load-active-packages-reregisters-active-package-tools
  "Active package loading restores package tools after init-tools resets core."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca::init-tools)
    (rplaca:set-package-enablement-scope "lispi" :global)
    (rplaca:load-active-packages)
    (is (not (null (gethash "read" rplaca::*tool-table*))))
    (clrhash rplaca::*tool-table*)
    (rplaca::init-tools)
    (is (null (gethash "read" rplaca::*tool-table*)))
    (rplaca:load-active-packages)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools)
                             #'string<)))
      (is (member "read" tool-names :test #'string=))
      (is (member "lisp_eval" tool-names :test #'string=)))))

(test init-tools-preserves-custom-tools
  "init-tools resets built-ins without wiping user-added tools."
  (with-tool-table-restored
    (clrhash rplaca::*tool-table*)
    (rplaca::register-tool
     "custom_probe"
     "Custom probe tool."
     '((:type . "object")
       (:properties . ((:payload . ((:type . "string"))))))
     (lambda (args)
       (declare (ignore args))
       "{\"ok\":true}"))
    (initialize-test-tools)
    (let* ((*current-caller* :user)
           (tools (coerce (rplaca::tool-definitions-for-api) 'list))
           (tool-names (sort (mapcar (lambda (tool) (cdr (assoc :name tool))) tools)
                             #'string<)))
      (is (equal '("custom_probe" "edit" "find" "grep" "lisp_eval"
                   "live_lisp_eval" "read" "recovery_list" "write")
                 tool-names))
      (is (not (null (gethash "custom_probe" rplaca::*tool-table*))))
      (is (not (null (gethash "lisp_eval" rplaca::*tool-table*)))))))

(test default-file-tools-read-write-edit-plain-text
  "The default file tools mutate text relative to the tool working directory."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "file-tools")))
           (file (merge-pathnames "nested/demo.txt" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (let ((rplaca::*tool-working-directory* root))
        (initialize-test-tools)
        (let ((write-result
                (rplaca:execute-tool
                 "write"
                 '(:path "nested/demo.txt"
                   :content "alpha
beta
gamma"))))
          (is (search "Successfully wrote" write-result))
          (is (string= "alpha
beta
gamma"
                       (uiop:read-file-string file))))
        (let ((read-result
                (rplaca:execute-tool
                 "read"
                 '(:path "nested/demo.txt"
                   :limit 2))))
          (is (search "alpha" read-result))
          (is (search "beta" read-result))
          (is (search "Use offset=3 to continue" read-result)))
        (let ((edit-result
                (rplaca:execute-tool
                 "edit"
                 '(:path "nested/demo.txt"
                   :old-text "beta"
                   :new-text "BETA"))))
          (is (search "Successfully replaced text" edit-result))
          (is (search "+BETA" edit-result))
          (is (string= "alpha
BETA
gamma"
                       (uiop:read-file-string file))))
        (let ((delete-result
                (rplaca:execute-tool
                 "edit"
                 '(:path "nested/demo.txt"
                   :old-text "gamma"
                   :new-text ""))))
          (is (search "Successfully replaced text" delete-result))
          (is (string= "alpha
BETA
"
                       (uiop:read-file-string file))))
        (rplaca:execute-tool
         "write"
         '(:path "nested/demo.txt"
           :content "reset"))
        (is (string= "reset" (uiop:read-file-string file)))))))

(test default-search-tools-find-files-and-grep-contents
  "find and grep search from the configured tool working directory."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "search-tools")))
           (source (merge-pathnames "src/alpha.lisp" root))
           (text (merge-pathnames "src/beta.txt" root))
           (ignored (merge-pathnames "node_modules/ignored.txt" root)))
      (ensure-directories-exist source)
      (ensure-directories-exist ignored)
      (write-test-file source "(defun alpha () :ok)
")
      (write-test-file text "intro
needle here
")
      (write-test-file ignored "needle should not be seen
")
      (let ((rplaca::*tool-working-directory* root))
        (initialize-test-tools)
        (let ((find-result (rplaca:execute-tool
                            "find"
                            '(:pattern "*.lisp"))))
          (is (search "src/alpha.lisp" find-result))
          (is-false (search "src/beta.txt" find-result)))
        (let ((find-result (rplaca:execute-tool
                            "find"
                            '(:pattern "beta"
                              :ignore-case t))))
          (is (search "src/beta.txt" find-result)))
        (let ((grep-result (rplaca:execute-tool
                            "grep"
                            '(:pattern "needle"
                              :glob "*.txt"))))
          (is (search "src/beta.txt:2:needle here" grep-result))
          (is-false (search "node_modules" grep-result)))))))

(test lispi-package-exposes-default-tool-implementations
  "The default read/write/edit implementations live in the lispi package."
  (let* ((specs (lispi:default-tool-specs))
         (names (sort (mapcar (lambda (spec)
                                (getf spec :name))
                              specs)
                      #'string<)))
    (is (equal '("edit" "find" "grep" "read" "write") names)))
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory "lispi-tools")))
         (file (merge-pathnames "demo.txt" root)))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((lispi:*tool-working-directory* root))
      (is (search "Successfully wrote"
                  (lispi:execute-write
                   '(:path "demo.txt"
                     :content "one
two"))))
      (is (search "Use offset=2 to continue"
                  (lispi:execute-read
                   '(:path "demo.txt"
                     :limit 1))))
      (is (search "demo.txt"
                  (lispi:execute-find
                   '(:pattern "demo"))))
      (is (search "demo.txt:2:two"
                  (lispi:execute-grep
                   '(:pattern "two"))))
      (is (search "Successfully replaced text"
                  (lispi:execute-edit
                   '(:path "demo.txt"
                     :old-text "two"
                     :new-text "TWO"))))
      (is (string= "one
TWO"
                   (uiop:read-file-string file))))))

(test default-write-and-edit-tools-reject-unbalanced-parentheses
  "write and edit fail before touching disk when content would be unbalanced."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "tool-paren-balance")))
           (file (merge-pathnames "sample.lisp" root))
           (balanced-with-ignored-parens
             (format nil
                     "(list \"(\" #\\) ; ignored )~% #| ignored ) #| nested ( |# |# :ok)")))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (let ((rplaca::*tool-working-directory* root))
        (initialize-test-tools)
        (rplaca:execute-tool
         "write"
         `(:path "sample.lisp"
           :content ,balanced-with-ignored-parens))
        (is (string= balanced-with-ignored-parens
                     (uiop:read-file-string file)))
        (signals error
          (rplaca:execute-tool
           "write"
           '(:path "sample.lisp"
             :content "(defun broken ()")))
        (is (string= balanced-with-ignored-parens
                     (uiop:read-file-string file)))
        (rplaca:execute-tool
         "write"
         '(:path "sample.lisp"
           :content "(defun foo () (+ 1 2))"))
        (signals error
          (rplaca:execute-tool
           "edit"
           '(:path "sample.lisp"
             :old-text "(+ 1 2)"
             :new-text "(list 1 2")))
        (is (string= "(defun foo () (+ 1 2))"
                     (uiop:read-file-string file)))))))

(test default-edit-tool-rejects-missing-and-duplicate-old-text
  "edit requires :old-text to be present exactly once."
  (with-tool-table-restored
    (let* ((root (uiop:ensure-directory-pathname
                  (temp-package-test-directory "edit-tool-errors")))
           (file (merge-pathnames "sample.txt" root)))
      (ensure-directories-exist (merge-pathnames #P".keep" root))
      (write-test-file file "same
same
")
      (let ((rplaca::*tool-working-directory* root))
        (initialize-test-tools)
        (signals error
          (rplaca:execute-tool
           "edit"
           '(:path "sample.txt"
             :old-text "missing"
             :new-text "replacement")))
        (signals error
          (rplaca:execute-tool
           "edit"
           '(:path "sample.txt"
             :old-text "same"
             :new-text "replacement")))
        (signals error
          (rplaca:execute-tool
           "edit"
           '(:path "sample.txt"
             :old-text ""
             :new-text "replacement")))))))

(test shell-exec-honors-timeout-and-returns-output
  "shell_exec records stdout/stderr and exit status on a fast command."
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory "shell-exec-fast"))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((rplaca::*tool-working-directory* root))
      (let* ((result-json
               (rplaca::execute-shell-exec
                '((:command . "printf 'hello'")
                  (:timeout . 5))))
             (result (rplaca::api-json-decode result-json)))
        (is (string= "printf 'hello'" (cdr (assoc :command result))))
        (is (equal 0 (cdr (assoc :exit--code result))))
        (is-false (cdr (assoc :timed--out result)))
        (is (string= "hello" (cdr (assoc :stdout result))))
        (is (string= "" (cdr (assoc :stderr result))))))))

(test shell-exec-times-out-and-kills-the-process
  "shell_exec reports timeout when the command outlives its deadline."
  (let* ((root (uiop:ensure-directory-pathname
                (temp-package-test-directory "shell-exec-timeout"))))
    (ensure-directories-exist (merge-pathnames #P".keep" root))
    (let ((rplaca::*tool-working-directory* root))
      (let* ((result-json
               (rplaca::execute-shell-exec
                '((:command . "sleep 2")
                  (:timeout . 0.2))))
             (result (rplaca::api-json-decode result-json)))
        (is (string= "sleep 2" (cdr (assoc :command result))))
        (is (cdr (assoc :timed--out result)))
        (is-false (cdr (assoc :exit--code result)))
        (is (string= "" (cdr (assoc :stdout result))))
        (is (string= "" (cdr (assoc :stderr result))))))))

(test build-system-prompt-is-compact-and-pi-style
  "The default system prompt lists the active provider tool surface."
  (with-tool-table-restored
    (initialize-test-tools)
    (with-function-override (rplaca::load-boot-files ()
                              nil)
      (with-package-state-override ((default-package-test-channels))
        (rplaca:set-package-enablement-scope "lispi" :global)
        (rplaca:load-autoload-packages)
        (is-false (search "## Structural editing with sexed"
                          rplaca::*default-core-system-prompt*))
        (let ((prompt (rplaca::build-system-prompt)))
          (is (search "operating inside rplaca" prompt))
          (is (search "## Tools" prompt))
          (is (search "- read: Read a text file" prompt))
          (is (search "- find: Search for files" prompt))
          (is (search "- grep: Search file contents" prompt))
          (is (search "- write: Create or overwrite a text file" prompt))
          (is (search "- edit: Edit a text file" prompt))
          (is (search "- lisp_eval: Evaluate one Common Lisp form" prompt))
          (is (search "- live_lisp_eval: DANGER:" prompt))
          (is (search "Tool calls and tool results use Lisp data mode" prompt))
          (is (search ":old-text" prompt))
          (is (search ":new-text" prompt))
          (is (search "Prefer provider tools for normal work" prompt))
          (is (search "Use find to locate files by name" prompt))
          (is (search "Use grep to locate literal text" prompt))
          (is (search "dangerous escape hatch" prompt))
          (is (search "lose the user's unsaved session" prompt))
          (is (search "Current date:" prompt))
          (is (search "Current working directory:" prompt))
          (is (search (rplaca::current-system-prompt-date) prompt))
          (is-false (search "only built-in tool available by default" prompt))
          (is-false (search "## Subagents" prompt))
          (is-false (search "Project file-buffer example" prompt))
          (is-false (search "Use the `sexed-*` functions for Lisp source edits" prompt))
          (is-false (search "(sexed-outline-to-string TEXT :max-depth 2)" prompt))
          (is-false (search "(sexed-replace-project-form \"PROJECT\" \"PATH\" SELECTOR NEW-TEXT)" prompt))
          (is-false (search "(sexed-stage-replace-project-form \"PROJECT\" \"PATH\" SELECTOR NEW-TEXT)" prompt))
          (is-false (search "(sexed-replace-scratch-form SELECTOR NEW-TEXT)" prompt))
          (is-false (search "(sexed-init-outline-to-string :max-depth 3)" prompt))
          (is-false (search "Do not try to set" prompt))
          (is-false (search "Message adapters such as `sexed-replace-message-form` take a `message` object" prompt))
          (is-false (search "fetching URLs, reading/writing files, running shell commands" prompt))
          (is-false (search "http_fetch" prompt))
          (is-false (search "shell_exec" prompt))
          (is-false (search "file_read" prompt)))))))

(test package-deftool-appears-in-system-prompt
  "Package entrypoints can register provider tools by evaluating deftool."
  (let* ((channel-root
           (make-package-channel-root
            :label "package-tool-channel"
            :package-name "package-tool"
            :manifest "(:name \"package-tool\"
 :description \"Package tool\"
 :entrypoint \"entry.lisp\"
 :autoload t)"
            :entrypoint-content
            "(defun package-tool-probe (value)
  (format nil \"package=~A\" value))
(deftool package-tool-probe
  :name \"package_probe\"
  :description \"Probe tool from a package.\"
  :args ((value :type \"string\" :description \"Value to echo.\")))")))
    (let ((rplaca::*agent-tool-metadata-table* (make-hash-table :test #'eq))
          (rplaca::*agent-tool-name-table* (make-hash-table :test #'equal)))
      (with-tool-table-restored
        (clrhash rplaca::*tool-table*)
        (with-package-state-override (nil)
          (rplaca:register-package-channel "custom" channel-root
                                             :description "Custom channel")
          (rplaca:load-rplaca-package "package-tool")
          (let* ((inactive-tools (coerce (rplaca::tool-definitions-for-api) 'list))
                 (inactive-tool-names (mapcar (lambda (tool)
                                                (cdr (assoc :name tool)))
                                              inactive-tools)))
            (is-false (member "package_probe" inactive-tool-names :test #'string=))
            (signals error
              (rplaca:execute-tool "package_probe" '(:value "nope"))))
          (rplaca:set-package-enablement-scope "package-tool" :global)
          (rplaca:load-active-packages)
          (let* ((tools (coerce (rplaca::tool-definitions-for-api) 'list))
                 (tool-names (mapcar (lambda (tool)
                                       (cdr (assoc :name tool)))
                                     tools))
                 (prompt (rplaca::build-system-prompt)))
            (is (member "package_probe" tool-names :test #'string=))
            (is (search "- package_probe: Probe tool from a package." prompt))
            (is (string= "package=ok"
                         (rplaca:execute-tool "package_probe"
                                                '(:value "ok"))))))))))

(test register-agent-definition-round-trips-through-registry
  "Programmatic agent definitions can be registered, replaced, found, and listed."
  (with-agent-definition-registry-override ()
    (let ((first (rplaca:register-agent-definition
                  "Pair"
                  :provider :openai-codex
                  :model "gpt-5.4"
                  :think-level "high"
                  :core-prompt "pair core"
                  :personality-prompt "pair personality"
                  :tool-names '("lisp_eval" doc-lookup))))
      (is (string= "Pair" (rplaca:agent-definition-name first)))
      (is (eq :openai-codex (rplaca:agent-definition-provider first)))
      (is (string= "high" (rplaca:agent-definition-think-level first)))
      (is (equal '("lisp_eval" "doc_lookup")
                 (rplaca:agent-definition-tool-names first))))
    (is (string= "pair core"
                 (rplaca:agent-definition-core-prompt
                  (rplaca:find-agent-definition "pair"))))
    (is (string= "pair personality"
                 (rplaca:agent-definition-personality-prompt
                  (rplaca:find-agent-definition "pair"))))
    (rplaca:register-agent-definition "Writer" :personality-prompt "writer personality")
    (rplaca:register-agent-definition "pair" :provider :zai :model "glm-5")
    (let* ((found (rplaca:find-agent-definition "PAIR"))
           (listed (rplaca:list-agent-definitions)))
      (is (eq :zai (rplaca:agent-definition-provider found)))
      (is (string= "glm-5" (rplaca:agent-definition-model found)))
      (is (equal '("pair" "Writer")
                 (mapcar #'rplaca:agent-definition-name listed))))))

(test register-pipeline-definition-round-trips-through-registry
  "Programmatic pipeline definitions normalize stages and can be listed."
  (with-pipeline-definition-registry-override ()
    (let ((definition
            (rplaca:define-pipeline
             "Plan Build Test"
             :description "plan then build"
             :entry-stage "plan"
             :max-steps 4
             :stages '((:name "plan"
                        :agent "planner"
                        :prompt "Plan {{input}}"
                        :next "build")
                       (:name build
                        :agent "builder"
                        :prompt "Build {{stage:plan}}")))))
      (is (string= "plan build test"
                   (rplaca:pipeline-definition-name definition)))
      (is (string= "plan"
                   (rplaca:pipeline-definition-entry-stage definition)))
      (is (= 2 (length (rplaca:pipeline-definition-stages definition))))
      (is (eq definition
              (rplaca:find-pipeline-definition "PLAN BUILD TEST")))
      (is (equal '("plan build test")
                 (mapcar #'rplaca:pipeline-definition-name
                         (rplaca:list-pipeline-definitions)))))))

(test run-pipeline-prompt-pipes-stage-output
  "Pipeline stages can template the original prompt and prior stage output."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (request-payloads nil))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-prompt")))
          (rplaca:define-pipeline
           "plan-build"
           :stages '((:name "plan"
                      :agent "planner"
                      :prompt "Plan this request: {{input}}"
                      :next "build")
                     (:name "build"
                      :agent "builder"
                      :prompt "Build from this plan: {{stage:plan}}")))
          (with-function-override (rplaca::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider callback model
                                                    max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (incf request-count)
                                   (push (rplaca::api-json-encode messages)
                                         request-payloads)
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (rplaca::canonical-text-block
                                           (if (= request-count 1)
                                               "PLAN OK"
                                               "BUILD OK")))))
            (rplaca::init-default-keymap)
            (initialize-test-tools)
            (let ((result (rplaca:run-pipeline-prompt
                           "ship fizzbuzz"
                           "plan-build"
                           :provider :zai
                           :model "glm-5")))
              (is (= 2 request-count))
              (is (string= "BUILD OK"
                           (rplaca:prompt-run-result-final-text result)))
              (is (string= "plan-build"
                           (rplaca:prompt-run-result-agent-name result)))
              (is (= 2 (rplaca:prompt-run-result-iterations result)))
              (let ((payloads (nreverse request-payloads)))
                (is (search "Plan this request: ship fizzbuzz"
                            (first payloads)))
                (is (search "Build from this plan: PLAN OK"
                            (second payloads)))))))))))

(test run-pipeline-on-buffer-supports-decision-loops
  "A stage :next function can deterministically route back to an earlier stage."
  (let ((path (temp-agent-defaults-path))
        (plan-count 0)
        (stage-order nil))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-loop")))
          (rplaca:define-pipeline
           "repair"
           :max-steps 6
           :stages `((:name "plan"
                      :agent "planner"
                      :prompt "Plan: {{input}} {{previous}}"
                      :next "test")
                     (:name "test"
                      :agent "tester"
                      :prompt "Test the latest plan: {{stage:plan}}"
                      :next ,(lambda (_context result)
                               (declare (ignore _context))
                               (if (search "FAIL"
                                           (or (rplaca:pipeline-stage-result-final-text
                                                result)
                                               "")
                                           :test #'char-equal)
                                   "plan"
                                   nil)))))
          (with-function-override (rplaca::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider callback
                                                    model max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (let* ((payload (rplaca::api-json-encode messages))
                                          (last-plan
                                            (search "[pipeline stage: plan]" payload
                                                    :from-end t))
                                          (last-test
                                            (search "[pipeline stage: test]" payload
                                                    :from-end t))
                                          (text
                                            (cond
                                              ((and last-test
                                                    (or (null last-plan)
                                                        (> last-test last-plan)))
                                               (if (= plan-count 1)
                                                   "FAIL tests"
                                                   "PASS tests"))
                                              ((and last-plan
                                                    (or (null last-test)
                                                        (> last-plan last-test)))
                                               (format nil "PLAN ~D" (incf plan-count)))
                                              (t "PASS tests"))))
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list (rplaca::canonical-text-block
                                             text)))))
            (rplaca::init-default-keymap)
            (initialize-test-tools)
            (let* ((buf (rplaca::make-prompt-buffer "fix failing tests"
                                                       "agent"))
                   (result (rplaca:run-pipeline-on-buffer
                            "repair"
                            "fix failing tests"
                            :buffer buf)))
              (setf stage-order
                    (mapcar #'rplaca:pipeline-stage-result-stage-name
                            (rplaca:pipeline-run-result-stage-results
                             result)))
              (is (equal '("plan" "test" "plan" "test")
                         stage-order))
              (is (eq :succeeded
                      (rplaca:pipeline-run-result-status result)))
              (is (string= "PASS tests"
                           (rplaca:pipeline-run-result-final-text
                            result))))))))))

(test run-pipeline-test-profiles-captures-command-results
  "Deterministic pipeline test profiles run shell commands and summarize failures."
  (with-pipeline-definition-registry-override ()
    (rplaca:register-pipeline-test-profile
     "pass"
     :description "Passing probe"
     :command "printf 'ok\\n'")
    (rplaca:register-pipeline-test-profile
     "fail"
     :description "Failing probe"
     :command "printf 'nope\\n' >&2; exit 3")
    (let ((report (rplaca:run-pipeline-test-profiles
                   '("pass" "fail")
                   :directory (uiop:ensure-directory-pathname (truename ".")))))
      (is-false (getf report :passed-p))
      (is (equal '("pass" "fail") (getf report :profiles)))
      (is (= 2 (length (getf report :results))))
      (is (search "pass: PASS" (getf report :summary) :test #'char-equal))
      (is (search "fail: FAIL" (getf report :summary) :test #'char-equal))
      (is (string= "ok
" (getf (first (getf report :results)) :stdout)))
      (is (= 3 (getf (second (getf report :results)) :exit-code))))))

(test bundled-self-modify-pipeline-loops-and-injects-selected-packages-and-skills
  "The shipped self-modify pipeline reparses plans after failing tests and injects selected package/skill context."
  (let ((path (temp-agent-defaults-path))
        (*sessions-dir* (temp-session-test-directory "self-modify-pipeline"))
        (responses nil)
        (test-reports nil)
        (captured-calls nil))
    (with-isolated-skills (root)
      root
      (register-skill-definition
       "demo-skill"
       :description "Demo self-modify skill"
       :contents "---\nname: demo-skill\ndescription: Demo self-modify skill\n---\nUse the DEMO-SKILL marker when this skill is injected.\n")
      (with-agent-defaults-path-override (path)
        (with-pipeline-definition-registry-override ()
          (let ((rplaca::*agent-definition-registry*
                  (make-hash-table :test #'equal))
                (rplaca::*tool-table*
                  (make-hash-table :test #'equal))
                (rplaca::*agent-tool-metadata-table*
                  (make-hash-table :test #'eq))
                (rplaca::*agent-tool-name-table*
                  (make-hash-table :test #'equal))
                (rplaca::*command-table*
                  (make-hash-table :test #'eq))
                (rplaca::*extended-docs*
                  (make-hash-table :test #'eq))
                (rplaca::*slash-command-table*
                  (make-hash-table :test #'equal))
                (rplaca::*compaction-point* nil))
            (with-package-state-override ((default-package-test-channels))
              (setf responses
                    (list
                     (list :kind :text
                           :text "{\"plan\":\"Round 1 plan\",\"implementation\":\"Implement round 1\",\"packages\":[\"sexed\"],\"skills\":[\"demo-skill\"],\"tests\":[\"unit\"],\"docs\":\"Update docs\",\"update_init\":false,\"init\":\"\"}")
                     (list :kind :text
                           :text "IMPLEMENT ROUND 1")
                     (list :kind :tool
                           :id "toolu_test_1"
                           :name "prove_run"
                           :input '((:methods . #("unit"))))
                     (list :kind :text
                           :text "{\"passed\":false,\"summary\":\"unit failed\",\"feedback\":\"unit failed on first run\",\"tests\":[\"unit\"]}")
                     (list :kind :text
                           :text "{\"plan\":\"Round 2 plan\",\"implementation\":\"Implement round 2\",\"packages\":[\"sexed\"],\"skills\":[\"demo-skill\"],\"tests\":[\"unit\"],\"docs\":\"Refresh docs\",\"update_init\":true,\"init\":\"Enable the new workflow in init.\"}")
                     (list :kind :text
                           :text "IMPLEMENT ROUND 2")
                     (list :kind :tool
                           :id "toolu_test_2"
                           :name "prove_run"
                           :input '((:methods . #("unit"))))
                     (list :kind :text
                           :text "{\"passed\":true,\"summary\":\"unit passed\",\"feedback\":\"unit passed on second run\",\"tests\":[\"unit\"]}")
                     (list :kind :text :text "DOCS DONE")
                     (list :kind :text :text "INIT DONE")
                     (list :kind :text :text "INIT DONE"))
                    test-reports
                    (list
                     (list :passed-p nil
                           :profiles '("unit")
                           :results (list (list :name "unit"
                                                :command "unit"
                                                :directory "/tmp"
                                                :exit-code 1
                                                :stdout "failing output"
                                                :stderr "failing stderr"
                                                :passed-p nil))
                           :summary "unit: FAIL (exit 1)\n\nOverall: FAILED")
                     (list :passed-p t
                           :profiles '("unit")
                           :results (list (list :name "unit"
                                                :command "unit"
                                                :directory "/tmp"
                                                :exit-code 0
                                                :stdout "passing output"
                                                :stderr ""
                                                :passed-p t))
                           :summary "unit: PASS (exit 0)\n\nOverall: PASSED")))
              (rplaca:set-package-enablement-scope "pipelines" :global)
              (rplaca:load-autoload-packages)
              (with-function-override
                  (rplaca:run-pipeline-test-profiles
                   (profile-names &key directory)
                   (declare (ignore directory))
                   (is (equal '("unit") profile-names))
                   (or (pop test-reports)
                       (error "no more deterministic test reports")))
                (with-function-override
                    (rplaca::provider-request-streaming
                     (provider messages callback
                               &key model max-tokens tools
                                 reasoning-effort system-prompt)
                     (declare (ignore provider callback model
                                      max-tokens
                                      reasoning-effort))
                     (let ((response (or (pop responses)
                                         (error "no more provider responses"))))
                       (push (list :messages messages
                                   :messages-json (rplaca::api-json-encode messages)
                                   :tool-names (mapcar (lambda (tool)
                                                         (cdr (assoc :name tool)))
                                                       (if (vectorp tools)
                                                           (coerce tools 'list)
                                                           tools))
                                   :system-prompt system-prompt)
                             captured-calls)
                       (ecase (getf response :kind)
                         (:text
                          (make-completed-stream-state-response
                           "end_turn"
                           (list (rplaca::canonical-text-block
                                  (getf response :text)))))
                         (:tool
                          (make-completed-stream-state-response
                           "tool_use"
                           (list (rplaca::canonical-tool-use-block
                                  (getf response :id)
                                  (getf response :name)
                                  (getf response :input))))))))
                  (rplaca::init-default-keymap)
                  (initialize-test-tools)
                  (let* ((buf (rplaca::make-prompt-buffer
                               "build a self-modifying workflow"
                               "agent"))
                         (result (rplaca:run-pipeline-on-buffer
                                  "self-modify"
                                  "build a self-modifying workflow"
                                  :buffer buf))
                         (stage-order
                           (mapcar #'rplaca:pipeline-stage-result-stage-name
                                   (rplaca:pipeline-run-result-stage-results
                                    result)))
                         (calls (nreverse captured-calls))
                         (first-implement (second calls))
                         (first-test (third calls))
                         (second-plan (fifth calls))
                         (init-call (first (last calls))))
                    (is (equal '("plan" "implement" "test"
                                 "plan" "implement" "test"
                                 "docs" "init")
                               stage-order))
                    (is (eq :succeeded
                            (rplaca:pipeline-run-result-status result)))
                    (is (string= "INIT DONE"
                                 (rplaca:pipeline-run-result-final-text
                                  result)))
                    (is (search "Structural editing with sexed"
                                (getf first-implement :system-prompt)
                                :test #'char-equal))
                    (is (search "recovery_list"
                                (getf first-implement :messages-json)
                                :test #'char-equal))
                    (is (search "isolated"
                                (getf first-implement :messages-json)
                                :test #'char-equal))
                    (is (search "<skill>"
                                (getf first-implement :messages-json)
                                :test #'char-equal))
                    (is (search "DEMO-SKILL marker"
                                (getf first-implement :messages-json)
                                :test #'char-equal))
                    (is (search "Self-testing with prove"
                                (getf first-test :system-prompt)
                                :test #'char-equal))
                    (is (equal '("prove_list_methods" "prove_run")
                               (getf first-test :tool-names)))
                    (is (search "unit failed on first run"
                                (getf second-plan :messages-json)
                                :test #'char-equal))
                    (is (search (namestring rplaca::*user-init-file*)
                                (princ-to-string (getf init-call :messages))
                                :test #'char-equal))))))))))))

(test run-pipeline-on-buffer-reports-invalid-route
  "Pipeline route errors are returned as failed pipeline results."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-route")))
          (rplaca:define-pipeline
           "bad-route"
           :stages '((:name "start"
                      :prompt "Start: {{input}}"
                      :next "missing")))
          (with-function-override (rplaca::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback
                                                    model max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (rplaca::canonical-text-block
                                           "DONE"))))
            (rplaca::init-default-keymap)
            (initialize-test-tools)
            (let* ((buf (rplaca::make-prompt-buffer "go" "agent"))
                   (result (rplaca:run-pipeline-on-buffer
                            "bad-route"
                            "go"
                            :buffer buf)))
              (is (eq :failed
                      (rplaca:pipeline-run-result-status result)))
              (is (= 1
                     (length (rplaca:pipeline-run-result-stage-results
                              result))))
              (is (search "no stage named missing"
                          (rplaca:pipeline-run-result-error result)
                          :test #'char-equal)))))))))

(test send-message-runs-active-buffer-pipeline
  "Interactive send-message applies its managed buffer pipeline on the frame."
  (let ((path (temp-agent-defaults-path))
        (request-payload nil))
    (with-agent-defaults-path-override (path)
      (with-pipeline-definition-registry-override ()
        (let ((*sessions-dir* (temp-session-test-directory "pipeline-send")))
          (rplaca:define-pipeline
           "one-stage"
           :stages '((:name "stage"
                      :agent "pipeline-agent"
                      :prompt "Pipeline saw: {{input}}")))
          (with-function-override (rplaca::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider callback model
                                                    max-tokens tools
                                                    reasoning-effort
                                                    system-prompt))
                                   (setf request-payload
                                         (rplaca::api-json-encode messages))
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (rplaca::canonical-text-block
                                           "PIPELINE DONE"))))
            (rplaca::init-default-keymap)
            (initialize-test-tools)
            (let ((buf (make-buffer "pipeline-chat"
                                    :pipeline-name "one-stage")))
              (rplaca:set-buffer-provider-override buf :zai)
              (rplaca:set-buffer-model-override buf "glm-5")
              (rplaca::set-message-text (buffer-input-message buf)
                                          "hello pipeline")
              (let ((operation (rplaca::send-message buf)))
                (is-true
                 (rplaca::interactive-buffer-operation-p operation))
                (is-true
                 (wait-for-llm-test
                  (lambda ()
                    (not
                     (bt:thread-alive-p
                      (rplaca::interactive-buffer-operation-worker
                       operation))))))
                (is-true
                 (rplaca::update-interactive-buffer-operation buf)))
              (is (string= "PIPELINE DONE"
                           (message-text
                            (message-prev (buffer-input-message buf)))))
              (is (eq :idle (buffer-status buf)))
              (is (search "Pipeline saw: hello pipeline"
                          request-payload)))))))))

(test build-agent-system-prompt-composes-boot-core-and-personality
  "Agent prompts are composed in boot -> core -> personality -> runtime order."
  (with-isolated-skills (root)
    root
    (with-agent-definition-registry-override ()
      (rplaca:register-agent-definition
       "pair"
       :core-prompt "PAIR CORE"
       :personality-prompt "PAIR PERSONALITY")
      (with-function-override (rplaca::load-boot-files ()
                                "BOOT PREFIX")
        (let* ((prompt (rplaca:build-agent-system-prompt "pair"))
               (boot-pos (search "BOOT PREFIX" prompt))
               (core-pos (search "PAIR CORE" prompt))
               (personality-pos (search "PAIR PERSONALITY" prompt))
               (date-pos (search "Current date:" prompt)))
          (is (not (null boot-pos)))
          (is (not (null core-pos)))
          (is (not (null personality-pos)))
          (is (not (null date-pos)))
          (is (< boot-pos core-pos personality-pos date-pos)))))))

(test load-boot-files-injects-ancestor-agents-md-instructions
  "Boot-file loading discovers AGENTS.md from the active directory ancestry."
  (let* ((root (temp-package-test-directory "agents-injection"))
         (nested (merge-pathnames #P"src/ui/" root))
         (root-agents (merge-pathnames "AGENTS.md" root))
         (nested-agents (merge-pathnames "AGENTS.md" nested)))
    (ensure-directories-exist (merge-pathnames #P".keep" nested))
    (write-test-file root-agents "ROOT AGENTS MARKER")
    (write-test-file nested-agents "NESTED AGENTS MARKER")
    (let* ((rplaca::*boot-file-names* '("AGENTS.md"))
           (instructions (rplaca:load-boot-files :directory nested))
           (root-pos (search "ROOT AGENTS MARKER" instructions))
           (nested-pos (search "NESTED AGENTS MARKER" instructions)))
      (is (search "# AGENTS.md instructions for " instructions))
      (is (search "<INSTRUCTIONS>" instructions))
      (is (search "</INSTRUCTIONS>" instructions))
      (is (not (null root-pos)))
      (is (not (null nested-pos)))
      (is (< root-pos nested-pos)))))

(test build-agent-system-prompt-injects-buffer-working-directory-agents-md
  "System prompts use the buffer working directory for AGENTS.md injection."
  (with-tool-table-restored
    (with-isolated-skills (skills-root)
      skills-root
      (with-package-state-override (nil)
        (let* ((root (temp-package-test-directory "buffer-agents-injection"))
               (nested (merge-pathnames #P"project/subdir/" root))
               (agents-path (merge-pathnames "AGENTS.md" root)))
          (ensure-directories-exist (merge-pathnames #P".keep" nested))
          (write-test-file agents-path "BUFFER WORKING DIRECTORY AGENTS")
          (let* ((rplaca::*boot-file-names* '("AGENTS.md"))
                 (rplaca::*default-core-system-prompt* "CORE")
                 (rplaca::*default-personality-prompt* "PERSONALITY")
                 (rplaca::*buffer-system-prompt-display-enabled* nil)
                 (buf (make-chat-buffer
                       "agents-buffer"
                       :working-directory nested
                       :session-persistence-mode :ephemeral))
                 (prompt (rplaca:build-agent-system-prompt "agent"
                                                             :buffer buf)))
            (is (search "BUFFER WORKING DIRECTORY AGENTS" prompt))
            (is (search (namestring nested) prompt))
            (let ((rplaca::*current-tool-buffer* buf))
              (is (equal (buffer-working-directory buf)
                         (rplaca::default-prompt-working-directory))))))))))

(test build-agent-system-prompt-falls-back-to-default-components
  "Missing agent prompt slots fall back to the default core and personality prompts."
  (with-agent-definition-registry-override ()
    (let ((rplaca::*default-personality-prompt* "DEFAULT PERSONALITY"))
      (rplaca:register-agent-definition "writer" :personality-prompt "WRITER PERSONALITY")
      (with-function-override (rplaca::load-boot-files ()
                                nil)
        (let ((prompt (rplaca:build-agent-system-prompt "writer")))
          (is (search "Tool calls and tool results use Lisp data mode" prompt))
          (is (search "WRITER PERSONALITY" prompt))
          (is-false (search "DEFAULT PERSONALITY" prompt))))
      (with-function-override (rplaca::load-boot-files ()
                                nil)
        (let ((prompt (rplaca:build-agent-system-prompt "missing")))
          (is (search "Tool calls and tool results use Lisp data mode" prompt))
          (is (search "DEFAULT PERSONALITY" prompt)))))))

(test parse-rplaca-prompt-args-supports-routing-and-output-options
  "The one-shot prompt parser accepts routing, visibility, and prompt text."
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("--agent" "writer"
                    "--provider" "openai-codex"
                    "--model" "gpt-5.4"
                    "--think" "high"
                    "--model-role" "review"
                    "--service-tier" "priority"
                    "--show-tools"
                    "--show-reasoning"
                    "--show-metadata"
                    "--clean-build"
                    "--isolated"
                    "--json"
                    "--package" "sexed"
                    "--package" "lispi"
                    "--session" "cache-probe"
                    "--continue"
                    "--ephemeral"
                    "--pipeline" "plan-build"
                    "--max-tool-iterations" "7"
                    "summarize" "this"))))
    (is (string= "writer" (rplaca::prompt-options-agent-name options)))
    (is (string= "openai-codex" (rplaca::prompt-options-provider options)))
    (is (string= "gpt-5.4" (rplaca::prompt-options-model options)))
    (is (string= "high" (rplaca::prompt-options-think-level options)))
    (is (string= "review" (rplaca::prompt-options-model-role options)))
    (is (string= "priority" (rplaca::prompt-options-service-tier options)))
    (is (rplaca::prompt-options-show-tools-p options))
    (is (rplaca::prompt-options-show-reasoning-p options))
    (is (rplaca::prompt-options-show-metadata-p options))
    (is (rplaca::prompt-options-json-p options))
    (is (rplaca::prompt-options-isolated-p options))
    (is (equal '("sexed" "lispi")
               (rplaca::prompt-options-packages options)))
    (is (string= "cache-probe"
                 (rplaca::prompt-options-session-name options)))
    (is (rplaca::prompt-options-continue-session-p options))
    (is (rplaca::prompt-options-ephemeral-p options))
    (is (string= "plan-build"
                 (rplaca::prompt-options-pipeline-name options)))
    (is (= 7 (rplaca::prompt-options-max-tool-iterations options)))
    (is (string= "summarize this" (rplaca::prompt-options-prompt options)))))

(test parse-rplaca-prompt-args-supports-no-session-alias
  "The prompt parser accepts --no-session as an explicit ephemeral alias."
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("--no-session" "say" "hello"))))
    (is (rplaca::prompt-options-ephemeral-p options))
    (is (string= "say hello" (rplaca::prompt-options-prompt options)))))

(test parse-rplaca-prompt-args-supports-jsonl-and-output-schema
  "Prompt parser accepts JSONL streaming and structured-output schema flags."
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("--jsonl"
                    "--output-schema" "{\"type\":\"object\"}"
                    "emit" "json"))))
    (is (rplaca::prompt-options-jsonl-p options))
    (is (string= "{\"type\":\"object\"}"
                 (rplaca::prompt-options-output-schema options)))
    (is (string= "emit json" (rplaca::prompt-options-prompt options)))))

(test write-prompt-run-result-jsonl-emits-turn-completed-record
  "JSONL prompt output writes a turn-completed record with structured output."
  (let* ((result (rplaca::make-prompt-run-result
                  :prompt "emit json"
                  :final-text "{\"status\":\"ok\"}"
                  :structured-output '((:status . "ok"))
                  :agent-name "writer"
                  :provider :zai
                  :model "glm-5"
                  :iterations 1
                  :stop-reason "end_turn"))
         (options (rplaca::make-prompt-options :jsonl-p t))
         (output (with-output-to-string (stream)
                   (let ((*standard-output* stream))
                     (rplaca::write-prompt-run-result result options)))))
    (is (search "\"event\":\"turn.completed\"" output))
    (is (search "\"final_text\":\"{\\\"status\\\":\\\"ok\\\"}\"" output))
    (is (search "\"structured_output\":{\"status\":\"ok\"}" output))))

(test run-prompt-options-resolves-continue-session-for-current-directory
  "Prompt options can resume the most recent saved session for the cwd."
  (let* ((*sessions-dir* (temp-session-test-directory "continue-prompt"))
         (working-directory (temp-session-test-directory "continue-prompt-cwd"))
         (session (load-or-create-session "continued-session"
                                          :working-directory working-directory))
         (buf (make-buffer "continued-session"
                           :agent-name "echo"
                           :working-directory working-directory
                           :session session))
         (captured nil))
    (ensure-directories-exist (merge-pathnames #P".keep" working-directory))
    (save-session buf)
    (let ((*default-pathname-defaults* working-directory))
      (with-function-override (rplaca::run-session-prompt
                               (prompt &key session-name &allow-other-keys)
                               (setf captured (list prompt session-name))
                               :continued)
        (let ((result (rplaca::run-prompt-options
                       (rplaca::make-prompt-options
                        :prompt "continue now"
                        :continue-session-p t))))
          (is (eq :continued result))
          (is (equal '("continue now" "continued-session")
                     captured)))))))

(test run-prompt-options-ephemeral-ignores-session-routing
  "Ephemeral prompt options bypass saved-session routing and run in no-session mode."
  (let ((captured nil))
    (with-function-override (rplaca::run-single-prompt
                             (prompt &key session-persistence-mode
                                     session-name continue-session-p
                                     &allow-other-keys)
                             (declare (ignore session-name continue-session-p))
                             (setf captured (list prompt session-persistence-mode))
                             :ephemeral)
      (with-function-override (rplaca::run-session-prompt
                               (prompt &rest args)
                               (declare (ignore prompt args))
                               :unexpected-session)
        (let ((result (rplaca::run-prompt-options
                       (rplaca::make-prompt-options
                        :prompt "no session"
                        :session-name "saved-session"
                        :continue-session-p t
                        :ephemeral-p t))))
          (is (eq :ephemeral result))
          (is (equal '("no session" :ephemeral)
                     captured)))))))

(test run-prompt-options-honors-ephemeral-mode-without-creating-session
  "Explicit ephemeral prompt mode keeps one-shot runs out of the session store."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (seen-persistence-mode nil)
        (*sessions-dir* (temp-session-test-directory "ephemeral-prompt")))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback
                                                  model max-tokens tools
                                                  reasoning-effort
                                                  system-prompt))
                                 (incf request-count)
                                 (setf seen-persistence-mode
                                       rplaca::*default-buffer-session-persistence-mode*)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (rplaca::canonical-text-block
                                         "ephemeral answer"))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let* ((options (rplaca::parse-rplaca-prompt-args
                           '("--ephemeral"
                             "--provider" "zai"
                             "--model" "glm-5"
                             "ephemeral" "prompt")))
                 (result (rplaca::run-prompt-options options)))
            (is (= 1 request-count))
            (is (eq :ephemeral seen-persistence-mode))
            (is (string= "ephemeral answer"
                         (rplaca::prompt-run-result-final-text result)))
            (is-false (probe-file (rplaca::session-path "rplaca:prompt")))
            (is-false (probe-file
                       (rplaca::session-sidecar-manifest-path
                        "rplaca:prompt")))))))))

(test default-session-prompt-session-name-prefers-most-recent-current-directory
  "session-prompt defaults to the newest saved session for the current cwd."
  (let* ((*sessions-dir* (temp-session-test-directory "session-prompt-default"))
         (working-directory (temp-session-test-directory "session-prompt-default-cwd"))
         (session (load-or-create-session "cwd-default"
                                          :working-directory working-directory))
         (buf (make-buffer "cwd-default"
                           :agent-name "echo"
                           :working-directory working-directory
                           :session session)))
    (ensure-directories-exist (merge-pathnames #P".keep" working-directory))
    (save-session buf)
    (let ((*default-pathname-defaults* working-directory))
      (is (string= "cwd-default"
                   (rplaca::default-session-prompt-session-name))))))

(test parse-rplaca-prompt-args-defaults-to-codex-for-plain-prompt-sh
  "Plain prompt.sh runs default to GPT-5.6 Sol without overriding explicit routing."
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("summarize" "this"))))
    (is (string= "openai-codex"
                 (rplaca::prompt-options-provider options)))
    (is (string= "gpt-5.6-sol"
                 (rplaca::prompt-options-model options))))
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("--agent" "writer" "summarize" "this"))))
    (is (null (rplaca::prompt-options-provider options)))
    (is (null (rplaca::prompt-options-model options))))
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("--provider" "zai" "summarize" "this"))))
    (is (string= "zai" (rplaca::prompt-options-provider options)))
    (is (null (rplaca::prompt-options-model options))))
  (let ((options (rplaca::parse-rplaca-prompt-args
                  '("--model" "custom-codex" "summarize" "this"))))
    (is (string= "openai-codex"
                 (rplaca::prompt-options-provider options)))
    (is (string= "custom-codex"
                 (rplaca::prompt-options-model options)))))

(test prompt-usage-string-docs-prompt-sh-codex-default
  "prompt.sh help renders its default provider/model without FORMAT errors."
  (let ((usage (rplaca::prompt-usage-string)))
    (is (search "Default without --agent: openai-codex" usage))
    (is (search "Default without --agent: gpt-5.6-sol" usage))
    (is (search "--model-role ROLE" usage))
    (is (search "--service-tier TIER" usage))
    (is (search "--package NAME" usage))
    (is (search "--session NAME" usage))
    (is (search "--ephemeral" usage))
    (is (search "--pipeline NAME" usage))
    (is (search "Skip ~/.rplaca.d/init.lisp" usage))))

(test compaction-threshold-policy
  "Compaction thresholds are configurable as nil, ratios, integers, or functions."
  (let ((buf (make-buffer "compact" :context-limit 1000)))
    (let ((rplaca::*compaction-point* nil))
      (is (null (rplaca:compaction-threshold-tokens buf :estimate 42))))
    (let ((rplaca::*compaction-point* 9/10))
      (is (= 900 (rplaca:compaction-threshold-tokens buf :estimate 42))))
    (let ((rplaca::*compaction-point* 1234))
      (is (= 1234 (rplaca:compaction-threshold-tokens buf :estimate 42))))
    (let ((rplaca::*compaction-point*
            (lambda (buffer estimate limit)
              (declare (ignore buffer estimate))
              (/ limit 4))))
      (is (= 250 (rplaca:compaction-threshold-tokens buf :estimate 42))))
    (let ((rplaca::*compaction-point*
            (lambda (buffer estimate limit)
              (declare (ignore buffer limit))
              (>= estimate 42))))
      (is (= 42 (rplaca:compaction-threshold-tokens buf :estimate 42))))))

(test maybe-compact-buffer-runs-custom-function-only-at-threshold
  "maybe-compact-buffer calls the configured function only when needed."
  (let ((buf (make-buffer "compact")))
    (append-test-user-message buf "hello")
    (let ((calls 0)
          (reasons nil))
      (let ((rplaca::*compaction-point* 1000000)
            (rplaca::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore buffer))
                (incf calls)
                (push reason reasons)
                t)))
        (multiple-value-bind (compacted-p estimate threshold)
            (rplaca:maybe-compact-buffer buf :reason :too-small)
          (declare (ignore estimate threshold))
          (is-false compacted-p)
          (is (= 0 calls))))
      (let ((rplaca::*compaction-point* 1)
            (rplaca::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore buffer))
                (incf calls)
                (push reason reasons)
                t)))
        (multiple-value-bind (compacted-p estimate threshold)
            (rplaca:maybe-compact-buffer buf :reason :large-enough)
          (declare (ignore estimate threshold))
          (is-true compacted-p)
          (is (= 1 calls))
          (is (equal '(:large-enough) reasons)))))))

(test default-compact-buffer-replaces-history-with-summary-and-recent-users
  "Default compaction summarizes old history without exposing tools."
  (let ((buf (make-buffer "compact" :agent-name "agent" :context-limit 100000))
        (captured-messages nil)
        (captured-tools nil)
        (captured-system-prompt nil))
    (append-test-user-message buf "first user context")
    (buffer-insert-agent-message buf "assistant work that should be summarized")
    (append-test-user-message buf "latest user request")
    (with-function-override (rplaca::provider-request-streaming
                             (provider messages callback
                                       &key model max-tokens tools
                                       reasoning-effort system-prompt)
                             (declare (ignore provider callback model
                                              max-tokens reasoning-effort))
                             (setf captured-messages messages
                                   captured-tools tools
                                   captured-system-prompt system-prompt)
                             (make-completed-stream-state-response
                              "end_turn"
                              (list (rplaca::canonical-text-block
                                     "summary body"))))
      (let ((rplaca::*compaction-preserved-user-message-token-limit* 1000))
        (is (eq buf (rplaca:default-compact-buffer buf :reason :manual)))))
    (is (equal '(:compaction-summary :user :user :system)
               (test-buffer-history-senders buf)))
    (let ((history (test-buffer-history-messages buf)))
      (is (search rplaca:*compaction-summary-prefix*
                  (message-text (first history))))
      (is (search "summary body" (message-text (first history))))
      (is (string= "first user context" (message-text (second history))))
      (is (string= "latest user request" (message-text (third history))))
      (is (search "Conversation compacted" (message-text (fourth history)))))
    (is (= 0 (length captured-tools)))
    (is (stringp captured-system-prompt))
    (let* ((prompt-message (car (last captured-messages)))
           (content (cdr (assoc :content prompt-message)))
           (block (aref content 0)))
      (is (string= rplaca:*compaction-prompt*
                   (cdr (assoc :text block)))))
    (let ((provider-messages (rplaca:build-conversation-messages buf)))
      (is (every (lambda (message)
                   (string= "user" (cdr (assoc :role message))))
                 provider-messages))
      (is (not (search "Conversation compacted"
                       (rplaca:api-json-encode provider-messages)))))))

(test default-compact-buffer-rotates-session-transcript
  "Compaction starts a new transcript segment referencing the previous one."
  (let* ((*sessions-dir* (temp-session-test-directory "compact-rotate"))
         (session (load-or-create-session "compact-rotate"))
         (buf (make-buffer "compact-rotate"
                           :agent-name "agent"
                           :context-limit 100000
                           :session session)))
    (append-test-user-message buf "first user context")
    (buffer-insert-agent-message buf "assistant work that should be summarized")
    (append-test-user-message buf "latest user request")
    (let ((previous-path (session-current-transcript-path session)))
      (with-function-override (rplaca::provider-request-streaming
                               (provider messages callback
                                         &key model max-tokens tools
                                         reasoning-effort system-prompt)
                               (declare (ignore provider messages callback model
                                                max-tokens tools reasoning-effort
                                                system-prompt))
                               (make-completed-stream-state-response
                                "end_turn"
                                (list (rplaca::canonical-text-block
                                       "summary body"))))
        (let ((rplaca::*compaction-preserved-user-message-token-limit* 1000))
          (is (eq buf (rplaca:default-compact-buffer
                       buf
                       :reason :manual)))))
      (is (not (equal (namestring previous-path)
                      (namestring (session-current-transcript-path session)))))
      (let* ((old-events (read-jsonl-events previous-path))
             (new-events (session-current-events session))
             (first-new (first new-events))
             (message-events
               (remove-if-not (lambda (event)
                                (string= "message"
                                         (event-value event :event)))
                              new-events)))
        (is (= 4 (length old-events)))
        (is (string= "previous-transcript"
                     (event-value first-new :event)))
        (is (string= (rplaca::session-path-string previous-path)
                     (event-value first-new :previous-transcript-path)))
        (is (find "COMPACTION-SUMMARY" message-events
                  :key (lambda (event)
                         (event-value event :sender))
                  :test #'string=))
        (is (find "SYSTEM" message-events
                  :key (lambda (event)
                         (event-value event :sender))
                  :test #'string=))))))

(test send-message-refuses-custom-interactive-compaction-and-continues
  "Unsafe custom snapshot compaction is refused without losing the user send."
  (let ((buf (make-buffer "compact-send"))
        (compactor-called-p nil)
        (saw-input nil)
        (saw-read-only nil)
        (sent-p nil))
    (rplaca::set-message-text (buffer-input-message buf)
                                "current user request")
    (with-function-override (rplaca::send-to-agent-with-context (buffer)
                              (setf sent-p t)
                              buffer)
      (let ((rplaca::*compaction-point* 1)
            (rplaca::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore reason))
                (setf compactor-called-p t
                      saw-input (message-text (buffer-input-message buffer))
                      saw-read-only (message-read-only-p
                                     (buffer-input-message buffer)))
                buffer)))
        (rplaca::send-message buf)))
    (is-false compactor-called-p)
    (is (null saw-input))
    (is-false saw-read-only)
    (is-true sent-p)
    (let* ((messages (test-buffer-history-messages buf))
           (user-message
             (find "current user request" messages
                   :key #'message-text :test #'string=)))
      (is-true user-message)
      (is-true (message-read-only-p user-message))
      (is (some (lambda (message)
                  (search "Interactive custom compaction is not supported"
                          (message-text message)))
                messages)))))

(test run-single-prompt-compacts-before-provider-request
  "Prompt mode applies compaction before sending provider requests."
  (let ((compacted-p nil)
        (provider-requested-p nil))
    (with-function-override (rplaca::provider-request-streaming
                             (provider messages callback
                                       &key model max-tokens tools
                                       reasoning-effort system-prompt)
                             (declare (ignore provider messages callback model
                                              max-tokens tools reasoning-effort
                                              system-prompt))
                             (setf provider-requested-p t)
                             (make-completed-stream-state-response
                              "end_turn"
                              (list (rplaca::canonical-text-block
                                     "final after compaction"))))
      (let ((rplaca::*compaction-point* 1)
            (rplaca::*compaction-function*
              (lambda (buffer &key reason)
                (declare (ignore buffer))
                (when (eq reason :prompt-request)
                  (setf compacted-p t))
                t)))
        (let ((result (rplaca:run-single-prompt
                       "hello"
                       :provider :zai
                       :model "glm-5")))
          (is-true compacted-p)
          (is-true provider-requested-p)
          (is (string= "final after compaction"
                       (rplaca:prompt-run-result-final-text result))))))))

(test make-prompt-buffer-attaches-session-transcript
  "Prompt-mode buffers are real sessions and record their seeded user prompt."
  (let* ((*sessions-dir* (temp-session-test-directory "prompt-buffer"))
         (buf (rplaca::make-prompt-buffer "hello from prompt" "agent"))
         (session (buffer-session buf)))
    (is (not (null session)))
    (is (probe-file (session-current-transcript-path session)))
    (let* ((events (session-current-events session))
           (message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            events))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "USER" (event-value event :sender)))
      (is (string= "hello from prompt" (event-value event :text))))))

(test run-single-prompt-returns-final-response
  "Non-interactive prompt mode returns a final assistant response without a UI."
  (let ((path (temp-agent-defaults-path))
        (seen-provider nil)
        (seen-model nil)
        (seen-messages nil))
    (with-agent-defaults-path-override (path)
      (with-function-override (rplaca::provider-request-streaming
                               (provider messages callback
                                         &key model max-tokens tools
                                         reasoning-effort system-prompt)
                               (declare (ignore callback))
                               (declare (ignore max-tokens tools reasoning-effort
                                                system-prompt))
                               (setf seen-provider provider
                                     seen-model model
                                     seen-messages messages)
                               (make-completed-stream-state-response
                                "end_turn"
                                (list (rplaca::canonical-text-block "final answer")
                                      (rplaca::canonical-reasoning-block
                                       "provider reasoning summary"))))
        (rplaca::init-default-keymap)
        (initialize-test-tools)
        (let ((result (rplaca:run-single-prompt
                       "Say hello"
                       :provider :zai
                       :model "glm-5")))
          (is (eq :zai seen-provider))
          (is (string= "glm-5" seen-model))
          (is (= 1 (length seen-messages)))
          (is (string= "final answer"
                       (rplaca:prompt-run-result-final-text result)))
          (is (equal '("provider reasoning summary")
                     (rplaca:prompt-run-result-reasoning-blocks result)))
          (is (= 1 (rplaca:prompt-run-result-iterations result)))
          (is (null (rplaca:prompt-run-result-tool-events result))))))))

(test run-single-prompt-refuses-command-only-lisp-eval-and-continues
  "Prompt mode records a Command-Only refusal and continues the tool loop."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (second-request-messages nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore callback))
                                 (declare (ignore provider model max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (rplaca::canonical-tool-use-block
                                        "call-1"
                                        "lisp_eval"
                                        '((:code . "(+ 2 3)"))))
                                      '(:input-tokens 100
                                        :output-tokens 10
                                        :total-tokens 110
                                        :cached-input-tokens 64
                                        :uncached-input-tokens 36
                                        :cache-hit-rate 0.64))
                                     (progn
                                       (setf second-request-messages messages)
                                       (make-completed-stream-state-response
                                        "end_turn"
                                        (list (rplaca::canonical-text-block
                                               "live evaluation was refused"))
                                        '(:input-tokens 120
                                          :output-tokens 20
                                          :total-tokens 140
                                          :cached-input-tokens 80
                                          :uncached-input-tokens 40
                                          :cache-hit-rate 0.6666667)))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let* ((result (rplaca:run-single-prompt
                          "Compute two plus three"
                          :provider :zai
                          :model "glm-5"))
                 (events (rplaca:prompt-run-result-tool-events result))
                 (event (first events)))
            (is (= 2 request-count))
            (is (= 3 (length second-request-messages)))
            (is (string= "live evaluation was refused"
                         (rplaca:prompt-run-result-final-text result)))
            (is (= 2 (rplaca:prompt-run-result-iterations result)))
            (let ((usage (rplaca:prompt-run-result-usage result)))
              (is (= 220 (getf usage :input-tokens)))
              (is (= 30 (getf usage :output-tokens)))
              (is (= 250 (getf usage :total-tokens)))
              (is (= 144 (getf usage :cached-input-tokens)))
              (is (= 76 (getf usage :uncached-input-tokens)))
              (is (< (abs (- (getf usage :cache-hit-rate)
                              (/ 144.0 220)))
                     0.0001))
              (let ((json-usage
                      (cdr (assoc :usage
                                  (rplaca::prompt-run-result-json result)))))
                (is (= 144 (cdr (assoc :cached--input--tokens
                                        json-usage))))))
            (is (= 1 (length events)))
            (is (string= "lisp_eval" (rplaca:prompt-tool-event-name event)))
            (is-true (rplaca:prompt-tool-event-denied-p event))
            (is (search "REFUSED" (rplaca:prompt-tool-event-display event)))
            (is (search "Command-Only"
                        (rplaca:prompt-tool-event-result-text event)))
            (let* ((tool-result-message (third second-request-messages))
                   (content (coerce (cdr (assoc :content tool-result-message))
                                    'list))
                   (tool-result (first content)))
              (is (string= "user" (cdr (assoc :role tool-result-message))))
              (is (string= "tool_result" (cdr (assoc :type tool-result))))
              (is (string= "call-1"
                           (cdr (assoc :tool--use--id tool-result))))
              (is (search "Command-Only"
                          (cdr (assoc :content tool-result)))))))))))

(test run-session-prompt-reuses-saved-session-context
  "Session prompt mode reloads prior turns before sending the next prompt."
  (let ((path (temp-agent-defaults-path))
        (*sessions-dir* (temp-session-test-directory "session-prompt"))
        (request-count 0)
        (second-request-messages nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore callback))
                                 (declare (ignore provider model max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list (rplaca::canonical-text-block
                                             "first answer")))
                                     (progn
                                       (setf second-request-messages messages)
                                       (make-completed-stream-state-response
                                        "end_turn"
                                        (list (rplaca::canonical-text-block
                                               "second answer"))))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let* ((first (rplaca:run-session-prompt
                         "First prompt"
                         :session-name "cache-probe"
                         :provider :zai
                         :model "glm-5"))
                 (second (rplaca:run-session-prompt
                          "Second prompt"
                          :session-name "cache-probe"
                          :provider :zai
                          :model "glm-5")))
            (is (string= "first answer"
                         (rplaca:prompt-run-result-final-text first)))
            (is (string= "second answer"
                         (rplaca:prompt-run-result-final-text second)))
            (is (= 2 request-count))
            (is (= 3 (length second-request-messages)))
            (is (string= "First prompt"
                         (rplaca::content-text-blocks
                          (coerce (cdr (assoc :content
                                              (first second-request-messages)))
                                  'list))))
            (is (string= "first answer"
                         (rplaca::content-text-blocks
                          (coerce (cdr (assoc :content
                                              (second second-request-messages)))
                                  'list))))
            (is (string= "Second prompt"
                         (rplaca::content-text-blocks
                          (coerce (cdr (assoc :content
                                              (third second-request-messages)))
                                  'list))))
            (let ((loaded (load-session "cache-probe")))
              (is (equal '(:user :agent :user :agent)
                         (test-buffer-history-senders loaded))))))))))

(test run-session-prompt-binds-openai-prompt-cache-controls
  "OpenAI session prompt mode sends a stable cache key for the saved session."
  (let ((path (temp-agent-defaults-path))
        (*sessions-dir* (temp-session-test-directory "session-prompt-cache"))
        (captured-cache-key nil)
        (captured-cache-retention nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore messages callback max-tokens
                                                  tools reasoning-effort
                                                  system-prompt))
                                 (is (eq :openai-codex provider))
                                 (is (string= "gpt-5.4" model))
                                 (setf captured-cache-key
                                       rplaca::*openai-codex-prompt-cache-key*
                                       captured-cache-retention
                                       rplaca::*openai-codex-prompt-cache-retention*)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (rplaca::canonical-text-block
                                         "cached"))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let ((result (rplaca:run-session-prompt
                         "Cache probe"
                         :session-name "cache-probe"
                         :provider :openai-codex
                         :model "gpt-5.4")))
            (is (string= "cached"
                         (rplaca:prompt-run-result-final-text result)))
            (is (string= (format nil "rplaca-agent-~(~A~)"
                                  (rplaca::session-name-hash
                                   "cache-probe"))
                         captured-cache-key))
            (is (null captured-cache-retention))))))))

(test run-subagent-uses-registered-agent-and-routing-overrides
  "run-subagent can delegate to a registered agent with explicit routing overrides."
  (let ((path (temp-agent-defaults-path))
        (seen-provider nil)
        (seen-model nil)
        (seen-think-level nil))
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (rplaca:register-agent-definition
         "researcher"
         :provider :zai
         :model "glm-5"
         :personality-prompt "research personality")
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore messages callback max-tokens
                                                  tools system-prompt))
                                 (setf seen-provider provider
                                       seen-model model
                                       seen-think-level reasoning-effort)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (rplaca::canonical-text-block
                                         "delegated answer"))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let ((result (rplaca:run-subagent
                         "Research this"
                         :agent-name "researcher"
                         :provider :openai-codex
                         :model "gpt-5.3-codex"
                         :think-level "high")))
            (is (eq :openai-codex seen-provider))
            (is (string= "gpt-5.3-codex" seen-model))
            (is (string= "high" seen-think-level))
            (is (string= "researcher"
                         (rplaca:prompt-run-result-agent-name result)))
            (is (string= "delegated answer"
                         (rplaca:prompt-run-result-final-text result)))))))))

(test run-subagent-uses-transient-prompts-without-registering-agent
  "Custom subagent prompts are dynamically scoped and do not mutate the registry."
  (let ((path (temp-agent-defaults-path))
        (seen-system-prompt nil))
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (with-function-override (rplaca::load-boot-files ()
                                  nil)
          (with-function-override (rplaca::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback model
                                                    max-tokens tools reasoning-effort))
                                   (setf seen-system-prompt system-prompt)
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (rplaca::canonical-text-block
                                           "custom answer"))))
            (rplaca::init-default-keymap)
            (initialize-test-tools)
            (let ((result (rplaca:run-subagent
                           "Use a custom prompt"
                           :agent-name "temporary-doc-agent"
                           :provider :zai
                           :model "glm-5"
                           :core-prompt "TEMP CORE"
                           :personality-prompt "TEMP PERSONALITY")))
              (is (search "TEMP CORE" seen-system-prompt))
              (is (search "TEMP PERSONALITY" seen-system-prompt))
              (is (null (rplaca:find-agent-definition
                         "temporary-doc-agent")))
              (is (string= "custom answer"
                           (rplaca:prompt-run-result-final-text result))))))))))

(test run-subagent-uses-agent-tool-selection-and-explicit-overrides
  "Agent defaults select request tools; explicit subagent names override them."
  (let ((path (temp-agent-defaults-path))
        (captured-tool-names nil))
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (with-tool-table-restored
          (clrhash rplaca::*tool-table*)
          (initialize-test-tools)
          (rplaca:register-tool
           "doc_lookup"
           "Look up docs."
           '((:type . "object")
             (:properties . ((:query . ((:type . "string"))))))
           (lambda (args)
             (declare (ignore args))
             "{\"doc\":\"ok\"}"))
          (rplaca:register-agent-definition
           "docs"
           :provider :zai
           :model "glm-5"
           :tool-names '("doc_lookup"))
          (with-function-override (rplaca::provider-request-streaming
                                   (provider messages callback
                                             &key model max-tokens tools
                                             reasoning-effort system-prompt)
                                   (declare (ignore provider messages callback model
                                                    max-tokens reasoning-effort
                                                    system-prompt))
                                   (setf captured-tool-names
                                         (mapcar (lambda (tool)
                                                   (cdr (assoc :name tool)))
                                                 (coerce tools 'list)))
                                   (make-completed-stream-state-response
                                    "end_turn"
                                    (list (rplaca::canonical-text-block
                                           "done"))))
            (rplaca::init-default-keymap)
            (rplaca:run-subagent "Find docs" :agent-name "docs")
            (is (equal '("doc_lookup") captured-tool-names))
            (setf captured-tool-names nil)
            (rplaca:run-subagent
             "Use lisp instead"
             :agent-name "docs"
             :tool-names '("lisp_eval"))
            (is (equal '("lisp_eval") captured-tool-names))))))))

(test run-subagent-custom-tools-are-temporary-and-executable
  "Custom subagent tools are exposed only for the run and record tool evidence."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (captured-tool-names nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (clrhash rplaca::*tool-table*)
        (initialize-test-tools)
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens reasoning-effort
                                                  system-prompt))
                                 (incf request-count)
                                 (when (= request-count 1)
                                   (setf captured-tool-names
                                         (mapcar (lambda (tool)
                                                   (cdr (assoc :name tool)))
                                                 (coerce tools 'list))))
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (rplaca::canonical-tool-use-block
                                        "call-custom"
                                        "custom_echo"
                                        '((:payload . "ok")))))
                                     (make-completed-stream-state-response
                                      "end_turn"
                                      (list (rplaca::canonical-text-block
                                             "custom done")))))
          (rplaca::init-default-keymap)
          (let* ((tool (rplaca:make-subagent-tool
                        :name "custom_echo"
                        :description "Echo a payload."
                        :input-schema
                        '((:type . "object")
                          (:properties . ((:payload . ((:type . "string")))))
                          (:required . #("payload")))
                        :execute-fn
                        (lambda (args)
                          (format nil "echo=~A" (cdr (assoc :payload args))))))
                 (result (rplaca:run-subagent
                          "Use the custom tool"
                          :agent-name "custom-tool-agent"
                          :provider :zai
                          :model "glm-5"
                          :custom-tools (list tool)))
                 (events (rplaca:prompt-run-result-tool-events result))
                 (event (first events)))
            (is (equal '("custom_echo") captured-tool-names))
            (is (= 2 request-count))
            (is (string= "custom done"
                         (rplaca:prompt-run-result-final-text result)))
            (is (= 1 (length events)))
            (is (string= "custom_echo"
                         (rplaca:prompt-tool-event-name event)))
            (is (search "echo=ok"
                        (rplaca:prompt-tool-event-result-text event)))
            (is (null (gethash "custom_echo" rplaca::*tool-table*)))))))))

(test run-subagent-custom-tool-plists-and-explicit-tool-names
  "Custom tool plists normalize correctly and explicit tool names can mix scopes."
  (let ((path (temp-agent-defaults-path))
        (captured-tool-names nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (clrhash rplaca::*tool-table*)
        (initialize-test-tools)
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens reasoning-effort
                                                  system-prompt))
                                 (setf captured-tool-names
                                       (sort
                                        (mapcar (lambda (tool)
                                                  (cdr (assoc :name tool)))
                                                (coerce tools 'list))
                                        #'string<))
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (rplaca::canonical-text-block
                                         "done"))))
          (rplaca::init-default-keymap)
          (rplaca:run-subagent
           "Use available tools"
           :agent-name "custom-tool-agent"
           :provider :zai
           :model "glm-5"
           :custom-tools
           (list (list :name "custom_plist"
                       :description "Plist-defined tool."
                       :schema '((:type . "object"))
                       :execute-fn (lambda (args)
                                     (declare (ignore args))
                                     "plist")))
           :tool-names '("custom_plist" "lisp_eval"))
          (is (equal '("custom_plist" "lisp_eval")
                     captured-tool-names))
          (is (null (gethash "custom_plist" rplaca::*tool-table*))))))))

(test run-subagent-records-unexposed-tool-call-as-tool-result-error
  "A provider call outside the selected workflow tool set returns an error."
  (let ((path (temp-agent-defaults-path))
        (request-count 0)
        (second-request-messages nil))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (clrhash rplaca::*tool-table*)
        (initialize-test-tools)
        (rplaca:register-tool
         "doc_lookup"
         "Look up docs."
         '((:type . "object")
           (:properties . ((:query . ((:type . "string"))))))
         (lambda (args)
           (declare (ignore args))
           "{\"doc\":\"ok\"}"))
        (rplaca:register-tool
         "write_probe"
         "A tool this subagent must not call."
         '((:type . "object")
           (:properties . ((:payload . ((:type . "string"))))))
         (lambda (args)
           (declare (ignore args))
           "{\"wrote\":true}"))
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (incf request-count)
                                 (if (= request-count 1)
                                     (make-completed-stream-state-response
                                      "tool_use"
                                      (list
                                       (rplaca::canonical-tool-use-block
                                        "call-write"
                                        "write_probe"
                                        '((:payload . "bad")))))
                                     (progn
                                       (setf second-request-messages messages)
                                       (make-completed-stream-state-response
                                        "end_turn"
                                        (list (rplaca::canonical-text-block
                                               "handled unavailable tool"))))))
          (rplaca::init-default-keymap)
          (let* ((result (rplaca:run-subagent
                          "Try the wrong tool"
                          :agent-name "docs"
                          :provider :zai
                          :model "glm-5"
                          :tool-names '("doc_lookup")))
                 (events (rplaca:prompt-run-result-tool-events result))
                 (event (first events))
                 (tool-result-message (third second-request-messages))
                 (tool-result (first (coerce
                                      (cdr (assoc :content
                                                  tool-result-message))
                                      'list))))
            (is (= 2 request-count))
            (is (= 1 (length events)))
            (is (string= "write_probe"
                         (rplaca:prompt-tool-event-name event)))
            (is (search "not exposed"
                        (rplaca:prompt-tool-event-result-text event)))
            (is (search "not exposed"
                        (cdr (assoc :content tool-result))))
            (is (string= "handled unavailable tool"
                         (rplaca:prompt-run-result-final-text result)))))))))

(test run-subagent-async-waits-and-registers-result
  "Async subagents return a handle, register it, and preserve final results."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (rplaca::canonical-text-block
                                         "async answer"))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (rplaca:run-subagent-async
                         "Do async work"
                         :agent-name "async-agent"
                         :provider :zai
                         :model "glm-5")))
            (is (string= "subagent-1"
                         (rplaca:subagent-handle-id handle)))
            (is (eq handle
                    (rplaca:find-subagent
                     (rplaca:subagent-handle-id handle))))
            (is (member handle (rplaca:list-subagents)))
            (multiple-value-bind (result status returned-handle)
                (rplaca:wait-subagent handle :timeout 2)
              (is (eq :succeeded status))
              (is (eq handle returned-handle))
              (is (rplaca:subagent-done-p handle))
              (is (string= "async answer"
                           (rplaca:prompt-run-result-final-text result)))
              (let ((snapshot (rplaca:subagent-snapshot handle)))
                (is (string= "subagent-1" (getf snapshot :id)))
                (is (eq :succeeded (getf snapshot :status)))
                (is (getf snapshot :done-p))
                (is (eq result (getf snapshot :result)))))))))))

(test run-subagent-async-records-failures
  "Async provider failures are captured on the handle instead of escaping."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (error "provider boom"))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (rplaca:run-subagent-async
                         "Fail async work"
                         :provider :zai
                         :model "glm-5")))
            (multiple-value-bind (result status)
                (rplaca:wait-subagent handle :timeout 2)
              (is (null result))
              (is (eq :failed status))
              (is (search "provider boom"
                          (rplaca:subagent-error handle))))))))))

(test cancel-subagent-closes-stream-and-settles-worker
  "Cancellation settles only after the provider worker has unwound."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (sleep 0.1)
                                 (make-completed-stream-state-response
                                  "end_turn"
                                  (list (rplaca::canonical-text-block
                                         "late answer"))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (rplaca:run-subagent-async
                         "Cancel async work"
                         :provider :zai
                         :model "glm-5")))
            (is (eq :running (rplaca:subagent-status handle)))
            (rplaca:cancel-subagent handle)
            (is (member (rplaca:subagent-status handle)
                        '(:cancelling :cancelled)))
            (multiple-value-bind (result status)
                (rplaca:wait-subagent handle :timeout 1)
              (is (null result))
              (is (eq :cancelled status)))
            (is (eq :cancelled (rplaca:subagent-status handle)))
            (is (rplaca:subagent-done-p handle))
            (is (getf (rplaca:subagent-snapshot handle)
                      :worker-finished-p))
            (is (null (rplaca:subagent-result handle)))))))))

(test cancel-subagent-cancels-active-provider-stream
  "An active stream is closed and no assistant/tool work is committed after cancel."
  (let ((path (temp-agent-defaults-path))
        (state nil))
    (with-agent-defaults-path-override (path)
      (with-subagent-registry-override ()
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback model
                                                  max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (setf state (rplaca::make-stream-state)))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (let ((handle (rplaca:run-subagent-async
                         "Cancel active stream"
                         :provider :zai
                         :model "glm-5")))
            (loop :repeat 200
                  :until (rplaca::subagent-handle-current-stream-state handle)
                  :do (sleep 0.005))
            (is (eq state
                    (rplaca::subagent-handle-current-stream-state handle)))
            (rplaca:cancel-subagent handle)
            (multiple-value-bind (result status)
                (rplaca:wait-subagent handle :timeout 2 :poll-interval 0.005)
              (is (null result))
              (is (eq :cancelled status)))
            (is-true (rplaca::stream-state-cancel-requested-p-safe state))
            (is (rplaca:subagent-done-p handle))))))))

(test prompt-run-tool-verification-helpers
  "Parent agents can check tool usage without parsing raw events."
  (let* ((result (rplaca::make-prompt-run-result
                  :tool-events
                  (list (rplaca::make-prompt-tool-event
                         :name "doc_lookup")
                        (rplaca::make-prompt-tool-event
                         :name "lisp_eval")))))
    (is (equal '("doc_lookup" "lisp_eval")
               (rplaca:prompt-run-tool-names result)))
    (is (= 2 (rplaca:prompt-run-tool-count result)))
    (is (= 1 (rplaca:prompt-run-tool-count result "doc_lookup")))
    (is (rplaca:prompt-run-used-tool-p result 'doc-lookup))
    (is-false (rplaca:prompt-run-used-tool-p result "missing_tool"))))

(test execute-lisp-eval-captures-output-and-history
  "lisp_eval captures printed output, values, and returns Lisp data."
  (with-tool-table-restored
    (let ((rplaca::*lisp-eval-history* nil)
          (rplaca::*last-eval-result* nil)
          (rplaca::*last-eval-condition* nil))
      (initialize-test-tools)
      (let* ((data (rplaca:execute-tool
                    "lisp_eval"
                    '(:code "(progn (format t \"hello\") (values 4 5))")))
             (decoded (rplaca::lisp-data-read data)))
        (is (search ":code" data :test #'char-equal))
        (is (search "4" (getf decoded :result)))
        (is (search "5" (getf decoded :result)))
        (is (string= "hello" (getf decoded :output)))
        (is (= 2 (getf decoded :values)))
        (is (equal '(4 5) rplaca:*last-eval-result*))
        (is (null rplaca:*last-eval-condition*))
        (is (search "hello" (rplaca:eval-history-to-string)))))))

(test execute-lisp-eval-prints-values-defensively
  "Result printing failures do not turn successful lisp_eval calls into errors."
  (with-tool-table-restored
    (let ((rplaca::*lisp-eval-history* nil)
          (rplaca::*last-eval-result* nil)
          (rplaca::*last-eval-condition* nil))
      (initialize-test-tools)
      (let* ((data (rplaca:execute-tool
                    "lisp_eval"
                    '(:code "(rplaca/tests::make-unprintable-eval-value)")))
             (decoded (rplaca::lisp-data-read data)))
        (is (= 1 (getf decoded :values)))
        (is (search "unprintable value" (getf decoded :result)))
        (is (null (getf decoded :error)))
        (is (not (null rplaca:*last-eval-result*)))
        (is (typep (first rplaca:*last-eval-result*)
                   'unprintable-eval-value))
        (is (null rplaca:*last-eval-condition*))
        (is (search "unprintable value"
                    (rplaca:eval-history-to-string)))))))

(test execute-lisp-eval-records-errors
  "Failed lisp_eval executions expose the condition and still record history."
  (with-tool-table-restored
    (let ((rplaca::*lisp-eval-history* nil)
          (rplaca::*last-eval-result* nil)
          (rplaca::*last-eval-condition* nil))
      (initialize-test-tools)
      (let* ((data (rplaca:execute-tool
                    "lisp_eval"
                    '(:code "(error \"boom\")")))
             (decoded (rplaca::lisp-data-read data)))
        (is (search ":error" data :test #'char-equal))
        (is (search "boom" (getf decoded :error)))
        (is (null rplaca:*last-eval-result*))
        (is (not (null rplaca:*last-eval-condition*)))
        (is (search "boom" (rplaca:eval-history-to-string)))))))

(test execute-lisp-eval-isolated-mode-runs-in-worker
  "Isolated lisp_eval evaluates in a worker process without mutating this image."
  (with-tool-table-restored
    (initialize-test-tools)
    (let ((symbol (find-symbol "*ISOLATED-EVAL-PROOF*" :cl-user)))
      (when symbol
        (unintern symbol :cl-user)))
    (let* ((data (rplaca:execute-tool
                  "lisp_eval"
                  '(:mode "isolated"
                    :package "CL-USER"
                    :code "(progn (defparameter *isolated-eval-proof* :worker) (values 8 9))")))
           (decoded (rplaca::lisp-data-read data)))
      (is (eq :isolated (getf decoded :mode)))
      (is (= 2 (getf decoded :values)))
      (is (search "8" (getf decoded :result)))
      (is (search "9" (getf decoded :result)))
      (is-false (find-symbol "*ISOLATED-EVAL-PROOF*" :cl-user)))))

(test execute-lisp-eval-isolated-mode-reports-errors
  "Isolated lisp_eval reports worker conditions as tool data instead of crashing."
  (with-tool-table-restored
    (initialize-test-tools)
    (let* ((data (rplaca:execute-tool
                  "lisp_eval"
                  '(:mode "isolated"
                    :package "CL-USER"
                    :code "(error \"isolated boom\")")))
           (decoded (rplaca::lisp-data-read data)))
      (is (eq :isolated (getf decoded :mode)))
      (is (search "isolated boom" (getf decoded :error))))))

(test run-single-prompt-error-carries-partial-tool-events
  "Prompt loop failures retain tool events for diagnostics."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (with-tool-table-restored
        (with-function-override (rplaca::provider-request-streaming
                                 (provider messages callback
                                           &key model max-tokens tools
                                           reasoning-effort system-prompt)
                                 (declare (ignore provider messages callback
                                                  model max-tokens tools
                                                  reasoning-effort system-prompt))
                                 (make-completed-stream-state-response
                                  "tool_use"
                                  (list
                                   (rplaca::canonical-tool-use-block
                                    "loop-call"
                                    "lisp_eval"
                                    '((:code . "(+ 1 1)"))))))
          (rplaca::init-default-keymap)
          (initialize-test-tools)
          (handler-case
              (progn
                (rplaca:run-single-prompt
                 "loop forever"
                 :provider :zai
                 :model "glm-5"
                 :max-tool-iterations 1)
                (fail "Expected prompt-run-error"))
            (rplaca:prompt-run-error (condition)
              (let ((events (rplaca:prompt-run-error-tool-events condition)))
                (is (search "Exceeded maximum tool iterations"
                            (rplaca:prompt-run-error-message condition)))
                (is (= 1 (rplaca:prompt-run-error-iterations condition)))
                (is (= 1 (length events)))
                (is (string= "lisp_eval"
                             (rplaca:prompt-tool-event-name
                              (first events))))
                (is-true
                 (rplaca:prompt-tool-event-denied-p (first events)))
                (is (search "Command-Only"
                            (rplaca:prompt-tool-event-result-text
                             (first events))))))))))))

(test provider-token-anthropic-is-unsupported
  "Anthropic no longer has a provider-specific token path."
  (signals error
    (rplaca::provider-token-path :anthropic))
  (signals error
    (rplaca::read-provider-token :anthropic))
  (signals error
    (rplaca::save-provider-token :anthropic "removed")))

(test provider-token-round-trip-openai-codex
  "OpenAI Codex tokens round-trip through provider-specific helpers."
  (let ((openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (nil openai-codex-path)
      (is (string= "openai-token"
                   (rplaca::save-provider-token :openai-codex "openai-token")))
      (is (string= "openai-token"
                   (rplaca::read-provider-token :openai-codex))))))

(test read-provider-token-trims-whitespace
  "Provider token reads trim surrounding whitespace."
  (let ((openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (nil openai-codex-path)
      (with-open-file (stream openai-codex-path
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (write-string "  trimmed-token  " stream)
        (terpri stream))
      (is (string= "trimmed-token"
                   (rplaca::read-provider-token :openai-codex))))))

;;; --------------------------------------------------------------------------
;;; Environment Variable Token Tests
;;; --------------------------------------------------------------------------

(defmacro with-env-var ((var value) &body body)
  "Temporarily set environment variable VAR to VALUE (or unset if VALUE is nil)."
  (let ((gvar (gensym "VAR-"))
        (gval (gensym "VAL-"))
        (gold (gensym "OLD-")))
    `(let* ((,gvar ,var)
            (,gval ,value)
            (,gold (uiop:getenv ,gvar)))
       (unwind-protect
            (progn
              (if ,gval
                  (setf (uiop:getenv ,gvar) ,gval)
                  (setf (uiop:getenv ,gvar) ""))
              ,@body)
         (if ,gold
             (setf (uiop:getenv ,gvar) ,gold)
             (setf (uiop:getenv ,gvar) ""))))))

(test ensure-prompt-workspace-project-registers-rplaca-source-root
  "Prompt mode registers the mounted workspace as the rplaca project."
  (let* ((base (project-test-directory))
         (workspace (merge-pathnames #P"workspace/" base))
         (custom (merge-pathnames #P"custom/" base))
         (definitions (merge-pathnames #P"defs/" base))
         (*project-registry* (make-hash-table :test #'equal))
         (*project-definitions-directory* definitions)
         (rplaca::*project-definitions-loaded-p* nil))
    (ensure-directories-exist (merge-pathnames #P".keep" workspace))
    (ensure-directories-exist (merge-pathnames #P".keep" custom))
    (ensure-directories-exist (merge-pathnames #P".keep" definitions))
    (with-env-var ("RPLACA_PROMPT_PROJECT_ROOT" (namestring workspace))
      (rplaca::ensure-prompt-workspace-project)
      (let ((project (find-project "rplaca")))
        (is (not (null project)))
        (is (eq :builtin (project-source project)))
        (is (equal '(:rplaca :rplaca/tests)
                   (project-systems project)))
        (is (string= (namestring (truename workspace))
                     (namestring (project-root project)))))
      (define-project "rplaca"
        :root custom
        :description "user-defined rplaca project"
        :source :programmatic
        :replace t)
      (rplaca::ensure-prompt-workspace-project)
      (let ((project (find-project "rplaca")))
        (is (string= "user-defined rplaca project"
                     (project-description project)))
        (is (string= (namestring (truename custom))
                     (namestring (project-root project))))))))

(test read-env-token-returns-value-when-set
  "read-env-token returns the token from a set environment variable."
  (with-env-var ("RPLACA_TEST_TOKEN" "test-env-token-123")
    (is (string= "test-env-token-123"
                 (rplaca::read-env-token "RPLACA_TEST_TOKEN")))))

(test read-env-token-trims-whitespace
  "read-env-token trims leading and trailing whitespace."
  (with-env-var ("RPLACA_TEST_TOKEN" "  env-token-padded  ")
    (is (string= "env-token-padded"
                 (rplaca::read-env-token "RPLACA_TEST_TOKEN")))))

(test read-env-token-returns-nil-for-empty
  "read-env-token returns nil for an empty environment variable."
  (with-env-var ("RPLACA_TEST_TOKEN" "")
    (is (null (rplaca::read-env-token "RPLACA_TEST_TOKEN")))))

(test read-env-token-returns-nil-for-whitespace-only
  "read-env-token returns nil for a whitespace-only environment variable."
  (with-env-var ("RPLACA_TEST_TOKEN" "   ")
    (is (null (rplaca::read-env-token "RPLACA_TEST_TOKEN")))))

(test read-env-token-returns-nil-for-unset
  "read-env-token returns nil for an unset environment variable."
  (is (null (rplaca::read-env-token "RPLACA_DEFINITELY_NOT_SET_12345"))))

(test zai-env-var-takes-highest-priority
  "ZAI_CODING_MAX_API_KEY env var takes priority over the static token file."
  (let ((openai-codex-path (temp-test-token-path :openai-codex))
        (zai-path (temp-test-token-path :zai)))
    (with-provider-token-path-overrides (nil openai-codex-path zai-path)
      ;; Set up file-based token
      (rplaca::save-provider-token :zai "file-zai-key")
      ;; Env var should win
      (with-env-var ("ZAI_CODING_MAX_API_KEY" "env-zai-key")
        (is (string= "env-zai-key"
                     (rplaca::read-provider-token :zai)))))))

(test zai-env-var-falls-through-to-file
  "When ZAI_CODING_MAX_API_KEY is unset, falls through to static token file."
  (let ((openai-codex-path (temp-test-token-path :openai-codex))
        (zai-path (temp-test-token-path :zai)))
    (with-provider-token-path-overrides (nil openai-codex-path zai-path)
      (rplaca::save-provider-token :zai "file-zai-key")
      (let ((rplaca::*zai-env-var* "RPLACA_UNSET_ZAI_ENV_98765"))
        ;; With env var unset, should use file
        (is (string= "file-zai-key"
                     (rplaca::read-provider-token :zai)))))))

(test env-var-does-not-affect-openai-codex
  "OpenAI Codex provider is not affected by Z.AI/OpenRouter env vars."
  (let ((openai-codex-path (temp-test-token-path :openai-codex)))
    (with-provider-token-path-overrides (nil openai-codex-path)
      (rplaca::save-provider-token :openai-codex "codex-file-token")
      (with-env-var ("ZAI_CODING_MAX_API_KEY" "env-zai-token")
        (with-env-var ("OPENROUTER_API_KEY" "env-openrouter-token")
          (is (string= "codex-file-token"
                       (rplaca::read-provider-token :openai-codex))))))))

(test default-env-var-names-are-correct
  "Default environment variable names are as documented."
  (is (string= "ZAI_CODING_MAX_API_KEY" rplaca::*zai-env-var*))
  (is (string= "OPENROUTER_API_KEY" rplaca::*openrouter-env-var*)))

;;; --------------------------------------------------------------------------

(test resolve-buffer-provider-and-model-buffer-override-wins
  "Buffer overrides win over agent defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"zai\"}}")
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.3-codex")
    (set-buffer-think-level-override buf "high")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (string= "high" think-level))))))

(test resolve-buffer-provider-and-model-agent-definition-wins-over-persisted-defaults
  "Programmatic agent definitions outrank persisted compatibility defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"zai\",\"model\":\"glm-5\"}}")
    (with-agent-defaults-path-override (path)
      (with-agent-definition-registry-override ()
        (rplaca:register-agent-definition
         "spark"
         :provider :openai-codex
         :model "gpt-5.4"
         :think-level "high")
        (multiple-value-bind (provider model think-level)
            (rplaca::resolve-buffer-provider-and-model buf)
          (is (eq :openai-codex provider))
          (is (string= "gpt-5.4" model))
          (is (string= "high" think-level)))))))

(test resolve-buffer-provider-and-model-agent-default-provider
  "Agent defaults are used when no buffer override is present."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.6-sol" model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-agent-default-model
  "Persisted agent default models are used when no buffer model override exists."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\",\"model\":\"gpt-5.3-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-unknown-agent-falls-back
  "Unknown agents fall back to the current built-in provider/model defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "unknown-agent")))
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buf)
        (is (eq rplaca::*default-provider* provider))
        (is (string= (rplaca::provider-fallback-model rplaca::*default-provider*)
                     model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-openai-codex-fallback-model
  "OpenAI Codex resolves to its built-in fallback model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.6-sol" model))
        (is (null think-level))))))

(test resolve-buffer-provider-and-model-unsupported-think-returns-nil
  "Unsupported think overrides do not resolve for the active model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.1-codex-max")
    (set-buffer-think-level-override buf "xhigh")
    (with-agent-defaults-path-override (path)
      (multiple-value-bind (provider model think-level)
          (rplaca::resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.1-codex-max" model))
        (is (null think-level))))))

(test reconcile-buffer-think-level-override-resets-unsupported-model
  "Reconciliation clears a think level that no longer applies to the model."
  (let ((buf (make-buffer "test")))
    (setf (buffer-provider-override buf) :openai-codex
          (buffer-model-override buf) "gpt-5.1-codex-max")
    (set-buffer-think-level-override buf "xhigh")
    (multiple-value-bind (status think-level)
        (rplaca::reconcile-buffer-think-level-override buf)
      (is (eq :reset status))
      (is (null think-level))
      (is (null (buffer-think-level-override buf))))))

(test resolve-buffer-provider-and-model-rejects-blank-persisted-model
  "Blank persisted default models are rejected when they become the resolved model."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\",\"model\":\"\"}}")
    (with-agent-defaults-path-override (path)
      (signals error
        (rplaca::resolve-buffer-provider-and-model buf)))))

(test resolve-buffer-provider-and-model-rejects-blank-model
  "Blank buffer model overrides are rejected."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (setf (buffer-model-override buf) "")
    (with-agent-defaults-path-override (path)
      (signals error
        (rplaca::resolve-buffer-provider-and-model buf)))))

(test agent-defaults-lazy-initialization-loads-file-and-built-ins
  "Lazy init loads file-backed defaults once and keeps built-in fallbacks."
  (let ((path (temp-agent-defaults-path)))
    (write-agent-defaults-file
     path
     "{\"spark\":{\"provider\":\"openai-codex\"}}")
    (with-agent-defaults-path-override (path)
       (let ((initial-registry rplaca::*agent-defaults-registry*))
         (is (null initial-registry))
         (is (eq :openai-codex (rplaca::agent-default "spark")))
         (is (not (null rplaca::*agent-defaults-registry*)))
         (is (eq rplaca::*default-provider*
                 (rplaca::agent-default "missing-agent")))))))

(test set-agent-default-persists-across-reload
  "set-agent-default persists provider and model across a registry reload."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (with-agent-defaults-path-override (path)
      (rplaca::set-agent-default "spark" :openai-codex :model "gpt-5.3-codex")
      (setf rplaca::*agent-defaults-registry* nil)
      (is (eq :openai-codex (rplaca::agent-default "spark")))
       (multiple-value-bind (provider model think-level)
           (rplaca::resolve-buffer-provider-and-model buf)
         (is (eq :openai-codex provider))
         (is (string= "gpt-5.3-codex" model))
         (is (null think-level))))))

(test clear-buffer-overrides-restores-agent-default-resolution
  "Clearing buffer overrides returns resolution to agent defaults."
  (let ((path (temp-agent-defaults-path))
        (buf (make-buffer "test" :agent-name "spark")))
    (with-agent-defaults-path-override (path)
      (set-agent-default "spark" :openai-codex :model "gpt-5.3-codex")
      (set-buffer-provider-override buf :zai)
      (set-buffer-model-override buf "glm-5")
      (set-buffer-think-level-override buf "high")
      (clear-buffer-routing-overrides buf)
      (multiple-value-bind (provider model think-level)
          (resolve-buffer-provider-and-model buf)
        (is (eq :openai-codex provider))
        (is (string= "gpt-5.3-codex" model))
        (is (null think-level))
        (is (null (buffer-think-level-override buf)))))))

(test canonicalize-message-content-wraps-plain-text
  "Plain text content is normalized to one canonical text block."
  (is (equal '(((:type . "text")
                (:text . "hello")))
             (rplaca::canonicalize-message-content "user" "hello"))))

(test canonicalize-message-content-accepts-assistant-tool-use
  "Assistant tool_use blocks are accepted as canonical content."
  (is (equal '(((:type . "tool_use")
                (:id . "toolu_123")
                (:name . "read_file")
                (:input . ((:path . "/tmp/example.txt")))))
             (rplaca::canonicalize-message-content
              "assistant"
              '(((:type . "tool_use")
                 (:id . "toolu_123")
                 (:name . "read_file")
                 (:input . ((:path . "/tmp/example.txt")))))))))

(test canonicalize-message-content-accepts-user-tool-result
  "User tool_result blocks are accepted as canonical content."
  (is (equal '(((:type . "tool_result")
                (:tool--use--id . "toolu_123")
                (:content . "done")))
             (rplaca::canonicalize-message-content
              "user"
              '(((:type . "tool_result")
                 (:tool-use-id . "toolu_123")
                 (:content . "done")))))))

(test canonical-tool-result-json-uses-tool-use-id-underscore-key
  "Canonical tool_result blocks encode tool_use_id (underscore), not camelCase."
  (let* ((block (first
                 (rplaca::canonicalize-message-content
                  "user"
                  '(((:type . "tool_result")
                     (:tool-use-id . "toolu_123")
                     (:content . "done"))))))
         (json (rplaca::api-json-encode block)))
    (is (search "\"tool_use_id\"" json))
    (is (not (search "\"toolUseId\"" json)))))

(test canonicalize-message-content-rejects-invalid-role-block-pairings
  "Invalid role/block pairings signal an error."
  (signals error
    (rplaca::canonicalize-message-content
     "user"
     '(((:type . "tool_use")
        (:id . "toolu_123")
        (:name . "read_file")
        (:input . ((:path . "/tmp/example.txt")))))))
  (signals error
    (rplaca::canonicalize-message-content
     "assistant"
     '(((:type . "tool_result")
         (:tool--use--id . "toolu_123")
         (:content . "done"))))))

(test canonicalize-message-content-rejects-invalid-role-for-plain-text
  "Plain string content rejects roles that cannot carry text blocks."
  (signals error
    (rplaca::canonicalize-message-content "system" "hello")))

(test provider-request-rejects-anthropic-provider
  "Anthropic is no longer a dispatchable provider."
  (signals error
    (rplaca::provider-request
     :anthropic
     '(((:role . "user") (:content . #())))
     :model "removed"))
  (signals error
    (rplaca::provider-request-streaming
     :anthropic
     '(((:role . "user") (:content . #())))
     (lambda (state) (declare (ignore state)))
     :model "removed")))

(test provider-request-dispatches-openai-codex-adapter
  "OpenAI Codex requests use the Codex adapter and preserve model + reasoning."
  (let ((captured-model nil)
        (captured-reasoning nil))
    (with-function-override (rplaca::openai-codex-request
                             (messages &key model max-tokens tools reasoning-effort system-prompt)
                             (declare (ignore messages max-tokens tools system-prompt))
                             (setf captured-model model
                                   captured-reasoning reasoning-effort)
                              '((:stop--reason . "stop")
                                (:content . #())))
      (is (equal '((:stop--reason . "stop")
                   (:content . #()))
                 (rplaca::provider-request
                  :openai-codex
                  '(((:role . "user") (:content . #())))
                  :model "gpt-5.3-codex"
                  :reasoning-effort "high")))
      (is (string= "gpt-5.3-codex" captured-model))
      (is (string= "high" captured-reasoning)))))

(test provider-request-streaming-dispatches-by-provider
  "Streaming adapter dispatch follows the selected provider, model, and reasoning."
  (let ((openai-model nil)
        (openai-reasoning nil))
    (with-function-override (rplaca::openai-codex-request-streaming
                              (messages callback &key model max-tokens tools reasoning-effort system-prompt)
                              (declare (ignore messages callback max-tokens tools system-prompt))
                              (setf openai-model model
                                    openai-reasoning reasoning-effort)
                              :openai-stream)
      (is (eq :openai-stream
              (rplaca::provider-request-streaming
               :openai-codex
               '(((:role . "user") (:content . #())))
               (lambda (state) (declare (ignore state)))
               :model "codex-stream"
               :reasoning-effort "medium")))
      (is (string= "codex-stream" openai-model))
      (is (string= "medium" openai-reasoning)))))

(test start-streaming-response-uses-resolved-provider-and-model
  "Live streaming resolves provider/model/think first and passes them to the adapter."
  (let ((buf (make-buffer "routing-test" :agent-name "spark"))
        (captured-provider nil)
        (captured-model nil)
        (captured-reasoning nil)
        (captured-system-prompt nil))
    (with-function-override (rplaca::resolve-buffer-provider-and-model (buffer)
                              (declare (ignore buffer))
                              (values :openai-codex "gpt-5.3-codex" "high"))
      (with-function-override (rplaca::tool-definitions-for-api (&key buffer agent-name)
                                (declare (ignore buffer agent-name))
                                #())
        (with-function-override (rplaca::build-conversation-messages (buffer)
                                  (declare (ignore buffer))
                                  '(((:role . "user") (:content . #()))))
          (with-function-override (rplaca::build-agent-system-prompt (agent-name &key buffer)
                                    (declare (ignore buffer))
                                    (format nil "prompt for ~A" agent-name))
          (with-function-override (rplaca::provider-request-streaming
                                    (provider messages callback &key model max-tokens tools reasoning-effort system-prompt)
                                    (declare (ignore messages callback max-tokens tools))
                                    (setf captured-provider provider
                                          captured-model model
                                          captured-reasoning reasoning-effort
                                          captured-system-prompt system-prompt)
                                    (rplaca::make-stream-state))
            (rplaca::start-streaming-response buf)
            (is (eq :openai-codex captured-provider))
            (is (string= "gpt-5.3-codex" captured-model))
            (is (string= "high" captured-reasoning))
            (is (string= "prompt for spark" captured-system-prompt))
            (let ((metadata (message-metadata (buffer-streaming-message buf))))
              (is (string= "spark"
                           (rplaca::message-metadata-value
                            metadata :agent)))
              (is (eq :openai-codex
                      (rplaca::message-metadata-value metadata :provider)))
              (is (string= "gpt-5.3-codex"
                           (rplaca::message-metadata-value
                            metadata :model)))))))))))

(test start-streaming-response-surfaces-resolver-errors-in-buffer
  "Resolver failures are caught and rendered into the buffer as agent errors."
  (let ((buf (make-buffer "routing-error-test" :agent-name "spark")))
    (with-function-override (rplaca::resolve-buffer-provider-and-model (buffer)
                              (declare (ignore buffer))
                              (error 'simple-error :format-control "resolver exploded"))
      (finishes (rplaca::start-streaming-response buf))
      (is (eq :error (buffer-status buf)))
      (is (search "resolver exploded"
                  (message-text (buffer-first-message buf)))))))

(test update-streaming-response-does-not-double-count-mirrored-text-block
  "OpenAI-compatible streams mirror partial text into content blocks; render it once."
  (let* ((buf (make-buffer "stream-openai" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf ""))
         (state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-text state) "hello"
            (rplaca::stream-state-content-blocks state)
            (list (rplaca::canonical-text-block "hello"))))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-true (rplaca::update-streaming-response buf))
    (is (string= "hello" (message-text msg)))
    (is (= 2 (buffer-message-count buf)))))

(test update-streaming-response-appends-accumulator-after-completed-blocks
  "Streaming state displays completed blocks plus the current text accumulator."
  (let* ((buf (make-buffer "stream-accumulator" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf ""))
         (state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-text state) "world"
            (rplaca::stream-state-content-blocks state)
            (list (rplaca::canonical-text-block "Hello, "))))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-true (rplaca::update-streaming-response buf))
    (is (string= "Hello, world" (message-text msg)))
    (is (= 2 (buffer-message-count buf)))))

(test update-streaming-response-finalizes-single-placeholder-message
  "Completing a stream updates the existing placeholder instead of inserting another agent message."
  (let* ((buf (make-buffer "stream-final" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf "partial"))
         (state (rplaca::make-stream-state)))
    (rplaca::put-message-metadata
     msg
     :agent "agent"
     :provider :zai
     :model "glm-5"
     :think-level nil)
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-text state) "final answer"
            (rplaca::stream-state-content-blocks state)
            (list (rplaca::canonical-text-block "final answer"))
            (rplaca::stream-state-stop-reason state) "end_turn"
            (rplaca::stream-state-usage state)
            '(:input-tokens 2006
              :output-tokens 300
              :total-tokens 2306
              :cached-input-tokens 1920
              :uncached-input-tokens 86
              :cache-hit-rate 0.9571286)
            (rplaca::stream-state-done-p state) t))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-false (rplaca::update-streaming-response buf))
    (is (string= "final answer" (message-text msg)))
    (let ((metadata (message-metadata msg)))
      (is (eq :zai (rplaca::message-metadata-value metadata :provider)))
      (is (string= "end_turn"
                   (rplaca::message-metadata-value metadata :stop-reason)))
      (is (= 1 (rplaca::message-metadata-value
                metadata :content-block-count)))
      (is (= 0 (rplaca::message-metadata-value
                metadata :tool-call-count)))
      (is (= 1920 (rplaca::message-metadata-value
                   metadata :cached-input-tokens)))
      (is (= 86 (rplaca::message-metadata-value
                 metadata :uncached-input-tokens))))
    (is (= 2 (buffer-message-count buf)))
    (is (null (buffer-pending-stream buf)))
    (is (null (buffer-streaming-message buf)))
    (is (eq :idle (buffer-status buf)))))

(test update-streaming-response-records-completed-placeholder-once
  "Streaming placeholders are written to transcripts only after finalization."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-final"))
         (session (load-or-create-session "stream-final"))
         (buf (make-buffer "stream-final"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-content-blocks state)
            (list (rplaca::canonical-text-block "final answer"))
            (rplaca::stream-state-stop-reason state) "end_turn"
            (rplaca::stream-state-done-p state) t))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is (= 1 (length (session-current-events session))))
    (is-false (rplaca::update-streaming-response buf))
    (let* ((events (session-current-events session))
           (message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            events))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "AGENT" (event-value event :sender)))
      (is (string= "final answer" (event-value event :text)))
      (is (vectorp (event-value event :raw-content))))))

(test update-streaming-response-records-streaming-error
  "Streaming errors are recorded as durable transcript messages."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-error"))
         (session (load-or-create-session "stream-error"))
         (buf (make-buffer "stream-error"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-error-p state) "boom"))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :streaming)
    (is-false (rplaca::update-streaming-response buf))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "AGENT" (event-value event :sender)))
      (is (search "Streaming error"
                  (event-value event :text))))))

(test stop-streaming-response-records-partial-message
  "Stopping a stream preserves arrived text but does not feed the stop marker to providers."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-stop"))
         (session (load-or-create-session "stream-stop"))
         (buf (make-buffer "stream-stop"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (rplaca::make-stream-state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-text state) "partial answer"
            (rplaca::stream-state-content-blocks state)
            (list (rplaca::canonical-text-block "partial answer"))))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :thinking)
    (is-true (rplaca::stop-streaming-response buf))
    (is (null (buffer-pending-stream buf)))
    (is (null (buffer-streaming-message buf)))
    (is (eq :cancelling (buffer-status buf)))
    (is-false (search "[Stopped by user]" (message-text msg)))
    (is-true
     (rplaca::deliver-buffer-runtime-stopped-notification buf))
    (is (eq :idle (buffer-status buf)))
    (is (search "[Stopped by user]" (message-text msg)))
    (is (string= "partial answer"
                 (rplaca::content-text-blocks (message-raw-content msg))))
    (is (not (search "[Stopped by user]"
                     (rplaca::content-text-blocks
                      (message-raw-content msg)))))
    (is (string= "cancelled"
                 (rplaca::message-metadata-value
                  (message-metadata msg)
                  :stop-reason)))
    (is-true (bt:with-lock-held ((rplaca::stream-state-lock state))
               (rplaca::stream-state-cancelled-p state)))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "AGENT" (event-value event :sender)))
      (is (search "[Stopped by user]" (event-value event :text)))
      (is (vectorp (event-value event :raw-content))))))

(test stop-streaming-response-records-system-message-when-empty
  "Stopping before any assistant text creates a display-only system message."
  (let* ((*sessions-dir* (temp-session-test-directory "stream-stop-empty"))
         (session (load-or-create-session "stream-stop-empty"))
         (buf (make-buffer "stream-stop-empty"
                           :agent-name "agent"
                           :session session))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (rplaca::make-stream-state)))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :thinking)
    (is-true (rplaca::stop-streaming-response buf))
    (is (eq :agent (message-sender msg)))
    (is-true
     (rplaca::deliver-buffer-runtime-stopped-notification buf))
    (is (eq :system (message-sender msg)))
    (is (null (message-raw-content msg)))
    (is (string= "[Response stopped by user]" (message-text msg)))
    (is (null (rplaca::build-conversation-messages buf)))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "SYSTEM" (event-value event :sender)))
      (is (string= "[Response stopped by user]"
                   (event-value event :text))))))

(test terminal-stream-retains-buffer-owner-until-reader-cleanup-settles
  "DONE publication cannot expose idle state or live reload before reader exit."
  (let* ((buf (make-buffer "stream-terminal-cleanup-barrier"
                           :agent-name "agent"
                           :session-persistence-mode :ephemeral))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (callback-entered
           (bt:make-semaphore :name "stream-terminal-callback-entered"))
         (callback-release
           (bt:make-semaphore :name "stream-terminal-callback-release"))
         (settled-wake
           (bt:make-semaphore :name "stream-terminal-settled-wake"))
         (applied
           (bt:make-semaphore :name "stream-terminal-applied"))
         (state
           (rplaca::make-stream-state
            :callback
            (lambda (ignored-state)
              (declare (ignore ignored-state))
              (bt:signal-semaphore callback-entered)
              (bt:wait-on-semaphore callback-release :timeout 5))))
         (pump nil)
         (automatic-result :not-run)
         (preflight-called-p nil)
         (rplaca::*runtime-settlement-notify-function*
           (lambda (changed-buffer reason)
             (declare (ignore changed-buffer))
             (when (eq reason :stream-settled)
               (bt:signal-semaphore settled-wake)))))
    (with-safe-reload-quiescent-process (buf)
      (unwind-protect
           (progn
             (setf (buffer-pending-stream buf) state
                   (buffer-streaming-message buf) msg
                   (buffer-status buf) :thinking)
             (rplaca::start-stream-state-reader-worker
              state
              (rplaca::stream-state-callback state)
              "test-terminal-stream-cleanup-barrier"
              (lambda (worker-state)
                (bt:with-lock-held
                    ((rplaca::stream-state-lock worker-state))
                  (setf (rplaca::stream-state-content-blocks worker-state)
                        (list (rplaca::canonical-text-block "settled"))
                        (rplaca::stream-state-stop-reason worker-state)
                        "end_turn"))))
             (is-true
              (bt:wait-on-semaphore callback-entered :timeout 2))
             (is-true
              (bt:with-lock-held ((rplaca::stream-state-lock state))
                (rplaca::stream-state-done-p state)))
             (setf pump
                   (bt:make-thread
                    (lambda ()
                      (when (bt:wait-on-semaphore settled-wake :timeout 5)
                        (setf automatic-result
                              (rplaca::update-streaming-response buf)))
                      (bt:signal-semaphore applied))
                    :name "simulated-clim-stream-event-pump"))
             ;; The first update installs the joiner and returns immediately.
             (is-true (rplaca::update-streaming-response buf))
             (is (eq state (buffer-pending-stream buf)))
             (is (eq :thinking (buffer-status buf)))
             (is-true
              (bt:thread-alive-p
               (rplaca::stream-state-reader-thread state)))
             (with-safe-reload-test-runners
                 ((lambda (&key timeout source-root)
                    (declare (ignore timeout source-root))
                    (setf preflight-called-p t)
                    (safe-reload-test-result :ok "Must not run."))
                  (lambda (&key buffer source-root)
                    (declare (ignore buffer source-root))
                    (safe-reload-test-result :ok "Must not run.")))
               (let ((result (rplaca:rplaca-safe-reload
                              :buffer buf :notify-p nil)))
                 (is (eq :refused
                         (rplaca::safe-reload-result-status result)))
                 (is-false preflight-called-p)))
             (bt:signal-semaphore callback-release)
             ;; The joiner supplies the only retry wake.
             (is-true (bt:wait-on-semaphore applied :timeout 3))
             (bt:join-thread pump)
             (setf pump nil)
             (is-false automatic-result)
             (is (null (buffer-pending-stream buf)))
             (is (null (rplaca::stream-state-reader-thread state)))
             (is (null
                  (rplaca::stream-state-reader-settlement-thread state)))
             (is (eq :idle (buffer-status buf)))
             (is (string= "settled" (message-text msg))))
        (bt:signal-semaphore callback-release)
        (bt:signal-semaphore settled-wake)
        (when pump
          (bt:join-thread pump))
        (rplaca::settle-stream-state-reader state)))))

(test stopped-stream-retains-buffer-owner-until-reader-cleanup-settles
  "Stop transfers ownership to teardown without admitting work before exit."
  (let* ((buf (make-buffer "stream-stop-cleanup-barrier"
                           :agent-name "agent"
                           :session-persistence-mode :ephemeral))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (reader-entered
           (bt:make-semaphore :name "stream-stop-reader-entered"))
         (cleanup-entered
           (bt:make-semaphore :name "stream-stop-cleanup-entered"))
         (cleanup-release
           (bt:make-semaphore :name "stream-stop-cleanup-release"))
         (state (rplaca::make-stream-state))
         (preflight-called-p nil))
    (with-safe-reload-quiescent-process (buf)
      (unwind-protect
           (progn
             (setf (buffer-pending-stream buf) state
                   (buffer-streaming-message buf) msg
                   (buffer-status buf) :thinking)
             (rplaca::start-stream-state-reader-worker
              state nil "test-stopped-stream-cleanup-barrier"
              (lambda (worker-state)
                (bt:signal-semaphore reader-entered)
                (loop :until
                        (rplaca::stream-state-cancel-requested-p-safe
                         worker-state)
                      :do (sleep 0.002))
                (bt:signal-semaphore cleanup-entered)
                (bt:wait-on-semaphore cleanup-release :timeout 5)))
             (is-true (bt:wait-on-semaphore reader-entered :timeout 2))
             (is-true (rplaca::stop-streaming-response buf))
             (is-true (bt:wait-on-semaphore cleanup-entered :timeout 2))
             ;; Stop detaches the public owner atomically; the teardown retains
             ;; the exact stream until its reader exits.
             (is (null (buffer-pending-stream buf)))
             (is (null (buffer-streaming-message buf)))
             (is (eq :cancelling (buffer-status buf)))
             (is-true (rplaca::buffer-runtime-stopping-p buf))
             (is-true (rplaca::buffer-runtime-teardown buf))
             (is-true
              (bt:thread-alive-p
               (rplaca::stream-state-reader-thread state)))
             (is-false (search "stopped by user"
                               (message-text msg) :test #'char-equal))
             (with-safe-reload-test-runners
                 ((lambda (&key timeout source-root)
                    (declare (ignore timeout source-root))
                    (setf preflight-called-p t)
                    (safe-reload-test-result :ok "Must not run."))
                  (lambda (&key buffer source-root)
                    (declare (ignore buffer source-root))
                    (safe-reload-test-result :ok "Must not run.")))
               (let ((result (rplaca:rplaca-safe-reload
                              :buffer buf :notify-p nil)))
                 (is (eq :refused
                         (rplaca::safe-reload-result-status result)))
                 (is-false preflight-called-p)))
             ;; No frame polling or join is needed after ownership transfers.
             (is-false (rplaca::update-streaming-response buf))
             (bt:signal-semaphore cleanup-release)
             (loop :repeat 400
                   :until
                   (let ((teardown
                           (rplaca::buffer-runtime-teardown buf)))
                     (and teardown
                          (rplaca::buffer-runtime-teardown-frame-delivery-p
                           teardown)))
                   :do (sleep 0.005))
             (is (null (buffer-pending-stream buf)))
             (is (null (rplaca::stream-state-reader-thread state)))
             (is (null
                  (rplaca::stream-state-reader-settlement-thread state)))
             (is (eq :cancelling (buffer-status buf)))
             (is-false (search "stopped by user"
                               (message-text msg) :test #'char-equal))
             (is-true
              (rplaca::deliver-buffer-runtime-stopped-notification buf))
             (is (eq :idle (buffer-status buf)))
             (is (search "stopped by user"
                         (message-text msg) :test #'char-equal)))
        (bt:signal-semaphore cleanup-release)
        (rplaca::settle-stream-state-reader state)))))

(test handle-key-event-escape-stops-active-stream
  "Esc dispatches to the stop command while a stream is active."
  (let* ((buf (make-buffer "stream-stop-key" :agent-name "agent"))
         (msg (buffer-insert-agent-message buf "" :record-p nil))
         (state (rplaca::make-stream-state)))
    (rplaca::init-default-keymap)
    (setf (buffer-keymap buf) *default-keymap*)
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (setf (rplaca::stream-state-text state) "partial"))
    (setf (buffer-pending-stream buf) state
          (buffer-streaming-message buf) msg
          (buffer-status buf) :thinking)
    (is (eq :redraw
            (rplaca::handle-key-event buf #\Esc)))
    (is (null (buffer-pending-stream buf)))
    (is-false (search "[Stopped by user]" (message-text msg)))
    (is-true
     (rplaca::deliver-buffer-runtime-stopped-notification buf))
    (is (search "[Stopped by user]" (message-text msg)))))

(test insert-tool-results-message-records-raw-content
  "Tool result transcript events preserve canonical raw content."
  (let* ((*sessions-dir* (temp-session-test-directory "tool-result"))
         (session (load-or-create-session "tool-result"))
         (buf (make-buffer "tool-result"
                           :agent-name "agent"
                           :session session)))
    (rplaca::insert-tool-results-message
     buf
     (list `((:result . "done")
             (:display . "[read_file] done")
             (:tool-id . "toolu_1"))))
    (let* ((message-events
             (remove-if-not (lambda (event)
                              (string= "message"
                                       (event-value event :event)))
                            (session-current-events session)))
           (event (first message-events)))
      (is (= 1 (length message-events)))
      (is (string= "TOOL-RESULT" (event-value event :sender)))
      (is (vectorp (event-value event :raw-content))))))

(test normalize-openai-token-usage-responses-shape
  "OpenAI Responses usage is normalized into prompt-cache telemetry."
  (let ((usage (rplaca::normalize-openai-token-usage
                '((:input--tokens . 2006)
                  (:output--tokens . 300)
                  (:total--tokens . 2306)
                  (:input--tokens--details . ((:cached--tokens . 1920)))))))
    (is (= 2006 (getf usage :input-tokens)))
    (is (= 300 (getf usage :output-tokens)))
    (is (= 2306 (getf usage :total-tokens)))
    (is (= 1920 (getf usage :cached-input-tokens)))
    (is (= 86 (getf usage :uncached-input-tokens)))
    (is (< (abs (- (getf usage :cache-hit-rate)
                    (/ 1920.0 2006)))
           0.0001))
    (is (string= "tokens: input=2006 cached=1920 uncached=86 output=300 total=2306 cache-hit=95.7%"
                 (rplaca::format-token-usage-summary usage)))))

(test normalize-openai-token-usage-chat-shape
  "Chat-style prompt token usage is normalized into the same telemetry shape."
  (let ((usage (rplaca::normalize-openai-token-usage
                '((:prompt--tokens . 1000)
                  (:completion--tokens . 50)
                  (:total--tokens . 1050)
                  (:prompt--tokens--details . ((:cached--tokens . 768)))))))
    (is (= 1000 (getf usage :input-tokens)))
    (is (= 50 (getf usage :output-tokens)))
    (is (= 1050 (getf usage :total-tokens)))
    (is (= 768 (getf usage :cached-input-tokens)))
    (is (= 232 (getf usage :uncached-input-tokens)))
    (is (< (abs (- (getf usage :cache-hit-rate) 0.768))
           0.0001))))

(test openai-codex-request-normalizes-response-shape
  "OpenAI Codex non-streaming normalizes Responses output items."
  (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                            (declare (ignore refresh-if-needed))
                            '(:source :token-override
                              :mode :api-key
                              :token "openai-token"
                              :base-url "https://api.openai.com/v1"
                              :refreshable-p nil))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values
                               "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"hi from codex\"}]},{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/codex.txt\\\"}\"}],\"usage\":{\"input_tokens\":2006,\"output_tokens\":300,\"total_tokens\":2306,\"input_tokens_details\":{\"cached_tokens\":1920}}}"
                               200))
      (let ((response (rplaca::openai-codex-request '() :model "gpt-5.3-codex")))
        (is (string= "tool_use" (rplaca::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "hi from codex"))
                     ((:type . "tool_use")
                      (:id . "call_1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/codex.txt")))))
                   (rplaca::response-content response)))
        (let ((usage (rplaca::response-usage response)))
          (is (= 2006 (getf usage :input-tokens)))
          (is (= 1920 (getf usage :cached-input-tokens)))
          (is (= 86 (getf usage :uncached-input-tokens))))))))

(test openai-codex-request-normalizes-reasoning-summary
  "OpenAI Codex non-streaming preserves Responses reasoning summaries."
  (let* ((response '((:output . #(((:type . "reasoning")
                                  (:summary . #(((:type . "summary_text")
                                                 (:text . "provider summary")))))
                                 ((:type . "message")
                                  (:role . "assistant")
                                  (:content . #(((:type . "output_text")
                                                 (:text . "final")))))))))
         (canonical (rplaca::responses-api-response->canonical-response
                     response))
         (content (rplaca::response-content canonical)))
    (is (string= "end_turn" (rplaca::response-stop-reason canonical)))
    (is (string= "final" (rplaca::content-text-blocks content)))
    (is (equal '("provider summary")
               (rplaca::content-reasoning-blocks content)))))

(test openai-codex-request-normalizes-reasoning-content
  "OpenAI Codex non-streaming preserves Responses reasoning content parts."
  (let* ((response '((:output . #(((:type . "reasoning")
                                  (:content . #(((:type . "reasoning_text")
                                                 (:text . "provider reasoning")))))
                                 ((:type . "message")
                                  (:role . "assistant")
                                  (:content . #(((:type . "output_text")
                                                 (:text . "final")))))))))
         (canonical (rplaca::responses-api-response->canonical-response
                     response))
         (content (rplaca::response-content canonical)))
    (is (string= "end_turn" (rplaca::response-stop-reason canonical)))
    (is (string= "final" (rplaca::content-text-blocks content)))
    (is (equal '("provider reasoning")
               (rplaca::content-reasoning-blocks content)))))

(test openai-codex-request-uses-responses-api-and-chatgpt-headers
  "OpenAI Codex requests target /responses and use instructions + ChatGPT headers."
  (let* ((captured-request-body nil)
         (captured-url nil)
         (captured-headers nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (auth '(:source :codex-chatgpt
                 :mode :chatgpt
                 :token "chatgpt-token"
                 :base-url "https://chatgpt.com/backend-api/codex"
                 :account-id "acct_123"
                 :refreshable-p t)))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-url (first args)
                                    captured-request-body (getf (rest args) :content)
                                    captured-headers (getf (rest args) :additional-headers))
                              (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                      200))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                auth)
        (with-function-override (rplaca::build-system-prompt ()
                                  "boot prompt")
          (rplaca::openai-codex-request messages :model "gpt-5.3-codex"))))
    (let* ((body (rplaca::api-json-decode captured-request-body))
           (input-items (coerce (cdr (assoc :input body)) 'list))
           (message-item (first input-items))
           (content-item (first (coerce (cdr (assoc :content message-item)) 'list))))
      (is (string= "https://chatgpt.com/backend-api/codex/responses" captured-url))
      (is (string= "Bearer chatgpt-token"
                   (cdr (assoc "Authorization" captured-headers :test #'string=))))
      (is (string= "acct_123"
                   (cdr (assoc "ChatGPT-Account-ID" captured-headers :test #'string=))))
      (is (search "\"store\":false" captured-request-body))
      (is (search "\"stream\":false" captured-request-body))
      (is (not (search "max_output_tokens" captured-request-body)))
      (is (< (search "\"tools\"" captured-request-body)
             (search "\"input\"" captured-request-body)))
      (let ((reasoning (cdr (assoc :reasoning body))))
        (is (string= "detailed" (cdr (assoc :summary reasoning))))
        (is (null (assoc :effort reasoning))))
      (is (string= "boot prompt" (cdr (assoc :instructions body))))
      (is (not (assoc :messages body)))
      (is (string= "message" (cdr (assoc :type message-item))))
      (is (string= "user" (cdr (assoc :role message-item))))
      (is (string= "input_text" (cdr (assoc :type content-item))))
      (is (string= "hello" (cdr (assoc :text content-item)))))))

(test openai-codex-request-includes-reasoning-effort
  "OpenAI Codex requests include reasoning.effort when set."
  (let ((captured-request-body nil))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                      200))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (rplaca::openai-codex-request '()
                                        :model "gpt-5.4"
                                        :reasoning-effort "xhigh")))
    (let* ((body (rplaca::api-json-decode captured-request-body))
           (reasoning (cdr (assoc :reasoning body)))
           (effort (cdr (assoc :effort reasoning))))
      (is (string= "xhigh" effort))
      (is (string= "detailed" (cdr (assoc :summary reasoning)))))))

(test openai-codex-request-includes-prompt-cache-controls
  "OpenAI Codex requests include prompt cache routing controls when bound."
  (let ((captured-request-body nil))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body
                                    (getf (rest args) :content))
                              (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                      200))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((rplaca::*openai-codex-prompt-cache-key*
                "rplaca-agent-cache-probe")
              (rplaca::*openai-codex-prompt-cache-retention* "24h"))
          (rplaca::openai-codex-request '()
                                          :model "gpt-5.4"))))
    (let ((body (rplaca::api-json-decode captured-request-body)))
      (is (string= "rplaca-agent-cache-probe"
                   (cdr (assoc :prompt--cache--key body))))
      (is (string= "24h"
                   (cdr (assoc :prompt--cache--retention body)))))))

(test openai-codex-request-retries-on-401-after-refresh
  "OpenAI Codex retries once after a 401 when ChatGPT auth is refreshable."
  (let ((captured-authz nil)
        (calls 0)
        (refresh-called-p nil))
    (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                              (declare (ignore refresh-if-needed))
                              '(:source :codex-chatgpt
                                :mode :chatgpt
                                :token "expired-token"
                                :base-url "https://chatgpt.com/backend-api/codex"
                                :account-id "acct_123"
                                :refreshable-p t))
      (with-function-override (rplaca::refresh-openai-codex-auth-descriptor ()
                                (setf refresh-called-p t)
                                '(:source :codex-chatgpt
                                  :mode :chatgpt
                                  :token "fresh-token"
                                  :base-url "https://chatgpt.com/backend-api/codex"
                                  :account-id "acct_123"
                                  :refreshable-p t))
        (with-function-override (drakma:http-request (url &rest args)
                                  (declare (ignore url))
                                  (push (cdr (assoc "Authorization"
                                                    (getf args :additional-headers)
                                                    :test #'string=))
                                        captured-authz)
                                  (incf calls)
                                  (if (= calls 1)
                                      (values "unauthorized" 401)
                                      (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                              200)))
          (let ((response (rplaca::openai-codex-request '() :model "gpt-5.3-codex")))
            (is (string= "end_turn" (rplaca::response-stop-reason response)))
            (is-true refresh-called-p)
            (is (= 2 calls))
            (is (equal '("Bearer expired-token" "Bearer fresh-token")
                       (nreverse captured-authz)))))))))

(test openai-codex-streaming-refresh-closes-rejected-response-body
  "A refreshable streaming 401 retires its body before opening the retry."
  (let* ((rejected-body
           (make-instance 'controlled-character-input-stream
                          :contents "unauthorized"))
         (accepted-body
           (make-instance 'controlled-character-input-stream
                          :contents "data: [DONE]"))
         (expired-auth
           '(:source :codex-chatgpt
             :mode :chatgpt
             :token "expired-token"
             :base-url "https://chatgpt.com/backend-api/codex"
             :account-id "acct_123"
             :refreshable-p t))
         (fresh-auth
           '(:source :codex-chatgpt
             :mode :chatgpt
             :token "fresh-token"
             :base-url "https://chatgpt.com/backend-api/codex"
             :account-id "acct_123"
             :refreshable-p t))
         (calls 0))
    (unwind-protect
         (with-function-override
             (rplaca::refresh-openai-codex-auth-descriptor () fresh-auth)
           (with-function-override (drakma:http-request (&rest args)
                                     (declare (ignore args))
                                     (incf calls)
                                     (if (= calls 1)
                                         (values rejected-body 401 nil)
                                         (values accepted-body 200 nil)))
             (multiple-value-bind (body status ignored effective-auth)
                 (rplaca::openai-codex-http-request
                  expired-auth "{}" :stream t)
               (declare (ignore ignored))
               (is (eq accepted-body body))
               (is (= 200 status))
               (is (eq fresh-auth effective-auth))
               (is (= 2 calls))
               (is (= 1 (controlled-stream-close-count rejected-body)))
               (is (= 0 (controlled-stream-close-count accepted-body))))))
      (unless (controlled-stream-closed-p rejected-body)
        (close rejected-body :abort t))
      (unless (controlled-stream-closed-p accepted-body)
        (close accepted-body :abort t)))))

(test openai-codex-request-retries-transient-503-with-backoff
  "OpenAI Codex retries transient 503 responses before failing the request."
  (let ((calls 0)
        (sleeps nil))
    (let ((rplaca::*provider-http-max-retries* 3)
          (rplaca::*provider-http-initial-backoff-seconds* 0.5)
          (rplaca::*provider-http-backoff-multiplier* 2.0)
          (rplaca::*provider-http-max-backoff-seconds* 8.0)
          (rplaca::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (case calls
                                    ((1 2)
                                     (values "service unavailable" 503 nil))
                                    (otherwise
                                     (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                             200))))
          (let ((response (rplaca::openai-codex-request '()
                                                          :model "gpt-5.3-codex")))
            (is (string= "end_turn"
                         (rplaca::response-stop-reason response)))))))
    (is (= 3 calls))
    (is (equalp '(0.5 1.0) (nreverse sleeps)))))

(test openai-codex-request-honors-retry-after-header
  "Retry-After controls the backoff delay for transient provider responses."
  (let ((calls 0)
        (sleeps nil))
    (let ((rplaca::*provider-http-max-retries* 2)
          (rplaca::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (if (= calls 1)
                                      (values "service unavailable"
                                              503
                                              '(("Retry-After" . "2")))
                                      (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                              200)))
          (let ((response (rplaca::openai-codex-request '()
                                                          :model "gpt-5.3-codex")))
            (is (string= "end_turn"
                         (rplaca::response-stop-reason response)))))))
    (is (= 2 calls))
    (is (equal '(2) (nreverse sleeps)))))

(test provider-http-retry-after-is-capped
  "A provider cannot force a retry sleep beyond the configured maximum."
  (let ((calls 0)
        (sleeps nil))
    (let ((rplaca::*provider-http-max-retries* 1)
          (rplaca::*provider-http-max-backoff-seconds* 1.25)
          (rplaca::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (multiple-value-bind (body status)
          (rplaca::provider-http-request-with-retries
           "retry-after-cap-test"
           (lambda ()
             (incf calls)
             (if (= calls 1)
                 (values "busy" 503 '(("Retry-After" . "600")))
                 (values "ok" 200 nil))))
        (is (string= "ok" body))
        (is (= 200 status)))
      (is (= 2 calls))
      (is (equal '(1.25) (nreverse sleeps))))))

(test provider-http-retry-backoff-is-cooperatively-cancellable
  "Cancellation wakes the default backoff sleeper without another request."
  (let ((entered (bt:make-semaphore :name "provider-backoff-entered"))
        (cancel-p nil)
        (calls 0)
        (result :unset)
        (rplaca::*provider-http-max-retries* 3)
        (rplaca::*provider-http-initial-backoff-seconds* 5.0)
        (rplaca::*provider-http-max-backoff-seconds* 5.0)
        (rplaca::*provider-http-cancel-poll-seconds* 0.01)
        (rplaca::*provider-http-sleep-function* #'sleep))
    (let ((worker
            (bt:make-thread
             (lambda ()
               (setf result
                     (multiple-value-list
                      (rplaca::provider-http-request-with-retries
                       "cancel-backoff-test"
                       (lambda ()
                         (incf calls)
                         (bt:signal-semaphore entered)
                         (error "transient connect failure"))
                       :cancel-p (lambda () cancel-p)))))
             :name "provider-backoff-cancel-test")))
      (is-true (bt:wait-on-semaphore entered :timeout 2.0))
      (setf cancel-p t)
      (bt:join-thread worker)
      (is (= 1 calls))
      (is (equal '(nil nil nil) result)))))

(test openai-codex-request-does-not-retry-client-errors
  "Non-transient HTTP errors are returned to the provider-specific handler."
  (let ((calls 0)
        (sleeps nil))
    (let ((rplaca::*provider-http-max-retries* 3)
          (rplaca::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (values "bad request" 400 nil))
          (signals error
            (rplaca::openai-codex-request '()
                                            :model "gpt-5.3-codex")))))
    (is (= 1 calls))
    (is (null sleeps))))

(test openai-codex-request-retries-connection-errors
  "Connection-level provider failures are retried before surfacing."
  (let ((calls 0)
        (sleeps nil))
    (let ((rplaca::*provider-http-max-retries* 2)
          (rplaca::*provider-http-initial-backoff-seconds* 0.25)
          (rplaca::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (incf calls)
                                  (if (= calls 1)
                                      (error "connection refused")
                                      (values "{\"output\":[{\"type\":\"message\",\"role\":\"assistant\",\"content\":[{\"type\":\"output_text\",\"text\":\"ok\"}]}]}"
                                              200)))
          (let ((response (rplaca::openai-codex-request '()
                                                          :model "gpt-5.3-codex")))
            (is (string= "end_turn"
                         (rplaca::response-stop-reason response)))))))
    (is (= 2 calls))
    (is (equalp '(0.25) (nreverse sleeps)))))

(test openai-codex-streaming-normalizes-response-shape
  "OpenAI Codex streaming adapter accumulates Responses output deltas."
  (let ((captured-force-binary nil)
        (captured-connection-timeout nil)
        (payloads '("data: {\"type\":\"response.output_text.delta\",\"delta\":\"hi from \"}"
                    ""
                    "data: {\"type\":\"response.output_text.delta\",\"delta\":\"codex\"}"
                    ""
                    "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-force-binary
                                    (getf (rest args) :force-binary)
                                    captured-connection-timeout
                                    (getf (rest args) :connection-timeout))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (rplaca::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (is-true captured-force-binary)
          (is (= rplaca::*provider-http-connection-timeout-seconds*
                 captured-connection-timeout))
          (is (string= "end_turn"
                       (bt:with-lock-held ((rplaca::stream-state-lock state))
                         (rplaca::stream-state-stop-reason state))))
          (is (equal '(((:type . "text")
                        (:text . "hi from codex")))
                     (bt:with-lock-held ((rplaca::stream-state-lock state))
                       (reverse (rplaca::stream-state-content-blocks state))))))))))

(test openai-codex-streaming-preserves-reasoning-summary
  "OpenAI Codex streaming adapter preserves reasoning summary events."
  (let ((state (rplaca::make-stream-state)))
    (rplaca::process-openai-codex-responses-sse-event
     "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"provider \"}"
     state)
    (rplaca::process-openai-codex-responses-sse-event
     "{\"type\":\"response.reasoning_summary_text.delta\",\"delta\":\"summary\"}"
     state)
    (rplaca::process-openai-codex-responses-sse-event
     "{\"type\":\"response.reasoning_summary_text.done\",\"text\":\"provider summary\"}"
     state)
    (rplaca::process-openai-codex-responses-sse-event
     "{\"type\":\"response.output_text.delta\",\"delta\":\"final\"}"
     state)
    (rplaca::process-openai-codex-responses-sse-event
     "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
     state)
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (let ((content (reverse
                      (copy-list
                       (rplaca::stream-state-content-blocks state)))))
        (is (string= "end_turn"
                     (rplaca::stream-state-stop-reason state)))
        (is (string= "final"
                     (rplaca::content-text-blocks content)))
        (is (equal '("provider summary")
                   (rplaca::content-reasoning-blocks content)))))))

(test openai-codex-streaming-records-completed-usage
  "OpenAI Codex streaming adapter records usage from response.completed."
  (let ((state (rplaca::make-stream-state)))
    (rplaca::process-openai-codex-responses-sse-event
     "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\",\"usage\":{\"input_tokens\":2006,\"output_tokens\":300,\"total_tokens\":2306,\"input_tokens_details\":{\"cached_tokens\":1920}}}}"
     state)
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (is-true (rplaca::stream-state-done-p state))
      (let ((usage (rplaca::stream-state-usage state)))
        (is (= 2006 (getf usage :input-tokens)))
        (is (= 300 (getf usage :output-tokens)))
        (is (= 2306 (getf usage :total-tokens)))
        (is (= 1920 (getf usage :cached-input-tokens)))
        (is (= 86 (getf usage :uncached-input-tokens)))))))

(test openai-codex-streaming-uses-responses-instructions
  "OpenAI Codex streaming requests send instructions + input, not chat messages."
  (let* ((captured-request-body nil)
         (captured-external-format-in nil)
         (captured-force-binary nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (payloads '("data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
                     "")))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content)
                                    captured-external-format-in (getf (rest args) :external-format-in)
                                    captured-force-binary (getf (rest args) :force-binary))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (with-function-override (rplaca::build-system-prompt ()
                                  "boot prompt")
          (let ((state (rplaca::openai-codex-request-streaming
                        messages
                        (lambda (state) (declare (ignore state)))
                        :model "gpt-5.3-codex")))
            (loop repeat 100
                  until (bt:with-lock-held ((rplaca::stream-state-lock state))
                          (rplaca::stream-state-done-p state))
                  do (sleep 0.01)))))
    (let* ((body (rplaca::api-json-decode captured-request-body))
           (input-items (coerce (cdr (assoc :input body)) 'list))
           (message-item (first input-items))
           (content-item (first (coerce (cdr (assoc :content message-item)) 'list))))
      (is (eq :utf-8 captured-external-format-in))
      (is-true captured-force-binary)
      (is (search "\"store\":false" captured-request-body))
      (is (search "\"stream\":true" captured-request-body))
      (is (not (search "max_output_tokens" captured-request-body)))
      (let ((reasoning (cdr (assoc :reasoning body))))
        (is (string= "detailed" (cdr (assoc :summary reasoning))))
        (is (null (assoc :effort reasoning))))
      (is (string= "boot prompt" (cdr (assoc :instructions body))))
      (is (string= "message" (cdr (assoc :type message-item))))
      (is (string= "input_text" (cdr (assoc :type content-item))))
      (is (string= "hello" (cdr (assoc :text content-item)))))
    (setf captured-request-body nil)
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (rplaca::openai-codex-request-streaming
                      '()
                      (lambda (state) (declare (ignore state)))
                      :model "gpt-5.4"
                      :reasoning-effort "high")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01)))))
    (let* ((body (rplaca::api-json-decode captured-request-body))
           (reasoning (cdr (assoc :reasoning body))))
      (is (string= "high" (cdr (assoc :effort reasoning))))
      (is (string= "detailed" (cdr (assoc :summary reasoning)))))))

(test openai-codex-streaming-decodes-utf8-punctuation-from-octets
  "OpenAI Codex streaming decodes UTF-8 punctuation correctly from octet streams."
  (let* ((captured-force-binary nil)
         (expected "Test received — I’m here.")
         (payload (format nil
                          "data: {\"type\":\"response.output_text.delta\",\"delta\":\"~A\"}~%~%data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}~%~%"
                          expected))
         (octets (flexi-streams:string-to-octets payload :external-format :utf-8)))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-force-binary (getf (rest args) :force-binary))
                              (values (flexi-streams:make-in-memory-input-stream octets)
                                      200
                                      nil))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (rplaca::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (is-true captured-force-binary)
          (is (string= "end_turn"
                       (bt:with-lock-held ((rplaca::stream-state-lock state))
                         (rplaca::stream-state-stop-reason state))))
          (is (equal `(((:type . "text")
                        (:text . ,expected)))
                     (bt:with-lock-held ((rplaca::stream-state-lock state))
                       (reverse (rplaca::stream-state-content-blocks state))))))))))

(test openai-codex-streaming-supports-multiple-tool-calls
  "OpenAI Codex streaming keeps two Responses function calls separate and canonical."
  (let ((payloads '("data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call_1\",\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/one.txt\\\"}\"}}"
                    ""
                    "data: {\"type\":\"response.output_item.done\",\"item\":{\"type\":\"function_call\",\"call_id\":\"call_2\",\"name\":\"write_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/two.txt\\\",\\\"content\\\":\\\"hello\\\"}\"}}"
                    ""
                    "data: {\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::resolve-openai-codex-auth (&key refresh-if-needed)
                                (declare (ignore refresh-if-needed))
                                '(:source :token-override
                                  :mode :api-key
                                  :token "openai-token"
                                  :base-url "https://api.openai.com/v1"
                                  :refreshable-p nil))
        (let ((state (rplaca::openai-codex-request-streaming '() (lambda (state) (declare (ignore state)))
                                                              :model "gpt-5.3-codex")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "tool_use"
                       (bt:with-lock-held ((rplaca::stream-state-lock state))
                         (rplaca::stream-state-stop-reason state))))
          (is (equal '(((:type . "tool_use")
                        (:id . "call_1")
                        (:name . "read_file")
                        (:input . ((:path . "/tmp/one.txt"))))
                       ((:type . "tool_use")
                        (:id . "call_2")
                        (:name . "write_file")
                        (:input . ((:path . "/tmp/two.txt")
                                   (:content . "hello")))))
                     (bt:with-lock-held ((rplaca::stream-state-lock state))
                       (reverse (rplaca::stream-state-content-blocks state))))))))))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth Tests
;;; --------------------------------------------------------------------------

(test generate-code-verifier-length-and-characters
  "Code verifier is 43 alphanumeric characters."
  (let ((verifier (rplaca::generate-code-verifier)))
    (is (= 43 (length verifier)))
    (is (every #'alphanumericp verifier))))

(test generate-code-verifier-uniqueness
  "Two generated verifiers are different."
  (let ((v1 (rplaca::generate-code-verifier))
        (v2 (rplaca::generate-code-verifier)))
    (is (not (string= v1 v2)))))

(test generate-oauth-state-length
  "OAuth state is a base64url token."
  (let ((state (rplaca::generate-oauth-state)))
    (is (> (length state) 30))
    (is (not (find #\+ state)))
    (is (not (find #\/ state)))
    (is (not (find #\= state)))))

(test compute-code-challenge-is-base64url
  "Code challenge is base64url encoded (no +, /, or = characters)."
  (let ((challenge (rplaca::compute-code-challenge "test-verifier-12345678901234567890123456")))
    (is (plusp (length challenge)))
    (is (not (find #\+ challenge)))
    (is (not (find #\/ challenge)))
    (is (not (find #\= challenge)))))

(test compute-code-challenge-deterministic
  "Same verifier produces the same challenge."
  (let* ((verifier "deterministic-test-verifier-1234567890abcdef")
         (c1 (rplaca::compute-code-challenge verifier))
         (c2 (rplaca::compute-code-challenge verifier)))
    (is (string= c1 c2))))

(test url-encode-param-preserves-safe-characters
  "URL encoding preserves unreserved characters (RFC 3986)."
  (is (string= "abc-_.~" (rplaca::url-encode-param "abc-_.~")))
  (is (string= "ABCxyz0189" (rplaca::url-encode-param "ABCxyz0189"))))

(test url-encode-param-encodes-special-characters
  "URL encoding percent-encodes spaces, slashes, and other special characters."
  (is (string= "hello%20world" (rplaca::url-encode-param "hello world")))
  (is (string= "a%2Fb" (rplaca::url-encode-param "a/b")))
  (is (string= "key%3Dvalue" (rplaca::url-encode-param "key=value")))
  (is (string= "q%26a" (rplaca::url-encode-param "q&a"))))

(test extract-oauth-callback-params-extracts-code-and-state
  "Callback URL parameters are correctly extracted."
  (multiple-value-bind (code state)
      (rplaca::extract-oauth-callback-params
       "http://localhost:1455/auth/callback?code=abc123&state=xyz789")
    (is (string= "abc123" code))
    (is (string= "xyz789" state))))

(test extract-oauth-callback-params-code-only
  "Callback URL with only code (no state) still works."
  (multiple-value-bind (code state)
      (rplaca::extract-oauth-callback-params
       "http://localhost:1455/auth/callback?code=onlycode")
    (is (string= "onlycode" code))
    (is (null state))))

(test extract-oauth-callback-params-rejects-missing-query
  "Callback URL without query parameters signals an error."
  (signals error
    (rplaca::extract-oauth-callback-params
     "http://localhost:1455/auth/callback")))

(test extract-oauth-callback-params-rejects-missing-code
  "Callback URL without a code parameter signals an error."
  (signals error
    (rplaca::extract-oauth-callback-params
     "http://localhost:1455/auth/callback?state=xyz789")))

(test openai-codex-oauth-start-returns-valid-url
  "oauth-start returns an authorization URL with all required PKCE parameters."
  (multiple-value-bind (url verifier state)
      (rplaca::openai-codex-oauth-start)
    (is (search "https://auth.openai.com/oauth/authorize?" url))
    (is (search "client_id=app_EMoamEEZ73f0CkXaXp7hrann" url))
    (is (search "response_type=code" url))
    (is (search "redirect_uri=http%3A%2F%2Flocalhost%3A1455%2Fauth%2Fcallback" url))
    (is (search "scope=openid%20profile%20email%20offline_access%20api.connectors.read%20api.connectors.invoke" url))
    (is (search "code_challenge_method=S256" url))
    (is (search "code_challenge=" url))
    (is (search "id_token_add_organizations=true" url))
    (is (search "codex_cli_simplified_flow=true" url))
    (is (search "originator=codex_cli_rs" url))
    (is (search "api.connectors.read" url))
    (is (= 43 (length verifier)))
    (is (search (format nil "state=~A" state) url))))

(test save-and-read-openai-codex-oauth-tokens-round-trip
  "OpenAI Codex auth.json round-trips through the compatibility helpers."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (rplaca::save-openai-codex-oauth-tokens
       "access-tok" "refresh-tok" nil
       :id-token "id-tok"
       :account-id "acct_123"
       :openai-api-key "sk-api"
       :auth-mode :chatgpt)
      (let ((creds (rplaca::read-openai-codex-oauth-tokens)))
        (is (eq :chatgpt (getf creds :auth-mode)))
        (is (string= "access-tok" (getf creds :access-token)))
        (is (string= "refresh-tok" (getf creds :refresh-token)))
        (is (string= "acct_123" (getf creds :account-id)))
        (is (string= "sk-api" (getf creds :openai-api-key)))))))

(test read-openai-codex-oauth-tokens-returns-nil-when-missing
  "Reading from a nonexistent path returns nil."
  (with-codex-auth-path-override (#P"/tmp/nonexistent-rplaca-oauth/auth.json")
    (is (null (rplaca::read-openai-codex-oauth-tokens)))))

(test read-openai-codex-oauth-token-returns-valid-token
  "read-openai-codex-oauth-token returns the ChatGPT access token from auth.json."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "fresh-token"
                              :refresh-token "refresh-tok"))
      (with-function-override (rplaca::openai-codex-chatgpt-auth-stale-p (auth-json)
                                (declare (ignore auth-json))
                                nil)
        (is (string= "fresh-token"
                     (rplaca::read-openai-codex-oauth-token)))))))

(test read-openai-codex-oauth-token-refreshes-when-expired
  "read-openai-codex-oauth-token refreshes stale ChatGPT auth via auth.json."
  (let ((path (temp-codex-auth-path))
        (refresh-called-p nil))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "old-token"
                              :refresh-token "good-refresh"))
      (with-function-override (rplaca::openai-codex-chatgpt-auth-stale-p (auth-json)
                                (declare (ignore auth-json))
                                t)
        (with-function-override (rplaca::refresh-openai-codex-auth-json (&optional auth-json)
                                  (declare (ignore auth-json))
                                  (setf refresh-called-p t)
                                  (make-codex-chatgpt-auth-payload
                                   :access-token "refreshed-token"
                                   :refresh-token "good-refresh"))
          (is (string= "refreshed-token"
                       (rplaca::read-openai-codex-oauth-token)))
          (is-true refresh-called-p))))))

(test read-provider-token-prefers-static-override-for-openai-codex
  "OpenAI Codex uses the rplaca token file before shared auth.json."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "oauth-token"))
      (with-provider-token-path-overrides (nil openai-codex-path)
      (rplaca::save-provider-token :openai-codex "static-token")
        (is (string= "static-token"
                     (rplaca::read-provider-token :openai-codex)))))))

(test read-provider-token-ignores-url-like-openai-codex-override
  "A URL-like OpenAI Codex override token is ignored in favor of shared auth.json."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                              :account-id "acct_456"))
      (with-provider-token-path-overrides (nil openai-codex-path)
        (rplaca::save-provider-token
         :openai-codex
         "http://localhost:1455/auth/callback?code=abc&state=xyz")
        (is (string= "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                     (rplaca::read-provider-token :openai-codex)))))))

(test read-provider-token-falls-back-to-codex-auth-json-for-openai-codex
  "OpenAI Codex falls back to shared auth.json when no override token exists."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "oauth-token"))
      (with-provider-token-path-overrides (nil openai-codex-path)
        (is (string= "oauth-token"
                     (rplaca::read-provider-token :openai-codex)))))))

(test resolve-openai-codex-auth-api-key-mode-uses-openai-base-url
  "API-key auth.json resolves to the OpenAI base URL."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-api-key-auth-payload
                              :api-key "sk-api"))
      (let ((auth (rplaca::resolve-openai-codex-auth)))
        (is (eq :api-key (getf auth :mode)))
        (is (string= "sk-api" (getf auth :token)))
        (is (string= "https://api.openai.com/v1" (getf auth :base-url)))))))

(test resolve-openai-codex-auth-chatgpt-mode-uses-chatgpt-base-url
  "ChatGPT auth.json resolves to the ChatGPT Codex backend and preserves account id."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :access-token "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                              :refresh-token "chatgpt-refresh"
                              :account-id "acct_456"))
      (with-function-override (rplaca::openai-codex-chatgpt-auth-stale-p (auth-json)
                                (declare (ignore auth-json))
                                nil)
        (let ((auth (rplaca::resolve-openai-codex-auth)))
          (is (eq :chatgpt (getf auth :mode)))
          (is (eq :codex-chatgpt (getf auth :source)))
          (is (string= "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1c2VyIn0.sig"
                       (getf auth :token)))
          (is (string= "acct_456" (getf auth :account-id)))
          (is (string= "https://chatgpt.com/backend-api/codex"
                       (getf auth :base-url)))))))))

(test resolve-openai-codex-auth-chatgpt-missing-account-id-errors
  "ChatGPT auth requires an account id in the shared auth store."
  (let ((path (temp-codex-auth-path)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-chatgpt-auth-payload
                              :account-id nil))
      (signals error
        (rplaca::resolve-openai-codex-auth)))))

(test provider-has-token-p-openai-codex-accepts-codex-auth-json
  "provider-has-token-p treats shared Codex auth.json as valid OpenAI Codex auth."
  (let ((path (temp-codex-auth-path))
        (openai-codex-path (temp-test-token-path :openai-codex)))
    (with-codex-auth-path-override (path)
      (write-codex-auth-json path
                             (make-codex-api-key-auth-payload
                              :api-key "sk-selector"))
      (with-provider-token-path-overrides (nil openai-codex-path)
        (is-true (rplaca::provider-has-token-p :openai-codex))))))

(test exchange-openai-oauth-code-makes-correct-request
  "Token exchange sends the correct form parameters to the token endpoint."
  (let ((captured-content nil)
        (captured-url nil))
    (with-function-override (drakma:http-request (url &rest args)
                              (setf captured-url url
                                    captured-content (getf args :content))
                              (values "{\"id_token\":\"id-token\",\"access_token\":\"new-access\",\"refresh_token\":\"new-refresh\"}"
                                      200))
      (let ((tokens (rplaca::exchange-openai-oauth-code "auth-code-123" "verifier-xyz")))
        (is (string= "https://auth.openai.com/oauth/token" captured-url))
        (is (search "grant_type=authorization_code" captured-content))
        (is (search "code=auth-code-123" captured-content))
        (is (search "code_verifier=verifier-xyz" captured-content))
        (is (string= "id-token" (getf tokens :id-token)))
        (is (string= "new-access" (getf tokens :access-token)))
        (is (string= "new-refresh" (getf tokens :refresh-token)))))))

(test openai-codex-oauth-finish-validates-state
  "oauth-finish rejects mismatched state parameters."
  (with-function-override (rplaca::exchange-openai-oauth-code (code verifier &key redirect-uri)
                            (declare (ignore code verifier redirect-uri))
                            (list :id-token "id-token"
                                  :access-token "tok"
                                  :refresh-token "ref"
                                  :account-id "acct_123"))
    (let ((path (temp-codex-auth-path)))
      (with-codex-auth-path-override (path)
      (signals error
        (rplaca::openai-codex-oauth-finish
         "http://localhost:1455/auth/callback?code=abc&state=wrong"
         "verifier"
         "expected-state"))))))

(test openai-codex-oauth-finish-succeeds-with-matching-state
  "oauth-finish completes and persists a Codex-compatible auth.json payload."
  (with-function-override (rplaca::exchange-openai-oauth-code (code verifier &key redirect-uri)
                            (is (string= "auth-code" code))
                            (is (string= "my-verifier" verifier))
                            (is (string= (rplaca::openai-oauth-redirect-uri) redirect-uri))
                            (list :id-token "id-token"
                                  :access-token "final-token"
                                  :refresh-token "final-refresh"
                                  :account-id "acct_789"))
    (with-function-override (rplaca::obtain-openai-codex-api-key (id-token)
                              (is (string= "id-token" id-token))
                              "sk-exchanged")
      (let ((path (temp-codex-auth-path)))
        (with-codex-auth-path-override (path)
          (let ((result (rplaca::openai-codex-oauth-finish
                         "http://localhost:1455/auth/callback?code=auth-code&state=good-state"
                         "my-verifier"
                         "good-state")))
            (is (string= "final-token" result))
            (let ((saved (rplaca::read-openai-codex-oauth-tokens)))
              (is (eq :chatgpt (getf saved :auth-mode)))
              (is (string= "final-token" (getf saved :access-token)))
              (is (string= "final-refresh" (getf saved :refresh-token)))
              (is (string= "acct_789" (getf saved :account-id)))
              (is (string= "sk-exchanged" (getf saved :openai-api-key))))))))))

#+sbcl
(test openai-oauth-start-failure-releases-listener
  "A PKCE/start failure cannot leak the listener bound before it."
  (let ((captured-port nil)
        (original-bind
          (symbol-function 'rplaca::bind-openai-oauth-listener)))
    (with-function-override
        (rplaca::bind-openai-oauth-listener
            (&optional (preferred-port rplaca::*openai-oauth-default-port*))
          (multiple-value-bind (listener port)
              (funcall original-bind preferred-port)
            (setf captured-port port)
            (values listener port)))
      (with-function-override
          (rplaca::openai-codex-oauth-start (&key redirect-uri)
            (declare (ignore redirect-uri))
            (error "injected PKCE construction failure"))
        (signals error
          (rplaca::start-openai-codex-oauth-login
           :open-browser-p nil))))
    (is-true captured-port)
    (multiple-value-bind (listener rebound-port)
        (rplaca::bind-openai-oauth-listener captured-port)
      (unwind-protect
           (is (= captured-port rebound-port))
        (ignore-errors
          (sb-bsd-sockets:socket-close listener))))))

#+sbcl
(test openai-oauth-thread-constructor-failure-releases-listener
  "A thread-constructor failure leaves no orphan callback listener."
  (let ((captured-port nil)
        (original-bind
          (symbol-function 'rplaca::bind-openai-oauth-listener)))
    (with-function-override
        (rplaca::bind-openai-oauth-listener
            (&optional (preferred-port rplaca::*openai-oauth-default-port*))
          (multiple-value-bind (listener port)
              (funcall original-bind preferred-port)
            (setf captured-port port)
            (values listener port)))
      (signals error
        (rplaca::start-openai-codex-oauth-login
         :open-browser-p nil
         :thread-constructor
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error "injected thread construction failure")))))
    (is-true captured-port)
    (multiple-value-bind (listener rebound-port)
        (rplaca::bind-openai-oauth-listener captured-port)
      (unwind-protect
           (is (= captured-port rebound-port))
        (ignore-errors
          (sb-bsd-sockets:socket-close listener))))))

#+sbcl
(defun openai-oauth-test-http-request (port method target)
  "Send one complete localhost HTTP request and return the complete response."
  (let ((socket nil)
        (stream nil))
    (unwind-protect
         (progn
           (setf socket
                 (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream
                                :protocol :tcp))
           (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
           (setf stream
                 (sb-bsd-sockets:socket-make-stream
                  socket
                  :input t
                  :output t
                  :element-type 'character
                  :external-format :utf-8
                  :buffering :line))
           (format stream
                   "~A ~A HTTP/1.1~C~CHost: localhost~C~CConnection: close~C~C~C~C"
                   method target
                   #\Return #\Linefeed
                   #\Return #\Linefeed
                   #\Return #\Linefeed
                   #\Return #\Linefeed)
           (finish-output stream)
           (with-output-to-string (response)
             (loop :for character := (read-char stream nil nil)
                   :while character
                   :do (write-char character response))))
      (when stream
        (ignore-errors
          (close stream :abort t)))
      (when socket
        (ignore-errors
          (sb-bsd-sockets:socket-close socket))))))

#+sbcl
(defun wait-for-openai-oauth-test-worker (worker)
  "Wait boundedly for WORKER and join it when it exits."
  (loop :repeat 400
        :while (bt:thread-alive-p worker)
        :do (sleep 0.005))
  (unless (bt:thread-alive-p worker)
    (bt:join-thread worker))
  (not (bt:thread-alive-p worker)))

#+sbcl
(defun openai-oauth-test-port-released-p (port)
  "Return true when a fresh listener can reclaim PORT."
  (multiple-value-bind (listener rebound-port)
      (rplaca::bind-openai-oauth-listener port)
    (unwind-protect
         (= port rebound-port)
      (ignore-errors
        (sb-bsd-sockets:socket-close listener)))))

#+sbcl
(defun run-openai-oauth-rejection-then-success-test (method target)
  "Return evidence that one rejected request does not retire the OAuth worker."
  (let ((flow nil)
        (worker nil)
        (port nil)
        (callback-url nil)
        (code-verifier nil)
        (expected-state nil))
    (with-function-override
        (rplaca::openai-codex-oauth-start (&key redirect-uri)
          (values (format nil "https://auth.invalid/?redirect=~A" redirect-uri)
                  "test-verifier"
                  "test-state"))
      (with-function-override
          (rplaca::openai-codex-oauth-finish
              (url verifier state &key redirect-uri)
            (declare (ignore redirect-uri))
            (setf callback-url url
                  code-verifier verifier
                  expected-state state)
            "test-access-token")
        (unwind-protect
             (progn
               (setf flow
                     (rplaca::start-openai-codex-oauth-login
                      :open-browser-p nil)
                     worker (rplaca::openai-oauth-flow-thread-snapshot flow)
                     port (rplaca::openai-oauth-flow-port flow))
               (let* ((rejection-response
                        (openai-oauth-test-http-request port method target))
                      (after-rejection
                        (rplaca::openai-oauth-flow-snapshot flow))
                      (worker-alive-after-rejection-p
                        (bt:thread-alive-p worker))
                      (success-response
                        (openai-oauth-test-http-request
                         port
                         "GET"
                         "/auth/callback?code=test-code&state=test-state"))
                      (worker-exited-p
                        (wait-for-openai-oauth-test-worker worker))
                      (final-snapshot
                        (rplaca::openai-oauth-flow-snapshot flow)))
                 (list :rejection-response rejection-response
                       :after-rejection after-rejection
                       :worker-alive-after-rejection-p
                       worker-alive-after-rejection-p
                       :success-response success-response
                       :worker-exited-p worker-exited-p
                       :final-snapshot final-snapshot
                       :callback-url callback-url
                       :code-verifier code-verifier
                       :expected-state expected-state
                       :port-released-p
                       (and worker-exited-p
                            (openai-oauth-test-port-released-p port)))))
          (when (and flow worker (bt:thread-alive-p worker))
            (rplaca::cancel-openai-codex-oauth-login flow)
            (wait-for-openai-oauth-test-worker worker)))))))

#+sbcl
(test openai-oauth-wrong-method-does-not-retire-listener
  "A rejected HTTP method can be followed by the real OAuth callback."
  (let* ((evidence
           (run-openai-oauth-rejection-then-success-test
            "POST"
            "/auth/callback?code=ignored&state=test-state"))
         (final-snapshot (getf evidence :final-snapshot)))
    (is (search "405 Method Not Allowed"
                (getf evidence :rejection-response)))
    (is-false (getf (getf evidence :after-rejection) :done-p))
    (is-true (getf evidence :worker-alive-after-rejection-p))
    (is (search "200 OK" (getf evidence :success-response)))
    (is-true (getf evidence :worker-exited-p))
    (is-true (getf final-snapshot :done-p))
    (is-true (getf final-snapshot :success-p))
    (is (string= "test-access-token" (getf final-snapshot :token)))
    (is (search "code=test-code&state=test-state"
                (getf evidence :callback-url)))
    (is (string= "test-verifier" (getf evidence :code-verifier)))
    (is (string= "test-state" (getf evidence :expected-state)))
    (is-true (getf evidence :port-released-p))))

#+sbcl
(test openai-oauth-wrong-path-does-not-retire-listener
  "A rejected callback path can be followed by the real OAuth callback."
  (let* ((evidence
           (run-openai-oauth-rejection-then-success-test
            "GET"
            "/favicon.ico"))
         (final-snapshot (getf evidence :final-snapshot)))
    (is (search "404 Not Found" (getf evidence :rejection-response)))
    (is-false (getf (getf evidence :after-rejection) :done-p))
    (is-true (getf evidence :worker-alive-after-rejection-p))
    (is (search "200 OK" (getf evidence :success-response)))
    (is-true (getf evidence :worker-exited-p))
    (is-true (getf final-snapshot :done-p))
    (is-true (getf final-snapshot :success-p))
    (is-true (getf evidence :port-released-p))))

#+sbcl
(test openai-oauth-rejection-budget-fails-and-releases-listener
  "Repeated invalid requests terminate the bounded callback worker cleanly."
  (let ((flow nil)
        (worker nil)
        (port nil))
    (with-function-override
        (rplaca::openai-codex-oauth-start (&key redirect-uri)
          (values (format nil "https://auth.invalid/?redirect=~A" redirect-uri)
                  "test-verifier"
                  "test-state"))
      (unwind-protect
           (progn
             (setf flow
                   (rplaca::start-openai-codex-oauth-login
                    :open-browser-p nil)
                   worker (rplaca::openai-oauth-flow-thread-snapshot flow)
                   port (rplaca::openai-oauth-flow-port flow))
             (loop :repeat rplaca::*openai-oauth-max-rejected-callback-requests*
                   :for response :=
                     (openai-oauth-test-http-request port "GET" "/favicon.ico")
                   :do (is (search "404 Not Found" response)))
             (is-true (wait-for-openai-oauth-test-worker worker))
             (let ((snapshot (rplaca::openai-oauth-flow-snapshot flow)))
               (is-true (getf snapshot :done-p))
               (is-false (getf snapshot :success-p))
               (is (search "Too many rejected"
                           (getf snapshot :error))))
             (is-true (openai-oauth-test-port-released-p port)))
        (when (and flow worker (bt:thread-alive-p worker))
          (rplaca::cancel-openai-codex-oauth-login flow)
          (wait-for-openai-oauth-test-worker worker))))))

#+sbcl
(test cancelling-openai-oauth-partial-request-interrupts-client
  "Cancellation closes an accepted client blocked on an incomplete header."
  (let* ((flow (rplaca::start-openai-codex-oauth-login
                :open-browser-p nil))
         (port (rplaca::openai-oauth-flow-port flow))
         (worker (rplaca::openai-oauth-flow-thread-snapshot flow))
         (client-socket nil)
         (client-stream nil))
    (unwind-protect
         (progn
           (setf client-socket
                 (make-instance 'sb-bsd-sockets:inet-socket
                                :type :stream
                                :protocol :tcp))
           (sb-bsd-sockets:socket-connect
            client-socket #(127 0 0 1) port)
           (setf client-stream
                 (sb-bsd-sockets:socket-make-stream
                  client-socket
                  :input t
                  :output t
                  :element-type 'character
                  :external-format :utf-8
                  :buffering :line))
           ;; Supply a request line and one header, but no terminating blank
           ;; line.  The server must be blocked in its header read when active.
           (format client-stream
                   "GET /auth/callback?code=x&state=y HTTP/1.1~C~CHost: localhost~C~C"
                   #\Return #\Linefeed #\Return #\Linefeed)
           (finish-output client-stream)
           (loop :repeat 400
                 :until (getf (rplaca::openai-oauth-flow-snapshot flow)
                              :client-active-p)
                 :do (sleep 0.005))
           (is-true
            (getf (rplaca::openai-oauth-flow-snapshot flow)
                  :client-active-p))
           (multiple-value-bind (cancelled-flow cancelled-now-p)
               (rplaca::cancel-openai-codex-oauth-login flow)
             (is (eq flow cancelled-flow))
             (is-true cancelled-now-p))
           (loop :repeat 400
                 :while (bt:thread-alive-p worker)
                 :do (sleep 0.005))
           (is-false (bt:thread-alive-p worker))
           (unless (bt:thread-alive-p worker)
             (bt:join-thread worker))
           (let ((snapshot (rplaca::openai-oauth-flow-snapshot flow)))
             (is-true (getf snapshot :cancelled-p))
             (is-false (getf snapshot :client-active-p))))
      (when client-stream
        (ignore-errors
          (close client-stream :abort t)))
      (when client-socket
        (ignore-errors
          (sb-bsd-sockets:socket-close client-socket)))
      (when (and worker (bt:thread-alive-p worker))
        (rplaca::cancel-openai-codex-oauth-login flow)
        (loop :repeat 400
              :while (bt:thread-alive-p worker)
              :do (sleep 0.005)))
      (when (and worker (not (bt:thread-alive-p worker)))
        (bt:join-thread worker)))))

(test stale-openai-oauth-snapshot-cannot-mutate-buffer
  "A replaced flow cannot clear its successor or apply stale cancellation."
  (let* ((buf (make-buffer "oauth-stale-claim"
                           :session-persistence-mode :ephemeral))
         (stale (rplaca::make-openai-oauth-flow :buffer buf))
         (replacement (rplaca::make-openai-oauth-flow :buffer buf))
         (original-snapshot
           (symbol-function 'rplaca::openai-oauth-flow-snapshot))
         (rplaca::*openai-oauth-pending* nil)
         (rplaca::*openai-oauth-pending-lock*
           (bt:make-lock "test-openai-oauth-pending"))
         (rplaca::*after-buffer-display-change-hook* nil))
    (setf (buffer-status buf) :oauth)
    (rplaca::openai-oauth-flow-set-result stale :cancelled t)
    (is-true (rplaca::publish-openai-oauth-pending-flow stale))
    (with-function-override
        (rplaca::openai-oauth-flow-snapshot (flow)
          (let ((snapshot (funcall original-snapshot flow)))
            (when (eq flow stale)
              (is (eq stale
                      (rplaca::claim-openai-oauth-pending-flow stale)))
              (is-true
               (rplaca::publish-openai-oauth-pending-flow replacement)))
            snapshot))
      (is-true (rplaca::update-openai-oauth-login buf)))
    (is (eq replacement (rplaca::openai-oauth-pending-flow)))
    (is (eq :oauth (buffer-status buf)))
    (is-false
     (some (lambda (message)
             (search "OAuth cancelled" (message-text message)))
           (test-buffer-history-messages buf)))
    (is (eq replacement (rplaca::take-openai-oauth-pending-flow)))))

(test oauth-command-rejection-preserves-existing-flow-status
  "Rejecting a second login cannot idle or cancel the already pending login."
  (let* ((buf (make-buffer "oauth-existing-command"
                           :session-persistence-mode :ephemeral))
         (flow (rplaca::make-openai-oauth-flow :buffer buf))
         (rplaca::*openai-oauth-pending* nil)
         (rplaca::*openai-oauth-pending-lock*
           (bt:make-lock "test-openai-oauth-existing-command"))
         (rplaca::*after-buffer-display-change-hook* nil))
    (setf (buffer-status buf) :oauth)
    (is-true (rplaca::publish-openai-oauth-pending-flow flow))
    (rplaca::openai-codex-oauth-command buf)
    (is (eq flow (rplaca::openai-oauth-pending-flow)))
    (is (eq :oauth (buffer-status buf)))
    (is-false
     (getf (rplaca::openai-oauth-flow-snapshot flow) :done-p))
    (is (eq flow (rplaca::take-openai-oauth-pending-flow)))))

(test oauth-buffer-teardown-clears-pending-flow-and-status
  "Buffer teardown atomically retires OAuth ownership and leaves idle status."
  (let* ((buf (make-buffer "oauth-teardown"
                           :session-persistence-mode :ephemeral))
         (flow (rplaca::make-openai-oauth-flow :buffer buf))
         (rplaca::*openai-oauth-pending* nil)
         (rplaca::*openai-oauth-pending-lock*
           (bt:make-lock "test-openai-oauth-teardown"))
         (rplaca::*after-buffer-display-change-hook* nil))
    (setf (buffer-status buf) :oauth)
    (is-true (rplaca::publish-openai-oauth-pending-flow flow))
    (rplaca::cancel-buffer-runtime-operations buf)
    (is (null (rplaca::openai-oauth-pending-flow)))
    (is-true
     (rplaca::deliver-buffer-runtime-stopped-notification buf))
    (is (eq :idle (buffer-status buf)))
    (is-true
     (getf (rplaca::openai-oauth-flow-snapshot flow) :cancelled-p))))

;;; --------------------------------------------------------------------------
;;; Z.AI (Zhipu AI) Provider Tests
;;; --------------------------------------------------------------------------

(test zai-provider-token-path
  "Z.AI provider token path is zai-api-key."
  (let ((path (rplaca::provider-token-path :zai)))
    (is (search "zai-api-key" (namestring path)))))

(test zai-known-provider-p
  "Z.AI is recognized as a known provider."
  (is-true (rplaca::known-provider-p :zai)))

(test zai-fallback-model-is-glm-5-2
  "Z.AI fallback model is glm-5.2."
  (is (string= "glm-5.2"
               (rplaca::provider-fallback-model :zai))))

(test zai-normalize-provider-keyword
  "normalize-provider accepts :zai."
  (is (eq :zai (rplaca::normalize-provider :zai))))

(test zai-normalize-provider-string
  "normalize-provider accepts \"zai\" string."
  (is (eq :zai (rplaca::normalize-provider "zai"))))

(test zai-read-provider-token-from-file
  "read-provider-token reads Z.AI API key from static file."
  (let ((zai-path (temp-test-token-path :zai))
        (openai-codex-path (temp-test-token-path :openai-codex))
        (rplaca::*zai-env-var* "RPLACA_UNSET_ZAI_ENV_98765"))
    (with-provider-token-path-overrides (nil openai-codex-path zai-path)
      (rplaca::save-provider-token :zai "zai-test-key-abc123")
      (is (string= "zai-test-key-abc123"
                   (rplaca::read-provider-token :zai))))))

(test zai-request-sends-correct-headers-and-body
  "Z.AI non-streaming sends correct Authorization and Accept-Language headers."
  (let ((captured-url nil)
        (captured-headers nil)
        (captured-body nil)
        (messages (list (list (cons :role "user")
                              (cons :content
                                    (list (list (cons :type "text")
                                                (cons :text "hello"))))))))
    (with-function-override (drakma:http-request (url &rest args)
                              (setf captured-url url
                                    captured-headers (getf args :additional-headers)
                                    captured-body (getf args :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"hi from glm\"}}]}"
                                      200))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key-test")
        (rplaca::zai-request messages :model "glm-5")))
    (is (string= "https://api.z.ai/api/coding/paas/v4/chat/completions" captured-url))
    (is (string= "Bearer zai-key-test"
                 (cdr (assoc "Authorization" captured-headers :test #'string=))))
    (is (string= "en-US,en"
                 (cdr (assoc "Accept-Language" captured-headers :test #'string=))))
    (let ((body (rplaca::api-json-decode captured-body)))
      (is (string= "glm-5" (cdr (assoc :model body)))))))

(test zai-request-sends-glm-5-2-reasoning-effort
  "Z.AI non-streaming requests preserve a selected GLM-5.2 effort."
  (let ((captured-body nil))
    (with-function-override (drakma:http-request (url &rest args)
                              (declare (ignore url))
                              (setf captured-body (getf args :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                      200))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key-test")
        (rplaca::zai-request
         '()
         :model "glm-5.2"
         :reasoning-effort "high")))
    (let ((body (rplaca::api-json-decode captured-body)))
      (is (string= "glm-5.2" (cdr (assoc :model body))))
      (is (string= "high" (cdr (assoc :reasoning--effort body)))))))

(test zai-request-normalizes-response
  "Z.AI non-streaming normalizes the OpenAI-compatible response to canonical shape."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"你好世界\"}}]}"
                             200))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (rplaca::zai-request '() :model "glm-5")))
        (is (string= "end_turn" (rplaca::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "你好世界")))
                    (rplaca::response-content response)))))))

(test zai-request-retries-transient-503
  "Z.AI non-streaming requests use the shared transient HTTP retry path."
  (let ((calls 0)
        (sleeps nil))
    (let ((rplaca::*provider-http-max-retries* 1)
          (rplaca::*provider-http-initial-backoff-seconds* 0.5)
          (rplaca::*provider-http-sleep-function*
            (lambda (seconds)
              (push seconds sleeps))))
      (with-function-override (drakma:http-request (&rest args)
                                (declare (ignore args))
                                (incf calls)
                                (if (= calls 1)
                                    (values "service unavailable" 503 nil)
                                    (values
                                     "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                     200)))
        (with-function-override (rplaca::read-provider-token (provider)
                                  (declare (ignore provider))
                                  "zai-key")
          (let ((response (rplaca::zai-request '() :model "glm-5")))
            (is (string= "end_turn"
                         (rplaca::response-stop-reason response)))))))
    (is (= 2 calls))
    (is (equalp '(0.5) (nreverse sleeps)))))

(test zai-request-with-tool-calls
  "Z.AI non-streaming handles tool_calls responses correctly."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"tool_calls\",\"message\":{\"content\":\"let me check\",\"tool_calls\":[{\"id\":\"call_z1\",\"type\":\"function\",\"function\":{\"name\":\"read_file\",\"arguments\":\"{\\\"path\\\":\\\"/tmp/test.txt\\\"}\"}}]}}]}"
                             200))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (rplaca::zai-request '() :model "glm-5")))
        (is (string= "tool_use" (rplaca::response-stop-reason response)))
        (is (equal '(((:type . "text")
                      (:text . "let me check"))
                     ((:type . "tool_use")
                      (:id . "call_z1")
                      (:name . "read_file")
                      (:input . ((:path . "/tmp/test.txt")))))
                    (rplaca::response-content response)))))))

(test zai-request-includes-system-prompt-message
  "Z.AI requests prepend the built system prompt as an OpenAI system message."
  (let ((captured-request-body nil)
        (messages (list (list (cons :role "user")
                              (cons :content
                                    (list (list (cons :type "text")
                                                (cons :text "hello"))))))))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                      200))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (with-function-override (rplaca::build-system-prompt ()
                                  "zai boot prompt")
          (rplaca::zai-request messages :model "glm-5"))))
    (let* ((body (rplaca::api-json-decode captured-request-body))
           (sent-messages (coerce (cdr (assoc :messages body)) 'list)))
      (is (string= "system" (cdr (assoc :role (first sent-messages)))))
      (is (string= "zai boot prompt" (cdr (assoc :content (first sent-messages)))))
      (is (string= "user" (cdr (assoc :role (second sent-messages))))))))

(test zai-request-uses-max-tokens-not-max-completion-tokens
  "Z.AI requests use max_tokens (not max_completion_tokens like OpenAI Codex)."
  (let ((captured-request-body nil))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"ok\"}}]}"
                                      200))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (rplaca::zai-request '() :model "glm-5" :max-tokens 4096)))
    (let ((body (rplaca::api-json-decode captured-request-body)))
      (is (= 4096 (cdr (assoc :max--tokens body))))
      (is (null (assoc :max--completion--tokens body))))))

(test zai-streaming-normalizes-response-shape
  "Z.AI streaming adapter accumulates canonical content blocks."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"content\":\"你好\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"content\":\"世界\"}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (rplaca::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "end_turn"
                       (bt:with-lock-held ((rplaca::stream-state-lock state))
                         (rplaca::stream-state-stop-reason state))))
          (is (equal '(((:type . "text")
                        (:text . "你好世界")))
                     (bt:with-lock-held ((rplaca::stream-state-lock state))
                       (reverse (rplaca::stream-state-content-blocks state))))))))))

(test zai-streaming-includes-system-prompt
  "Z.AI streaming requests prepend the built system prompt."
  (let* ((captured-request-body nil)
         (messages (list (list (cons :role "user")
                               (cons :content
                                     (list (list (cons :type "text")
                                                 (cons :text "hello")))))))
         (payloads '("data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                     ""
                     "data: [DONE]"
                     "")))
    (with-function-override (drakma:http-request (&rest args)
                              (setf captured-request-body (getf (rest args) :content))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (with-function-override (rplaca::build-system-prompt ()
                                  "zai system prompt")
          (let ((state (rplaca::zai-request-streaming
                        messages
                        (lambda (state) (declare (ignore state)))
                        :model "glm-5")))
            (loop repeat 100
                  until (bt:with-lock-held ((rplaca::stream-state-lock state))
                          (rplaca::stream-state-done-p state))
                  do (sleep 0.01))))))
    (let* ((body (rplaca::api-json-decode captured-request-body))
           (sent-messages (coerce (cdr (assoc :messages body)) 'list)))
      (is (string= "system" (cdr (assoc :role (first sent-messages)))))
      (is (string= "zai system prompt" (cdr (assoc :content (first sent-messages))))))))

(test zai-streaming-with-tool-calls
  "Z.AI streaming supports tool calls via OpenAI-compatible protocol."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"tool_calls\":[{\"index\":0,\"id\":\"call_z2\",\"type\":\"function\",\"function\":{\"name\":\"shell\",\"arguments\":\"{\\\"command\\\":\\\"ls\\\"}\"}}]}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"tool_calls\"}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (rplaca::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (is (string= "tool_use"
                       (bt:with-lock-held ((rplaca::stream-state-lock state))
                         (rplaca::stream-state-stop-reason state))))
          (is (equal '(((:type . "tool_use")
                        (:id . "call_z2")
                        (:name . "shell")
                        (:input . ((:command . "ls")))))
                     (bt:with-lock-held ((rplaca::stream-state-lock state))
                       (reverse (rplaca::stream-state-content-blocks state))))))))))

(test zai-streaming-sends-glm-5-2-tool-options
  "Z.AI streaming opts into tool argument streaming and preserves effort."
  (let ((captured-body nil)
        (payloads '("data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                    ""
                    "data: [DONE]"
                    ""))
        (tools (vector
                '((:name . "artifactum_list")
                  (:description . "List artifacts.")
                  (:input--schema
                   (:type . "object")
                   (:properties)
                   (:required . #()))))))
    (with-function-override (drakma:http-request (url &rest args)
                              (declare (ignore url))
                              (setf captured-body (getf args :content))
                              (values (make-string-input-stream
                                       (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key-test")
        (let ((state
                (rplaca::zai-request-streaming
                 '()
                 (lambda (stream-state)
                   (declare (ignore stream-state)))
                 :model "glm-5.2"
                 :reasoning-effort "max"
                 :tools tools)))
          (loop repeat 100
                until (bt:with-lock-held
                          ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01)))))
    (let ((body (rplaca::api-json-decode captured-body)))
      (is (eq t (cdr (assoc :tool--stream body))))
      (is (string= "max" (cdr (assoc :reasoning--effort body)))))))

(test zai-provider-dispatch-preserves-reasoning-effort
  "provider-request forwards supported Z.AI reasoning effort."
  (let ((captured-effort nil))
    (with-function-override
        (rplaca::zai-request
         (messages &key model max-tokens tools reasoning-effort system-prompt)
         (declare (ignore messages model max-tokens tools system-prompt))
         (setf captured-effort reasoning-effort)
         '((:stop--reason . "end_turn") (:content . #())))
      (rplaca::provider-request
       :zai '() :model "glm-5.2" :reasoning-effort "high")
      (is (string= "high" captured-effort)))))

(test zai-provider-dispatch-routes-correctly
  "provider-request dispatches :zai to zai-request."
  (let ((dispatched-provider nil))
    (with-function-override (rplaca::zai-request (messages &key model max-tokens tools system-prompt)
                              (declare (ignore messages model max-tokens tools system-prompt))
                              (setf dispatched-provider :zai)
                              '((:stop--reason . "end_turn") (:content . #())))
      (rplaca::provider-request :zai '() :model "glm-5")
      (is (eq :zai dispatched-provider)))))

(test zai-agent-defaults-round-trip
  "Agent defaults registry handles :zai as a provider."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (rplaca::set-agent-default "zhipu" :zai :model "glm-4.7")
      (is (eq :zai (rplaca::agent-default "zhipu")))
      (is (string= "glm-4.7"
                   (rplaca::agent-default-model "zhipu" :zai))))))

;;; --------------------------------------------------------------------------
;;; Reasoning Content Handling (Z.AI GLM, DeepSeek R1, etc.)
;;; --------------------------------------------------------------------------

(test reasoning-content-non-streaming-content-preferred
  "Non-streaming: when both content and reasoning_content are present, content wins."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"Hello\",\"reasoning_content\":\"The user wants a greeting...\"}}]}"
                             200))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let* ((response (rplaca::zai-request '() :model "glm-5"))
             (content (rplaca::response-content response)))
        (is (string= "Hello"
                     (cdr (assoc :text (first content)))))
        (is (equal '("The user wants a greeting...")
                   (rplaca::content-reasoning-blocks content)))))))

(test reasoning-content-non-streaming-fallback
  "Non-streaming: when content is blank but reasoning_content is present, use reasoning."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"length\",\"message\":{\"content\":\"\",\"reasoning_content\":\"The user wants a greeting. Options: Hello, Hi...\"}}]}"
                             200))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (rplaca::zai-request '() :model "glm-5")))
        (is (string= "The user wants a greeting. Options: Hello, Hi..."
                     (cdr (assoc :text (first (rplaca::response-content response))))))
        ;; finish_reason should be "max_tokens" (mapped from "length")
        (is (string= "max_tokens" (rplaca::response-stop-reason response)))))))

(test reasoning-content-non-streaming-no-reasoning
  "Non-streaming: when only content is present (no reasoning), works normally."
  (with-function-override (drakma:http-request (&rest args)
                            (declare (ignore args))
                            (values
                             "{\"choices\":[{\"finish_reason\":\"stop\",\"message\":{\"content\":\"Hello world\"}}]}"
                             200))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (let ((response (rplaca::zai-request '() :model "glm-5")))
        (is (string= "Hello world"
                     (cdr (assoc :text (first (rplaca::response-content response))))))))))

(test reasoning-content-streaming-with-reasoning-then-content
  "Streaming: reasoning_content chunks are preserved separately from content."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"Thinking...\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\" more thoughts\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"stop\"}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (rplaca::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (let ((content-blocks
                  (bt:with-lock-held ((rplaca::stream-state-lock state))
                    (is (string= "Hello" (rplaca::stream-state-text state)))
                    (nreverse
                     (copy-list
                      (rplaca::stream-state-content-blocks state))))))
            (is (equal '("Thinking... more thoughts")
                       (rplaca::content-reasoning-blocks content-blocks)))
            (is (string= "Hello"
                         (rplaca::content-text-blocks content-blocks)))
            (is (string= "Hello"
                         (rplaca::stream-state-display-text state)))
            (is (string= (format nil "Hello~%;; reasoning~%Thinking... more thoughts")
                         (rplaca::stream-state-display-text
                          state
                          :show-reasoning-p t)))))))))

(test reasoning-content-streaming-reasoning-only
  "Streaming: when only reasoning_content chunks arrive, it is captured as reasoning."
  (let ((payloads '("data: {\"choices\":[{\"delta\":{\"reasoning_content\":\"Step 1: analyze...\"}}]}"
                    ""
                    "data: {\"choices\":[{\"delta\":{\"reasoning_content\":\" Step 2: decide...\"}}]}"
                    ""
                    "data: {\"choices\":[{\"finish_reason\":\"length\",\"delta\":{\"content\":\"\"}}]}"
                    ""
                    "data: [DONE]"
                    "")))
    (with-function-override (drakma:http-request (&rest args)
                              (declare (ignore args))
                              (values (make-string-input-stream (format nil "~{~A~%~}" payloads))
                                      200
                                      nil))
      (with-function-override (rplaca::read-provider-token (provider)
                                (declare (ignore provider))
                                "zai-key")
        (let ((state (rplaca::zai-request-streaming '() (lambda (state) (declare (ignore state)))
                                                      :model "glm-5")))
          (loop repeat 100
                until (bt:with-lock-held ((rplaca::stream-state-lock state))
                        (rplaca::stream-state-done-p state))
                do (sleep 0.01))
          (let ((content-blocks
                  (bt:with-lock-held ((rplaca::stream-state-lock state))
                    (is (string= "" (rplaca::stream-state-text state)))
                    (is (string= "max_tokens"
                                 (rplaca::stream-state-stop-reason state)))
                    (nreverse
                     (copy-list
                      (rplaca::stream-state-content-blocks state))))))
            (is (equal '("Step 1: analyze... Step 2: decide...")
                       (rplaca::content-reasoning-blocks content-blocks)))
            (is (string= ""
                         (rplaca::stream-state-display-text state)))
            (is (string= (format nil ";; reasoning~%Step 1: analyze... Step 2: decide...")
                         (rplaca::stream-state-display-text
                          state
                          :show-reasoning-p t)))))))))

;;; --------------------------------------------------------------------------
;;; Known Models Tests
;;; --------------------------------------------------------------------------

(test provider-known-models-openai-codex
  "Known OpenAI Codex models list is non-empty and contains the default."
  (let ((models (rplaca::provider-known-models :openai-codex)))
    (is (listp models))
    (is (plusp (length models)))
    (is (member "gpt-5.6-sol" models :test #'string=))
    (is (member "gpt-5.6-terra" models :test #'string=))
    (is (member "gpt-5.6-luna" models :test #'string=))
    (is (member "gpt-5.5" models :test #'string=))
    (is (member "gpt-5.3-codex" models :test #'string=))
    (is (member "gpt-5.4" models :test #'string=))
    (is (member "gpt-5.4-mini" models :test #'string=))
    (is (member "gpt-5.4-nano" models :test #'string=))
    (is (member "gpt-5.2-codex" models :test #'string=))
    (is (member "gpt-5.1-codex-max" models :test #'string=))
    (is (member "gpt-5.1-codex-mini" models :test #'string=))
    (is (member "gpt-5.2" models :test #'string=))
    (is (member rplaca::*openai-codex-model* models :test #'string=))
    (is (member "gpt-6-astra" models :test #'string=))
    (is (= 13 (length models)))))

(test astra-reasoning-and-responses-request
  "Astra retains supported reasoning in Responses and drops an old none override."
  (let ((buffer (make-buffer "astra-request-test"
                             :session-persistence-mode :ephemeral)))
    (is (equal '("low" "medium" "high" "xhigh" "max")
               (rplaca::provider-model-supported-think-levels
                :openai-codex "gpt-6-astra")))
    (dolist (effort '("low" "medium" "high" "xhigh" "max"))
      (setf (buffer-think-level-override buffer) effort)
      (let* ((resolved (rplaca::resolved-buffer-think-level
                        buffer :openai-codex "gpt-6-astra"))
             (body (rplaca::api-json-decode
                    (rplaca::openai-codex-responses-request-body
                     nil "gpt-6-astra" 128 nil :stream t
                     :reasoning-effort resolved :system-prompt "Test."))))
        (is (equal effort resolved))
        (is (equal "gpt-6-astra" (cdr (assoc :model body))))
        (is (equal effort (cdr (assoc :effort (cdr (assoc :reasoning body))))))
        (is (eq t (cdr (assoc :stream body))))
        (dolist (parameter '(:temperature :top--p :top--logprobs))
          (is (null (assoc parameter body))))))
    (setf (buffer-think-level-override buffer) "none")
    (multiple-value-bind (status effort)
        (rplaca::reconcile-buffer-think-level-override
         buffer :provider :openai-codex :model "gpt-6-astra")
      (is (eq :reset status))
      (is (null effort)))))

(test normalize-provider-openai-codex-storage-forms
  "normalize-provider accepts both kebab-case and JSON camelCase storage forms."
  (is (eq :openai-codex
          (rplaca::normalize-provider "openai-codex")))
  (is (eq :openai-codex
          (rplaca::normalize-provider "openaiCodex"))))

(test provider-model-supported-think-levels-openai-codex
  "OpenAI-Codex think levels are model-specific."
  (let ((gpt-56-sol (rplaca::provider-model-supported-think-levels
                     :openai-codex "gpt-5.6-sol"))
        (gpt-56-terra (rplaca::provider-model-supported-think-levels
                       :openai-codex "gpt-5.6-terra"))
        (gpt-56-luna (rplaca::provider-model-supported-think-levels
                      :openai-codex "gpt-5.6-luna"))
        (gpt-55 (rplaca::provider-model-supported-think-levels
                 :openai-codex "gpt-5.5"))
        (gpt-54 (rplaca::provider-model-supported-think-levels
                 :openai-codex "gpt-5.4"))
        (gpt-54-mini (rplaca::provider-model-supported-think-levels
                      :openai-codex "gpt-5.4-mini"))
        (gpt-54-nano (rplaca::provider-model-supported-think-levels
                      :openai-codex "gpt-5.4-nano"))
        (gpt-53-codex (rplaca::provider-model-supported-think-levels
                       :openai-codex "gpt-5.3-codex"))
        (gpt-51-max (rplaca::provider-model-supported-think-levels
                     :openai-codex "gpt-5.1-codex-max")))
    (is (equal '("none" "low" "medium" "high" "xhigh" "max")
               gpt-56-sol))
    (is (equal gpt-56-sol gpt-56-terra))
    (is (equal gpt-56-sol gpt-56-luna))
    (is (equal '("none" "low" "medium" "high" "xhigh") gpt-55))
    (is (equal '("none" "low" "medium" "high" "xhigh") gpt-54))
    (is (equal gpt-54 gpt-54-mini))
    (is (equal gpt-54 gpt-54-nano))
    (is (equal '("low" "medium" "high" "xhigh") gpt-53-codex))
    (is (equal '("none" "low" "medium" "high") gpt-51-max))
    (is (equal '("high" "max")
               (rplaca::provider-model-supported-think-levels
                :zai "glm-5.2")))
    ;; GLM-5.1 documents thinking mode but not a reasoning_effort enum.
    (is (null (rplaca::provider-model-supported-think-levels
               :zai "glm-5.1")))
    (is (null (rplaca::provider-model-supported-think-levels
               :zai "glm-5")))))

(test provider-known-models-zai
  "Known Z.AI models list is non-empty and contains the default."
  (let ((models (rplaca::provider-known-models :zai)))
    (is (listp models))
    (is (plusp (length models)))
    (is (string= "glm-5.2" (first models)))
    (is (member "glm-5.2" models :test #'string=))
    (is (member "glm-5.1" models :test #'string=))
    (is (member "glm-5" models :test #'string=))
    ;; Should include turbo and older variants
    (is (member "glm-5-turbo" models :test #'string=))
    (is (member "glm-4.7" models :test #'string=))))

(test provider-known-models-unknown-returns-nil
  "Unknown provider returns nil for known models."
  (is (null (rplaca::provider-known-models :unknown-provider))))

(test available-models-for-selector-marks-active
  "available-models-for-selector marks the current buffer's model as active."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      ;; Set agent default so resolution works
      (rplaca::set-agent-default "coder" :zai :model "glm-5")
      (let ((buf (make-buffer "test" :agent-name "coder")))
        ;; Mock provider-has-token-p to only return t for :zai
        (with-function-override (rplaca::provider-has-token-p (provider)
                                  (eq provider :zai))
          (let ((entries (rplaca::available-models-for-selector buf)))
            ;; Should have entries for Z.AI only
            (is (plusp (length entries)))
            (is (every (lambda (e) (eq :zai (getf e :provider))) entries))
            ;; Exactly one entry should be active
            (let ((active-count (count-if (lambda (e) (getf e :active-p)) entries)))
              (is (= 1 active-count)))
            ;; The active entry should be the default model
            (let ((active (find-if (lambda (e) (getf e :active-p)) entries)))
              (is (string= "glm-5" (getf active :model))))))))))

(test available-models-for-selector-multi-provider
  "available-models-for-selector includes models from multiple providers."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (rplaca::set-agent-default "coder" :zai :model "glm-5")
      (let ((buf (make-buffer "test" :agent-name "coder")))
        ;; Mock: both openai-codex and zai have tokens
        (with-function-override (rplaca::provider-has-token-p (provider)
                                  (not (null (member provider '(:openai-codex :zai)))))
          (let ((entries (rplaca::available-models-for-selector buf)))
            ;; Should have entries from both providers
            (is (plusp (length entries)))
            (let ((providers (remove-duplicates
                              (mapcar (lambda (e) (getf e :provider)) entries))))
              (is (member :openai-codex providers))
              (is (member :zai providers)))))))))

(test available-models-for-selector-no-tokens
  "available-models-for-selector returns nil when no provider has a token."
  (let ((path (temp-agent-defaults-path)))
    (with-agent-defaults-path-override (path)
      (let ((buf (make-buffer "test" :agent-name "agent")))
        ;; Mock: no tokens available
        (with-function-override (rplaca::provider-has-token-p (provider)
                                  (declare (ignore provider))
                                  nil)
          (let ((entries (rplaca::available-models-for-selector buf)))
            (is (null entries))))))))

;;; --------------------------------------------------------------------------
;;; OpenRouter Tests
;;; --------------------------------------------------------------------------

(test openrouter-provider-token-path
  "OpenRouter provider token path is provider-specific."
  (let ((home (user-homedir-pathname)))
    (is (equal (merge-pathnames #P".config/rplaca/openrouter-api-key" home)
               (rplaca::provider-token-path :openrouter)))))

(test openrouter-token-round-trip
  "OpenRouter API keys round-trip through provider-specific helpers."
  (let ((or-path (merge-pathnames
                  (format nil ".config/rplaca/test-openrouter-~A" (gensym))
                  (user-homedir-pathname)))
        (rplaca::*openrouter-env-var* "RPLACA_UNSET_OPENROUTER_ENV_98765"))
    (unwind-protect
         (let ((original (symbol-function 'rplaca::provider-token-path)))
           (unwind-protect
                (progn
                  (setf (symbol-function 'rplaca::provider-token-path)
                        (lambda (provider)
                          (if (eq provider :openrouter)
                              or-path
                              (funcall original provider))))
                  (is (string= "sk-or-test-key"
                               (rplaca::save-provider-token :openrouter "sk-or-test-key")))
                  (is (string= "sk-or-test-key"
                               (rplaca::read-provider-token :openrouter))))
             (setf (symbol-function 'rplaca::provider-token-path) original)))
      (ignore-errors (delete-file or-path)))))

(test openrouter-env-var-token
  "read-provider-token prefers OPENROUTER_API_KEY environment variable."
  (with-env-var ("OPENROUTER_API_KEY" "sk-or-env-token")
    (is (string= "sk-or-env-token"
                 (rplaca::read-env-token rplaca::*openrouter-env-var*)))))

(test openrouter-provider-known
  "known-provider-p recognises :openrouter."
  (is (rplaca::known-provider-p :openrouter)))

(test openrouter-normalize-provider
  "normalize-provider accepts :openrouter and the string form."
  (is (eq :openrouter (rplaca::normalize-provider :openrouter)))
  (is (eq :openrouter (rplaca::normalize-provider "openrouter")))
  (is (eq :openrouter (rplaca::normalize-provider "OPENROUTER"))))

(test openrouter-fallback-model
  "provider-fallback-model returns a model string for :openrouter."
  (let ((m (rplaca::provider-fallback-model :openrouter)))
    (is (and (stringp m) (plusp (length m))))))

(test openrouter-provider-known-models-static
  "provider-known-models returns the static fallback list for :openrouter
when no cached models are present."
  (let ((rplaca::*openrouter-cached-models* nil))
    (let ((models (rplaca::provider-known-models :openrouter)))
      (is (listp models))
      (is (plusp (length models)))
      ;; First static model is the default
      (is (string= "openai/gpt-5.6-sol" (first models)))
      (dolist (model '("openai/gpt-5.6-terra"
                       "openai/gpt-5.6-luna"
                       "openai/gpt-5.5"
                       "openai/gpt-5.4-mini"
                       "openai/gpt-5.4-nano"
                       "z-ai/glm-5.2"
                       "z-ai/glm-5.1"
                       "anthropic/claude-fable-5"
                       "anthropic/claude-opus-5"
                       "anthropic/claude-sonnet-5"
                       "anthropic/claude-haiku-4.5"
                       "google/gemini-3.6-flash"
                       "google/gemini-3.5-flash"
                       "google/gemini-3.5-flash-lite"))
        (is (member model models :test #'string=))))))

(test openrouter-provider-known-models-cached
  "provider-known-models returns the cached list when *openrouter-cached-models* is set."
  (let ((rplaca::*openrouter-cached-models* '("custom/model-a" "custom/model-b")))
    (is (equal '("custom/model-a" "custom/model-b")
               (rplaca::provider-known-models :openrouter)))))

(test fetch-openrouter-models-returns-cached
  "fetch-openrouter-models returns *openrouter-cached-models* without an HTTP call."
  (let ((rplaca::*openrouter-cached-models* '("cached/model-1" "cached/model-2")))
    (is (equal '("cached/model-1" "cached/model-2")
               (rplaca::fetch-openrouter-models)))))

(test fetch-openrouter-models-parses-api-response
  "fetch-openrouter-models populates *openrouter-cached-models* from parsed JSON."
  (let ((rplaca::*openrouter-cached-models* nil))
    (with-function-override (rplaca::read-provider-token
                             (provider)
                             (when (eq provider :openrouter) "sk-or-test"))
      (with-function-override (drakma:http-request
                               (url &rest args)
                               (declare (ignore url args))
                               (values "{\"data\":[{\"id\":\"openai/gpt-4o\"},{\"id\":\"google/gemini-2.5-pro\"}]}"
                                       200))
        (let ((models (rplaca::fetch-openrouter-models)))
          (is (member "openai/gpt-4o" models :test #'string=))
          (is (member "google/gemini-2.5-pro" models :test #'string=))
          ;; Cache should be populated
          (is (equal models rplaca::*openrouter-cached-models*)))))))

(test fetch-openrouter-models-falls-back-on-no-token
  "fetch-openrouter-models returns static fallback when no API key is configured."
  (let ((rplaca::*openrouter-cached-models* nil))
    (with-function-override (rplaca::read-provider-token
                             (provider)
                             (declare (ignore provider))
                             nil)
      (let ((models (rplaca::fetch-openrouter-models)))
        (is (listp models))
        (is (plusp (length models)))))))

(test fetch-openrouter-models-falls-back-on-http-error
  "fetch-openrouter-models returns static fallback on HTTP errors."
  (let ((rplaca::*openrouter-cached-models* nil))
    (with-function-override (rplaca::read-provider-token
                             (provider)
                             (when (eq provider :openrouter) "sk-or-test"))
      (with-function-override (drakma:http-request
                               (url &rest args)
                               (declare (ignore url args))
                               (values "Unauthorized" 401))
        (let ((models (rplaca::fetch-openrouter-models)))
          (is (listp models))
          (is (plusp (length models))))))))

(test provider-request-routes-openrouter
  "provider-request dispatches :openrouter to openrouter-request."
  (let ((routed-to nil))
    (with-function-override (rplaca::openrouter-request
                             (messages &key model max-tokens tools system-prompt)
                             (declare (ignore messages max-tokens tools system-prompt))
                             (setf routed-to model)
                             `((:stop--reason . "end_turn")
                               (:content . ,(vector `((:type . "text")
                                                      (:text . "ok"))))))
      (rplaca::provider-request :openrouter nil :model "openai/gpt-4o-mini")
      (is (string= "openai/gpt-4o-mini" routed-to)))))

(test provider-request-streaming-routes-openrouter
  "provider-request-streaming dispatches :openrouter to openrouter-request-streaming."
  (let ((routed-to nil))
    (with-function-override (rplaca::openrouter-request-streaming
                             (messages callback &key model max-tokens tools system-prompt)
                             (declare (ignore messages callback max-tokens tools system-prompt))
                             (setf routed-to model)
                             (rplaca::make-stream-state))
      (rplaca::provider-request-streaming :openrouter nil nil
                                            :model "anthropic/claude-3-5-haiku")
      (is (string= "anthropic/claude-3-5-haiku" routed-to)))))

;;; --------------------------------------------------------------------------
;;; Streaming lifecycle race tests
;;; --------------------------------------------------------------------------

(test sse-event-processors-reject-mutation-after-terminal-state
  "Chat Completions and Responses event processors ignore post-terminal data."
  (let ((state (rplaca::make-stream-state)))
    (is (eq :updated
            (rplaca::process-openai-sse-event
             "{\"choices\":[{\"delta\":{\"content\":\"before\"}}]}"
             state)))
    (is (eq :terminal
            (rplaca::process-openai-sse-event "[DONE]" state)))
    (is (null
         (rplaca::process-openai-sse-event
          "{\"choices\":[{\"delta\":{\"content\":\"-after\"}}]}"
          state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (is (string= "before" (rplaca::stream-state-text state)))
      (is-true (rplaca::stream-state-done-p state))))
  (let ((state (rplaca::make-stream-state)))
    (is (eq :updated
            (rplaca::process-openai-codex-responses-sse-event
             "{\"type\":\"response.output_text.delta\",\"delta\":\"before\"}"
             state)))
    (is (eq :terminal
            (rplaca::process-openai-codex-responses-sse-event
             "{\"type\":\"response.completed\",\"response\":{\"id\":\"resp_1\"}}"
             state)))
    (is (null
         (rplaca::process-openai-codex-responses-sse-event
          "{\"type\":\"response.output_text.delta\",\"delta\":\"-after\"}"
          state)))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (is (string= "before" (rplaca::stream-state-text state)))
      (is-true (rplaca::stream-state-done-p state)))))

(test cancellation-wins-against-in-flight-sse-delta-commit
  "A delta parsed before cancellation re-checks active state before mutation."
  (dolist (case
           (list
            (list #'rplaca::process-openai-sse-event
                  "{\"choices\":[{\"delta\":{\"content\":\"late\"}}]}")
            (list #'rplaca::process-openai-codex-responses-sse-event
                  "{\"type\":\"response.output_text.delta\",\"delta\":\"late\"}")))
    (let* ((decode-entered
             (bt:make-semaphore :name "test-sse-decode-entered"))
           (decode-release
             (bt:make-semaphore :name "test-sse-decode-release"))
           (callback-count 0)
           (state
             (rplaca::make-stream-state
              :callback (lambda (ignored-state)
                          (declare (ignore ignored-state))
                          (incf callback-count)))))
      (with-function-override (rplaca::api-json-decode (json)
                                (let ((decoded (funcall original-function json)))
                                  (bt:signal-semaphore decode-entered)
                                  (bt:wait-on-semaphore decode-release
                                                        :timeout 2)
                                  decoded))
        (let ((thread
                (bt:make-thread
                 (lambda ()
                   (funcall (first case) (second case) state))
                 :name "test-cancel-vs-sse-delta")))
          (is-true (bt:wait-on-semaphore decode-entered :timeout 2))
          (is-true (rplaca::cancel-stream-state state))
          (bt:signal-semaphore decode-release)
          (bt:join-thread thread)))
      (bt:with-lock-held ((rplaca::stream-state-lock state))
        (is (string= "" (rplaca::stream-state-text state)))
        (is-true (rplaca::stream-state-cancelled-p state))
        (is-true (rplaca::stream-state-done-p state)))
      (is (= 1 callback-count)))))

(test cancellation-closes-blocked-reader-once-and-clears-reader-refs
  "Cancel versus EOF/close has one close, one terminal callback, and no stale refs."
  (let* ((stream
           (make-instance 'controlled-character-input-stream
                          :block-first-read-p t))
         (callback-count 0)
         (state
           (rplaca::make-stream-state
            :callback (lambda (ignored-state)
                        (declare (ignore ignored-state))
                        (incf callback-count)))))
    (rplaca::start-stream-state-reader-worker
     state
     (rplaca::stream-state-callback state)
     "test-blocked-sse-reader"
     (lambda (worker-state)
       (when (rplaca::register-stream-state-stream worker-state stream)
         (rplaca::read-openai-sse-stream
          stream worker-state nil :defer-terminal-callback t))))
    (let ((thread (stream-state-reader-thread-snapshot state)))
      (is-true thread)
      (is-true
       (bt:wait-on-semaphore (controlled-stream-read-entered stream)
                             :timeout 2))
      (is-true (rplaca::cancel-stream-state state))
      (rplaca::settle-stream-state-reader state))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (is-true (rplaca::stream-state-cancelled-p state))
      (is-true (rplaca::stream-state-done-p state))
      (is (null (rplaca::stream-state-close-stream state)))
      (is (null (rplaca::stream-state-reader-thread state))))
    (is (= 1 (controlled-stream-close-count stream)))
    (is (= 1 callback-count))))

(test cancellation-closes-attached-stream-with-no-reader-owner
  "Cancellation closes an attached orphan stream that has no managed reader."
  (let ((stream (make-instance 'controlled-character-input-stream))
        (state (rplaca::make-stream-state)))
    (is-true (rplaca::register-stream-state-stream state stream))
    (is-true (rplaca::cancel-stream-state state))
    (bt:with-lock-held ((rplaca::stream-state-lock state))
      (is-true (rplaca::stream-state-cancelled-p state))
      (is (null (rplaca::stream-state-close-stream state)))
      (is (null (rplaca::stream-state-reader-thread state))))
    (is (= 1 (controlled-stream-close-count stream)))))

#+sbcl
(test provider-transport-descriptor-traverses-cl-plus-ssl-layer
  "The SBCL cancellation fast path recognizes Drakma's TLS stream layer."
  (with-open-file (raw-stream #P"/dev/null"
                              :direction :input
                              :element-type '(unsigned-byte 8))
    (let* ((file-descriptor (sb-sys:fd-stream-fd raw-stream))
           (ssl-stream
             (make-instance 'cl+ssl::ssl-stream
                            :socket raw-stream
                            :close-callback nil)))
      (is (= file-descriptor
             (rplaca::provider-stream-transport-file-descriptor
              ssl-stream))))))

#+sbcl
(test cancellation-shuts-down-blocked-drakma-transport
  "A cancelled Drakma SSE read settles well before its configured I/O timeout."
  (let* ((listener
           (make-instance 'sb-bsd-sockets:inet-socket
                          :type :stream
                          :protocol :tcp))
         (server-release
           (bt:make-semaphore :name "test-stalled-sse-server-release"))
         (server-thread nil)
         (body nil)
         (observed-stream nil)
         (state nil)
         (reader nil))
    (sb-bsd-sockets:socket-bind listener #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen listener 1)
    (multiple-value-bind (ignored-address port)
        (sb-bsd-sockets:socket-name listener)
      (declare (ignore ignored-address))
      (setf server-thread
            (bt:make-thread
             (lambda ()
               (let ((client nil)
                     (client-stream nil))
                 (unwind-protect
                      (progn
                        (setf client
                              (nth-value
                               0
                               (sb-bsd-sockets:socket-accept listener)))
                        (setf client-stream
                              (sb-bsd-sockets:socket-make-stream
                               client
                               :input t
                               :output t
                               :element-type 'character
                               :external-format :utf-8
                               :buffering :none))
                        (loop :for line := (read-line client-stream nil nil)
                              :while (and line
                                          (plusp
                                           (length
                                            (string-trim '(#\Return) line)))))
                        (format client-stream
                                "HTTP/1.1 200 OK~C~CContent-Type: text/event-stream~C~CConnection: keep-alive~C~C~C~C"
                                #\Return #\Linefeed
                                #\Return #\Linefeed
                                #\Return #\Linefeed
                                #\Return #\Linefeed)
                        (force-output client-stream)
                        ;; Deliberately send no body.  Cancellation must wake
                        ;; the client's blocked descriptor read.
                        (bt:wait-on-semaphore server-release :timeout 5))
                   (when client-stream
                     (ignore-errors
                       (close client-stream :abort t)))
                   (when client
                     (ignore-errors
                       (sb-bsd-sockets:socket-close client))))))
             :name "test-stalled-sse-server"))
      (unwind-protect
           (progn
             (multiple-value-bind (response-body status-code)
                 (drakma:http-request
                  (format nil "http://127.0.0.1:~D/events" port)
                  :want-stream t
                  :connection-timeout 20)
               (setf body response-body)
               (is (= 200 status-code)))
             (setf observed-stream
                   (make-instance 'observed-provider-character-input-stream
                                  :underlying-stream body)
                   state (rplaca::make-stream-state))
             (rplaca::start-stream-state-reader-worker
              state nil "test-blocked-drakma-sse-reader"
              (lambda (worker-state)
                (when (rplaca::register-stream-state-stream
                       worker-state observed-stream)
                  (rplaca::read-openai-sse-stream
                   observed-stream worker-state nil
                   :defer-terminal-callback t))))
             (setf reader (stream-state-reader-thread-snapshot state))
             (is-true reader)
             (is-true
              (bt:wait-on-semaphore
               (observed-provider-read-entered observed-stream)
               :timeout 2))
             (let* ((started-at (get-internal-real-time))
                    (deadline
                      (+ started-at internal-time-units-per-second)))
               (is-true (rplaca::cancel-stream-state state))
               (loop :while (and (bt:thread-alive-p reader)
                                 (< (get-internal-real-time) deadline))
                     :do (sleep 0.005))
               (is (< (/ (- (get-internal-real-time) started-at)
                         (float internal-time-units-per-second 1.0))
                      1.0)))
             (is-false (bt:thread-alive-p reader))
             (unless (bt:thread-alive-p reader)
               (rplaca::settle-stream-state-reader state))
             (bt:with-lock-held ((rplaca::stream-state-lock state))
               (is-true (rplaca::stream-state-cancelled-p state))
               (is (null (rplaca::stream-state-close-stream state)))
               (is (null (rplaca::stream-state-reader-thread state)))))
        (bt:signal-semaphore server-release)
        (when server-thread
          (loop :repeat 400
                :while (bt:thread-alive-p server-thread)
                :do (sleep 0.005))
          (unless (bt:thread-alive-p server-thread)
            (bt:join-thread server-thread)))
        (when (and reader (bt:thread-alive-p reader))
          ;; Server release closes the accepted socket and gives a failing old
          ;; implementation a bounded cleanup path after the assertion.
          (loop :repeat 400
                :while (bt:thread-alive-p reader)
                :do (sleep 0.005))
          (unless (bt:thread-alive-p reader)
            (bt:join-thread reader)))
        (when (and observed-stream (open-stream-p observed-stream))
          (ignore-errors
            (close observed-stream :abort t)))
        (when (and body (open-stream-p body))
          (ignore-errors
            (close body :abort t)))
        (ignore-errors
          (sb-bsd-sockets:socket-close listener))))))

(test streaming-entrypoint-returns-while-http-request-is-blocked
  "The streaming caller gets STATE while the managed worker is still connecting."
  (let ((request-entered
          (bt:make-semaphore :name "test-http-request-entered"))
        (request-release
          (bt:make-semaphore :name "test-http-request-release"))
        (callback-count 0)
        (callback-suppression-value :unset))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              "zai-key")
      (with-function-override (drakma:http-request (&rest args)
                                (declare (ignore args))
                                (bt:signal-semaphore request-entered)
                                (bt:wait-on-semaphore request-release
                                                      :timeout 2)
                                (values
                                 (make-string-input-stream
                                  (format nil "data: [DONE]~%~%"))
                                 200
                                 nil))
        (let* ((state
                 (let ((rplaca::*suppress-chat-redisplay-requests* t))
                   (rplaca::zai-request-streaming
                    nil
                    (lambda (ignored-state)
                      (declare (ignore ignored-state))
                      (setf callback-suppression-value
                            rplaca::*suppress-chat-redisplay-requests*)
                      (incf callback-count))
                    :model "glm-5")))
               (thread (stream-state-reader-thread-snapshot state)))
          (is-true thread)
          (is-true (bt:wait-on-semaphore request-entered :timeout 2))
          (is-false
           (bt:with-lock-held ((rplaca::stream-state-lock state))
             (rplaca::stream-state-done-p state)))
          (bt:signal-semaphore request-release)
          (rplaca::settle-stream-state-reader state)
          (bt:with-lock-held ((rplaca::stream-state-lock state))
            (is-true (rplaca::stream-state-done-p state))
            (is (null (rplaca::stream-state-error-p state)))
            (is (null (rplaca::stream-state-close-stream state)))
            (is (null (rplaca::stream-state-reader-thread state)))))))
    (is (= 1 callback-count))
    (is (null callback-suppression-value))))

(test cancel-before-connect-skips-provider-http-request
  "Cancellation during auth resolution prevents the HTTP connect from starting."
  (let ((auth-entered
          (bt:make-semaphore :name "test-stream-auth-entered"))
        (auth-release
          (bt:make-semaphore :name "test-stream-auth-release"))
        (http-calls 0)
        (callback-count 0))
    (with-function-override (rplaca::read-provider-token (provider)
                              (declare (ignore provider))
                              (bt:signal-semaphore auth-entered)
                              (bt:wait-on-semaphore auth-release :timeout 2)
                              "zai-key")
      (with-function-override (drakma:http-request (&rest args)
                                (declare (ignore args))
                                (incf http-calls)
                                (error "HTTP must not start after cancellation"))
        (let* ((state
                 (rplaca::zai-request-streaming
                  nil
                  (lambda (ignored-state)
                    (declare (ignore ignored-state))
                    (incf callback-count))
                  :model "glm-5"))
               (thread (stream-state-reader-thread-snapshot state)))
          (is-true thread)
          (is-true (bt:wait-on-semaphore auth-entered :timeout 2))
          (is-true (rplaca::cancel-stream-state state))
          (bt:signal-semaphore auth-release)
          (rplaca::settle-stream-state-reader state)
          (bt:with-lock-held ((rplaca::stream-state-lock state))
            (is-true (rplaca::stream-state-cancelled-p state))
            (is (null (rplaca::stream-state-reader-thread state)))))))
    (is (= 0 http-calls))
    (is (= 1 callback-count))))

(test openrouter-and-zai-non-200-stream-bodies-are-closed
  "Final non-200 streaming response bodies are closed exactly once."
  (dolist (provider '(:openrouter :zai))
    (let ((stream
            (make-instance 'controlled-character-input-stream
                           :contents "denied"))
          (callback-count 0))
      (with-function-override (rplaca::read-provider-token (ignored-provider)
                                (declare (ignore ignored-provider))
                                "provider-key")
        (with-function-override (drakma:http-request (&rest args)
                                  (declare (ignore args))
                                  (values stream 401 nil))
          (let* ((state
                   (ecase provider
                     (:openrouter
                      (rplaca::openrouter-request-streaming
                       nil
                       (lambda (ignored-state)
                         (declare (ignore ignored-state))
                         (incf callback-count))))
                     (:zai
                      (rplaca::zai-request-streaming
                       nil
                       (lambda (ignored-state)
                         (declare (ignore ignored-state))
                         (incf callback-count))))))
                 (thread (stream-state-reader-thread-snapshot state)))
            (is-true thread)
            (rplaca::settle-stream-state-reader state)
            (bt:with-lock-held ((rplaca::stream-state-lock state))
              (is-true (rplaca::stream-state-done-p state))
              (is (search "401"
                          (rplaca::stream-state-error-p state)))
              (is (null (rplaca::stream-state-close-stream state)))
              (is (null (rplaca::stream-state-reader-thread state)))))))
      (is (= 1 (controlled-stream-close-count stream)))
      (is (= 1 callback-count)))))

(test cancellation-wakes-held-open-non-200-stream-body
  "A final HTTP error body is registered before its first blocking read."
  (let ((stream
          (make-instance 'controlled-character-input-stream
                         :block-first-read-p t))
        (callback-count 0)
        (reader nil))
    (unwind-protect
         (with-function-override (rplaca::read-provider-token (provider)
                                   (declare (ignore provider))
                                   "provider-key")
           (with-function-override (drakma:http-request (&rest args)
                                     (declare (ignore args))
                                     (values stream 401 nil))
             (let* ((rplaca::*provider-http-max-retries* 0)
                    (state
                      (rplaca::openrouter-request-streaming
                       nil
                       (lambda (ignored-state)
                         (declare (ignore ignored-state))
                         (incf callback-count)))))
               (setf reader (stream-state-reader-thread-snapshot state))
               (is-true reader)
               (is-true
                (bt:wait-on-semaphore
                 (controlled-stream-read-entered stream)
                 :timeout 2))
               (bt:with-lock-held ((rplaca::stream-state-lock state))
                 (is (eq stream
                         (rplaca::stream-state-close-stream state))))
               (let* ((started-at (get-internal-real-time))
                      (deadline
                        (+ started-at internal-time-units-per-second)))
                 (is-true (rplaca::cancel-stream-state state))
                 (loop :while (and (bt:thread-alive-p reader)
                                   (< (get-internal-real-time) deadline))
                       :do (sleep 0.005))
                 (is (< (/ (- (get-internal-real-time) started-at)
                           (float internal-time-units-per-second 1.0))
                        1.0)))
               (is-false (bt:thread-alive-p reader))
               (unless (bt:thread-alive-p reader)
                 (rplaca::settle-stream-state-reader state))
               (bt:with-lock-held ((rplaca::stream-state-lock state))
                 (is-true (rplaca::stream-state-cancelled-p state))
                 (is (null (rplaca::stream-state-close-stream state)))
                 (is (null (rplaca::stream-state-reader-thread state)))))))
      (bt:signal-semaphore (controlled-stream-read-release stream))
      (when (and reader (bt:thread-alive-p reader))
        (rplaca::settle-stream-state-reader state))
      (unless (controlled-stream-closed-p stream)
        (close stream :abort t)))
    (is (= 1 (controlled-stream-close-count stream)))
    (is (= 1 callback-count))))

(test prompt-stream-state-callback-failure-cancels-and-settles-reader
  "A failing observer cannot strand the provider state it was given."
  (let* ((stream
           (make-instance 'controlled-character-input-stream
                          :block-first-read-p t))
         (buffer
           (make-buffer "prompt-stream-callback-failure"
                        :agent-name "agent"
                        :session-persistence-mode :ephemeral))
         (captured-state nil)
         (captured-reader nil)
         (observed-condition nil))
    (set-buffer-provider-override buffer :openrouter)
    (set-buffer-model-override buffer "e2e-model")
    (unwind-protect
         (with-function-override
             (rplaca::provider-request-streaming
              (provider messages callback &rest args)
              (declare (ignore provider messages args))
              (setf captured-state
                    (rplaca::make-stream-state :callback callback))
              (rplaca::start-stream-state-reader-worker
               captured-state callback
               "test-prompt-callback-owned-reader"
               (lambda (worker-state)
                 (when (rplaca::register-stream-state-stream
                        worker-state stream)
                   (read-char stream nil nil))))
              (setf captured-reader
                    (stream-state-reader-thread-snapshot captured-state))
              captured-state)
           (handler-case
               (rplaca::prompt-request-once
                buffer
                :stream-state-callback
                (lambda (state)
                  (declare (ignore state))
                  (error "simulated stream observer failure")))
             (error (condition)
               (setf observed-condition condition))))
      (bt:signal-semaphore (controlled-stream-read-release stream))
      (when captured-reader
        (rplaca::settle-stream-state-reader captured-state))
      (unless (controlled-stream-closed-p stream)
        (close stream :abort t)))
    (is-true observed-condition)
    (is (search "simulated stream observer failure"
                (princ-to-string observed-condition)))
    (is-true captured-state)
    (is-true captured-reader)
    (is-false (bt:thread-alive-p captured-reader))
    (bt:with-lock-held ((rplaca::stream-state-lock captured-state))
      (is-true (rplaca::stream-state-cancelled-p captured-state))
      (is-true (rplaca::stream-state-done-p captured-state))
      (is (null (rplaca::stream-state-close-stream captured-state)))
      (is (null (rplaca::stream-state-reader-thread captured-state))))
    (is (= 1 (controlled-stream-close-count stream)))))
