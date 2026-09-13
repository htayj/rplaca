(in-package :rplaca/tests)

(in-suite rplaca-suite)

(defun test-tool-use (id name &optional (input nil))
  "Return one canonical tool call for runtime lifecycle tests."
  (rplaca::canonical-tool-use-block id name input))

(defun install-terminal-tool-stream (buffer tool-name)
  "Install a provider-complete TOOL_NAME response that the frame has not polled."
  (let ((state
          (rplaca::make-stream-state
           :done-p t
           :stop-reason "tool_use"
           :content-blocks
           (list (test-tool-use "terminal-call" tool-name))))
        (message
          (rplaca::buffer-insert-agent-message
           buffer "" :record-p nil :run-hook-p nil)))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (buffer-pending-stream buffer) state
            (buffer-streaming-message buffer) message
            (buffer-status buffer) :thinking))
    (values state message)))

(test stop-and-teardown-never-run-tools-from-an-already-terminal-stream
  "Cancellation intent wins even when provider EOF arrived before frame polling."
  (dolist (tool-name '("read" "request_user_input"))
    (dolist (action '(:stop :teardown))
      (let ((buffer (make-buffer
                     (format nil "terminal-~A-~A" tool-name action)
                     :agent-name "agent"
                     :session-persistence-mode :ephemeral))
            (tool-call-starts 0))
        (multiple-value-bind (state message)
            (install-terminal-tool-stream buffer tool-name)
          (with-function-override
              (rplaca::begin-tool-calls (ignored-buffer tool-uses)
                (declare (ignore ignored-buffer tool-uses))
                (incf tool-call-starts))
            (ecase action
              (:stop
               (is-true (rplaca::stop-streaming-response buffer)))
              (:teardown
               (is (eq buffer
                       (rplaca::cancel-buffer-runtime-operations buffer)))))
            (is-true
             (rplaca::deliver-buffer-runtime-stopped-notification buffer)))
          (is (= 0 tool-call-starts))
          (is (null (buffer-pending-stream buffer)))
          (is (null (buffer-streaming-message buffer)))
          (is (null (buffer-pending-tool-calls buffer)))
          (is (eq :idle (buffer-status buffer)))
          (is (eq :system (message-sender message)))
          (is (search "stopped" (string-downcase (message-text message))))
          ;; The provider state may have become terminal before cancellation;
          ;; the proof is that application ownership and continuation are gone.
          (is-true (rplaca::stream-state-done-p state)))))))

(test stop-teardown-wins-after-terminal-snapshot-before-stream-claim
  "Stop atomically detaches a terminal owner before a stale updater can claim."
  (let* ((buffer (make-buffer "terminal-stop-before-claim"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (at-claim
           (bt:make-semaphore :name "terminal-stop-at-claim"))
         (release-claim
           (bt:make-semaphore :name "terminal-stop-release-claim"))
         (original-claim
           (symbol-function 'rplaca::claim-buffer-runtime-application))
         (advance-calls 0)
         (continuation-calls 0)
         (updater nil)
         (updater-result :not-run)
         (updater-error nil))
    (multiple-value-bind (state message)
        (install-terminal-tool-stream buffer "read")
      (declare (ignore state))
      (with-function-override
          (rplaca::claim-buffer-runtime-application
              (claim-buffer kind subject &optional auxiliary)
            (when (and (eq kind :stream)
                       (eq (bt:current-thread) updater))
              ;; Terminal flags and reader settlement were already observed.
              (bt:signal-semaphore at-claim)
              (bt:wait-on-semaphore release-claim :timeout 5.0))
            (funcall original-claim claim-buffer kind subject auxiliary))
        (with-function-override
            (rplaca::advance-tool-calls (ignored-buffer)
              (declare (ignore ignored-buffer))
              (incf advance-calls))
          (with-function-override
              (rplaca::start-streaming-response (ignored-buffer)
                (declare (ignore ignored-buffer))
                (incf continuation-calls))
            (unwind-protect
                 (progn
                   (setf updater
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (setf updater-result
                                      (rplaca::update-streaming-response
                                       buffer))
                              (error (condition)
                                (setf updater-error condition))))
                          :name "terminal-stop-stale-updater"))
                   (is-true
                    (bt:wait-on-semaphore at-claim :timeout 2.0))
                   (let* ((started-at (get-internal-real-time))
                          (stop-result
                            (rplaca::stop-streaming-response buffer))
                          (elapsed
                            (/ (- (get-internal-real-time) started-at)
                               (float internal-time-units-per-second 1.0))))
                     (is-true stop-result)
                     (is (< elapsed 0.25)))
                   (is-true (bt:thread-alive-p updater))
                   (is (= 0 advance-calls))
                   (is (= 0 continuation-calls))
                   (is (null (buffer-pending-stream buffer)))
                   (is (null (buffer-streaming-message buffer)))
                   (is-true
                    (rplaca::deliver-buffer-runtime-stopped-notification
                     buffer))
                   (is (eq :system (message-sender message)))
                   (is (search "stopped"
                               (string-downcase (message-text message))))
                   (bt:signal-semaphore release-claim)
                   (bt:join-thread updater)
                   (setf updater nil)
                   (is (null updater-error))
                   (is-false updater-result)
                   (is (= 0 advance-calls))
                   (is (= 0 continuation-calls)))
              (bt:signal-semaphore release-claim)
              (when (and updater (bt:thread-alive-p updater))
                (bt:join-thread updater)))))))))

(test stop-returns-before-claimed-stream-application-and-blocks-continuation
  "Stop invalidates a claimed terminal application without waiting for it."
  (let* ((buffer (make-buffer "terminal-stop-claimed-application"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (application-entered
           (bt:make-semaphore :name "terminal-application-entered"))
         (release-application
           (bt:make-semaphore :name "terminal-application-release"))
         (original-record
           (symbol-function 'rplaca::record-buffer-message))
         (advance-calls 0)
         (continuation-calls 0)
         (updater nil)
         (updater-error nil))
    (multiple-value-bind (state message)
        (install-terminal-tool-stream buffer "read")
      (declare (ignore state))
      (with-function-override
          (rplaca::record-buffer-message (record-buffer recorded-message)
            (when (eq recorded-message message)
              (bt:signal-semaphore application-entered)
              (bt:wait-on-semaphore release-application :timeout 5.0))
            (funcall original-record record-buffer recorded-message))
        (with-function-override
            (rplaca::advance-tool-calls (ignored-buffer)
              (declare (ignore ignored-buffer))
              (incf advance-calls))
          (with-function-override
              (rplaca::start-streaming-response (ignored-buffer)
                (declare (ignore ignored-buffer))
                (incf continuation-calls))
            (unwind-protect
                 (progn
                   (setf updater
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (rplaca::update-streaming-response buffer)
                              (error (condition)
                                (setf updater-error condition))))
                          :name "terminal-stop-claimed-updater"))
                   (is-true
                    (bt:wait-on-semaphore application-entered :timeout 2.0))
                   (is-true (rplaca::buffer-runtime-application buffer))
                   (let* ((started-at (get-internal-real-time))
                          (stop-result
                            (rplaca::stop-streaming-response buffer))
                          (elapsed
                            (/ (- (get-internal-real-time) started-at)
                               (float internal-time-units-per-second 1.0))))
                     (is-true stop-result)
                     (is (< elapsed 0.25)))
                   (is-true (bt:thread-alive-p updater))
                   (is-true (rplaca::buffer-runtime-stopping-p buffer))
                   (is-true (rplaca::buffer-runtime-teardown buffer))
                   (is (= 0 advance-calls))
                   (is (= 0 continuation-calls))
                   (bt:signal-semaphore release-application)
                   (bt:join-thread updater)
                   (setf updater nil)
                   (is (null updater-error))
                   (is (= 0 advance-calls))
                   (is (= 0 continuation-calls))
                   (is (null
                        (rplaca::buffer-runtime-application buffer)))
                   (is-true
                    (rplaca::deliver-buffer-runtime-stopped-notification
                     buffer))
                   (is (null (rplaca::buffer-runtime-teardown buffer)))
                   (is-false (rplaca::buffer-runtime-stopping-p buffer))
                   (is (eq :idle (buffer-status buffer)))
                   (is-true
                    (find :tool-result
                          (test-buffer-history-messages buffer)
                          :key #'message-sender)))
              (bt:signal-semaphore release-application)
              (when (and updater (bt:thread-alive-p updater))
                (bt:join-thread updater)))))))))

(test teardown-waits-for-claimed-tool-application-and-blocks-continuation
  "A detached terminal tool remains owned until its frame-process apply exits."
  (let* ((buf (make-buffer "tool-application-teardown-race"
                           :agent-name "agent"
                           :session-persistence-mode :ephemeral))
         (generation (rplaca::buffer-runtime-generation buf))
         (state
           (rplaca::make-interactive-tool-execution
            :tool-name "race_tool"
            :tool-id "race-tool-1"
            :buffer-generation generation
            :result-text "completed-before-teardown"
            :done-p t))
         (application-entered
           (bt:make-semaphore :name "tool-application-entered"))
         (release-application
           (bt:make-semaphore :name "tool-application-release"))
         (teardown-returned
           (bt:make-semaphore :name "tool-teardown-returned"))
         (advance-calls 0)
         (update-result nil)
         (update-error nil)
         (teardown-error nil))
    (setf (buffer-pending-tool-calls buf)
          (list (test-tool-use "race-tool-1" "race_tool")))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buf))
      (setf (buffer-pending-tool-execution buf) state
            (buffer-status buf) :tool-running))
    (with-function-override
        (rplaca::interactive-tool-result-display (ignored-state result)
          (declare (ignore ignored-state result))
          (bt:signal-semaphore application-entered)
          (unless (bt:wait-on-semaphore release-application :timeout 5.0)
            (error "Timed out releasing claimed tool application"))
          "[race tool complete]")
      (with-function-override
          (rplaca::advance-tool-calls (ignored-buffer)
            (declare (ignore ignored-buffer))
            (incf advance-calls))
        (let ((updater
                (bt:make-thread
                 (lambda ()
                   (handler-case
                       (setf update-result
                             (rplaca::update-interactive-tool-execution buf))
                     (error (condition)
                       (setf update-error condition))))
                 :name "tool-application-race-updater"))
              (teardown nil))
          (unwind-protect
               (progn
                 (is-true
                  (bt:wait-on-semaphore application-entered :timeout 2.0))
                 (setf teardown
                       (bt:make-thread
                        (lambda ()
                          (handler-case
                              (rplaca::cancel-buffer-runtime-operations buf)
                            (error (condition)
                              (setf teardown-error condition)))
                          (bt:signal-semaphore teardown-returned))
                        :name "tool-application-race-teardown"))
                 (is-false
                  (bt:wait-on-semaphore teardown-returned :timeout 0.05))
                 (is-true (rplaca::buffer-runtime-stopping-p buf))
                 (bt:signal-semaphore release-application)
                 (bt:join-thread updater)
                 (bt:join-thread teardown)
                 (setf teardown nil)
                 (is (null update-error))
                 (is (null teardown-error))
                 (is-true update-result)
                 (is (= 0 advance-calls))
                 (is (null (rplaca::buffer-runtime-application buf)))
                 (is-true
                  (rplaca::deliver-buffer-runtime-stopped-notification buf))
                 (is (null (rplaca::buffer-runtime-teardown buf)))
                 (is-false (rplaca::buffer-runtime-stopping-p buf))
                 (let ((result-message
                         (find :tool-result
                               (test-buffer-history-messages buf)
                               :key #'message-sender)))
                   (is-true result-message)
                   (is (search "completed-before-teardown"
                               (cdr (assoc :content
                                           (first
                                            (message-raw-content
                                             result-message))))))))
            (bt:signal-semaphore release-application)
            (when (bt:thread-alive-p updater)
              (bt:join-thread updater))
            (when teardown
              (bt:join-thread teardown))))))))

(test teardown-waits-for-stream-application-and-completes-tool-protocol
  "Teardown cannot pass a claimed stream or strand its recorded tool_use."
  (let* ((buf (make-buffer "stream-application-teardown-race"
                           :agent-name "agent"
                           :session-persistence-mode :ephemeral))
         (application-entered
           (bt:make-semaphore :name "stream-application-entered"))
         (release-application
           (bt:make-semaphore :name "stream-application-release"))
         (teardown-returned
           (bt:make-semaphore :name "stream-teardown-returned"))
         (advance-calls 0)
         (update-error nil)
         (teardown-error nil)
         (original-record
           (symbol-function 'rplaca::record-buffer-message)))
    (multiple-value-bind (state message)
        (install-terminal-tool-stream buf "stream_race_tool")
      (declare (ignore state))
      (with-function-override
          (rplaca::record-buffer-message (buffer recorded-message)
            (when (eq recorded-message message)
              (bt:signal-semaphore application-entered)
              (unless (bt:wait-on-semaphore release-application :timeout 5.0)
                (error "Timed out releasing claimed stream application")))
            (funcall original-record buffer recorded-message))
        (with-function-override
            (rplaca::advance-tool-calls (ignored-buffer)
              (declare (ignore ignored-buffer))
              (incf advance-calls))
          (let ((updater
                  (bt:make-thread
                   (lambda ()
                     (handler-case
                         (rplaca::update-streaming-response buf)
                       (error (condition)
                         (setf update-error condition))))
                   :name "stream-application-race-updater"))
                (teardown nil))
            (unwind-protect
                 (progn
                   (is-true
                    (bt:wait-on-semaphore application-entered :timeout 2.0))
                   (setf teardown
                         (bt:make-thread
                          (lambda ()
                            (handler-case
                                (rplaca::cancel-buffer-runtime-operations buf)
                              (error (condition)
                                (setf teardown-error condition)))
                            (bt:signal-semaphore teardown-returned))
                          :name "stream-application-race-teardown"))
                   (is-false
                    (bt:wait-on-semaphore teardown-returned :timeout 0.05))
                   (is-true (rplaca::buffer-runtime-stopping-p buf))
                   (bt:signal-semaphore release-application)
                   (bt:join-thread updater)
                   (bt:join-thread teardown)
                   (setf teardown nil)
                   (is (null update-error))
                   (is (null teardown-error))
                   (is (= 0 advance-calls))
                   (is (null (rplaca::buffer-runtime-application buf)))
                   (is-true
                    (rplaca::deliver-buffer-runtime-stopped-notification buf))
                   (is (null (rplaca::buffer-runtime-teardown buf)))
                   (is-false (rplaca::buffer-runtime-stopping-p buf))
                   (let* ((result-message
                            (find :tool-result
                                  (test-buffer-history-messages buf)
                                  :key #'message-sender))
                          (blocks (and result-message
                                       (message-raw-content result-message))))
                     (is-true result-message)
                     (is (= 1 (length blocks)))
                     (is (string= "terminal-call"
                                  (cdr (assoc :tool--use--id
                                              (first blocks)))))))
              (bt:signal-semaphore release-application)
              (when (bt:thread-alive-p updater)
                (bt:join-thread updater))
              (when teardown
                (bt:join-thread teardown)))))))))

(test provider-start-losing-to-teardown-cannot-publish-late-state
  "A completed teardown invalidates an in-flight provider preparation generation."
  (let ((provider-entered
          (bt:make-semaphore :name "late-provider-entered"))
        (release-provider
          (bt:make-semaphore :name "late-provider-release"))
        (buffer (make-buffer "late-provider-publication"
                             :agent-name "agent"
                             :session-persistence-mode :ephemeral))
        (state nil)
        (start-result :unset))
    (with-function-override (rplaca::load-active-packages
                                (&key buffer agent-name)
                              (declare (ignore buffer agent-name))
                              nil)
      (with-function-override
          (rplaca::resolve-buffer-provider-and-model (ignored-buffer)
            (declare (ignore ignored-buffer))
            (values :e2e "e2e" nil))
        (with-function-override
            (rplaca::tool-definitions-for-api (&key buffer agent-name)
              (declare (ignore buffer agent-name))
              nil)
          (with-function-override
              (rplaca::build-conversation-messages (ignored-buffer)
                (declare (ignore ignored-buffer))
                nil)
            (with-function-override
                (rplaca::build-agent-system-prompt
                 (agent-name &key buffer)
                  (declare (ignore agent-name buffer))
                  "test prompt")
              (with-function-override
                  (rplaca::provider-request-streaming
                   (provider messages callback &rest arguments)
                    (declare (ignore provider messages callback arguments))
                    (setf state (rplaca::make-stream-state))
                    (bt:signal-semaphore provider-entered)
                    (unless (bt:wait-on-semaphore release-provider :timeout 3)
                      (error "Provider release barrier timed out"))
                    state)
                (let ((worker
                        (bt:make-thread
                         (lambda ()
                           (setf start-result
                                 (rplaca::start-streaming-response buffer)))
                         :name "late-provider-publication-test")))
                  (unwind-protect
                       (progn
                         (is-true
                          (bt:wait-on-semaphore provider-entered :timeout 3))
                         (rplaca::cancel-buffer-runtime-operations buffer)
                         (bt:signal-semaphore release-provider)
                         (bt:join-thread worker)
                         (is (null start-result))
                         (is (null (buffer-pending-stream buffer)))
                         (is (null (buffer-streaming-message buffer)))
                         (is (null
                              (rplaca::buffer-runtime-start-generation
                               buffer)))
                         (is-true
                          (rplaca::stream-state-cancel-requested-p-safe
                           state)))
                    (bt:signal-semaphore release-provider)))))))))))

(test oauth-start-losing-to-teardown-cannot-publish-late-flow
  "A buffer teardown invalidates OAuth construction before global publication."
  (let* ((oauth-entered
           (bt:make-semaphore :name "late-oauth-entered"))
         (release-oauth
           (bt:make-semaphore :name "late-oauth-release"))
         (buffer (make-buffer "late-oauth-publication"
                              :session-persistence-mode :ephemeral))
         (saved-pending rplaca::*openai-oauth-pending*)
         (saved-pending-lock rplaca::*openai-oauth-pending-lock*)
         (test-pending-lock (bt:make-lock "late-oauth-pending"))
         (flow nil)
         (start-result :unset)
         (start-error nil)
         (worker nil))
    (unwind-protect
         (progn
           ;; The command runs in a finite would-be frame owner.  Use process
           ;; bindings so its publication and this test inspect one registry.
           (setf rplaca::*openai-oauth-pending* nil
                 rplaca::*openai-oauth-pending-lock* test-pending-lock)
           (with-function-override
               (rplaca::start-openai-codex-oauth-login
                   (&key buffer open-browser-p thread-constructor)
                 (declare (ignore open-browser-p thread-constructor))
                 (setf flow
                       (rplaca::make-openai-oauth-flow :buffer buffer))
                 (bt:signal-semaphore oauth-entered)
                 (unless (bt:wait-on-semaphore release-oauth :timeout 5.0)
                   (error "OAuth release barrier timed out"))
                 flow)
             (setf worker
                   (bt:make-thread
                    (lambda ()
                      (handler-case
                          (setf start-result
                                (progn
                                  (rplaca::openai-codex-oauth-command buffer)
                                  :returned))
                        (error (condition)
                          (setf start-error condition))))
                    :name "late-oauth-publication-test"))
             (is-true
              (bt:wait-on-semaphore oauth-entered :timeout 2.0))
             (is (eq worker
                     (rplaca::buffer-runtime-start-owner buffer)))
             (rplaca::cancel-buffer-runtime-operations buffer)
             (is-true (rplaca::buffer-runtime-stopping-p buffer))
             (is (null
                  (rplaca::buffer-runtime-start-generation buffer)))
             (bt:signal-semaphore release-oauth)
             (bt:join-thread worker)
             (setf worker nil)
             (is (null start-error))
             (is (eq :returned start-result))
             (is (null (rplaca::openai-oauth-pending-flow)))
             (is-true
              (getf (rplaca::openai-oauth-flow-snapshot flow)
                    :cancelled-p))
             (is (null (rplaca::buffer-runtime-start-owner buffer)))
             (loop :repeat 400
                   :until
                   (let ((teardown
                           (rplaca::buffer-runtime-teardown buffer)))
                     (and teardown
                          (rplaca::buffer-runtime-teardown-frame-delivery-p
                           teardown)))
                   :do (sleep 0.005))
             (is-true
              (rplaca::deliver-buffer-runtime-stopped-notification buffer))
             (is (null (rplaca::openai-oauth-pending-flow)))
             (is (null (rplaca::buffer-runtime-teardown buffer)))
             (is-false (rplaca::buffer-runtime-stopping-p buffer))
             (is (eq :idle (buffer-status buffer)))))
      (bt:signal-semaphore release-oauth)
      (when (and worker (bt:thread-alive-p worker))
        (bt:join-thread worker))
      (let ((pending (rplaca::take-openai-oauth-pending-flow)))
        (when pending
          (ignore-errors
            (rplaca::cancel-openai-codex-oauth-login pending))
          (ignore-errors
            (rplaca::join-openai-oauth-flow-worker pending))))
      (setf rplaca::*openai-oauth-pending* saved-pending
            rplaca::*openai-oauth-pending-lock* saved-pending-lock))))

(test stop-during-provider-start-returns-before-owner-and-rejects-late-state
  "Stop transfers an in-flight start owner to teardown without waiting on it."
  (let ((provider-entered
          (bt:make-semaphore :name "stop-provider-entered"))
        (release-provider
          (bt:make-semaphore :name "stop-provider-release"))
        (buffer (make-buffer "stop-provider-preparation"
                             :agent-name "agent"
                             :session-persistence-mode :ephemeral))
        (state nil)
        (worker nil)
        (start-result :unset)
        (start-error nil))
    (with-function-override
        (rplaca::load-active-packages (&key buffer agent-name)
          (declare (ignore buffer agent-name))
          nil)
      (with-function-override
          (rplaca::resolve-buffer-provider-and-model (ignored-buffer)
            (declare (ignore ignored-buffer))
            (values :e2e "e2e" nil))
        (with-function-override
            (rplaca::tool-definitions-for-api (&key buffer agent-name)
              (declare (ignore buffer agent-name))
              nil)
          (with-function-override
              (rplaca::build-conversation-messages (ignored-buffer)
                (declare (ignore ignored-buffer))
                nil)
            (with-function-override
                (rplaca::build-agent-system-prompt
                    (agent-name &key buffer)
                  (declare (ignore agent-name buffer))
                  "test prompt")
              (with-function-override
                  (rplaca::provider-request-streaming
                      (provider messages callback &rest arguments)
                    (declare (ignore provider messages callback arguments))
                    (setf state (rplaca::make-stream-state))
                    (bt:signal-semaphore provider-entered)
                    (unless (bt:wait-on-semaphore release-provider
                                                  :timeout 5.0)
                      (error "Provider release barrier timed out"))
                    state)
                (unwind-protect
                     (progn
                       (setf worker
                             (bt:make-thread
                              (lambda ()
                                (handler-case
                                    (setf start-result
                                          (rplaca::start-streaming-response
                                           buffer))
                                  (error (condition)
                                    (setf start-error condition))))
                              :name "stop-provider-start-owner"))
                       (is-true
                        (bt:wait-on-semaphore provider-entered :timeout 2.0))
                       (is (eq worker
                               (rplaca::buffer-runtime-start-owner buffer)))
                       (let* ((started-at (get-internal-real-time))
                              (stop-result
                                (rplaca::stop-streaming-response buffer))
                              (elapsed
                                (/ (- (get-internal-real-time) started-at)
                                   (float internal-time-units-per-second 1.0))))
                         (is-true stop-result)
                         (is (< elapsed 0.25)))
                       (is-true (bt:thread-alive-p worker))
                       (is-true
                        (rplaca::buffer-runtime-stopping-p buffer))
                       (is-true
                        (rplaca::buffer-runtime-teardown buffer))
                       (is (null
                            (rplaca::buffer-runtime-start-generation
                             buffer)))
                       (is (null (buffer-pending-stream buffer)))
                       (bt:signal-semaphore release-provider)
                       (bt:join-thread worker)
                       (setf worker nil)
                       (loop :repeat 400
                             :until
                             (let ((teardown
                                     (rplaca::buffer-runtime-teardown buffer)))
                               (and teardown
                                    (rplaca::buffer-runtime-teardown-frame-delivery-p
                                     teardown)))
                             :do (sleep 0.005))
                       (is (null start-error))
                       (is (null start-result))
                       (is-true
                        (rplaca::stream-state-cancel-requested-p-safe state))
                       (is (null (buffer-pending-stream buffer)))
                       (is (null (buffer-streaming-message buffer)))
                       (is (null
                            (rplaca::buffer-runtime-start-owner buffer)))
                       (is-true
                        (rplaca::deliver-buffer-runtime-stopped-notification
                         buffer))
                       (is (null
                            (rplaca::buffer-runtime-teardown buffer)))
                       (is-false
                        (rplaca::buffer-runtime-stopping-p buffer))
                       (is (eq :idle (buffer-status buffer))))
                  (bt:signal-semaphore release-provider)
                  (when (and worker (bt:thread-alive-p worker))
                    (bt:join-thread worker)))))))))))

(test reentrant-stop-never-hands-current-start-owner-to-reaper
  "A current-thread start owner releases teardown itself and is never joined."
  (let* ((buffer (make-buffer "reentrant-stop-start-owner"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (generation (rplaca::buffer-runtime-generation buffer))
         (current (bt:current-thread)))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (rplaca::buffer-runtime-start-generation buffer) generation
            (rplaca::buffer-runtime-start-owner buffer) current
            (buffer-status buffer) :thinking))
    (let* ((started-at (get-internal-real-time))
           (stop-result (rplaca::stop-streaming-response buffer))
           (elapsed (/ (- (get-internal-real-time) started-at)
                       (float internal-time-units-per-second 1.0)))
           (teardown (rplaca::buffer-runtime-teardown buffer)))
      (is-true stop-result)
      (is (< elapsed 0.25))
      (is-true teardown)
      (is-false
       (rplaca::buffer-runtime-teardown-reaper-started-p teardown))
      (is-true
       (rplaca::buffer-runtime-teardown-workers-settled-p teardown))
      (is (eq current
              (rplaca::buffer-runtime-start-owner buffer)))
      (is-true (rplaca::buffer-runtime-stopping-p buffer)))
    (is-true
     (rplaca::release-buffer-stream-start buffer generation))
    (is (null (rplaca::buffer-runtime-start-owner buffer)))
    (is-true
     (rplaca::deliver-buffer-runtime-stopped-notification buffer))
    (is (null (rplaca::buffer-runtime-teardown buffer)))
    (is-false (rplaca::buffer-runtime-stopping-p buffer))
    (is (eq :idle (buffer-status buffer)))))

(test failed-runtime-reaper-spawn-is-retryable-by-next-stop
  "A constructor failure retains exact ownership and a later Stop starts it."
  (let* ((buffer (make-buffer "runtime-reaper-spawn-retry"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (release (bt:make-semaphore :name "runtime-reaper-retry-release"))
         (worker
           (bt:make-thread
            (lambda ()
              (bt:wait-on-semaphore release :timeout 5.0))
            :name "runtime-reaper-retry-owner"))
         (operation
           (rplaca::make-interactive-buffer-operation
            :kind :reaper-retry
            :buffer-generation (rplaca::buffer-runtime-generation buffer)
            :worker worker))
         (attempts 0)
         (constructor
           (lambda (function &key name)
             (incf attempts)
             (when (= attempts 1)
               (error "injected reaper constructor failure"))
             (bt:make-thread function :name name))))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (buffer-pending-interactive-operation buffer) operation
            (buffer-status buffer) :working))
    (let ((rplaca::*runtime-teardown-reaper-thread-constructor*
            constructor))
      (unwind-protect
           (progn
             (is-true (rplaca::stop-streaming-response buffer))
             (let ((teardown
                     (rplaca::buffer-runtime-teardown buffer)))
               (is-true teardown)
               (is (= 1 attempts))
               (is-false
                (rplaca::buffer-runtime-teardown-reaper-started-p
                 teardown))
               (is-false
                (rplaca::buffer-runtime-teardown-workers-settled-p
                 teardown)))
             (is-true (rplaca::buffer-runtime-stopping-p buffer))
             (is-true (bt:thread-alive-p worker))
             (is-true (rplaca::stop-streaming-response buffer))
             (is (= 2 attempts))
             (is-true
              (rplaca::buffer-runtime-teardown-reaper-started-p
               (rplaca::buffer-runtime-teardown buffer)))
             (bt:signal-semaphore release)
             (is-true
              (loop :repeat 400
                    :for teardown :=
                      (rplaca::buffer-runtime-teardown buffer)
                    :when (and teardown
                               (rplaca::buffer-runtime-teardown-frame-delivery-p
                                teardown))
                      :return t
                    :do (sleep 0.005)
                    :finally (return nil)))
             (is-true
              (rplaca::deliver-buffer-runtime-stopped-notification buffer))
             (is (null (rplaca::buffer-runtime-teardown buffer)))
             (is-false (rplaca::buffer-runtime-stopping-p buffer)))
        (bt:signal-semaphore release)
        (when (bt:thread-alive-p worker)
          (bt:join-thread worker))))))

(test reentrant-stop-from-current-reader-is-finished-by-separate-reaper
  "A finite current reader is retained until a different thread joins it."
  (let* ((buffer (make-buffer "reentrant-stop-current-reader"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (message
           (rplaca::buffer-insert-agent-message
            buffer "" :record-p nil :run-hook-p nil))
         (state (rplaca::make-stream-state))
         (allow-stop
           (bt:make-semaphore :name "current-reader-allow-stop"))
         (stop-returned
           (bt:make-semaphore :name "current-reader-stop-returned"))
         (allow-exit
           (bt:make-semaphore :name "current-reader-allow-exit"))
         (stop-result nil)
         (stop-error nil))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (buffer-pending-stream buffer) state
            (buffer-streaming-message buffer) message
            (buffer-status buffer) :thinking))
    (rplaca::start-stream-state-reader-worker
     state nil "reentrant-stop-current-reader"
     (lambda (ignored-state)
       (declare (ignore ignored-state))
       (bt:wait-on-semaphore allow-stop :timeout 5.0)
       (handler-case
           (setf stop-result
                 (rplaca::stop-streaming-response buffer))
         (error (condition)
           (setf stop-error condition)))
       (bt:signal-semaphore stop-returned)
       (bt:wait-on-semaphore allow-exit :timeout 5.0)))
    (unwind-protect
         (progn
           (let ((reader
                   (rplaca::stream-state-reader-thread-snapshot state)))
             (is-true reader)
             (bt:signal-semaphore allow-stop)
             (is-true
              (bt:wait-on-semaphore stop-returned :timeout 2.0))
             (is (null stop-error))
             (is-true stop-result)
             (is-true (bt:thread-alive-p reader))
             (let ((teardown
                     (rplaca::buffer-runtime-teardown buffer)))
               (is-true teardown)
               (is-true
                (rplaca::buffer-runtime-teardown-reaper-started-p teardown))
               (is-false
                (rplaca::buffer-runtime-teardown-workers-settled-p
                 teardown)))
             (is-true (rplaca::buffer-runtime-stopping-p buffer))
             (is (null (buffer-pending-stream buffer)))
             (bt:signal-semaphore allow-exit)
             (loop :repeat 400
                   :until
                   (let ((teardown
                           (rplaca::buffer-runtime-teardown buffer)))
                     (and teardown
                          (rplaca::buffer-runtime-teardown-frame-delivery-p
                           teardown)))
                   :do (sleep 0.005))
             (is-false (bt:thread-alive-p reader))
             (is (null (rplaca::stream-state-reader-thread state)))
             (is (null
                  (rplaca::stream-state-reader-settlement-thread state)))
             (is-true (rplaca::buffer-runtime-stopping-p buffer))
             (is (eq :cancelling (buffer-status buffer)))
             (is (eq :agent (message-sender message)))
             (is-true
              (rplaca::deliver-buffer-runtime-stopped-notification buffer))
             (is (null (rplaca::buffer-runtime-teardown buffer)))
             (is-false (rplaca::buffer-runtime-stopping-p buffer))
             (is (eq :idle (buffer-status buffer)))
             (is (eq :system (message-sender message)))))
      (bt:signal-semaphore allow-stop)
      (bt:signal-semaphore allow-exit)
      (rplaca::settle-stream-state-reader state))))

(test failed-disposal-preserves-buffer-and-allows-retry
  "A cancellation failure does not mark or remove a half-disposed buffer."
  (let* ((buffer (make-buffer "dispose-failure"
                              :session-persistence-mode :ephemeral))
         (other (make-buffer "dispose-failure-other"
                             :session-persistence-mode :ephemeral))
         (rplaca::*buffer-ring* (list buffer other)))
    (with-function-override
        (rplaca::cancel-buffer-runtime-operations (ignored-buffer)
          (declare (ignore ignored-buffer))
          (error "injected disposal cancellation failure"))
      (signals error (rplaca::kill-buffer-from-ring buffer)))
    (is-false (rplaca::buffer-disposed-p buffer))
    (is-false (rplaca::buffer-disposing-p buffer))
    (is-true (member buffer rplaca::*buffer-ring* :test #'eq))
    (is (eq other (rplaca::kill-buffer-from-ring buffer)))
    (is-true (rplaca::buffer-disposed-p buffer))
    (is-false (member buffer rplaca::*buffer-ring* :test #'eq))))

(test subagent-thread-creation-failure-settles-visible-handle
  "A failed runner spawn never leaves a registered subagent in :RUNNING."
  (with-subagent-registry-override ()
    (with-function-override
        (rplaca::make-subagent-worker-thread (function name)
          (declare (ignore function name))
          (error "injected subagent thread creation failure"))
      (signals error
        (rplaca:run-subagent-async "spawn failure")))
    (let ((handles (rplaca:list-subagents)))
      (is (= 1 (length handles)))
      (let ((handle (first handles)))
        (is (eq :failed (rplaca:subagent-status handle)))
        (is-true (rplaca:subagent-done-p handle))
        (is-true
         (getf (rplaca:subagent-snapshot handle) :worker-finished-p))
        (is (search "thread creation failure"
                    (rplaca:subagent-error handle)))))))

(test tool-thread-creation-failure-preserves-frame-and-protocol
  "A failed worker constructor becomes a tool result instead of a CLIM error."
  (with-tool-table-restored
    (rplaca:register-tool
     "worker_start_test"
     "Exercise managed worker constructor failure."
     '((:type . "object"))
     (lambda (arguments)
       (declare (ignore arguments))
       "unexpected"))
    (let* ((buf (make-buffer "tool-worker-start-failure"
                             :agent-name "agent"
                             :session-persistence-mode :ephemeral))
           (tool-use (test-tool-use "worker-start-call" "worker_start_test"))
           (continuations 0))
      (with-function-override
          (rplaca::make-interactive-tool-worker-thread
              (function name &key initial-bindings)
            (declare (ignore function name initial-bindings))
            (error "simulated interactive tool thread failure"))
        (with-function-override
            (rplaca::start-streaming-response (ignored-buffer)
              (declare (ignore ignored-buffer))
              (incf continuations))
          ;; Any constructor condition escaping the managed dispatch boundary
          ;; would fail the test here.
          (rplaca::begin-tool-calls buf (list tool-use))
          (let* ((state (buffer-pending-tool-execution buf))
                 (snapshot
                   (rplaca::interactive-tool-execution-snapshot state)))
            (is-true state)
            (is (null (rplaca::interactive-tool-execution-worker state)))
            (is-true (getf snapshot :done-p))
            (is (search "simulated interactive tool thread failure"
                        (or (getf snapshot :error) "")))
            (is-true (rplaca::update-interactive-tool-execution buf))
            (is (= 1 continuations))
            (is (null (buffer-pending-tool-calls buf)))
            (is (null (buffer-pending-tool-execution buf)))
            (let* ((result-message
                     (find :tool-result
                           (test-buffer-history-messages buf)
                           :key #'message-sender))
                   (block (first (message-raw-content result-message))))
              (is-true result-message)
              (is (string= "worker-start-call"
                           (cdr (assoc :tool--use--id block))))
              (is (search "simulated interactive tool thread failure"
                          (cdr (assoc :content block)))))))))))

(test cancelled-prompt-tool-loop-persists-one-result-per-tool-use
  "Prompt cancellation cannot leave a persisted assistant tool_use unresolved."
  (with-tool-table-restored
    (let ((cancel-p nil)
          (buf (make-buffer "prompt-tool-cancel-protocol"
                            :agent-name "agent"
                            :session-persistence-mode :ephemeral)))
      (rplaca:register-tool
       "prompt_protocol_test"
       "Return a deterministic prompt-mode result."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         "completed-result"))
      (let ((tool-uses
              (list (test-tool-use "prompt-call-1" "prompt_protocol_test")
                    (test-tool-use "prompt-call-2" "prompt_protocol_test"))))
        ;; RUN-PROMPT-BUFFER-LOOP persists this assistant message before it
        ;; enters EXECUTE-PROMPT-TOOL-CALLS.
        (rplaca::insert-agent-message-from-content buf tool-uses :agent)
        (signals rplaca::prompt-run-cancelled
          (rplaca::execute-prompt-tool-calls
           buf tool-uses
           :event-callback
           (lambda (event)
             (when (and (string= "tool.result" (getf event :event))
                        (string= "prompt-call-1" (getf event :id)))
               (setf cancel-p t)))
           :cancel-requested-p (lambda () cancel-p)))
        (let* ((history (test-buffer-history-messages buf))
               (result-message (find :tool-result history
                                     :key #'message-sender))
               (blocks (and result-message
                            (message-raw-content result-message))))
          (is-true result-message)
          (is (= 2 (length blocks)))
          (is (equal '("prompt-call-1" "prompt-call-2")
                     (mapcar (lambda (block)
                               (cdr (assoc :tool--use--id block)))
                             blocks)))
          (is (search "completed-result"
                      (cdr (assoc :content (first blocks)))))
          (is (search "Cancelled by user"
                      (cdr (assoc :content (second blocks)))))
          ;; The next provider payload contains adjacent assistant tool_use and
          ;; user tool_result messages with matching cardinality.
          (let* ((messages (rplaca::build-conversation-messages buf))
                 (assistant (first messages))
                 (tool-results (second messages)))
            (is (string= "assistant" (cdr (assoc :role assistant))))
            (is (string= "user" (cdr (assoc :role tool-results))))
            (is (= 2 (length (coerce (cdr (assoc :content assistant)) 'list))))
            (is (= 2 (length (coerce (cdr (assoc :content tool-results))
                                     'list))))))))))

(test interactive-tool-worker-does-not-block-frame-owner
  "A blocking allowed tool runs outside the caller that owns CLIM state."
  (with-tool-table-restored
    (let ((entered (bt:make-semaphore :name "tool-entered"))
          (release (bt:make-semaphore :name "tool-release"))
          (provider-resumed-p nil))
      (rplaca:register-tool
       "blocking_stability_test"
       "Wait on a deterministic test barrier."
       '((:type . "object"))
       (lambda (args)
         (declare (ignore args))
         (bt:signal-semaphore entered)
         (unless (bt:wait-on-semaphore release :timeout 5.0)
           (error "Timed out waiting for the test release barrier"))
         "worker-result"))
      (with-function-override
          (rplaca::start-streaming-response (buffer)
            (declare (ignore buffer))
            (setf provider-resumed-p t)
            :stubbed)
        (let ((buf (make-buffer "tool-worker" :agent-name "agent")))
          (unwind-protect
               (progn
                 (rplaca::begin-tool-calls
                  buf
                  (list (test-tool-use "call-1"
                                       "blocking_stability_test")))
                 ;; BEGIN-TOOL-CALLS has returned while the tool is blocked.
                 (is (bt:wait-on-semaphore entered :timeout 2.0))
                 (is (eq :tool-running (buffer-status buf)))
                 (is (rplaca:buffer-pending-tool-execution buf))
                 (is-false
                  (rplaca::update-interactive-tool-execution buf))
                 (bt:signal-semaphore release)
                 (loop :repeat 400
                       :for state :=
                         (rplaca:buffer-pending-tool-execution buf)
                       :until (and state
                                   (getf
                                    (rplaca::interactive-tool-execution-snapshot
                                     state)
                                    :done-p))
                       :do (sleep 0.005))
                 (is-true
                  (rplaca::update-interactive-tool-execution buf))
                 (is-true provider-resumed-p)
                 (is (null (rplaca:buffer-pending-tool-execution buf)))
                 (is (some (lambda (message)
                             (and (eq :tool-result
                                      (message-sender message))
                                  (search "worker-result"
                                          (message-text message))))
                           (test-buffer-history-messages buf))))
            ;; Harmless if the worker already consumed the permit.
            (bt:signal-semaphore release)))))))

(test background-tool-buffer-and-payload-effects-cross-only-at-update-boundary
  "Worker tools receive owned data; live buffer effects wait for CLIM apply."
  (with-tool-table-restored
    (let* ((entered (bt:make-semaphore :name "owned-tool-entered"))
           (release (bt:make-semaphore :name "owned-tool-release"))
           (nested (vector (list :label "live")))
           (input `((:payload . ,nested)))
           (live-buffer nil)
           (worker-buffer nil)
           (worker-saw-before-context-p nil))
      (rplaca:register-tool
       "owned_buffer_test"
       "Mutate only detached worker state."
       '((:type . "object"))
       (lambda (arguments)
         (setf worker-buffer rplaca::*current-tool-buffer*)
         (setf worker-saw-before-context-p
               (not (null
                     (some (lambda (message)
                             (search "before-hook context"
                                     (message-text message)))
                           (test-buffer-history-messages worker-buffer)))))
         (let ((worker-vector (cdr (assoc :payload arguments))))
           (setf (getf (aref worker-vector 0) :label) "worker"))
         (buffer-insert-system-message worker-buffer "deferred worker message")
         (bt:signal-semaphore entered)
         (unless (bt:wait-on-semaphore release :timeout 5.0)
           (error "Timed out waiting for owned tool release"))
         "owned-result"))
      (with-function-override
          (rplaca::start-streaming-response (ignored-buffer)
            (declare (ignore ignored-buffer))
            :stubbed)
        (setf live-buffer
              (make-buffer "owned-tool-buffer"
                           :agent-name "agent"
                           :session-persistence-mode :ephemeral))
        (unwind-protect
             (progn
               (let ((rplaca::*before-tool-hook*
                       (list
                        (lambda (name arguments)
                          (declare (ignore name arguments))
                          (buffer-insert-system-message
                           rplaca::*current-tool-buffer*
                           "before-hook context"
                           :record-p nil)))))
                 (rplaca::begin-tool-calls
                  live-buffer
                  (list (test-tool-use "owned-call"
                                       "owned_buffer_test"
                                       input))))
               (is-true (bt:wait-on-semaphore entered :timeout 2.0))
               (let* ((state (buffer-pending-tool-execution live-buffer))
                      (owned-input
                        (rplaca::interactive-tool-execution-tool-input state))
                      (owned-vector (cdr (assoc :payload owned-input))))
                 (is-true state)
                 (is-false (eq live-buffer worker-buffer))
                 (is-false (eq input owned-input))
                 (is-false (eq nested owned-vector))
                 (is-true worker-saw-before-context-p)
                 (is (string= "live" (getf (aref nested 0) :label)))
                 (is (string= "worker" (getf (aref owned-vector 0) :label)))
                 (is-false
                  (some (lambda (message)
                          (search "deferred worker message"
                                  (message-text message)))
                        (test-buffer-history-messages live-buffer)))
                 (bt:signal-semaphore release)
                 (loop :repeat 400
                       :while (bt:thread-alive-p
                               (rplaca::interactive-tool-execution-worker
                                state))
                       :do (sleep 0.005))
                 (is-true
                  (rplaca::update-interactive-tool-execution live-buffer))
                 (is-true
                  (some (lambda (message)
                          (search "deferred worker message"
                                  (message-text message)))
                        (test-buffer-history-messages live-buffer)))))
          (bt:signal-semaphore release))))))

(test cancelled-background-tool-drops-unapplied-buffer-effects
  "Cancellation invalidates immutable worker effects before frame application."
  (with-tool-table-restored
    (let ((entered (bt:make-semaphore :name "cancel-effect-entered"))
          (release (bt:make-semaphore :name "cancel-effect-release")))
      (rplaca:register-tool
       "cancel_effect_test"
       "Publish a buffer effect, then wait."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         (buffer-insert-system-message rplaca::*current-tool-buffer*
                                       "must never reach live buffer")
         (bt:signal-semaphore entered)
         (unless (bt:wait-on-semaphore release :timeout 5.0)
           (error "Timed out waiting for cancellation release"))
         "late-result"))
      (let ((buffer (make-buffer "cancel-effect"
                                 :agent-name "agent"
                                 :session-persistence-mode :ephemeral)))
        (unwind-protect
             (progn
               (rplaca::begin-tool-calls
                buffer
                (list (test-tool-use "cancel-effect-call"
                                     "cancel_effect_test")))
               (is-true (bt:wait-on-semaphore entered :timeout 2.0))
               (let ((state (buffer-pending-tool-execution buffer)))
                 (rplaca::cancel-interactive-tool-execution state)
                 (bt:signal-semaphore release)
                 (loop :repeat 400
                       :while (bt:thread-alive-p
                               (rplaca::interactive-tool-execution-worker
                                state))
                       :do (sleep 0.005))
                 (is-true
                  (rplaca::update-interactive-tool-execution buffer))
                 (is-false
                  (some (lambda (message)
                          (search "must never reach live buffer"
                                  (message-text message)))
                        (test-buffer-history-messages buffer)))))
          (bt:signal-semaphore release))))))

(test frame-owned-tool-executes-at-the-provider-update-boundary
  "Frame tools run only when the CLIM updater applies their pending state."
  (with-tool-table-restored
    (let ((calls 0)
          (execution-thread nil))
      (rplaca:register-tool
       "frame_owned_test"
       "Run only on the frame process."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         (incf calls)
         (setf execution-thread (bt:current-thread))
         (buffer-insert-system-message rplaca::*current-tool-buffer*
                                       "frame-owned effect")
         "frame-result")
       :execution :frame)
      (with-function-override
          (rplaca::start-streaming-response (ignored-buffer)
            (declare (ignore ignored-buffer))
            :stubbed)
        (let ((buffer (make-buffer "frame-owned"
                                   :agent-name "agent"
                                   :session-persistence-mode :ephemeral)))
          (rplaca::begin-tool-calls
           buffer
           (list (test-tool-use "frame-call" "frame_owned_test")))
          (is-true (buffer-pending-tool-execution buffer))
          (is (= 0 calls))
          (is (null execution-thread))
          (is-true (rplaca::update-interactive-tool-execution buffer))
          (is (= 1 calls))
          (is (eq execution-thread (bt:current-thread)))
          (is-true
           (some (lambda (message)
                   (search "frame-owned effect" (message-text message)))
                 (test-buffer-history-messages buffer)))
          (is-false
           (some (lambda (message)
                   (and (eq :tool-result (message-sender message))
                        (search "REFUSED"
                                (string-upcase (message-text message)))))
                 (test-buffer-history-messages buffer))))))))

(test command-only-tool-refuses-generic-managed-dispatch
  "Command-only tools cannot fall through to a background or frame worker."
  (with-tool-table-restored
    (let ((calls 0))
      (rplaca:register-tool
       "command_only_test"
       "Available only through a user command."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         (incf calls)
         "unexpected")
       :execution :command-only)
      (with-function-override
          (rplaca::start-streaming-response (ignored-buffer)
            (declare (ignore ignored-buffer))
            :stubbed)
        (let ((buffer (make-buffer "command-only"
                                   :agent-name "agent"
                                   :session-persistence-mode :ephemeral)))
          (rplaca::begin-tool-calls
           buffer
           (list (test-tool-use "command-only-call" "command_only_test")))
          (is (= 0 calls))
          (is (null (buffer-pending-tool-execution buffer)))
          (is (null (buffer-pending-tool-calls buffer)))
          (let ((result
                  (find :tool-result
                        (test-buffer-history-messages buffer)
                        :key #'message-sender)))
            (is-true result)
            (is (search "available only through its trusted user command"
                        (string-downcase (message-text result))))))))))

(test background-tool-before-hook-vetoes-on-frame-before-worker-launch
  "A signaling before hook remains a frame-owned veto with no tool side effect."
  (with-tool-table-restored
    (let ((tool-calls 0)
          (after-calls 0)
          (before-thread nil)
          (frame-thread (bt:current-thread)))
      (rplaca:register-tool
       "before_veto_test"
       "Must not run after a before-hook veto."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         (incf tool-calls)
         "unexpected"))
      (let ((rplaca::*before-tool-hook*
              (list (lambda (name arguments)
                      (declare (ignore name arguments))
                      (setf before-thread (bt:current-thread))
                      (error "deterministic before-hook veto"))))
            (rplaca::*after-tool-hook*
              (list (lambda (&rest arguments)
                      (declare (ignore arguments))
                      (incf after-calls)))))
        (with-function-override
            (rplaca::start-streaming-response (ignored-buffer)
              (declare (ignore ignored-buffer))
              :stubbed)
          (let ((buffer (make-buffer "before-veto"
                                     :agent-name "agent"
                                     :session-persistence-mode :ephemeral)))
            (rplaca::begin-tool-calls
             buffer
             (list (test-tool-use "veto-call" "before_veto_test")))
            (let* ((state (buffer-pending-tool-execution buffer))
                   (snapshot
                     (rplaca::interactive-tool-execution-snapshot state)))
              (is-true state)
              (is-true (getf snapshot :done-p))
              (is (null (rplaca::interactive-tool-execution-worker state)))
              (is (eq frame-thread before-thread))
              (is (= 0 tool-calls))
              (is (= 0 after-calls))
              (is (search "deterministic before-hook veto"
                          (or (getf snapshot :error) "")))
              (is-true (rplaca::update-interactive-tool-execution buffer))
              (is (= 0 tool-calls))
              (is (= 0 after-calls)))))))))

(test background-tool-settlement-wakeup-applies-after-held-worker-cleanup
  "The first early update installs one post-exit wake; no polling is required."
  (with-tool-table-restored
    (let ((cleanup-entered
            (bt:make-semaphore :name "tool-cleanup-entered"))
          (release-cleanup
            (bt:make-semaphore :name "tool-cleanup-release"))
          (settled-wake
            (bt:make-semaphore :name "tool-settled-wake"))
          (applied
            (bt:make-semaphore :name "tool-settled-applied"))
          (buffer nil)
          (pump nil)
          (automatic-result nil))
      (rplaca:register-tool
       "settlement_wakeup_test"
       "Return before the worker's held final notification."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         "settled-result"))
      (with-function-override
          (rplaca::wake-buffer-display-change (changed-buffer reason)
            (declare (ignore changed-buffer))
            (case reason
              (:tool-complete
               (unless (eq (bt:current-thread) pump)
                 (bt:signal-semaphore cleanup-entered)
                 (unless (bt:wait-on-semaphore release-cleanup :timeout 5.0)
                   (error "Timed out holding tool worker cleanup"))))
              (:tool-settled
               (bt:signal-semaphore settled-wake)))
            nil)
        (with-function-override
            (rplaca::start-streaming-response (ignored-buffer)
              (declare (ignore ignored-buffer))
              :stubbed)
          (setf buffer
                (make-buffer "settlement-wakeup"
                             :agent-name "agent"
                             :session-persistence-mode :ephemeral)
                pump
                (bt:make-thread
                 (lambda ()
                   (when (bt:wait-on-semaphore settled-wake :timeout 5.0)
                     (setf automatic-result
                           (rplaca::update-interactive-tool-execution buffer)))
                   (bt:signal-semaphore applied))
                 :name "simulated-clim-tool-event-pump"))
          (unwind-protect
               (progn
                 (rplaca::begin-tool-calls
                  buffer
                  (list (test-tool-use "settlement-call"
                                       "settlement_wakeup_test")))
                 (is-true
                  (bt:wait-on-semaphore cleanup-entered :timeout 2.0))
                 (let ((state (buffer-pending-tool-execution buffer)))
                   (is-true (getf
                             (rplaca::interactive-tool-execution-snapshot
                              state)
                             :done-p))
                   (is-true (bt:thread-alive-p
                             (rplaca::interactive-tool-execution-worker
                              state)))
                   ;; This first update must not detach a still-unwinding owner.
                   (is-false
                    (rplaca::update-interactive-tool-execution buffer))
                   (is (eq state (buffer-pending-tool-execution buffer)))
                   (bt:signal-semaphore release-cleanup)
                   ;; The joiner emits the only retry wake; the test never
                   ;; manually calls UPDATE a second time.
                   (is-true (bt:wait-on-semaphore applied :timeout 3.0))
                   (is-true automatic-result)
                   (is (null (buffer-pending-tool-execution buffer)))))
            (bt:signal-semaphore release-cleanup)
            (when pump
              (bt:join-thread pump))))))))

(test stopping-blocked-tool-retains-exclusive-runtime-ownership-until-exit
  "A cooperatively cancelling tool prevents a new provider from overlapping it."
  (with-tool-table-restored
    (let ((entered (bt:make-semaphore :name "stop-tool-entered"))
          (release (bt:make-semaphore :name "stop-tool-release")))
      (rplaca:register-tool
       "blocking_stop_test"
       "Wait on a deterministic stop barrier."
       '((:type . "object"))
       (lambda (arguments)
         (declare (ignore arguments))
         (bt:signal-semaphore entered)
         (unless (bt:wait-on-semaphore release :timeout 5)
           (error "Timed out waiting for the stop release barrier"))
         "late-tool-result"))
      (let ((buffer (make-buffer "stop-blocked-tool" :agent-name "agent")))
        (unwind-protect
             (progn
               (rplaca::begin-tool-calls
                buffer
                (list (test-tool-use "stop-call" "blocking_stop_test")
                      (test-tool-use "queued-call" "blocking_stop_test")))
               (is-true (bt:wait-on-semaphore entered :timeout 2))
               (let ((state
                       (rplaca:buffer-pending-tool-execution buffer)))
                 (is-true (rplaca::stop-streaming-response buffer))
                 (is (null
                      (rplaca:buffer-pending-tool-execution buffer)))
                 (is (member
                      state
                      (rplaca::buffer-runtime-teardown-tool-states
                       (rplaca::buffer-runtime-teardown buffer))
                      :test #'eq))
                 (is (eq :cancelling (buffer-status buffer)))
                 (is-false
                  (find :tool-result
                        (test-buffer-history-messages buffer)
                        :key #'message-sender))
                 (is (null
                      (rplaca::reserve-buffer-stream-start buffer)))
                 (bt:signal-semaphore release)
                 (loop :repeat 400
                       :until
                       (getf
                        (rplaca::interactive-tool-execution-snapshot state)
                        :done-p)
                       :do (sleep 0.005))
                 (is-true
                  (loop :repeat 400
                        :for teardown :=
                          (rplaca::buffer-runtime-teardown buffer)
                        :when (and teardown
                                   (rplaca::buffer-runtime-teardown-frame-delivery-p
                                    teardown))
                          :return t
                        :do (sleep 0.005)
                        :finally (return nil)))
                 (is-false
                  (rplaca::update-interactive-tool-execution buffer))
                 (is-true
                  (rplaca::deliver-buffer-runtime-stopped-notification
                   buffer))
                 (is (null
                      (rplaca:buffer-pending-tool-execution buffer)))
                 (let* ((result-message
                          (find :tool-result
                                (test-buffer-history-messages buffer)
                                :key #'message-sender))
                        (result-blocks
                          (and result-message
                               (message-raw-content result-message))))
                   (is-true result-message)
                   (is (= 2 (length result-blocks)))
                   (is (equal '("stop-call" "queued-call")
                              (mapcar
                               (lambda (block)
                                 (cdr (assoc :tool--use--id block)))
                               result-blocks))))
                 (let ((generation
                         (rplaca::reserve-buffer-stream-start buffer)))
                   (is (integerp generation))
                   (is-true
                    (rplaca::release-buffer-stream-start
                     buffer generation)))))
          (bt:signal-semaphore release))))))

(test disposed-buffer-ignores-late-tool-worker-completion
  "Disposal stays stopping until a blocked tool exits, then retires its result."
  (with-tool-table-restored
    (let ((entered (bt:make-semaphore :name "dispose-tool-entered"))
          (release (bt:make-semaphore :name "dispose-tool-release")))
      (rplaca:register-tool
       "blocking_dispose_test"
       "Wait on a deterministic disposal barrier."
       '((:type . "object"))
       (lambda (args)
         (declare (ignore args))
         (bt:signal-semaphore entered)
         (unless (bt:wait-on-semaphore release :timeout 5.0)
           (error "Timed out waiting for the disposal release barrier"))
         "late-result"))
      (let ((buf (make-buffer "dispose-tool" :agent-name "agent")))
        (unwind-protect
             (progn
               (rplaca::begin-tool-calls
                buf
                (list (test-tool-use "call-2" "blocking_dispose_test")))
               (is (bt:wait-on-semaphore entered :timeout 2.0))
               (let ((state (rplaca:buffer-pending-tool-execution buf)))
                 (rplaca:dispose-buffer buf)
                 ;; The bounded caller returns, but never falsely exposes this
                 ;; buffer as settled while arbitrary tool code still runs.
                 (is-false (rplaca:buffer-disposed-p buf))
                 (is-true (rplaca::buffer-disposing-p buf))
                 (is-true (rplaca::buffer-runtime-stopping-p buf))
                 (is (null (rplaca:buffer-pending-tool-execution buf)))
                 (is (= 1 (length (buffer-pending-tool-calls buf))))
                 (bt:signal-semaphore release)
                 (loop :repeat 400
                       :until (rplaca:buffer-disposed-p buf)
                       :do (sleep 0.005))
                 (is-true
                  (getf (rplaca::interactive-tool-execution-snapshot state)
                        :done-p))
                 (is-true (rplaca:buffer-disposed-p buf))
                 (is-false (rplaca::buffer-disposing-p buf))
                 (is-false (rplaca::buffer-runtime-stopping-p buf))
                 (is (null (buffer-pending-tool-calls buf)))
                 (is-false
                  (rplaca::update-interactive-tool-execution buf))
                 (let ((result-message
                         (find :tool-result
                               (test-buffer-history-messages buf)
                               :key #'message-sender)))
                   (is-true result-message)
                   (is (search "cancelled"
                               (message-text result-message)
                               :test #'char-equal)))
                 (is-false
                  (some (lambda (message)
                          (and (eq :tool-result (message-sender message))
                               (search "late-result" (message-text message))))
                        (test-buffer-history-messages buf)))))
          (bt:signal-semaphore release))))))

#+sbcl
(test disposed-buffer-closes-and-settles-oauth-listener
  "Buffer teardown closes its OAuth listener and releases the bound port."
  (let* ((buf (make-buffer "dispose-oauth" :agent-name "agent"))
         (flow (rplaca::start-openai-codex-oauth-login
                :buffer buf
                :open-browser-p nil))
         (port (rplaca::openai-oauth-flow-port flow))
         (thread (rplaca::openai-oauth-flow-thread flow))
         (waiter
           (rplaca::ensure-openai-oauth-settlement-waiter flow thread)))
    (unwind-protect
         (progn
           (setf rplaca::*openai-oauth-pending* flow)
           (rplaca:dispose-buffer buf)
           (is-true
            (getf (rplaca::openai-oauth-flow-snapshot flow)
                  :cancelled-p))
           (loop :repeat 400
                 :while (bt:thread-alive-p thread)
                 :do (sleep 0.005))
           (is-false (bt:thread-alive-p thread))
           (bt:join-thread thread)
           (is-false (bt:thread-alive-p waiter))
           (bt:join-thread waiter)
           (is (null (rplaca::openai-oauth-flow-thread flow)))
           (is (null
                (rplaca::openai-oauth-flow-settlement-thread flow)))
           (multiple-value-bind (listener rebound-port)
               (rplaca::bind-openai-oauth-listener port)
             (unwind-protect
                  (is (= port rebound-port))
               (ignore-errors
                 (sb-bsd-sockets:socket-close listener)))))
      (unless (getf (rplaca::openai-oauth-flow-snapshot flow) :done-p)
        (rplaca::cancel-openai-codex-oauth-login flow))
      (when (and waiter (bt:thread-alive-p waiter))
        (bt:join-thread waiter)))))
