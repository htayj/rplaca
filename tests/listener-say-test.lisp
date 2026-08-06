(in-package :rplaca/tests)

(in-suite listener-say-suite)

(defmacro with-listener-say-function ((name function) &body body)
  (let ((existed-p (gensym "EXISTED-P"))
        (original (gensym "ORIGINAL")))
    `(let ((,existed-p (fboundp ',name))
           (,original (and (fboundp ',name) (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name) ,function)
              ,@body)
         (if ,existed-p
             (setf (symbol-function ',name) ,original)
             (fmakunbound ',name))))))

(defmacro with-listener-say-functions (bindings &body body)
  (if bindings
      `(with-listener-say-function ,(first bindings)
         (with-listener-say-functions ,(rest bindings) ,@body))
      `(progn ,@body)))

(defun make-listener-say-buffer (&key (name "listener-say-test")
                                      (session-persistence-mode :ephemeral)
                                      pipeline-name)
  (make-buffer name
               :agent-name "test-agent"
               :pipeline-name pipeline-name
               :session-persistence-mode session-persistence-mode))

(defun make-listener-say-frame (buffer)
  (make-instance 'rplaca::rplaca-listener
                 :conversation-buffer buffer
                 :listener-context (rplaca::make-listener-context)))

(defun finalized-messages (buffer)
  (loop :for message := (buffer-first-message buffer)
          :then (message-next message)
        :until (eq message (buffer-input-message buffer))
        :collect message))

(defun count-substring-in-listener-output (needle text)
  (loop :with count := 0
        :with start := 0
        :for position := (search needle text :start2 start)
        :while position
        :do (incf count)
            (setf start (+ position (length needle)))
        :finally (return count)))

(defun make-listener-say-pipeline-result
    (pipeline-name final-text &key tool-events reasoning provider model think-level
                                    (iterations 1) usage)
  (let* ((prompt-result
           (rplaca::make-prompt-run-result
            :prompt "final stage"
            :final-text final-text
            :tool-events tool-events
            :reasoning-blocks reasoning
            :agent-name "final-stage"
            :provider provider
            :model model
            :think-level think-level
            :iterations iterations
            :usage usage))
         (stage-result
           (rplaca::make-pipeline-stage-result
            :stage-name "final-stage"
            :prompt "final stage"
            :result prompt-result
            :status :succeeded)))
    (rplaca::make-pipeline-run-result
     :pipeline-name pipeline-name
     :original-prompt "question"
     :stage-results (list stage-result)
     :final-stage-result stage-result
     :status :succeeded)))

(defun call-com-say-with-output (frame source send-function await-function)
  (let ((output (make-string-output-stream)))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
             (declare (ignore ignored-frame))
             output))
         (rplaca::await-listener-agent-turn await-function))
      (with-listener-say-function
          (rplaca::send-prose-message
           (lambda (buffer text)
             (multiple-value-bind (result accepted-p)
                 (funcall send-function buffer text)
               (values result
                       accepted-p
                       (and accepted-p
                            (rplaca::listener-turn-boundary-for-message
                             (message-prev
                              (buffer-input-message buffer))))))))
        (let ((clim:*application-frame* frame))
          (values (rplaca::com-say source)
                  (get-output-stream-string output)))))))

(test listener-say-production-surface-is-registered
  (is (fboundp 'rplaca::dispatch-finalized-prose-input))
  (is (fboundp 'rplaca::send-prose-message))
  (is (fboundp 'rplaca::com-say))
  (is-true
   (clim:command-present-in-command-table-p
    'rplaca::com-say
    (clim:find-command-table 'rplaca::rplaca-listener))))

(test send-prose-message-finalizes-once-after-lazy-session-attachment
  (let* ((root (merge-pathnames
                (format nil "rplaca-todo8-~A/" (gensym)) #P"/tmp/"))
         (rplaca::*sessions-dir* (ensure-directories-exist root))
         (rplaca::*suppress-session-autosave* t)
         (rplaca::*before-send-message-hook* nil)
         (rplaca::*after-send-message-hook* nil)
         (rplaca::*after-message-insert-hook* nil)
         (buffer (make-listener-say-buffer
                  :name "todo8-lazy-session"
                  :session-persistence-mode :persistent))
         (before-count 0)
         (after-count 0)
         (insert-count 0)
         (provider-count 0)
         (session-present-at-insert nil)
         (provider-text nil)
         (exact-text (format nil "  /review~%!shell-looking  ")))
    (unwind-protect
         (progn
           (rplaca:add-hook
            'rplaca:*before-send-message-hook*
            (lambda (hook-buffer text)
              (is (eq buffer hook-buffer))
              (is (string= exact-text text))
              (incf before-count)))
           (rplaca:add-hook
            'rplaca:*after-send-message-hook*
            (lambda (hook-buffer text result)
              (is (eq buffer hook-buffer))
              (is (string= exact-text text))
              (is (eq :provider-started result))
              (incf after-count)))
           (rplaca:add-hook
            'rplaca:*after-message-insert-hook*
            (lambda (hook-buffer message)
              (when (eq buffer hook-buffer)
                (setf session-present-at-insert
                      (not (null (buffer-session hook-buffer))))
                (is (string= exact-text (message-text message)))
                (incf insert-count))))
           (with-listener-say-functions
               ((rplaca::start-interactive-compaction
                 (lambda (live-buffer &key reason continuation &allow-other-keys)
                   (declare (ignore live-buffer reason continuation))
                   (values nil nil 0 0)))
                (rplaca::send-to-agent-with-context
                 (lambda (live-buffer)
                   (incf provider-count)
                   (setf provider-text
                         (message-text
                          (message-prev (buffer-input-message live-buffer))))
                   :provider-started)))
             (multiple-value-bind (result accepted-p)
                 (rplaca::send-prose-message buffer exact-text)
               (is (eq :provider-started result))
               (is-true accepted-p)))
           (is (= 1 before-count))
           (is (= 1 after-count))
           (is (= 1 insert-count))
           (is (= 1 provider-count))
           (is-true session-present-at-insert)
           (is (buffer-session buffer))
           (is (string= exact-text provider-text))
           (is (= 1 (length (finalized-messages buffer))))
           (is (string= "" (message-text (buffer-input-message buffer)))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(test send-prose-message-bypasses-chat-rewriters-for-literal-input
  (dolist (text '("/review target" "/unknown target" "!shell-looking"
                  "?prefix-looking" "/template argument"))
    (let ((buffer (make-listener-say-buffer))
          (provider-text nil))
      (with-listener-say-functions
          ((rplaca::process-slash-command
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (error "slash processing must be bypassed")))
           (rplaca::expand-slash-template-input
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (error "template processing must be bypassed")))
           (rplaca::find-prefix-handler
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (error "prefix lookup must be bypassed")))
           (rplaca::process-prefix-command
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (error "prefix processing must be bypassed")))
           (rplaca::start-interactive-compaction
            (lambda (live-buffer &key reason continuation &allow-other-keys)
              (declare (ignore live-buffer reason continuation))
              (values nil nil 0 0)))
           (rplaca::send-to-agent-with-context
            (lambda (live-buffer)
              (setf provider-text
                    (message-text
                     (message-prev (buffer-input-message live-buffer))))
              :started)))
        (multiple-value-bind (result accepted-p)
            (rplaca::send-prose-message buffer text)
          (is (eq :started result))
          (is-true accepted-p)))
      (is (string= text provider-text) "literal prose changed for ~S" text))))

(test send-prose-message-compaction-continuation-is-the-sole-dispatch
  (let ((buffer (make-listener-say-buffer))
        (captured-continuation nil)
        (dispatch-count 0)
        (after-count 0)
        (operation (list :compaction-operation)))
    (let ((rplaca::*after-send-message-hook*
            (list (lambda (&rest ignored)
                    (declare (ignore ignored))
                    (incf after-count)))))
      (with-listener-say-functions
          ((rplaca::send-to-agent-with-context
            (lambda (live-buffer)
              (declare (ignore live-buffer))
              (incf dispatch-count)
              :started)))
        ;; Bind the continuation under a distinct keyword name in the stub.
        (with-listener-say-function
            (rplaca::start-interactive-compaction
             (lambda (live-buffer &key reason continuation &allow-other-keys)
               (declare (ignore live-buffer reason))
               (setf captured-continuation continuation)
               (values operation t 100 50)))
          (multiple-value-bind (result accepted-p)
              (rplaca::send-prose-message buffer "compact me")
            (is (eq operation result))
            (is-true accepted-p)))
        (is (= 0 dispatch-count))
        (is (= 0 after-count))
        (is (functionp captured-continuation))
        (funcall captured-continuation buffer)
        (is (= 1 dispatch-count))
        (is (= 1 after-count))))))

(test dispatch-finalized-prose-input-retains-managed-after-hook-contract
  (let* ((buffer (make-listener-say-buffer :pipeline-name "demo"))
         (operation (rplaca::make-interactive-buffer-operation :kind :pipeline))
         (after-count 0))
    (let ((rplaca::*after-send-message-hook*
            (list (lambda (&rest ignored)
                    (declare (ignore ignored))
                    (incf after-count)))))
      (with-listener-say-function
          (rplaca::start-interactive-pipeline-for-buffer
           (lambda (live-buffer prompt)
             (declare (ignore live-buffer prompt))
             operation))
        (is (eq operation
                (rplaca::dispatch-finalized-prose-input buffer "literal"))))
      (is (= 0 after-count)))))

(test send-prose-message-rejects-blank-and-busy-before-mutation
  (dolist (text (list "" (format nil " ~C~C" #\Tab #\Newline)))
    (let ((buffer (make-listener-say-buffer))
          (dispatch-count 0))
      (with-listener-say-function
          (rplaca::send-to-agent-with-context
           (lambda (ignored)
             (declare (ignore ignored))
             (incf dispatch-count)))
        (multiple-value-bind (result accepted-p)
            (rplaca::send-prose-message buffer text)
          (is (null result))
          (is-false accepted-p)))
      (is (= 0 dispatch-count))
      (is (= 1 (rplaca:buffer-message-count buffer)))))
  (let ((buffer (make-listener-say-buffer)))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (multiple-value-bind (result accepted-p)
        (rplaca::send-prose-message buffer "must not append")
      (is (null result))
      (is-false accepted-p))
    (is (= 1 (rplaca:buffer-message-count buffer)))
    (is (string= "" (message-text (buffer-input-message buffer))))))

(test com-say-expands-interpolation-before-one-send-and-one-await
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (send-count 0)
         (await-count 0)
         (sent-text nil))
    (multiple-value-bind (turn output)
        (call-com-say-with-output
         frame
         "sum ,(+ 1 2), pair ,(values 4 5), none ,(values), literal ,,(x)"
         (lambda (live-buffer text)
           (incf send-count)
           (setf sent-text text)
           (set-message-text (buffer-input-message live-buffer) text)
           (buffer-finalize-input live-buffer)
           (values :provider-started t))
         (lambda (live-frame live-buffer dispatch-result)
           (is (eq frame live-frame))
           (is (eq buffer live-buffer))
           (is (eq :provider-started dispatch-result))
           (incf await-count)
           (rplaca::buffer-insert-agent-message
            live-buffer "assistant body"
            :raw-content
            (list (rplaca::canonical-text-block "assistant body")
                  (rplaca::canonical-reasoning-block "reason")
                  (rplaca::canonical-tool-use-block
                   "tool-1" "read" '((:path . "README.md"))))
            :metadata '((:provider . :test) (:model . "model-x")
                        (:artifact-refs . ("artifact-1"))
                        (:media-refs . ("media-1"))))))
      (is (= 1 send-count))
      (is (= 1 await-count))
      (is (string= "sum 3, pair 4, none , literal ,(x)" sent-text))
      (is (typep turn 'rplaca::assistant-turn))
      (is (eq :complete (rplaca::assistant-turn-status turn)))
      (is (string= "assistant body"
                   (rplaca::assistant-turn-primary-text turn)))
      (is (= 1 (length (rplaca::assistant-turn-tool-uses turn))))
      (is (equal '("reason") (rplaca::assistant-turn-reasoning turn)))
      (is (equal '("artifact-1")
                 (rplaca::assistant-turn-artifact-refs turn)))
      (is (equal '("media-1") (rplaca::assistant-turn-media-refs turn)))
      (is (= 1 (count-substring-in-listener-output "assistant body" output)))
      (is (string= "assistant body" (string-trim '(#\Newline) output)))
      (is (eq turn (rplaca::rplaca-listener-pending-assistant-turn frame)))
      (is (= 3 (rplaca:buffer-message-count buffer))))))

(test com-say-interpolation-error-emits-once-and-mutates-nothing
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (send-count 0)
         (await-count 0))
    (multiple-value-bind (turn output)
        (call-com-say-with-output
         frame "broken ,(error \"interpolation failed\")"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (incf send-count))
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (incf await-count)))
      (is (null turn))
      (is (= 0 send-count))
      (is (= 0 await-count))
      (is (= 1 (rplaca:buffer-message-count buffer)))
      (is (= 1 (count-substring-in-listener-output
                "Could not interpolate prose" output))))))

(test com-say-error-and-cancel-each-assemble-and-emit-one-turn
  (dolist (case '(:error :cancelled))
    (let* ((buffer (make-listener-say-buffer))
           (frame (make-listener-say-frame buffer)))
      (multiple-value-bind (turn output)
          (call-com-say-with-output
           frame "question"
           (lambda (live-buffer text)
             (set-message-text (buffer-input-message live-buffer) text)
             (buffer-finalize-input live-buffer)
             (values :provider-started t))
           (lambda (live-frame live-buffer dispatch-result)
             (declare (ignore live-frame live-buffer dispatch-result))
             (ecase case
               (:error
                (error 'rplaca::prompt-run-error
                       :message "provider failed"
                       :iterations 2 :provider :test :model "m"
                       :think-level "high"))
               (:cancelled
                (error 'rplaca::prompt-run-cancelled)))))
        (is (eq case (rplaca::assistant-turn-status turn)))
        (is (= 1 (count-substring-in-listener-output
                  (rplaca::assistant-turn-primary-text turn) output)))
        (is (eq turn (rplaca::rplaca-listener-pending-assistant-turn frame)))
        (is (= 2 (rplaca:buffer-message-count buffer)))))))

(test com-say-busy-guard-precedes-interpolation-and-send
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (send-count 0)
         (await-count 0))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (multiple-value-bind (turn output)
        (call-com-say-with-output
         frame "must not eval ,(error \"interpolation ran\")"
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (incf send-count))
         (lambda (&rest ignored)
           (declare (ignore ignored))
           (incf await-count)))
      (is (null turn))
      (is (= 0 send-count))
      (is (= 0 await-count))
      (is (= 1 (rplaca:buffer-message-count buffer)))
      (is (= 1 (count-substring-in-listener-output
                "already active" output))))))

(test com-say-direct-provider-placeholder-is-after-the-user-boundary
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (placeholder nil))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::send-to-agent-with-context
          (lambda (live-buffer)
            (setf placeholder
                  (rplaca::buffer-insert-agent-message live-buffer ""))
            :direct-provider-state))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (is (eq frame live-frame))
            (is (eq buffer live-buffer))
            (is (eq :direct-provider-state dispatch-result))
            (set-message-text placeholder "direct provider answer")
            (setf (message-raw-content placeholder)
                  (list (rplaca::canonical-text-block
                         "direct provider answer"))))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "direct provider question")))
          (is (eq :complete (rplaca::assistant-turn-status turn)))
          (is (string= "direct provider answer"
                       (rplaca::assistant-turn-primary-text turn))))))
    (is (= 1 (count-substring-in-listener-output
              "direct provider answer"
              (get-output-stream-string output))))))

(test com-say-boundary-resolves-after-compaction-recreates-duplicate-users
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (captured-continuation nil)
         (old-user nil))
    (set-message-text (buffer-input-message buffer) "duplicate prompt")
    (buffer-finalize-input buffer)
    (setf old-user (message-prev (buffer-input-message buffer)))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason))
            (setf captured-continuation continuation)
            (values (list :compaction-operation) t 100 50)))
         (rplaca::send-to-agent-with-context
          (lambda (live-buffer)
            (rplaca::buffer-insert-agent-message
             live-buffer "post-compaction answer"
             :raw-content
             (list (rplaca::canonical-text-block
                    "post-compaction answer")))
            :direct-provider-state))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (let* ((current-user
                     (message-prev (buffer-input-message live-buffer)))
                   (timestamp (message-timestamp current-user))
                   (snapshots
                     (rplaca::collect-recent-user-message-snapshots
                      live-buffer 10000)))
              (setf (message-timestamp old-user) timestamp
                    (getf (first snapshots) :timestamp) timestamp)
              (rplaca::replace-buffer-with-compacted-history
               live-buffer "[compacted]" snapshots)
              (is (not (find current-user
                             (finalized-messages live-buffer)
                             :test #'eq)))
              (funcall captured-continuation live-buffer)))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "duplicate prompt")))
          (is (eq :complete (rplaca::assistant-turn-status turn)))
          (is (string= "post-compaction answer"
                       (rplaca::assistant-turn-primary-text turn))))))
    (is (= 1 (count-substring-in-listener-output
              "post-compaction answer"
              (get-output-stream-string output))))))

(test com-say-compaction-boundary-does-not-rebind-to-identical-next-user
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (captured-continuation nil)
         (recreated-original-user nil)
         (provider-dispatch-user nil)
         (handoff-count 0)
         (handoff-original
           (symbol-function
            'rplaca::handoff-listener-turn-boundary-after-compaction)))
    (set-message-text (buffer-input-message buffer) "identical prompt")
    (buffer-finalize-input buffer)
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::handoff-listener-turn-boundary-after-compaction
          (lambda (live-buffer boundary)
            (incf handoff-count)
            (funcall handoff-original live-buffer boundary)))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason))
            (setf captured-continuation continuation)
            (values (list :compaction-operation) t 100 50)))
         (rplaca::send-to-agent-with-context
          (lambda (live-buffer)
            (is (= 1 handoff-count))
            (setf provider-dispatch-user
                  (find :user (finalized-messages live-buffer)
                        :key #'message-sender :test #'eq :from-end t))
            (rplaca::buffer-insert-agent-message
             live-buffer "first-answer"
             :raw-content
             (list (rplaca::canonical-text-block "first-answer")))
            (set-message-text (buffer-input-message live-buffer)
                              "identical prompt")
            (buffer-finalize-input live-buffer)
            (setf (message-timestamp
                   (message-prev (buffer-input-message live-buffer)))
                  (message-timestamp provider-dispatch-user))
            (rplaca::buffer-insert-agent-message
             live-buffer "second-answer"
             :raw-content
             (list (rplaca::canonical-text-block "second-answer")))
            :direct-provider-state))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (let* ((original-user
                     (message-prev (buffer-input-message live-buffer)))
                   (timestamp (message-timestamp original-user))
                   (snapshots
                     (rplaca::collect-recent-user-message-snapshots
                      live-buffer 10000)))
              (dolist (snapshot snapshots)
                (when (string= "identical prompt" (getf snapshot :text))
                  (setf (getf snapshot :timestamp) timestamp)))
              (rplaca::replace-buffer-with-compacted-history
               live-buffer "[compacted]" snapshots)
              (setf recreated-original-user
                    (find :user (finalized-messages live-buffer)
                          :key #'message-sender :test #'eq :from-end t))
              (funcall captured-continuation live-buffer)))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "identical prompt")))
          (is (= 1 handoff-count))
          (is (eq recreated-original-user provider-dispatch-user))
          (is (string= "first-answer"
                       (rplaca::assistant-turn-primary-text turn)))
          (is (null (search "second-answer"
                            (rplaca::assistant-turn-primary-text turn)))))))
    (let ((rendered (get-output-stream-string output)))
      (is (= 1 (count-substring-in-listener-output
                "first-answer" rendered)))
      (is (= 0 (count-substring-in-listener-output
                "second-answer" rendered))))))

(test com-say-includes-pipeline-stage-agent-assistant-role
  (let* ((buffer (make-listener-say-buffer :pipeline-name "review"))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (operation (rplaca::make-interactive-buffer-operation :kind :pipeline)))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::start-interactive-pipeline-for-buffer
          (lambda (live-buffer prompt)
            (declare (ignore live-buffer prompt))
            operation))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame))
            (is (eq operation dispatch-result))
            (rplaca::buffer-insert-read-only-message
             live-buffer :review-stage "stage agent answer"
             :raw-content
             (list (rplaca::canonical-text-block "stage agent answer"))))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "pipeline question")))
          (is (eq :complete (rplaca::assistant-turn-status turn)))
          (is (string= "stage agent answer"
                       (rplaca::assistant-turn-primary-text turn))))))
    (is (= 1 (count-substring-in-listener-output
              "stage agent answer"
              (get-output-stream-string output))))))

(test com-say-consumes-custom-pipeline-runner-canonical-result
  (let* ((buffer (make-listener-say-buffer :pipeline-name "custom"))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (operation (rplaca::make-interactive-buffer-operation :kind :pipeline))
         (prompt-result
           (rplaca::make-prompt-run-result
            :prompt "custom question"
            :final-text "custom runner answer"
            :reasoning-blocks (list "custom reason")
            :agent-name "custom-stage"
            :provider :test
            :model "test-model"
            :iterations 1
            :stop-reason "pipeline_stage_complete"))
         (stage-result
           (rplaca::make-pipeline-stage-result
            :stage-name "custom-stage"
            :prompt "custom question"
            :result prompt-result
            :status :succeeded))
         (run-result
           (rplaca::make-pipeline-run-result
            :pipeline-name "custom"
            :original-prompt "custom question"
            :stage-results (list stage-result)
            :final-stage-result stage-result
            :status :succeeded)))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::start-interactive-pipeline-for-buffer
          (lambda (live-buffer prompt)
            (declare (ignore live-buffer prompt))
            operation))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (rplaca::run-hook-with-args
             'rplaca:*after-send-message-hook*
             live-buffer "custom question" run-result))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "custom question")))
          (is (eq :complete (rplaca::assistant-turn-status turn)))
          (is (string= "custom runner answer"
                       (rplaca::assistant-turn-primary-text turn)))
          (is (equal '("custom reason")
                     (rplaca::assistant-turn-reasoning turn))))))
    (is (= 1 (count-substring-in-listener-output
              "custom runner answer"
              (get-output-stream-string output))))))

(test com-say-final-canonical-pipeline-result-outranks-buffered-stage-response
  (let* ((buffer (make-listener-say-buffer :pipeline-name "mixed"))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (operation (rplaca::make-interactive-buffer-operation :kind :pipeline))
         (first-prompt-result
           (rplaca::make-prompt-run-result
            :prompt "first stage"
            :final-text "stale first-stage answer"
            :tool-events
            (list (rplaca::make-prompt-tool-event
                   :id "first-tool" :name "read"
                   :input '((:path . "first"))))
            :reasoning-blocks (list "first canonical reason")
            :agent-name "first-stage"
            :iterations 1))
         (final-prompt-result
           (rplaca::make-prompt-run-result
            :prompt "final stage"
            :final-text "final custom answer"
            :tool-events
            (list (rplaca::make-prompt-tool-event
                   :id "final-tool" :name "write"
                   :input '((:path . "final"))))
            :reasoning-blocks (list "final canonical reason")
            :agent-name "final-stage"
            :provider :test
            :model "test-model"
            :iterations 1))
         (first-stage-result
           (rplaca::make-pipeline-stage-result
            :stage-name "first-stage" :prompt "first stage"
            :result first-prompt-result :status :succeeded))
         (final-stage-result
           (rplaca::make-pipeline-stage-result
            :stage-name "final-stage" :prompt "final stage"
            :result final-prompt-result :status :succeeded))
         (run-result
           (rplaca::make-pipeline-run-result
            :pipeline-name "mixed"
            :original-prompt "mixed question"
            :stage-results (list first-stage-result final-stage-result)
            :final-stage-result final-stage-result
            :status :succeeded)))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::start-interactive-pipeline-for-buffer
          (lambda (live-buffer prompt)
            (declare (ignore live-buffer prompt))
            operation))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (rplaca::buffer-insert-read-only-message
             live-buffer :first-stage "stale first-stage answer"
             :raw-content
             (list (rplaca::canonical-text-block "stale first-stage answer")
                   (rplaca::canonical-reasoning-block "buffered stale reason")
                   (rplaca::canonical-tool-use-block
                    "first-tool" "read" '((:path . "first")))))
            (rplaca::run-hook-with-args
             'rplaca:*after-send-message-hook*
             live-buffer "mixed question" run-result))))
      (let ((clim:*application-frame* frame))
        (let* ((turn (rplaca::com-say "mixed question"))
               (tool-names
                 (mapcar (lambda (block) (getf block :name))
                         (rplaca::assistant-turn-tool-uses turn))))
          (is (string= "final custom answer"
                       (rplaca::assistant-turn-primary-text turn)))
          (is (equal '("first canonical reason" "final canonical reason")
                     (rplaca::assistant-turn-reasoning turn)))
          (is (equal '("read" "write") tool-names)))))
    (let ((text (get-output-stream-string output)))
      (is (= 1 (count-substring-in-listener-output
                "final custom answer" text)))
      (is (= 0 (count-substring-in-listener-output
                "stale first-stage answer" text))))))

(test com-say-turn-range-stops-at-next-finalized-user
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (first-placeholder nil))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::send-to-agent-with-context
          (lambda (live-buffer)
            (setf first-placeholder
                  (rplaca::buffer-insert-agent-message live-buffer ""))
            :provider-state))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (set-message-text first-placeholder "first turn answer")
            (setf (message-raw-content first-placeholder)
                  (list (rplaca::canonical-text-block "first turn answer")))
            (set-message-text (buffer-input-message live-buffer)
                              "queued second question")
            (buffer-finalize-input live-buffer)
            (rplaca::buffer-insert-agent-message
             live-buffer "second turn answer"
             :raw-content
             (list (rplaca::canonical-text-block "second turn answer"))))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "first question")))
          (is (string= "first turn answer"
                       (rplaca::assistant-turn-primary-text turn))))))
    (let ((text (get-output-stream-string output)))
      (is (= 1 (count-substring-in-listener-output "first turn answer" text)))
      (is (= 0 (count-substring-in-listener-output "second turn answer" text))))))

(test com-say-blank-canonical-pipeline-primary-keeps-canonical-facets
  (let* ((buffer (make-listener-say-buffer :pipeline-name "blank-final"))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream))
         (operation (rplaca::make-interactive-buffer-operation :kind :pipeline))
         (canonical-event
           (rplaca::make-prompt-tool-event
            :id "canonical-tool" :name "write"
            :input '((:path . "canonical"))))
         (run-result
           (make-listener-say-pipeline-result
            "blank-final" "   "
            :tool-events (list canonical-event)
            :reasoning (list "canonical aggregate reason")
            :provider :canonical-provider
            :model "canonical-model")))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            output))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::start-interactive-pipeline-for-buffer
          (lambda (live-buffer prompt)
            (declare (ignore live-buffer prompt))
            operation))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (rplaca::buffer-insert-read-only-message
             live-buffer :buffered-stage "buffered fallback answer"
             :raw-content
             (list (rplaca::canonical-text-block "buffered fallback answer")
                   (rplaca::canonical-reasoning-block "buffered stale reason")
                   (rplaca::canonical-tool-use-block
                    "buffered-tool" "read" '((:path . "buffered"))))
             :metadata '((:provider . :buffered-provider)))
            (rplaca::run-hook-with-args
             'rplaca:*after-send-message-hook*
             live-buffer "blank canonical question" run-result))))
      (let ((clim:*application-frame* frame))
        (let ((turn (rplaca::com-say "blank canonical question")))
          (is (string= "buffered fallback answer"
                       (rplaca::assistant-turn-primary-text turn)))
          (is (equal '("write")
                     (mapcar (lambda (use) (getf use :name))
                             (rplaca::assistant-turn-tool-uses turn))))
          (is (equal '("canonical aggregate reason")
                     (rplaca::assistant-turn-reasoning turn)))
          (is (eq :canonical-provider
                  (getf (rplaca::assistant-turn-metadata turn) :provider)))
          (is (rplaca::prompt-run-result-p
               (rplaca::assistant-turn-inspect-payload turn))))))))

(test assistant-turn-tool-uses-are-renderer-plists-with-complete-payload
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (output (make-string-output-stream)))
    (multiple-value-bind (turn ignored-output)
        (call-com-say-with-output
         frame "tool question"
         (lambda (live-buffer text)
           (set-message-text (buffer-input-message live-buffer) text)
           (buffer-finalize-input live-buffer)
           (values :started t))
         (lambda (live-frame live-buffer dispatch-result)
           (declare (ignore live-frame dispatch-result))
           (rplaca::buffer-insert-agent-message
            live-buffer "tool answer"
            :raw-content
            (list (rplaca::canonical-text-block "tool answer")
                  (rplaca::canonical-tool-use-block
                   "tool-id" "read" '((:path . "README.md")))))))
      (declare (ignore ignored-output))
      (let ((use (first (rplaca::assistant-turn-tool-uses turn))))
        (is (string= "tool-id" (getf use :id)))
        (is (string= "read" (getf use :name)))
        (is (equal '((:path . "README.md")) (getf use :input))))
      (rplaca::render-listener-tool-facets output turn)
      (let ((rendered (get-output-stream-string output)))
        (is (search "read" rendered))
        (is (null (search "unknown" rendered)))))))

(test assistant-turn-metadata-is-unique-and-canonical-result-wins
  (let* ((buffer (make-listener-say-buffer :pipeline-name "metadata-pipeline"))
         (frame (make-listener-say-frame buffer))
         (operation (rplaca::make-interactive-buffer-operation :kind :pipeline))
         (run-result
           (make-listener-say-pipeline-result
            "metadata-pipeline" "canonical answer"
            :provider :canonical-provider
            :model "canonical-model"
            :think-level "high"
            :iterations 3
            :usage '((:input-tokens . 10)))))
    (with-listener-say-functions
        ((clim:frame-standard-output
          (lambda (ignored-frame)
            (declare (ignore ignored-frame))
            (make-string-output-stream)))
         (rplaca::start-interactive-compaction
          (lambda (live-buffer &key reason continuation &allow-other-keys)
            (declare (ignore live-buffer reason continuation))
            (values nil nil 0 0)))
         (rplaca::start-interactive-pipeline-for-buffer
          (lambda (live-buffer prompt)
            (declare (ignore live-buffer prompt))
            operation))
         (rplaca::await-listener-agent-turn
          (lambda (live-frame live-buffer dispatch-result)
            (declare (ignore live-frame dispatch-result))
            (rplaca::buffer-insert-read-only-message
             live-buffer :buffered-stage "buffered answer"
             :metadata '((:agent . "buffered-agent")
                         (:provider . :buffered-provider)
                         (:model . "buffered-model")
                         (:stage-only . "retained")))
            (rplaca::run-hook-with-args
             'rplaca:*after-send-message-hook*
             live-buffer "metadata question" run-result))))
      (let ((clim:*application-frame* frame))
        (let* ((turn (rplaca::com-say "metadata question"))
               (metadata (rplaca::assistant-turn-metadata turn))
               (keys (loop :for tail :on metadata :by #'cddr
                           :collect (first tail))))
          (is (= (length keys)
                 (length (remove-duplicates keys :test #'eq))))
          (is (string= "metadata-pipeline" (getf metadata :agent)))
          (is (eq :canonical-provider (getf metadata :provider)))
          (is (string= "canonical-model" (getf metadata :model)))
          (is (string= "high" (getf metadata :think-level)))
          (is (= 3 (getf metadata :iterations)))
          (is (string= "pipeline_complete" (getf metadata :stop-reason)))
          (is (equal '((:input-tokens . 10)) (getf metadata :usage)))
          (is (string= "retained" (getf metadata :stage-only)))
          (is (string= "test-agent" (getf metadata :buffer-agent)))
          (is (eq :idle (getf metadata :buffer-status)))
          (is (member :buffer-session-name keys :test #'eq))
          (is (member :buffer-session-id keys :test #'eq)))))))

(test com-say-requires-the-todo9-await-boundary
  (let* ((buffer (make-listener-say-buffer))
         (frame (make-listener-say-frame buffer))
         (await-existed-p (fboundp 'rplaca::await-listener-agent-turn))
         (await-original (and await-existed-p
                              (symbol-function
                               'rplaca::await-listener-agent-turn))))
    (unwind-protect
         (progn
           (when await-existed-p
             (fmakunbound 'rplaca::await-listener-agent-turn))
           (with-listener-say-functions
               ((clim:frame-standard-output
                 (lambda (ignored-frame)
                   (declare (ignore ignored-frame))
                   (make-string-output-stream)))
                (rplaca::send-prose-message
                 (lambda (live-buffer text)
                   (set-message-text (buffer-input-message live-buffer) text)
                   (buffer-finalize-input live-buffer)
                   (values
                    :started t
                    (rplaca::listener-turn-boundary-for-message
                     (message-prev (buffer-input-message live-buffer)))))))
             (let ((clim:*application-frame* frame))
               (signals error (rplaca::com-say "question")))))
      (when await-existed-p
        (setf (symbol-function 'rplaca::await-listener-agent-turn)
              await-original)))))
