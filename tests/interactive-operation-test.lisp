(in-package :rplaca/tests)

(in-suite interactive-operation-suite)

(defun wait-for-runtime-operation-test (predicate &key (timeout 3.0))
  "Wait until PREDICATE is true, returning its value or NIL at TIMEOUT."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop :for value := (funcall predicate)
          :when value :return value
          :when (>= (get-internal-real-time) deadline) :return nil
          :do (sleep 0.005))))

(defun runtime-history-contains-p (buffer needle)
  "Return true when a finalized BUFFER message contains NEEDLE."
  (some (lambda (message)
          (search needle (message-text message) :test #'char-equal))
        (test-buffer-history-messages buffer)))

(defun interactive-subprocess-temp-artifacts ()
  "Return legacy managed-command stdout/stderr artifacts in the temp root."
  (let ((root (uiop:temporary-directory)))
    (sort
     (mapcar #'namestring
             (append
              (directory (merge-pathnames #P"rplaca-operation-*.stdout"
                                          root))
              (directory (merge-pathnames #P"rplaca-operation-*.stderr"
                                          root))))
     #'string<)))

(test interactive-operation-terminal-publication-never-joins-live-worker
  "The first terminal update installs a waiter and returns before worker exit."
  (let* ((buffer (make-buffer "operation-settlement"
                              :session-persistence-mode :ephemeral))
         (release (bt:make-semaphore :name "operation-settlement-release"))
         (wake (bt:make-semaphore :name "operation-settlement-wake"))
         (apply-count 0)
         (worker
           (bt:make-thread
            (lambda ()
              (bt:wait-on-semaphore release :timeout 5.0))
            :name "operation-terminal-live-worker"))
         (operation
           (rplaca::make-interactive-buffer-operation
            :kind :test
            :buffer-generation (rplaca::buffer-runtime-generation buffer)
            :result '(:immutable-result t)
            :done-p t
            :worker worker
            :apply-function
            (lambda (live-buffer state result error-text)
              (declare (ignore live-buffer state error-text))
              (is (equal '(:immutable-result t) result))
              (incf apply-count)))))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (buffer-pending-interactive-operation buffer) operation
            (buffer-status buffer) :working))
    (let ((rplaca::*runtime-settlement-notify-function*
            (lambda (ignored-buffer reason)
              (declare (ignore ignored-buffer reason))
              (bt:signal-semaphore wake))))
      (let ((started-at (get-internal-real-time)))
        (is-false (rplaca::update-interactive-buffer-operation buffer))
        (is (< (/ (- (get-internal-real-time) started-at)
                  (float internal-time-units-per-second 1.0))
               0.25)))
      (is (= 0 apply-count))
      (is (eq operation (buffer-pending-interactive-operation buffer)))
      (bt:signal-semaphore release)
      (is-true (bt:wait-on-semaphore wake :timeout 2.0))
      (is-true
       (wait-for-runtime-operation-test
        (lambda ()
          (rplaca::interactive-buffer-operation-settlement-waiter-done-p
           operation))))
      ;; Defstruct RESULT sharing is safe because this boundary is reached only
      ;; after the exact state-producing worker exits.
      (is-true (rplaca::update-interactive-buffer-operation buffer))
      (is (= 1 apply-count))
      (is (null (buffer-pending-interactive-operation buffer)))
      (is-false (rplaca::update-interactive-buffer-operation buffer))
      (is (= 1 apply-count)))))

(test blocked-settlement-wake-never-blocks-frame-updater
  "A live settlement waiter in its wake hook is never joined by the updater."
  (let* ((buffer (make-buffer "blocked-operation-settlement-wake"
                              :session-persistence-mode :ephemeral))
         (worker-returned
           (bt:make-semaphore :name "blocked-wake-worker-returned"))
         (wake-entered
           (bt:make-semaphore :name "blocked-settlement-wake-entered"))
         (wake-release
           (bt:make-semaphore :name "blocked-settlement-wake-release"))
         (updater-returned
           (bt:make-semaphore :name "blocked-wake-updater-returned"))
         (apply-count 0)
         (applied-result :not-run)
         (updater-result :not-run)
         (updater-error nil)
         (worker
           (bt:make-thread
            (lambda ()
              (bt:signal-semaphore worker-returned))
            :name "blocked-wake-terminal-worker"))
         (operation
           (rplaca::make-interactive-buffer-operation
            :kind :blocked-wake
            :buffer-generation (rplaca::buffer-runtime-generation buffer)
            :result :complete
            :done-p t
            :worker worker
            :apply-function
            (lambda (live-buffer state result error-text)
              (declare (ignore live-buffer state error-text))
              (setf applied-result result)
              (incf apply-count))))
         (waiter nil)
         (updater nil))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (buffer-pending-interactive-operation buffer) operation
            (buffer-status buffer) :working))
    (unwind-protect
         (let ((rplaca::*runtime-settlement-notify-function*
                 (lambda (ignored-buffer ignored-reason)
                   (declare (ignore ignored-buffer ignored-reason))
                   (bt:signal-semaphore wake-entered)
                   (bt:wait-on-semaphore wake-release :timeout 5.0))))
           (is-true
            (bt:wait-on-semaphore worker-returned :timeout 2.0))
           (is-true
            (wait-for-runtime-operation-test
             (lambda () (not (bt:thread-alive-p worker)))))
           (setf waiter
                 (rplaca::ensure-interactive-buffer-operation-settlement-waiter
                  buffer operation worker))
           (is-true waiter)
           (is-true (bt:wait-on-semaphore wake-entered :timeout 2.0))
           (is-false
            (rplaca::interactive-buffer-operation-settlement-waiter-done-p
             operation))
           (setf updater
                 (bt:make-thread
                  (lambda ()
                    (unwind-protect
                         (handler-case
                             (setf updater-result
                                   (rplaca::update-interactive-buffer-operation
                                    buffer))
                           (error (condition)
                             (setf updater-error condition)))
                      (bt:signal-semaphore updater-returned)))
                  :name "blocked-wake-frame-updater"))
           ;; The waiter is deliberately live inside the wake hook.  The
           ;; updater must apply from the already-dead primary worker without
           ;; joining the notification-only waiter; joining it here would
           ;; freeze the CLIM event process.
           (is-true
            (bt:wait-on-semaphore updater-returned :timeout 0.5))
           (is (null updater-error))
           (is-true updater-result)
           (is (= 1 apply-count))
           (is (eq :complete applied-result))
           (is (null (buffer-pending-interactive-operation buffer)))
           (is-true (bt:thread-alive-p waiter))
           (is-false
            (rplaca::interactive-buffer-operation-settlement-waiter-done-p
             operation))
           (bt:signal-semaphore wake-release)
           (bt:join-thread waiter)
           (is-true
            (rplaca::interactive-buffer-operation-settlement-waiter-done-p
             operation)))
      (bt:signal-semaphore wake-release)
      (when (and updater (bt:thread-alive-p updater))
        (bt:join-thread updater))
      (when (and waiter (bt:thread-alive-p waiter))
        (bt:join-thread waiter))
      (when (bt:thread-alive-p worker)
        (bt:join-thread worker)))))

(test interactive-operation-stop-is-prompt-and-drops-late-worker-effects
  "Stop retains ownership until exit and applies only the cancellation callback."
  (let* ((buffer (make-buffer "operation-stop"
                              :session-persistence-mode :ephemeral))
         (entered (bt:make-semaphore :name "operation-stop-entered"))
         (release (bt:make-semaphore :name "operation-stop-release"))
         (cancel-applies 0)
         (normal-applies 0)
         (operation
           (rplaca::start-interactive-buffer-operation
            buffer
            :test
            (lambda (snapshot state)
              (declare (ignore state))
              (buffer-insert-system-message snapshot "late detached effect")
              (bt:signal-semaphore entered)
              (bt:wait-on-semaphore release :timeout 5.0)
              :late-result)
            (lambda (live-buffer state result error-text)
              (declare (ignore live-buffer state result error-text))
              (incf normal-applies))
            :cancel-function
            (lambda (live-buffer state)
              (declare (ignore state))
              (incf cancel-applies)
              (buffer-insert-system-message live-buffer "operation cancelled")))))
    (unwind-protect
         (progn
           (is-true (bt:wait-on-semaphore entered :timeout 2.0))
           (is-false (runtime-history-contains-p buffer "late detached effect"))
           (let ((started-at (get-internal-real-time)))
             (is-true (rplaca::stop-streaming-response buffer))
             (is (< (/ (- (get-internal-real-time) started-at)
                       (float internal-time-units-per-second 1.0))
                    0.25)))
           (is (null (buffer-pending-interactive-operation buffer)))
           (is (member operation
                       (rplaca::buffer-runtime-teardown-operation-states
                        (rplaca::buffer-runtime-teardown buffer))
                       :test #'eq))
           (is (eq :cancelling (buffer-status buffer)))
           (bt:signal-semaphore release)
           (is-true
            (wait-for-runtime-operation-test
             (lambda ()
               (not (bt:thread-alive-p
                     (rplaca::interactive-buffer-operation-worker
                      operation))))))
           (is-true
            (wait-for-runtime-operation-test
             (lambda ()
               (let ((teardown
                       (rplaca::buffer-runtime-teardown buffer)))
                 (and teardown
                      (rplaca::buffer-runtime-teardown-frame-delivery-p
                       teardown))))))
           (is (= 0 cancel-applies))
           (is-true (rplaca::buffer-runtime-stopping-p buffer))
           (is-false (rplaca::update-interactive-buffer-operation buffer))
           ;; This models HANDLE-CHAT-FRAME-REDISPLAY after the private reaper
           ;; wake.  The cancellation callback is a frame-owned UI effect.
           (is-true
            (rplaca::deliver-buffer-runtime-stopped-notification buffer))
           (is (= 1 cancel-applies))
           (is-false (rplaca::buffer-runtime-stopping-p buffer))
           (is (null (rplaca::buffer-runtime-teardown buffer)))
           (is (= 0 normal-applies))
           (is-false (runtime-history-contains-p buffer "late detached effect"))
           (is-true (runtime-history-contains-p buffer "operation cancelled"))
           (is (null (buffer-pending-interactive-operation buffer))))
      (bt:signal-semaphore release))))

(test interactive-operation-publication-loss-reaps-gated-worker
  "A generation loser cannot leak a constructed but unpublished worker."
  (let ((buffer (make-buffer "operation-publication-loser"
                             :session-persistence-mode :ephemeral))
        (constructed-worker nil)
        (runner-calls 0))
    (let ((rplaca::*interactive-operation-worker-constructor*
            (lambda (function name)
              (setf constructed-worker
                    (bt:make-thread function :name name))
              ;; Deterministically invalidate the generation after construction
              ;; while the worker is still behind START-GATE.
              (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
                (incf (rplaca::buffer-runtime-generation buffer)))
              constructed-worker)))
      (is (null
           (rplaca::start-interactive-buffer-operation
            buffer
            :publication-loser
            (lambda (snapshot state)
              (declare (ignore snapshot state))
              (incf runner-calls))
            (lambda (&rest ignored)
              (declare (ignore ignored))))))
      (is-true constructed-worker)
      (is-false (bt:thread-alive-p constructed-worker))
      (is (= 0 runner-calls))
      (is (null (buffer-pending-interactive-operation buffer))))))

(test interactive-operation-effect-replay-failure-is-contained
  "Effect application failure settles ownership and cannot run continuation."
  (let* ((buffer (make-buffer "operation-effect-failure"
                              :session-persistence-mode :ephemeral))
         (apply-calls 0)
         (debug-events nil)
         (operation
           (rplaca::make-interactive-buffer-operation
            :kind :effect-failure
            :buffer-generation (rplaca::buffer-runtime-generation buffer)
            :done-p t
            :effects '((:message . :bad-effect))
            :apply-function
            (lambda (&rest ignored)
              (declare (ignore ignored))
              (incf apply-calls)))))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (buffer-pending-interactive-operation buffer) operation))
    (with-function-override
        (rplaca::file-debug-event (event-name &rest payload)
          (declare (ignore payload))
          (push event-name debug-events))
      (with-function-override
          (rplaca::apply-interactive-tool-effects (ignored-buffer effects)
            (declare (ignore ignored-buffer effects))
            (error "simulated effect replay failure"))
        (let ((escaped nil))
          (handler-case
              (is-true
               (rplaca::update-interactive-buffer-operation buffer))
            (error (condition)
              (setf escaped condition)))
          (is (null escaped)))))
    (is (= 0 apply-calls))
    (is (eq :error (buffer-status buffer)))
    (is (null (buffer-pending-interactive-operation buffer)))
    (is (member "runtime-interactive-operation-apply-error"
                debug-events :test #'string=))
    (is-true
     (runtime-history-contains-p buffer "failed while applying"))))

(test settlement-waiter-notify-errors-are-contained-and-reapable
  "OAuth, tool, stream, and generic operation waiters contain notify errors."
  (let ((buffer (make-buffer "settlement-notify-errors"
                             :session-persistence-mode :ephemeral))
        (debug-events nil)
        (debug-lock (bt:make-lock "settlement-notify-debug-events"))
        (rplaca::*runtime-settlement-notify-function*
          (lambda (ignored-buffer ignored-reason)
            (declare (ignore ignored-buffer ignored-reason))
            (error "simulated settlement notify failure"))))
    (labels ((exercise (marker installer)
               (let ((release
                       (bt:make-semaphore
                        :name "settlement-notify-worker-release"))
                     (worker nil)
                     (waiter nil))
                 (unwind-protect
                      (progn
                        (setf worker
                              (bt:make-thread
                               (lambda ()
                                 (bt:wait-on-semaphore release :timeout 5.0))
                               :name "settlement-notify-worker"))
                        (setf waiter (funcall installer worker))
                        (is-true waiter)
                        (bt:signal-semaphore release)
                        (bt:join-thread waiter)
                        (is-false (bt:thread-alive-p waiter))
                        (is (member marker debug-events :test #'string=)))
                   (bt:signal-semaphore release)
                   (when (and worker (bt:thread-alive-p worker))
                     (bt:join-thread worker))
                   (when (and waiter (bt:thread-alive-p waiter))
                     (bt:join-thread waiter))))))
      (with-function-override
          (rplaca::file-debug-event (event-name &rest payload)
            (declare (ignore payload))
            (bt:with-lock-held (debug-lock)
              (push event-name debug-events)))
        (let ((flow (rplaca::make-openai-oauth-flow :buffer buffer)))
          (exercise
           "runtime-oauth-settlement-waiter-error"
           (lambda (worker)
             (rplaca::ensure-openai-oauth-settlement-waiter flow worker))))
        (let ((state (rplaca::make-interactive-tool-execution)))
          (exercise
           "runtime-tool-settlement-waiter-error"
           (lambda (worker)
             (rplaca::ensure-interactive-tool-settlement-waiter
              buffer state worker))))
        (let ((state (rplaca::make-stream-state)))
          (exercise
           "runtime-stream-settlement-waiter-error"
           (lambda (worker)
             (rplaca::ensure-stream-state-reader-settlement-waiter
              buffer state worker))))
        (let ((operation
                (rplaca::make-interactive-buffer-operation :kind :test)))
          (exercise
           "runtime-interactive-operation-settlement-waiter-error"
           (lambda (worker)
             (rplaca::ensure-interactive-buffer-operation-settlement-waiter
              buffer operation worker))))))))

(test managed-settlement-waiters-use-private-clim-wake-only
  "Every managed settlement waiter bypasses blocking public observers."
  (let* ((buffer (make-buffer "settlement-private-clim-wake"
                              :session-persistence-mode :ephemeral))
         (frame-thread (bt:current-thread))
         (private-wake
           (bt:make-semaphore :name "settlement-private-wake"))
         (public-off-thread
           (bt:make-semaphore :name "settlement-public-off-thread"))
         (event-lock (bt:make-lock "settlement-hook-events"))
         (private-events nil)
         (public-events nil)
         (public-block-release nil)
         (saved-private rplaca::*buffer-display-wakeup-hook*)
         (saved-public rplaca::*after-buffer-display-change-hook*))
    (labels ((release-public-blockers ()
               ;; More than one obsolete public notification may be waiting
               ;; when this regression test fails.  Always make cleanup finite.
               (when public-block-release
                 (dotimes (ignored 8)
                   (declare (ignore ignored))
                   (bt:signal-semaphore public-block-release))))
             (exercise (expected-reason installer)
               (let ((worker-release
                       (bt:make-semaphore
                        :name "settlement-private-worker-release"))
                     (worker nil)
                     (waiter nil))
                 (setf public-block-release
                       (bt:make-semaphore
                        :name "settlement-public-block-release"))
                 (unwind-protect
                      (progn
                        (setf worker
                              (bt:make-thread
                               (lambda ()
                                 (bt:wait-on-semaphore worker-release
                                                       :timeout 5.0))
                               :name "settlement-private-worker"))
                        (setf waiter (funcall installer worker))
                        (is-true waiter)
                        (bt:signal-semaphore worker-release)
                        (is-true
                         (bt:wait-on-semaphore private-wake :timeout 2.0))
                        (let ((off-thread-p
                                (bt:wait-on-semaphore public-off-thread
                                                      :timeout 0.05)))
                          (is-false off-thread-p)
                          (when off-thread-p
                            (release-public-blockers)))
                        (bt:join-thread waiter)
                        (is-false (bt:thread-alive-p worker))
                        (is-false (bt:thread-alive-p waiter))
                        (bt:with-lock-held (event-lock)
                          (let ((event
                                  (find expected-reason private-events
                                        :key #'second :test #'eq)))
                            (is-true event)
                            (is (eq waiter (first event))))))
                   (bt:signal-semaphore worker-release)
                   (release-public-blockers)
                   (when (and worker (bt:thread-alive-p worker))
                     (bt:join-thread worker))
                   (when (and waiter (bt:thread-alive-p waiter))
                     (bt:join-thread waiter))))))
      (unwind-protect
           (progn
             ;; These hooks are process-global because managed waiters start
             ;; with clean thread bindings, as they do in production.
             (setf rplaca::*buffer-display-wakeup-hook*
                   (list
                    (lambda (hook-buffer reason)
                      (when (eq hook-buffer buffer)
                        (bt:with-lock-held (event-lock)
                          (push (list (bt:current-thread) reason)
                                private-events))
                        (bt:signal-semaphore private-wake))))
                   rplaca::*after-buffer-display-change-hook*
                   (list
                    (lambda (hook-buffer reason)
                      (when (eq hook-buffer buffer)
                        (bt:with-lock-held (event-lock)
                          (push (list (bt:current-thread) reason)
                                public-events))
                        (unless (eq frame-thread (bt:current-thread))
                          (bt:signal-semaphore public-off-thread)
                          (let ((release public-block-release))
                            (when release
                              (bt:wait-on-semaphore release
                                                    :timeout 5.0))))))))
             (let ((rplaca::*runtime-settlement-notify-function* nil))
               (let ((flow (rplaca::make-openai-oauth-flow :buffer buffer)))
                 (exercise
                  :oauth-settled
                  (lambda (worker)
                    (rplaca::ensure-openai-oauth-settlement-waiter
                     flow worker))))
               (let ((state (rplaca::make-interactive-tool-execution)))
                 (exercise
                  :tool-settled
                  (lambda (worker)
                    (rplaca::ensure-interactive-tool-settlement-waiter
                     buffer state worker))))
               (let ((state (rplaca::make-stream-state)))
                 (exercise
                  :stream-settled
                  (lambda (worker)
                    (rplaca::ensure-stream-state-reader-settlement-waiter
                     buffer state worker))))
               (let ((operation
                       (rplaca::make-interactive-buffer-operation
                        :kind :test)))
                 (exercise
                  :interactive-operation-settled
                  (lambda (worker)
                    (rplaca::ensure-interactive-buffer-operation-settlement-waiter
                     buffer operation worker)))))
             (bt:with-lock-held (event-lock)
               (is (= 4 (length private-events)))
               (is (null public-events))))
        (release-public-blockers)
        (setf rplaca::*buffer-display-wakeup-hook* saved-private
              rplaca::*after-buffer-display-change-hook* saved-public)))))

(test cancelled-managed-states-refuse-new-settlement-waiters
  "Cancellation and waiter publication are linearized by each owner lock."
  (let* ((buffer (make-buffer "cancelled-waiter-refusal"
                              :session-persistence-mode :ephemeral))
         (worker (bt:current-thread))
         (flow (rplaca::make-openai-oauth-flow :buffer buffer))
         (tool (rplaca::make-interactive-tool-execution))
         (stream (rplaca::make-stream-state))
         (operation
           (rplaca::make-interactive-buffer-operation :kind :test)))
    (rplaca::openai-oauth-flow-set-result flow :cancelled t)
    (rplaca::cancel-interactive-tool-execution tool)
    (rplaca::cancel-stream-state stream)
    (rplaca::cancel-interactive-buffer-operation operation)
    (is-false (rplaca::ensure-openai-oauth-settlement-waiter flow worker))
    (is-false
     (rplaca::ensure-interactive-tool-settlement-waiter
      buffer tool worker))
    (is-false
     (rplaca::ensure-stream-state-reader-settlement-waiter
      buffer stream worker))
    (is-false
     (rplaca::ensure-interactive-buffer-operation-settlement-waiter
      buffer operation worker))
    (is (null (rplaca::openai-oauth-flow-settlement-thread flow)))
    (is (null (rplaca::interactive-tool-execution-settlement-waiter tool)))
    (is (null (rplaca::stream-state-reader-settlement-thread stream)))
    (is (null
         (rplaca::interactive-buffer-operation-settlement-waiter
          operation))))
  ;; Deterministic updater-after-cancel barrier: even a racing updater that was
  ;; already scheduled before cancellation may not publish a late waiter.
  (let* ((buffer (make-buffer "cancelled-waiter-barrier"
                              :session-persistence-mode :ephemeral))
         (operation
           (rplaca::make-interactive-buffer-operation :kind :barrier))
         (ready (bt:make-semaphore :name "cancelled-waiter-ready"))
         (release (bt:make-semaphore :name "cancelled-waiter-release"))
         (result :not-run)
         (updater
           (bt:make-thread
            (lambda ()
              (bt:signal-semaphore ready)
              (bt:wait-on-semaphore release :timeout 5.0)
              (setf result
                    (rplaca::ensure-interactive-buffer-operation-settlement-waiter
                     buffer operation (bt:current-thread))))
            :name "cancelled-waiter-updater")))
    (unwind-protect
         (progn
           (is-true (bt:wait-on-semaphore ready :timeout 2.0))
           (rplaca::cancel-interactive-buffer-operation operation)
           (bt:signal-semaphore release)
           (bt:join-thread updater)
           (is (null result))
           (is (null
                (rplaca::interactive-buffer-operation-settlement-waiter
                 operation))))
      (bt:signal-semaphore release)
      (when (bt:thread-alive-p updater)
        (bt:join-thread updater)))))

(test interactive-tool-cancellation-callback-is-once-only-and-outside-lock
  "Managed tool cancellation invokes its cooperative callback once outside the state lock."
  (let* ((calls 0)
         (state (rplaca::make-interactive-tool-execution
                 :tool-name "cancellable-test")))
    (setf (rplaca::interactive-tool-execution-cancel-function state)
          (lambda ()
            ;; Reacquiring the state lock proves cancellation did not invoke
            ;; extension code while holding the runtime owner lock.
            (bt:with-lock-held
                ((rplaca::interactive-tool-execution-lock state))
              (incf calls))))
    (is (eq state (rplaca::cancel-interactive-tool-execution state)))
    (is (eq state (rplaca::cancel-interactive-tool-execution state)))
    (is (= 1 calls))
    (is-true
     (rplaca::interactive-tool-execution-cancel-requested-p state))
    (is (null
         (rplaca::interactive-tool-execution-cancel-function state)))))

(test managed-operation-public-hooks-run-only-after-frame-application
  "Completion and settlement wake privately; applied observers are frame-owned."
  (let* ((buffer (make-buffer "operation-frame-owned-public-hook"
                              :session-persistence-mode :ephemeral))
         (frame-thread (bt:current-thread))
         (terminal-wake
           (bt:make-semaphore :name "operation-private-terminal-wake"))
         (terminal-release
           (bt:make-semaphore :name "operation-private-terminal-release"))
         (settled-wake
           (bt:make-semaphore :name "operation-private-settled-wake"))
         (public-off-thread
           (bt:make-semaphore :name "operation-public-off-thread"))
         (public-block-release
           (bt:make-semaphore :name "operation-public-block-release"))
         (event-lock (bt:make-lock "operation-frame-hook-events"))
         (private-events nil)
         (public-events nil)
         (apply-count 0)
         (operation nil)
         (saved-private rplaca::*buffer-display-wakeup-hook*)
         (saved-public rplaca::*after-buffer-display-change-hook*))
    (labels ((release-public-blockers ()
               (dotimes (ignored 8)
                 (declare (ignore ignored))
                 (bt:signal-semaphore public-block-release)))
             (public-events-snapshot ()
               (bt:with-lock-held (event-lock)
                 (copy-list public-events))))
      (unwind-protect
           (progn
             (setf rplaca::*buffer-display-wakeup-hook*
                   (list
                    (lambda (hook-buffer reason)
                      (when (eq hook-buffer buffer)
                        (bt:with-lock-held (event-lock)
                          (push (list (bt:current-thread) reason)
                                private-events))
                        (case reason
                          (:interactive-operation-complete
                           (bt:signal-semaphore terminal-wake)
                           ;; Hold the exact publisher alive so the first frame
                           ;; update must install, rather than skip, its waiter.
                           (bt:wait-on-semaphore terminal-release
                                                 :timeout 5.0))
                          (:interactive-operation-settled
                           (bt:signal-semaphore settled-wake))))))
                   rplaca::*after-buffer-display-change-hook*
                   (list
                    (lambda (hook-buffer reason)
                      (when (eq hook-buffer buffer)
                        (bt:with-lock-held (event-lock)
                          (push (list (bt:current-thread) reason)
                                public-events))
                        (if (eq frame-thread (bt:current-thread))
                            ;; A throwing extension must not prevent either
                            ;; synchronous start or terminal frame application.
                            (error "simulated frame public hook failure")
                            (progn
                              (bt:signal-semaphore public-off-thread)
                              (bt:wait-on-semaphore public-block-release
                                                    :timeout 5.0)))))))
             (setf operation
                   (rplaca::start-interactive-buffer-operation
                    buffer
                    :test
                    (lambda (snapshot state)
                      (declare (ignore snapshot state))
                      :completed)
                    (lambda (live-buffer state result error-text)
                      (declare (ignore live-buffer state error-text))
                      (is (eq :completed result))
                      (incf apply-count))))
             (is-true operation)
             (is-true
              (bt:wait-on-semaphore terminal-wake :timeout 2.0))
             (is-false
              (bt:wait-on-semaphore public-off-thread :timeout 0.05))
             ;; DONE-P is visible, but the publisher is deliberately live.
             (is-false
              (rplaca::update-interactive-buffer-operation buffer))
             (is-true
              (rplaca::interactive-buffer-operation-settlement-waiter
               operation))
             (let ((events (public-events-snapshot)))
               (is (member :interactive-operation-started events
                           :key #'second :test #'eq))
               (is-false
                (member :interactive-operation-complete events
                        :key #'second :test #'eq))
               (is-false
                (member :interactive-operation-settled events
                        :key #'second :test #'eq)))
             (bt:signal-semaphore terminal-release)
             (let ((off-thread-p
                     (bt:wait-on-semaphore public-off-thread :timeout 0.10)))
               (is-false off-thread-p)
               (when off-thread-p
                 (release-public-blockers)))
             (is-true
              (bt:wait-on-semaphore settled-wake :timeout 2.0))
             (is-true
              (wait-for-runtime-operation-test
               (lambda ()
                 (rplaca::interactive-buffer-operation-settlement-waiter-done-p
                  operation))))
             (let ((off-thread-p
                     (bt:wait-on-semaphore public-off-thread :timeout 0.05)))
               (is-false off-thread-p)
               (when off-thread-p
                 (release-public-blockers)))
             (is-true
              (rplaca::update-interactive-buffer-operation buffer))
             (is (= 1 apply-count))
             (is (null (buffer-pending-interactive-operation buffer)))
             (let ((events (public-events-snapshot)))
               (is (member :interactive-operation-applied events
                           :key #'second :test #'eq))
               (is-true
                (every (lambda (event)
                         (eq frame-thread (first event)))
                       events))
               (is-false
                (member :interactive-operation-complete events
                        :key #'second :test #'eq))
               (is-false
                (member :interactive-operation-settled events
                        :key #'second :test #'eq))))
        (bt:signal-semaphore terminal-release)
        (release-public-blockers)
        (when operation
          (let ((worker
                  (rplaca::interactive-buffer-operation-worker operation))
                (waiter
                  (rplaca::interactive-buffer-operation-settlement-waiter
                   operation)))
            (when (and worker (bt:thread-alive-p worker))
              (bt:join-thread worker))
            (when (and waiter (bt:thread-alive-p waiter))
              (bt:join-thread waiter))))
        (setf rplaca::*buffer-display-wakeup-hook* saved-private
              rplaca::*after-buffer-display-change-hook* saved-public)))))

(test teardown-reaper-body-error-is-contained-and-retains-stopping
  "A reaper finalization error is logged without falsely clearing ownership."
  (let* ((buffer (make-buffer "teardown-reaper-body-error"
                              :session-persistence-mode :ephemeral))
         (teardown (rplaca::make-buffer-runtime-teardown
                    :token (list :test-teardown)))
         (debug-events nil)
         (reaper nil)
         (escaped nil))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (rplaca::buffer-runtime-teardown buffer) teardown
            (rplaca::buffer-runtime-stopping-p buffer) t))
    (with-function-override
        (rplaca::file-debug-event (event-name &rest payload)
          (declare (ignore payload))
          (push event-name debug-events))
      (with-function-override
          (rplaca::maybe-finish-buffer-runtime-teardown (ignored-buffer)
            (declare (ignore ignored-buffer))
            (error "simulated teardown finalization failure"))
        (setf reaper
              (rplaca::launch-buffer-runtime-teardown-reaper
               buffer teardown nil))
        (handler-case
            (bt:join-thread reaper)
          (error (condition)
            (setf escaped condition)))))
    (is (null escaped))
    (is (member "runtime-worker-reaper-error"
                debug-events :test #'string=))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (is (eq teardown (rplaca::buffer-runtime-teardown buffer)))
      (is-true (rplaca::buffer-runtime-stopping-p buffer))
      (is-true
       (rplaca::buffer-runtime-teardown-workers-settled-p teardown)))))

(test teardown-finalizing-claim-is-reset-and-retryable-after-error
  "An unexpected private-settlement error cannot permanently wedge FINALIZING."
  (let* ((buffer (make-buffer "teardown-finalizing-retry"
                              :session-persistence-mode :ephemeral))
         (tool (rplaca::make-interactive-tool-execution))
         (teardown
           (rplaca::make-buffer-runtime-teardown
            :token (list :finalizing-retry)
            :tool-states (list tool)
            :initializing-p nil
            :workers-settled-p t)))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (rplaca::buffer-runtime-teardown buffer) teardown
            (rplaca::buffer-runtime-stopping-p buffer) t
            (buffer-status buffer) :cancelling))
    (with-function-override
        (rplaca::release-interactive-tool-execution-payload (ignored-state)
          (declare (ignore ignored-state))
          (error "injected private settlement failure"))
      (signals error
        (rplaca::maybe-finish-buffer-runtime-teardown buffer)))
    (is (eq teardown (rplaca::buffer-runtime-teardown buffer)))
    (is-true (rplaca::buffer-runtime-stopping-p buffer))
    (is-false
     (rplaca::buffer-runtime-teardown-finalizing-p teardown))
    (is-false
     (rplaca::buffer-runtime-teardown-reaper-started-p teardown))
    (is-true (rplaca::maybe-finish-buffer-runtime-teardown buffer))
    (is-true
     (rplaca::buffer-runtime-teardown-frame-delivery-p teardown))
    (is-true
     (rplaca::deliver-buffer-runtime-stopped-notification buffer))
    (is (null (rplaca::buffer-runtime-teardown buffer)))
    (is-false (rplaca::buffer-runtime-stopping-p buffer))))

(test teardown-reaper-defers-public-hook-until-frame-delivery
  "Reapers only wake CLIM; queued-work hooks run after STOPPING is released."
  (let* ((buffer (make-buffer "teardown-frame-delivery"
                              :session-persistence-mode :ephemeral))
         (teardown (rplaca::make-buffer-runtime-teardown
                    :token (list :frame-delivery)
                    :initializing-p nil))
         (private-events nil)
         (public-events nil)
         (public-admission nil)
         (reaper nil)
         (frame-thread (bt:current-thread))
         (saved-private rplaca::*buffer-display-wakeup-hook*)
         (saved-public rplaca::*after-buffer-display-change-hook*))
    (unwind-protect
         (progn
           ;; Install process-global test hooks because the reaper intentionally
           ;; starts with clean dynamic bindings, just like production workers.
           (setf rplaca::*buffer-display-wakeup-hook*
                 (list
                  (lambda (hook-buffer reason)
                    (when (eq hook-buffer buffer)
                      (push (list (bt:current-thread) reason) private-events))))
                 rplaca::*after-buffer-display-change-hook*
                 (list
                  (lambda (hook-buffer reason)
                    (when (eq hook-buffer buffer)
                      (push (list (bt:current-thread)
                                  reason
                                  (rplaca::buffer-runtime-stopping-p buffer)
                                  (rplaca::buffer-runtime-teardown buffer))
                            public-events)
                      (setf public-admission
                            (handler-case
                                (rplaca::interactive-buffer-operation-publication-allowed-p
                                 buffer)
                              (error (condition)
                                (format nil "~A" condition))))))))
           (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
             (setf (rplaca::buffer-runtime-teardown buffer) teardown
                   (rplaca::buffer-runtime-stopping-p buffer) t
                   (buffer-status buffer) :cancelling
                   (buffer-stashed-input buffer) "preserved draft"
                   (rplaca::buffer-user-input-pending buffer) :test-request))
           (setf reaper
                 (rplaca::launch-buffer-runtime-teardown-reaper
                  buffer teardown nil))
           (is-true reaper)
           (bt:join-thread reaper)
           (is (= 1 (length private-events)))
           (is (eq reaper (first (first private-events))))
           (is (eq :runtime-stopped-pending
                   (second (first private-events))))
           (is (null public-events))
           (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
             (is-true (rplaca::buffer-runtime-stopping-p buffer))
             (is (eq teardown
                     (rplaca::buffer-runtime-teardown buffer)))
             (is-true
              (rplaca::buffer-runtime-stopped-notification-p buffer))
             (is (eq :cancelling (buffer-status buffer)))
             (is (string= "preserved draft"
                          (buffer-stashed-input buffer)))
             (is (eq :test-request
                     (rplaca::buffer-user-input-pending buffer))))
           (is (string= "" (message-text (buffer-input-message buffer))))
           ;; This call models HANDLE-CHAT-FRAME-REDISPLAY on the frame thread.
           (is-true
            (rplaca::deliver-buffer-runtime-stopped-notification buffer))
           (is (= 1 (length public-events)))
           (is (string= "preserved draft"
                        (message-text (buffer-input-message buffer))))
           (is (null (buffer-stashed-input buffer)))
           (is (null (rplaca::buffer-user-input-pending buffer)))
           (is (eq :idle (buffer-status buffer)))
           (destructuring-bind
               (thread reason stopping-p live-teardown)
               (first public-events)
             (is (eq frame-thread thread))
             (is (eq :runtime-stopped reason))
             (is-false stopping-p)
             (is (null live-teardown)))
           (is-true public-admission)
           (is-false
            (rplaca::deliver-buffer-runtime-stopped-notification buffer)))
      (setf rplaca::*buffer-display-wakeup-hook* saved-private
            rplaca::*after-buffer-display-change-hook* saved-public))))

(test teardown-frame-delivery-blocks-reentrant-hooks-until-public-release
  "Insertion hooks remain blocked; the final public hook is the admission point."
  (let* ((buffer (make-buffer "teardown-reentrant-frame-hooks"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (teardown
           (rplaca::make-buffer-runtime-teardown
            :token (list :reentrant-frame-hooks)
            :stop-p t
            :initializing-p nil
            :workers-settled-p t))
         (blocked-error nil)
         (blocked-busy-p nil)
         (released-generation nil)
         (after-send-state nil)
         (rplaca::*after-message-insert-hook*
           (list
            (lambda (hook-buffer message)
              (when (and (eq hook-buffer buffer)
                         (eq :tool-result (message-sender message)))
                (setf blocked-busy-p
                      (rplaca::buffer-agent-busy-p buffer))
                (setf blocked-error
                      (handler-case
                          (if (rplaca::reserve-buffer-stream-start buffer)
                              :admitted
                              :blocked)
                        (error (condition) condition)))))))
         (rplaca::*after-send-message-hook*
           (list
            (lambda (hook-buffer ignored-text ignored-result)
              (declare (ignore ignored-text ignored-result))
              (when (eq hook-buffer buffer)
                (setf after-send-state
                      (list
                       (rplaca::buffer-runtime-stopping-p buffer)
                       (rplaca::buffer-runtime-stopped-notification-p buffer)
                       (rplaca::buffer-runtime-teardown buffer)))))))
         (rplaca::*after-buffer-display-change-hook*
           (list
            (lambda (hook-buffer reason)
              (when (and (eq hook-buffer buffer)
                         (eq reason :runtime-stopped))
                (rplaca::deliver-buffer-queued-message
                 buffer (list :text "queued after stop")))))))
    (setf (buffer-pending-tool-calls buffer)
          (list (rplaca::canonical-tool-use-block
                 "reentrant-tool" "read" nil)))
    (bt:with-lock-held ((rplaca::buffer-runtime-lock buffer))
      (setf (rplaca::buffer-runtime-teardown buffer) teardown
            (rplaca::buffer-runtime-stopping-p buffer) t
            (rplaca::buffer-runtime-stopped-notification-p buffer) t
            (rplaca::buffer-runtime-teardown-frame-delivery-p teardown) t
            (buffer-status buffer) :cancelling))
    (with-function-override
        (rplaca::send-to-agent-with-context (hook-buffer)
          (setf released-generation
                (rplaca::reserve-buffer-stream-start hook-buffer)))
      (is-true
       (rplaca::deliver-buffer-runtime-stopped-notification buffer)))
    (is (eq :blocked blocked-error))
    (is-true blocked-busy-p)
    (is (integerp released-generation))
    (is (equal '(nil nil nil) after-send-state))
    (is (null (rplaca::buffer-runtime-teardown buffer)))
    (is-false (rplaca::buffer-runtime-stopping-p buffer))
    (is-false (rplaca::buffer-runtime-stopped-notification-p buffer))
    (when released-generation
      (is-true
       (rplaca::release-buffer-stream-start
        buffer released-generation)))))

(test interactive-subprocess-bounds-output-and-wall-time
  "Multi-megabyte output is drained concurrently without disk growth."
  (let* ((artifacts-before (interactive-subprocess-temp-artifacts))
         (started-at (get-internal-real-time))
         (bounded
           (rplaca::run-interactive-subprocess
            "(dd if=/dev/zero bs=1048576 count=2 2>/dev/null | tr '\\000' O) & (dd if=/dev/zero bs=1048576 count=2 2>/dev/null | tr '\\000' E >&2) & wait"
            :timeout 5
            :output-limit 4096))
         (elapsed (/ (- (get-internal-real-time) started-at)
                     (float internal-time-units-per-second 1.0)))
         (artifacts-after (interactive-subprocess-temp-artifacts)))
    (is (= 4096 (length (getf bounded :stdout))))
    (is (= 4096 (length (getf bounded :stderr))))
    (is-true (every (lambda (character) (char= character #\O))
                    (getf bounded :stdout)))
    (is-true (every (lambda (character) (char= character #\E))
                    (getf bounded :stderr)))
    (is-true (getf bounded :stdout-truncated-p))
    (is-true (getf bounded :stderr-truncated-p))
    (is-false (getf bounded :timed-out-p))
    (is (< elapsed 3.0))
    (is (equal artifacts-before artifacts-after)))
  (let* ((started-at (get-internal-real-time))
         (timed
           (rplaca::run-interactive-subprocess
            "sleep 30"
            :timeout 0.05
            :output-limit 32))
         (elapsed (/ (- (get-internal-real-time) started-at)
                     (float internal-time-units-per-second 1.0))))
    (is-true (getf timed :timed-out-p))
    (is (< elapsed 2.0))))

(test interactive-subprocess-drain-constructor-failure-cleans-group
  "A partial pipe-drain startup failure cannot strand its session or worker."
  (let ((constructor-calls 0)
        (drain-worker nil)
        (cleared-owner-calls 0)
        (signalled-groups nil)
        (escaped nil)
        (original-kill rplaca::*posix-kill-function*))
    (let ((rplaca::*interactive-subprocess-drain-thread-constructor*
            (lambda (function name)
              (incf constructor-calls)
              (when (= constructor-calls 2)
                (error "simulated stderr drain construction failure"))
              (setf drain-worker (bt:make-thread function :name name))))
          (rplaca::*posix-kill-function*
            (lambda (pid signal)
              (when (minusp pid)
                (pushnew (- pid) signalled-groups))
              (funcall original-kill pid signal))))
      (handler-case
          (rplaca::run-interactive-subprocess
           "sleep 30"
           :timeout 5
           :process-callback
           (lambda (process)
             (when (null process)
               (incf cleared-owner-calls))))
        (error (condition)
          (setf escaped condition))))
    (is-true escaped)
    (is (= 2 constructor-calls))
    (is-true drain-worker)
    (is-false (bt:thread-alive-p drain-worker))
    (is (= 1 cleared-owner-calls))
    (is-true signalled-groups)
    (dolist (process-group-id signalled-groups)
      (is-false
       (rplaca::linux-process-group-live-p process-group-id)))))

(test interactive-subprocess-cancellation-kills-descendant-tree
  "An ignore-TERM forking child cannot escape its managed process group."
  (let ((lock (bt:make-lock "operation-tree-test"))
        (cancel-p nil)
        (process nil)
        (result nil))
    (labels ((cancel-requested-p ()
               (bt:with-lock-held (lock) cancel-p))
             (remember-process (value)
               (when value
                 (bt:with-lock-held (lock)
                   (setf process value))))
             (process-snapshot ()
               (bt:with-lock-held (lock) process)))
      (let ((runner
              (bt:make-thread
               (lambda ()
                 (setf result
                       (rplaca::run-interactive-subprocess
                        "sh -c 'trap \"\" TERM; while :; do sleep 1; done' & wait"
                        :timeout 10
                        :cancel-requested-p #'cancel-requested-p
                        :process-callback #'remember-process)))
               :name "operation-tree-test-runner")))
        (unwind-protect
             (let* ((owned-process
                      (wait-for-runtime-operation-test #'process-snapshot))
                    (pid (and owned-process
                              (sb-ext:process-pid owned-process)))
                    (descendants
                      (and pid
                           (wait-for-runtime-operation-test
                            (lambda ()
                              (rplaca::linux-process-descendant-pids pid))))))
               (is-true owned-process)
               (is-true rplaca::*interactive-subprocess-setsid-path*)
               (is-true descendants)
               (bt:with-lock-held (lock)
                 (setf cancel-p t))
               (bt:join-thread runner)
               (setf runner nil)
               (is-true (getf result :cancelled-p))
               (is-true (getf result :process-group-id))
               (dolist (child descendants)
                 (is-true
                  (wait-for-runtime-operation-test
                   (lambda ()
                     (not (rplaca::linux-process-id-live-p child)))
                   :timeout 2.0))))
               ;; This scans the whole Linux process table, proving that even
               ;; a child spawned after the procfs descendant snapshot cannot
               ;; remain executable in the isolated group.
               (is-false
                (rplaca::linux-process-group-live-p
                 (getf result :process-group-id)))
          (bt:with-lock-held (lock)
            (setf cancel-p t))
          (when (and runner (bt:thread-alive-p runner))
            (bt:join-thread runner)))))))

(test interactive-subprocess-cleans-group-after-session-leader-exits
  "A normally exiting shell cannot leave its background child alive."
  (is-true rplaca::*interactive-subprocess-setsid-path*)
  (dotimes (iteration 5)
    (declare (ignore iteration))
    (let* ((started-at (get-internal-real-time))
           (result
             (rplaca::run-interactive-subprocess
              "sleep 30 & child=$!; printf '%s' \"$child\""
              :timeout 5
              :output-limit 32))
           (elapsed (/ (- (get-internal-real-time) started-at)
                       (float internal-time-units-per-second 1.0)))
           (process-group-id (getf result :process-group-id))
           (child-pid (parse-integer (getf result :stdout)
                                     :junk-allowed t)))
      (is-true process-group-id)
      (is-true child-pid)
      (is (< elapsed 2.0))
      (is-false (rplaca::linux-process-id-live-p child-pid))
      (is-false
       (rplaca::linux-process-group-live-p process-group-id)))))

(test shell-prefix-result-and-after-hook-apply-exactly-once
  "The shell command and after-send hook cross the frame boundary once."
  (let ((rplaca::*before-send-message-hook* nil)
        (rplaca::*after-send-message-hook* nil)
        (after-count 0)
        (buffer (make-buffer "managed-shell-prefix"
                             :session-persistence-mode :ephemeral)))
    (rplaca:add-hook
     'rplaca:*after-send-message-hook*
     (lambda (hook-buffer text result)
       (declare (ignore hook-buffer result))
       (is (string= "!printf shell-ok" text))
       (incf after-count)))
    (set-message-text (buffer-input-message buffer) "!printf shell-ok")
    (let ((operation (rplaca::send-message buffer)))
      (is-true (rplaca::interactive-buffer-operation-p operation))
      (is (= 0 after-count))
      (is-true
       (wait-for-runtime-operation-test
        (lambda ()
          (not (bt:thread-alive-p
                (rplaca::interactive-buffer-operation-worker operation))))))
      (is-true (rplaca::update-interactive-buffer-operation buffer))
      (is (= 1 after-count))
      (is-true (runtime-history-contains-p buffer "shell-ok"))
      (is-false (rplaca::update-interactive-buffer-operation buffer))
      (is (= 1 after-count)))))

(test shell-prefix-cancellation-kills-process-and-applies-hook-once
  "Stopping a shell command terminates its tree and settles visible state once."
  (let ((rplaca::*before-send-message-hook* nil)
        (rplaca::*after-send-message-hook* nil)
        (after-count 0)
        (buffer (make-buffer "managed-shell-prefix-cancel"
                             :session-persistence-mode :ephemeral)))
    (rplaca:add-hook
     'rplaca:*after-send-message-hook*
     (lambda (hook-buffer text result)
       (declare (ignore hook-buffer))
       (is (string= "!sleep 30" text))
       (is (eq :cancelled result))
       (incf after-count)))
    (set-message-text (buffer-input-message buffer) "!sleep 30")
    (let ((operation (rplaca::send-message buffer)))
      (is-true (rplaca::interactive-buffer-operation-p operation))
      (is-true
       (wait-for-runtime-operation-test
        (lambda ()
          (bt:with-lock-held
              ((rplaca::interactive-buffer-operation-lock operation))
            (rplaca::interactive-buffer-operation-process operation)))))
      (is-true (rplaca::stop-streaming-response buffer))
      (is (= 0 after-count))
      (is-true
       (wait-for-runtime-operation-test
        (lambda ()
          (not (bt:thread-alive-p
                (rplaca::interactive-buffer-operation-worker operation))))
        :timeout 3.0))
      (is-true
       (wait-for-runtime-operation-test
        (lambda ()
          (let ((teardown (rplaca::buffer-runtime-teardown buffer)))
            (and teardown
                 (rplaca::buffer-runtime-teardown-frame-delivery-p
                  teardown))))))
      (is-false (rplaca::update-interactive-buffer-operation buffer))
      (is-true
       (rplaca::deliver-buffer-runtime-stopped-notification buffer))
      (is (= 1 after-count))
      (is-true (runtime-history-contains-p buffer "cancelled"))
      (is (null (buffer-pending-interactive-operation buffer)))
      (is-false (rplaca::update-interactive-buffer-operation buffer))
      (is (= 1 after-count)))))

(defvar *provider-live-eval-test-ran-p* nil)

(test package-owned-agent-and-pipeline-registrations-are-retirable
  "Package reset helpers remove agents, pipelines, and test profiles exactly."
  (let ((rplaca::*agent-definition-registry*
          (make-hash-table :test #'equal))
        (rplaca::*pipeline-definition-registry*
          (make-hash-table :test #'equal))
        (rplaca::*pipeline-test-profile-registry*
          (make-hash-table :test #'equal)))
    (let ((rplaca:*current-rplaca-package* "owned-package"))
      (rplaca:register-agent-definition "owned-agent" :provider :zai)
      (rplaca:define-pipeline
       "owned-pipeline" :stages '((:name "stage" :prompt "test")))
      (rplaca:define-pipeline-test-profile
       "owned-profile" :command '("true")))
    (rplaca:register-agent-definition "unowned-agent" :provider :zai)
    (rplaca:define-pipeline
     "unowned-pipeline" :stages '((:name "stage" :prompt "test")))
    (rplaca:define-pipeline-test-profile
     "unowned-profile" :command '("true"))
    (is (= 1 (length
              (rplaca:package-owned-agent-definitions "owned-package"))))
    (is (= 1 (length
              (rplaca:package-owned-pipeline-definitions
               "owned-package"))))
    (is (= 1 (length
              (rplaca:package-owned-pipeline-test-profiles
               "owned-package"))))
    (rplaca:remove-agent-definitions-for-package "owned-package")
    (rplaca:remove-pipeline-registrations-for-package "owned-package")
    (is (null (rplaca:find-agent-definition "owned-agent")))
    (is (null (rplaca:find-pipeline-definition "owned-pipeline")))
    (is (null (rplaca:find-pipeline-test-profile "owned-profile")))
    (is-true (rplaca:find-agent-definition "unowned-agent"))
    (is-true (rplaca:find-pipeline-definition "unowned-pipeline"))
    (is-true (rplaca:find-pipeline-test-profile "unowned-profile"))))

(test provider-immediate-live-eval-is-refused
  "Provider prompt mode never executes live lisp_eval bodies."
  (let* ((*provider-live-eval-test-ran-p* nil)
         (buffer (make-buffer "provider-live-eval"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral))
         (tool-use
           (test-tool-use
            "live-eval"
            "lisp_eval"
            `((:code . ,"(setf rplaca/tests::*provider-live-eval-test-ran-p* t)")
              (:mode . "live")))))
    (multiple-value-bind (result event)
        (rplaca::execute-prompt-tool-call buffer tool-use :agent)
      (declare (ignore event))
      (is-false *provider-live-eval-test-ran-p*)
      (is (search "REFUSED"
                  (string-upcase (cdr (assoc :display result))))))
    (is (eq :command-only
            (rplaca::interactive-tool-execution-policy
             "lisp_eval" '((:mode . "live")))))
    (is (eq :background
            (rplaca::interactive-tool-execution-policy
             "lisp_eval" '((:mode . "isolated")))))
    (is (eq :frame
            (rplaca::interactive-tool-execution-policy
             "live_lisp_eval" '((:code . "(+ 1 2)")))))))

(test interactive-pipeline-provider-stream-is-cancellable
  "Stop cancels a pipeline provider stream and applies no worker messages."
  (with-pipeline-definition-registry-override ()
    (rplaca:define-pipeline
     "managed-cancel"
     :stages '((:name "stage" :prompt "{{input}}")))
    (let* ((rplaca::*compaction-point* nil)
           (rplaca::*before-send-message-hook* nil)
           (rplaca::*after-send-message-hook* nil)
           (after-count 0)
           (state (rplaca::make-stream-state))
           (buffer (make-buffer "managed-pipeline-cancel"
                                :agent-name "agent"
                                :pipeline-name "managed-cancel"
                                :session-persistence-mode :ephemeral)))
      (set-buffer-provider-override buffer :zai)
      (set-buffer-model-override buffer "test-model")
      (rplaca:add-hook
       'rplaca:*after-send-message-hook*
       (lambda (&rest ignored)
         (declare (ignore ignored))
         (incf after-count)))
      (with-function-override
          (rplaca::provider-request-streaming
           (provider messages callback &rest args)
           (declare (ignore provider messages callback args))
           state)
        (set-message-text (buffer-input-message buffer) "cancel pipeline")
        (let ((operation (rplaca::send-message buffer)))
          (is-true (rplaca::interactive-buffer-operation-p operation))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (bt:with-lock-held
                  ((rplaca::interactive-buffer-operation-lock operation))
                (eq state
                    (rplaca::interactive-buffer-operation-current-stream
                     operation))))))
          (is-true (rplaca::stop-streaming-response buffer))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (not (bt:thread-alive-p
                    (rplaca::interactive-buffer-operation-worker
                     operation))))))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (let ((teardown
                      (rplaca::buffer-runtime-teardown buffer)))
                (and teardown
                     (rplaca::buffer-runtime-teardown-frame-delivery-p
                      teardown))))))
          (is-false (rplaca::update-interactive-buffer-operation buffer))
          (is-true
           (rplaca::deliver-buffer-runtime-stopped-notification buffer))
          (is (= 1 after-count))
          (is-true (runtime-history-contains-p buffer "Pipeline cancelled"))
          (is-false (runtime-history-contains-p buffer "pipeline stage"))
          (bt:with-lock-held ((rplaca::stream-state-lock state))
            (is-true (rplaca::stream-state-cancelled-p state))))))))

(test interactive-compaction-applies-then-continues-send-exactly-once
  "Compaction cannot mutate live history or duplicate its intended send."
  (let* ((rplaca::*compaction-point* 1)
         (rplaca::*compaction-function* #'rplaca:default-compact-buffer)
         (rplaca::*before-send-message-hook* nil)
         (rplaca::*after-send-message-hook* nil)
         (send-count 0)
         (after-count 0)
         (buffer (make-buffer "managed-compaction"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral)))
    (set-buffer-provider-override buffer :zai)
    (set-buffer-model-override buffer "test-model")
    (rplaca:add-hook
     'rplaca:*after-send-message-hook*
     (lambda (&rest ignored)
       (declare (ignore ignored))
       (incf after-count)))
    (with-function-override
        (rplaca::provider-request-streaming
         (provider messages callback &rest args)
         (declare (ignore provider messages callback args))
         (make-completed-stream-state-response
          "end_turn"
          (list (rplaca::canonical-text-block "managed summary"))))
      (with-function-override
          (rplaca::send-to-agent-with-context (live-buffer)
            (incf send-count)
            live-buffer)
        (set-message-text (buffer-input-message buffer) "continue once")
        (let ((operation (rplaca::send-message buffer)))
          (is-true (rplaca::interactive-buffer-operation-p operation))
          (is (= 0 send-count))
          (is-false (runtime-history-contains-p buffer "managed summary"))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (not (bt:thread-alive-p
                    (rplaca::interactive-buffer-operation-worker
                     operation))))))
          (is-true (rplaca::update-interactive-buffer-operation buffer))
          (is (= 1 send-count))
          (is (= 1 after-count))
          (is-true (runtime-history-contains-p buffer "managed summary"))
          (is-false (rplaca::update-interactive-buffer-operation buffer))
          (is (= 1 send-count)))))))

(test cancelled-interactive-compaction-never-runs-send-continuation
  "Stop closes compaction provider I/O and drops the queued send continuation."
  (let* ((rplaca::*compaction-point* 1)
         (rplaca::*compaction-function* #'rplaca:default-compact-buffer)
         (rplaca::*before-send-message-hook* nil)
         (rplaca::*after-send-message-hook* nil)
         (send-count 0)
         (state (rplaca::make-stream-state))
         (buffer (make-buffer "managed-compaction-cancel"
                              :agent-name "agent"
                              :session-persistence-mode :ephemeral)))
    (set-buffer-provider-override buffer :zai)
    (set-buffer-model-override buffer "test-model")
    (with-function-override
        (rplaca::provider-request-streaming
         (provider messages callback &rest args)
         (declare (ignore provider messages callback args))
         state)
      (with-function-override
          (rplaca::send-to-agent-with-context (live-buffer)
            (declare (ignore live-buffer))
            (incf send-count))
        (set-message-text (buffer-input-message buffer) "cancel compaction")
        (let ((operation (rplaca::send-message buffer)))
          (is-true (rplaca::interactive-buffer-operation-p operation))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (bt:with-lock-held
                  ((rplaca::interactive-buffer-operation-lock operation))
                (eq state
                    (rplaca::interactive-buffer-operation-current-stream
                     operation))))))
          (is-true (rplaca::stop-streaming-response buffer))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (not (bt:thread-alive-p
                    (rplaca::interactive-buffer-operation-worker
                     operation))))))
          (is-true
           (wait-for-runtime-operation-test
            (lambda ()
              (let ((teardown
                      (rplaca::buffer-runtime-teardown buffer)))
                (and teardown
                     (rplaca::buffer-runtime-teardown-frame-delivery-p
                      teardown))))))
          (is-false (rplaca::update-interactive-buffer-operation buffer))
          (is-true
           (rplaca::deliver-buffer-runtime-stopped-notification buffer))
          (is (= 0 send-count))
          (bt:with-lock-held ((rplaca::stream-state-lock state))
            (is-true (rplaca::stream-state-cancelled-p state))))))))
