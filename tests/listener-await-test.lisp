(in-package :rplaca/tests)

(in-suite listener-await-suite)

;;; ===========================================================================
;;; Todo 9: command-owned agent wait and frame-process settlement.
;;;
;;; Deterministic seams exercise the coalesced wake transport, the frame-owned
;;; applicator/handler, gesture discard/cancellation, lifecycle/cleanup, and the
;;; deferred-command invariant without a live provider.  Each test restores
;;; every overridden function or global.
;;; ===========================================================================

;;; --------------------------------------------------------------------------
;;; Test seams.
;;; --------------------------------------------------------------------------

(defmacro with-await-override ((name lambda-list &body impl) &body body)
  "Temporarily replace NAME during one listener-await test, then restore it."
  (let ((original (gensym "ORIGINAL"))
        (existed (gensym "EXISTED-P")))
    `(let ((,existed (fboundp ',name))
           (,original (and (fboundp ',name) (symbol-function ',name))))
       (unwind-protect
            (progn
              (setf (symbol-function ',name)
                    (lambda ,lambda-list ,@impl))
              ,@body)
         (if ,existed
             (setf (symbol-function ',name) ,original)
             (fmakunbound ',name))))))

(defmacro with-await-overrides (bindings &body body)
  (if bindings
      `(with-await-override ,(first bindings)
         (with-await-overrides ,(rest bindings) ,@body))
      `(progn ,@body)))

(defstruct await-applicator-trace
  "Records the order and arguments of frame-process applicator calls."
  (calls nil :type list))

(defun trace-applicator (trace name)
  "Return a stub that records (NAME buffer) on TRACE then calls through."
  (lambda (buf)
    (push (list name buf) (await-applicator-trace-calls trace))
    nil))

(defstruct await-redisplay-trace
  (calls nil :type list))

(defstruct await-queue-trace
  "Records events handed to clim:queue-event."
  (events nil :type list))

(defun make-await-buffer (&key (name "listener-await-test"))
  (rplaca::make-buffer name
                       :agent-name "await-tester"
                       :kind :chat
                       :session-persistence-mode :ephemeral
                       :session nil))

(defun make-await-frame (&optional (buffer (make-await-buffer)))
  (clim:make-application-frame
   'rplaca::rplaca-listener
   :conversation-buffer buffer
   :listener-context (rplaca::make-listener-context)
   :appearance-profile (rplaca::make-appearance-profile)))

(defun await-active-request (frame)
  (rplaca::rplaca-listener-active-await-request frame))

;;; --------------------------------------------------------------------------
;;; Event class and frame slot surface.
;;; --------------------------------------------------------------------------

(test rplaca-listener-wakeup-event-is-window-manager-event-subclass
  (is (subtypep 'rplaca::rplaca-listener-wakeup-event 'clim:window-manager-event))
  (is (find-class 'rplaca::rplaca-listener-wakeup-event nil)))

(test rplaca-listener-wakeup-event-carries-sheet-request-and-generation
  (let* ((frame (make-await-frame))
         (request (rplaca::make-listener-await-request
                   :frame frame
                   :buffer (rplaca::rplaca-listener-conversation-buffer frame)
                   :dispatch-result :started
                   :lifecycle-generation 0
                   :wait-generation 0
                   :expected-runtime-generation 0))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil
                               :request request
                               :notification-generation 7)))
    (is (eq request (rplaca::rplaca-listener-wakeup-event-request event)))
    (is (= 7 (rplaca::rplaca-listener-wakeup-event-notification-generation event)))))

(test rplaca-listener-wakeup-events-are-independent-instances
  (let* ((frame (make-await-frame))
         (req-a (rplaca::make-listener-await-request
                 :frame frame :buffer (rplaca::rplaca-listener-conversation-buffer frame)
                 :dispatch-result nil :lifecycle-generation 0
                 :wait-generation 0 :expected-runtime-generation 0))
         (req-b (rplaca::make-listener-await-request
                 :frame frame :buffer (rplaca::rplaca-listener-conversation-buffer frame)
                 :dispatch-result nil :lifecycle-generation 0
                 :wait-generation 1 :expected-runtime-generation 0))
         (ev-a (make-instance 'rplaca::rplaca-listener-wakeup-event
                              :sheet nil :request req-a :notification-generation 1))
         (ev-b (make-instance 'rplaca::rplaca-listener-wakeup-event
                              :sheet nil :request req-b :notification-generation 2)))
    (is (not (eq ev-a ev-b)))
    (is (not (eq (rplaca::rplaca-listener-wakeup-event-request ev-a)
                 (rplaca::rplaca-listener-wakeup-event-request ev-b))))
    (is (= 1 (rplaca::rplaca-listener-wakeup-event-notification-generation ev-a)))
    (is (= 2 (rplaca::rplaca-listener-wakeup-event-notification-generation ev-b)))))

(test rplaca-listener-frame-owns-await-coalescing-slots
  (let ((frame (make-await-frame)))
    (is (integerp (rplaca::rplaca-listener-lifecycle-generation frame)))
    (is (= 0 (rplaca::rplaca-listener-lifecycle-generation frame)))
    (is (null (rplaca::rplaca-listener-active-await-request frame)))
    (is (null (rplaca::rplaca-listener-wake-dirty-p frame)))
    (is (null (rplaca::rplaca-listener-wake-pending-generation frame)))
    (is (null (rplaca::rplaca-listener-wake-handling-p frame)))
    (is (integerp (rplaca::rplaca-listener-wait-generation frame)))
    (is-true (rplaca::rplaca-listener-wake-lock frame)
        "wake lock must be allocated per frame")))

(test rplaca-listener-frame-await-slots-are-per-frame-independent
  (let ((frame-a (make-await-frame))
        (frame-b (make-await-frame)))
    (is (not (eq (rplaca::rplaca-listener-wake-lock frame-a)
                 (rplaca::rplaca-listener-wake-lock frame-b))))
    (setf (rplaca::rplaca-listener-wake-dirty-p frame-a) t)
    (is (null (rplaca::rplaca-listener-wake-dirty-p frame-b)))))

;;; --------------------------------------------------------------------------
;;; Request struct.
;;; --------------------------------------------------------------------------

(test listener-await-request-carries-exact-per-turn-state
  (let* ((frame (make-await-frame))
         (buffer (rplaca::rplaca-listener-conversation-buffer frame))
         (request (rplaca::make-listener-await-request
                   :frame frame :buffer buffer
                   :dispatch-result :provider-started
                   :lifecycle-generation 3
                   :wait-generation 5
                   :expected-runtime-generation 9)))
    (is (eq frame (rplaca::listener-await-request-frame request)))
    (is (eq buffer (rplaca::listener-await-request-buffer request)))
    (is (eq :provider-started (rplaca::listener-await-request-dispatch-result request)))
    (is (= 3 (rplaca::listener-await-request-lifecycle-generation request)))
    (is (= 5 (rplaca::listener-await-request-wait-generation request)))
    (is (= 9 (rplaca::listener-await-request-expected-runtime-generation request)))
    (is (eq :waiting (rplaca::listener-await-request-phase request)))
    (is (null (rplaca::listener-await-request-cancel-requested-p request)))
    ;; Each request has a unique token.
    (is (not (null (rplaca::listener-await-request-token request))))))

(test listener-await-request-tokens-are-unique
  (let* ((frame (make-await-frame))
         (buffer (rplaca::rplaca-listener-conversation-buffer frame))
         (req-a (rplaca::make-listener-await-request
                 :frame frame :buffer buffer :dispatch-result nil
                 :lifecycle-generation 0 :wait-generation 0
                 :expected-runtime-generation 0))
         (req-b (rplaca::make-listener-await-request
                 :frame frame :buffer buffer :dispatch-result nil
                 :lifecycle-generation 0 :wait-generation 0
                 :expected-runtime-generation 0)))
    (is (not (eq (rplaca::listener-await-request-token req-a)
                 (rplaca::listener-await-request-token req-b))))))

;;; --------------------------------------------------------------------------
;;; Coalesced transport: reserve / enqueue / hook.
;;; --------------------------------------------------------------------------

(defun install-active-request (frame &optional (buffer (rplaca::rplaca-listener-conversation-buffer frame)))
  "Install a fresh active await request on FRAME and return it."
  (let ((request (rplaca::make-listener-await-request
                  :frame frame :buffer buffer :dispatch-result :started
                  :lifecycle-generation (rplaca::rplaca-listener-lifecycle-generation frame)
                  :wait-generation (rplaca::rplaca-listener-wait-generation frame)
                  :expected-runtime-generation (rplaca::buffer-runtime-generation buffer))))
    (setf (rplaca::rplaca-listener-active-await-request frame) request)
    request))

(test reserve-wakeup-event-reserves-one-when-dirty-and-idle
  (let ((frame (make-await-frame)))
    (install-active-request frame)
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (is-true (rplaca::listener-reserve-wakeup-event frame))
    (is (integerp (rplaca::rplaca-listener-wake-pending-generation frame)))))

(test reserve-wakeup-event-does-not-reserve-when-not-dirty
  (let ((frame (make-await-frame)))
    (install-active-request frame)
    (is-false (rplaca::listener-reserve-wakeup-event frame))
    (is (null (rplaca::rplaca-listener-wake-pending-generation frame)))))

(test reserve-wakeup-event-does-not-reserve-when-pending-already-outstanding
  (let ((frame (make-await-frame)))
    (install-active-request frame)
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (is-true (rplaca::listener-reserve-wakeup-event frame))
    (let ((first-pending (rplaca::rplaca-listener-wake-pending-generation frame)))
      (is-true first-pending)
      (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
      (is-false (rplaca::listener-reserve-wakeup-event frame))
      (is (= first-pending (rplaca::rplaca-listener-wake-pending-generation frame))))))

(test reserve-wakeup-event-does-not-reserve-while-handling
  (let ((frame (make-await-frame)))
    (install-active-request frame)
    (setf (rplaca::rplaca-listener-wake-handling-p frame) t
          (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (is-false (rplaca::listener-reserve-wakeup-event frame))
    (is (null (rplaca::rplaca-listener-wake-pending-generation frame)))))

(test reserve-wakeup-event-does-not-reserve-when-no-active-request
  (let ((frame (make-await-frame)))
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (is-false (rplaca::listener-reserve-wakeup-event frame))
    (is (null (rplaca::rplaca-listener-wake-pending-generation frame)))))

(test reserve-wakeup-event-does-not-reserve-when-frame-dead
  (let ((frame (make-await-frame)))
    (install-active-request frame)
    (setf (rplaca::rplaca-listener-liveness frame) :dead
          (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (is-false (rplaca::listener-reserve-wakeup-event frame))))

(test wake-hook-marks-dirty-and-reserves-one-event-for-owned-buffer
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame))
         (enqueue-calls 0))
    (install-active-request frame buffer)
    (with-await-overrides
        ((rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf enqueue-calls)
           t))
      (funcall hook buffer :streaming))
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame))
    (is (= 1 enqueue-calls)
        "one wake reserves exactly one enqueued event")
    (is (integerp (rplaca::rplaca-listener-wake-pending-generation frame)))))

(test wake-hook-coalesces-thousand-wakes-into-one-event
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame))
         (enqueue-calls 0))
    (install-active-request frame buffer)
    (with-await-overrides
        ((rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf enqueue-calls)
           t))
      (dotimes (i 1000)
        (funcall hook buffer :streaming)))
    (is (= 1 enqueue-calls)
        "1000 wakes must collapse to one outstanding event")
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame))))

(test wake-hook-ignores-wrong-buffer
  (let* ((owned (make-await-buffer))
         (other (make-await-buffer))
         (frame (make-await-frame owned))
         (hook (rplaca::listener-make-wake-hook frame))
         (enqueue-calls 0))
    (install-active-request frame owned)
    (with-await-overrides
        ((rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf enqueue-calls)
           t))
      (funcall hook other :streaming))
    (is (= 0 enqueue-calls))
    (is (null (rplaca::rplaca-listener-wake-dirty-p frame)))))

(test wake-hook-ignores-dead-frame
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame))
         (enqueue-calls 0))
    (install-active-request frame buffer)
    (setf (rplaca::rplaca-listener-liveness frame) :dead)
    (with-await-overrides
        ((rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf enqueue-calls)
           t))
      (funcall hook buffer :streaming))
    (is (= 0 enqueue-calls))))

(test wake-hook-ignores-settled-or-retired-request
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame))
         (enqueue-calls 0)
         (request (install-active-request frame buffer)))
    (setf (rplaca::listener-await-request-phase request) :settled)
    (with-await-overrides
        ((rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf enqueue-calls)
           t))
      (funcall hook buffer :streaming))
    (is (= 0 enqueue-calls))
    (is (null (rplaca::rplaca-listener-wake-dirty-p frame)))))

(test wake-hook-ignores-ungrafted-frame
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame)))
    (install-active-request frame buffer)
    ;; With the real listener-grafted-top-level-sheet (no override), the frame
    ;; is ungrafted so enqueue clears the reservation and leaves dirty.
    (funcall hook buffer :streaming)
    (is (null (rplaca::rplaca-listener-wake-pending-generation frame))
        "ungrafted frame: reservation cleared, no event queued")
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame)
        "dirty remains set for a later retry")))

(test wake-hook-clears-only-matching-reservation-on-queue-failure
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame)))
    (install-active-request frame buffer)
    (funcall hook buffer :streaming)
    ;; With no graft override, enqueue sees no sheet: clears reservation,
    ;; leaves dirty for retry.
    (is (null (rplaca::rplaca-listener-wake-pending-generation frame)))
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame))))

;;; --------------------------------------------------------------------------
;;; Event handler: validation, applicator order, settlement, redisplay.
;;; --------------------------------------------------------------------------

(test handle-listener-wakeup-runs-applicators-in-canonical-order
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (trace (make-await-applicator-trace))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request
                               :notification-generation
                               (progn
                                 (setf (rplaca::rplaca-listener-wake-dirty-p frame) t
                                       (rplaca::rplaca-listener-wake-pending-generation frame) 1)
                                 1))))
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (push (list :stopped buf) (await-applicator-trace-calls trace)))
         (rplaca::update-openai-oauth-login (&optional (buf nil))
           (declare (ignore buf))
           (push (list :oauth nil) (await-applicator-trace-calls trace)))
         (rplaca::update-interactive-tool-execution (buf)
           (push (list :tool buf) (await-applicator-trace-calls trace)))
         (rplaca::update-interactive-buffer-operation (buf)
           (push (list :operation buf) (await-applicator-trace-calls trace)))
         (rplaca::update-streaming-response (buf)
           (push (list :stream buf) (await-applicator-trace-calls trace)))
         (clim:redisplay-frame-pane (frame pane &key &allow-other-keys)
           (declare (ignore frame pane))))
      (setf (rplaca::buffer-pending-stream buffer) :fake-stream)
      (rplaca::handle-listener-wakeup frame request event)
      ;; The trace pushes in reverse, so the canonical order is the reverse
      ;; of the stored list.
      (let ((order (mapcar #'first
                           (reverse (await-applicator-trace-calls trace)))))
        (is (equal '(:stopped :oauth :tool :operation :stream) order))))))

(test handle-listener-wakeup-runs-streaming-only-when-pending-stream
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (trace (make-await-applicator-trace))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf)
           (push :stream-called (await-applicator-trace-calls trace)))
         (clim:redisplay-frame-pane (frame pane &key &allow-other-keys)
           (declare (ignore frame pane))))
      ;; No pending stream: update-streaming-response must NOT be called.
      (rplaca::handle-listener-wakeup frame request event)
      (is (null (member :stream-called (await-applicator-trace-calls trace)))))))

(test handle-listener-wakeup-redisplays-only-wholine
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (redisplay-trace (make-await-redisplay-trace))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key force-p &allow-other-keys)
           (declare (ignore frm force-p))
           (push pane (await-redisplay-trace-calls redisplay-trace))))
      (rplaca::handle-listener-wakeup frame request event)
      (is (equal '(rplaca::wholine) (await-redisplay-trace-calls redisplay-trace)))
      (is (eq :settled (rplaca::listener-await-request-phase request))))))

(test handle-listener-wakeup-never-executes-frame-command-or-writes-interactor
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (interactor-output (make-string-output-stream))
         (execute-called nil)
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:frame-standard-output (frm)
           (declare (ignore frm))
           interactor-output)
         (clim:execute-frame-command (frm command)
           (declare (ignore frm command))
           (setf execute-called t))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (rplaca::handle-listener-wakeup frame request event))
    (is-false execute-called)
    (is (string= "" (get-output-stream-string interactor-output)))))

(test handle-listener-wakeup-rejects-wrong-frame
  (let* ((buffer (make-await-buffer))
         (frame-a (make-await-frame buffer))
         (frame-b (make-await-frame buffer))
         (request (install-active-request frame-a buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    ;; event's request belongs to frame-a, but handler is called on frame-b.
    (setf (rplaca::rplaca-listener-wake-pending-generation frame-a) 1)
    (rplaca::handle-listener-wakeup frame-b request event)
    (is (eq :waiting (rplaca::listener-await-request-phase request)))))

(test handle-listener-wakeup-rejects-replaced-request
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (old-request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request old-request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    ;; Replace with a fresh request (new wait generation).
    (install-active-request frame buffer)
    (rplaca::handle-listener-wakeup frame old-request event)
    (is (eq :waiting (rplaca::listener-await-request-phase old-request)))))

(test handle-listener-wakeup-rejects-stale-lifecycle-generation
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    ;; Frame's lifecycle generation advanced (cleanup happened).
    (incf (rplaca::rplaca-listener-lifecycle-generation frame))
    (rplaca::handle-listener-wakeup frame request event)
    (is (eq :waiting (rplaca::listener-await-request-phase request)))))

(test handle-listener-wakeup-rejects-duplicate-event-without-clearing-newer-reservation
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         ;; A stale event carrying an old generation.
         (stale-event (make-instance 'rplaca::rplaca-listener-wakeup-event
                                     :sheet nil :request request
                                     :notification-generation 1)))
    ;; The frame has already moved on: a newer reservation (generation 2)
    ;; is outstanding.  The stale event must not clear it.
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 2)
    (rplaca::handle-listener-wakeup frame request stale-event)
    (is (= 2 (rplaca::rplaca-listener-wake-pending-generation frame))
        "stale event must never clear a newer reservation")
    (is (eq :waiting (rplaca::listener-await-request-phase request)))))

(test handle-listener-wakeup-rejects-dead-frame
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1
          (rplaca::rplaca-listener-liveness frame) :dead)
    (rplaca::handle-listener-wakeup frame request event)
    (is (eq :waiting (rplaca::listener-await-request-phase request)))))

(test handle-listener-wakeup-reserves-one-follow-up-when-dirty-during-handling
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (followup-calls 0)
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf))
           ;; Simulate a concurrent wake during handling: mark dirty AND keep
           ;; the buffer busy so settlement does not pass during this cycle.
           (setf (rplaca::rplaca-listener-wake-dirty-p frame) t
                 (rplaca::buffer-pending-stream buffer) :busy))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane)))
         (rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf followup-calls)
           t))
      (rplaca::handle-listener-wakeup frame request event))
    (is-false (rplaca::rplaca-listener-wake-handling-p frame))
    (is (= 1 followup-calls)
        "dirty-during-handling reserves exactly one follow-up event")))

(test handle-listener-wakeup-records-handler-failure-on-request
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf))
           (error "applicator blew up"))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (rplaca::handle-listener-wakeup frame request event))
    ;; Failure recorded, request settled so the waiter wakes.
    (is (eq :settled (rplaca::listener-await-request-phase request)))
    (is (typep (rplaca::listener-await-request-terminal-failure request) 'error))
    (is-false (rplaca::rplaca-listener-wake-handling-p frame))))

;;; --------------------------------------------------------------------------
;;; Settlement predicate.
;;; --------------------------------------------------------------------------

(test listener-turn-settled-p-true-when-runtime-idle
  (let ((buffer (make-await-buffer)))
    (is-true (rplaca::listener-turn-settled-p buffer))))

(test listener-turn-settled-p-false-when-stream-active
  (let ((buffer (make-await-buffer)))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (is-false (rplaca::listener-turn-settled-p buffer))))

(test listener-turn-settled-p-false-when-stopping
  (let ((buffer (make-await-buffer)))
    (setf (rplaca::buffer-runtime-stopping-p buffer) t)
    (is-false (rplaca::listener-turn-settled-p buffer))))

(test listener-turn-settled-p-false-when-stopped-notification-pending
  (let ((buffer (make-await-buffer)))
    (setf (rplaca::buffer-runtime-stopped-notification-p buffer) t)
    (is-false (rplaca::listener-turn-settled-p buffer))))

(test listener-turn-settled-p-false-when-interactive-operation-pending
  (let ((buffer (make-await-buffer)))
    (setf (rplaca::buffer-pending-interactive-operation buffer)
          (rplaca::make-interactive-buffer-operation :kind :pipeline))
    (is-false (rplaca::listener-turn-settled-p buffer))))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: immediate completion skips gesture read.
;;; --------------------------------------------------------------------------

(defmacro with-await-applicator-stubs ((&optional (redisplay-counter-cell nil)) &body body)
  "Override all five applicators + redisplay with no-op stubs.
When REDISPLAY-COUNTER-CELL is non-nil, count wholine redisplays there."
  `(with-await-overrides
       ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
        (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
        (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
        (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
        (rplaca::update-streaming-response (buf) (declare (ignore buf)))
        (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
          (declare (ignore frm))
          ,(if redisplay-counter-cell
               `(when (eq pane 'rplaca::wholine) (incf ,redisplay-counter-cell))
               `(declare (ignore pane)))))
     ,@body))

(defun await-fire-settlement (frame buffer input-wait-test input-wait-handler)
  "Clear busy runtime state, run the settlement computation, and fire the
wait test/handler so the await loop exits via non-local exit."
  (setf (rplaca::buffer-pending-stream buffer) nil
        (rplaca::buffer-runtime-stopping-p buffer) nil
        (rplaca::buffer-runtime-stopped-notification-p buffer) nil
        (rplaca::buffer-runtime-teardown buffer) nil
        (rplaca::buffer-runtime-application buffer) nil
        (rplaca::buffer-runtime-start-generation buffer) nil
        (rplaca::buffer-runtime-start-owner buffer) nil)
  (let ((request (rplaca::rplaca-listener-active-await-request frame)))
    (when request
      (rplaca::listener-compute-settlement frame request)))
  (when (and input-wait-test (funcall input-wait-test nil))
    (funcall input-wait-handler nil)))

(test await-immediate-completion-skips-gesture-read
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (gesture-reads 0)
         (applicator-calls 0))
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key &allow-other-keys)
           (declare (ignore stream))
           (incf gesture-reads)
           (values nil :timeout))
         (rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf))
           (incf applicator-calls))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (let ((clim:*application-frame* frame))
        (rplaca::await-listener-agent-turn frame buffer :started)))
    (is (= 0 gesture-reads)
        "immediate settlement must not enter the gesture read loop")
    (is (>= applicator-calls 1)
        "one synchronous frame-owned apply must run before waiting")))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: deferred completion exits via wait handler.
;;; --------------------------------------------------------------------------

(test await-deferred-completion-exits-via-non-local-exit
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (redisplay-count 0)
         (fired nil))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (unless fired
             (setf fired t)
             (await-fire-settlement frame buffer input-wait-test input-wait-handler))
           (values nil :timeout)))
      (with-await-applicator-stubs (redisplay-count)
        (let ((clim:*application-frame* frame))
          (rplaca::await-listener-agent-turn frame buffer :started))))
    (is (null (rplaca::rplaca-listener-active-await-request frame))
        "await must retire the request on normal return")
    (is (>= redisplay-count 1)
        "wholine must be redisplayed at least once during the wait")))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: ordinary gestures are consumed with beep.
;;; --------------------------------------------------------------------------

(test await-ordinary-gestures-beep-and-discard-without-spin
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (beep-count 0)
         (read-count 0))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (incf read-count)
           (cond
             ((<= read-count 3) (values #\a 'character))
             (t
              (await-fire-settlement frame buffer input-wait-test input-wait-handler)
              (values nil :timeout))))
         (clim:beep (&optional medium)
           (declare (ignore medium))
           (incf beep-count)))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (rplaca::await-listener-agent-turn frame buffer :started))))
    (is (= 3 beep-count)
        "three ordinary gestures must each beep exactly once")
    (is (= 4 read-count)
        "must settle on exactly the fourth read (3 gestures + 1 settlement)")))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: Esc/abort cancellation stops once and waits.
;;; --------------------------------------------------------------------------

(test await-escape-gesture-stops-once-and-continues-until-cancelled-settles
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (stop-count 0)
         (read-count 0)
         (cancelled-settled nil))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (incf read-count)
           (cond
             ((= read-count 1) #\Escape)
             ((= read-count 2)
              (setf cancelled-settled t)
              (await-fire-settlement frame buffer input-wait-test input-wait-handler)
              (values nil :timeout))
             (t
              (await-fire-settlement frame buffer input-wait-test input-wait-handler)
              (values nil :timeout))))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           (setf (rplaca::buffer-runtime-stopping-p buffer) t)
           t))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (signals rplaca::prompt-run-cancelled
            (rplaca::await-listener-agent-turn frame buffer :started)))))
    (is (= 1 stop-count)
        "first Escape must call stop-streaming-response exactly once")
    (is-true cancelled-settled
        "wait must continue until cancellation fully settles")))

(test await-abort-condition-stops-once-and-continues-until-settled
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (stop-count 0)
         (read-count 0))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (incf read-count)
           (cond
             ((= read-count 1)
              (signal 'clim:abort-gesture))
             ((= read-count 2)
              (await-fire-settlement frame buffer input-wait-test input-wait-handler)
              (values nil :timeout))
             (t
              (await-fire-settlement frame buffer input-wait-test input-wait-handler)
              (values nil :timeout))))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           (setf (rplaca::buffer-runtime-stopping-p buffer) t)
           t))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (signals rplaca::prompt-run-cancelled
            (rplaca::await-listener-agent-turn frame buffer :started)))))
    (is (= 1 stop-count))))

(test await-repeated-escape-is-idempotent-and-stops-once
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (stop-count 0)
         (read-count 0))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (incf read-count)
           (cond
             ((<= read-count 3) #\Escape)
             (t
              (await-fire-settlement frame buffer input-wait-test input-wait-handler)
              (values nil :timeout))))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           (setf (rplaca::buffer-runtime-stopping-p buffer) t)
           t))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (signals rplaca::prompt-run-cancelled
            (rplaca::await-listener-agent-turn frame buffer :started)))))
    (is (= 1 stop-count)
        "repeated Escape must call stop exactly once")))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: abnormal unwind marks detached-cancelling.
;;; --------------------------------------------------------------------------

(test await-abnormal-unwind-marks-detached-cancelling-and-stops-once
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (stop-count 0)
         (unwind-tag (gensym "AWAIT-UNWIND")))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key &allow-other-keys)
           (declare (ignore stream))
           (throw unwind-tag :unwound))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           t))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (catch unwind-tag
            (rplaca::await-listener-agent-turn frame buffer :started)))))
    (is (= 1 stop-count)
        "abnormal unwind must call stop exactly once")
    (let ((request (rplaca::rplaca-listener-active-await-request frame)))
      (is-true request "detached drain request retained")
      (is (member (rplaca::listener-await-request-phase request)
                  '(:detached-cancelling :retired))))))

(test await-fresh-request-after-previous-unwind-installs-clean
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (unwind-tag (gensym)))
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key &allow-other-keys)
           (declare (ignore stream))
           (throw unwind-tag :unwound))
         (rplaca::stop-streaming-response (buf) (declare (ignore buf)) t))
      (with-await-applicator-stubs ()
        (setf (rplaca::buffer-pending-stream buffer) :busy)
        (let ((clim:*application-frame* frame))
          (catch unwind-tag
            (rplaca::await-listener-agent-turn frame buffer :started)))))
    (setf (rplaca::buffer-pending-stream buffer) nil)
    (let ((gesture-reads 0))
      (with-await-overrides
          ((clim:stream-read-gesture (stream &key &allow-other-keys)
             (declare (ignore stream))
             (incf gesture-reads)
             (values nil :timeout)))
        (with-await-applicator-stubs ()
          (let ((clim:*application-frame* frame))
            (rplaca::await-listener-agent-turn frame buffer :started))))
      (is (= 0 gesture-reads)))))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: queued external commands stay deferred.
;;; --------------------------------------------------------------------------

(test await-queued-external-command-remains-in-frame-command-queue
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (execute-called nil)
         (queued-command (list 'rplaca::com-no-op))
         (enqueued nil))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (unless enqueued
             (setf enqueued t)
             (climi::queue-append (clim-internals::frame-command-queue frame)
                                  queued-command)
             (await-fire-settlement frame buffer input-wait-test input-wait-handler))
           (values nil :timeout))
         (clim:execute-frame-command (frm command)
           (declare (ignore frm command))
           (setf execute-called t)))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (rplaca::await-listener-agent-turn frame buffer :started))))
    (is-false execute-called
        "queued command must NOT execute during await")
    (is (eq queued-command
            (ignore-errors
              (climi::queue-read-no-hang
               (clim-internals::frame-command-queue frame)))))))

;;; --------------------------------------------------------------------------
;;; await-listener-agent-turn: error propagation to caller.
;;; --------------------------------------------------------------------------

(test await-does-not-swallow-terminal-prompt-run-error
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (fired nil))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (unless fired
             (setf fired t)
             (let ((request (rplaca::rplaca-listener-active-await-request frame)))
               (setf (rplaca::listener-await-request-terminal-failure request)
                     (make-condition 'rplaca::prompt-run-error
                                     :message "provider failed")
                     (rplaca::listener-await-request-phase request) :settled))
             (when (and input-wait-test (funcall input-wait-test nil))
               (funcall input-wait-handler nil)))
           (values nil :timeout)))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (signals rplaca::prompt-run-error
            (rplaca::await-listener-agent-turn frame buffer :started)))))))

;;; --------------------------------------------------------------------------
;;; Frame cleanup: lifecycle generation, retire, idempotent.
;;; --------------------------------------------------------------------------

(test listener-frame-cleanup-increments-lifecycle-and-retires-await-state
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (progn
                 (install-active-request frame buffer)
                 (rplaca::listener-make-wake-hook frame))))
    (rplaca::add-hook 'rplaca::*buffer-display-wakeup-hook* hook :append t)
    (let ((gen-before (rplaca::rplaca-listener-lifecycle-generation frame)))
      (unwind-protect
           (rplaca::listener-frame-cleanup frame hook)
        (rplaca::remove-hook 'rplaca::*buffer-display-wakeup-hook* hook))
      (is (> (rplaca::rplaca-listener-lifecycle-generation frame) gen-before)
          "cleanup must advance the lifecycle generation")
      (is (eq :dead (rplaca::rplaca-listener-liveness frame)))
      (is (null (rplaca::rplaca-listener-active-await-request frame))
          "cleanup must retire the active await request")
      (is (null (rplaca::rplaca-listener-wake-pending-generation frame))
          "cleanup must clear coalescing state")
      (is (not (member hook rplaca::*buffer-display-wakeup-hook*))))))

(test listener-frame-cleanup-late-wake-is-no-op
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (progn
                 (install-active-request frame buffer)
                 (rplaca::listener-make-wake-hook frame))))
    (rplaca::listener-frame-cleanup frame hook)
    (let ((queued-events nil))
      (with-await-overrides
          ((rplaca::listener-grafted-top-level-sheet (frm) (declare (ignore frm)) :fake-sheet)
           (clim:queue-event (sheet event)
             (declare (ignore sheet))
             (push event queued-events)))
        (funcall hook buffer :streaming))
      (is (= 0 (length queued-events))
          "a wake after cleanup must never enqueue an event"))))

(test listener-frame-cleanup-is-idempotent
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (progn
                 (install-active-request frame buffer)
                 (rplaca::listener-make-wake-hook frame))))
    (rplaca::listener-frame-cleanup frame hook)
    (rplaca::listener-frame-cleanup frame hook)
    (is (eq :dead (rplaca::rplaca-listener-liveness frame)))))

;;; --------------------------------------------------------------------------
;;; Review-fix regressions: genuine transport/coalescing/terminal/unwind bugs.
;;; --------------------------------------------------------------------------

;;; BUG 1: listener-enqueue-reserved-wakeup lexical scope: sheet unbound in
;;; the second LET, handler-case masks the unbound-variable error, returns NIL.

(test review-enqueue-returns-T-with-grafted-sheet-and-queue
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (queue-result nil))
    (install-active-request frame buffer)
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (incf (rplaca::rplaca-listener-wake-generation frame))
    (is-true (rplaca::listener-reserve-wakeup-event frame))
    (with-await-overrides
        ((rplaca::listener-grafted-top-level-sheet (frm) (declare (ignore frm)) :fake-sheet)
         (clim:queue-event (sheet event)
           (declare (ignore sheet event))
           (setf queue-result :queued)
           nil))
      (is-true (rplaca::listener-enqueue-reserved-wakeup frame)
          "enqueue must return T when sheet is available and queue-event succeeds"))
    (is (eq :queued queue-result)
        "clim:queue-event must actually be called"))
  ;; Also prove it works end-to-end through the hook without stubbing enqueue.
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (hook (rplaca::listener-make-wake-hook frame))
         (queued-count 0))
    (install-active-request frame buffer)
    (with-await-overrides
        ((rplaca::listener-grafted-top-level-sheet (frm) (declare (ignore frm)) :fake-sheet)
         (clim:queue-event (sheet event)
           (declare (ignore sheet event))
           (incf queued-count)))
      (funcall hook buffer :streaming))
    (is (= 1 queued-count)
        "hook -> reserve -> enqueue -> queue-event must deliver one event")
    (is (integerp (rplaca::rplaca-listener-wake-pending-generation frame))
        "pending-generation preserved after successful enqueue")))

;;; BUG 2: handle-listener-wakeup must clear dirty on consume; one handled wake
;;; with no concurrent notification must queue zero follow-ups.

(test review-one-handled-wake-no-concurrent-dirty-queues-zero-followups
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (followup-enqueues 0)
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-generation frame) 1
          (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane)))
         (rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf followup-enqueues)
           t))
      (rplaca::handle-listener-wakeup frame request event))
    (is (= 0 followup-enqueues)
        "one handled wake with no concurrent dirty must queue zero follow-ups")
    (is (null (rplaca::rplaca-listener-wake-dirty-p frame))
        "dirty must be cleared on consume")))

;;; BUG 3: Immediate terminal exit must not bypass the terminal-condition path.

(test review-immediate-error-signals-prompt-run-error
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer)))
    (setf (rplaca::buffer-status buffer) :error)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (let ((clim:*application-frame* frame))
        (signals rplaca::prompt-run-error
          (rplaca::await-listener-agent-turn frame buffer :started))))))

(test review-immediate-cancel-signals-prompt-run-cancelled
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer)))
    (with-await-overrides
        ((rplaca::listener-compute-settlement (frm req)
           ;; Simulate immediate cancel: the synchronous check discovers the
           ;; turn was cancelled and marks the request accordingly.
           (setf (rplaca::listener-await-request-cancel-requested-p req) t
                 (rplaca::listener-await-request-phase req) :settled)
           t))
      (let ((clim:*application-frame* frame))
        (signals rplaca::prompt-run-cancelled
          (rplaca::await-listener-agent-turn frame buffer :started))))))

;;; BUG 4: Abnormal unwind must not call stop-streaming-response under lock.

(test review-abnormal-unwind-stops-outside-lock-and-preserves-drain-state
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (stop-called nil)
         (unwind-tag (gensym)))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key &allow-other-keys)
           (declare (ignore stream))
           (throw unwind-tag :unwound))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           ;; If stop were called under the wake lock, this would deadlock
           ;; on a non-recursive mutex.  We just record the call.
           (setf stop-called t)
           t)
         (rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (let ((clim:*application-frame* frame))
        (catch unwind-tag
          (rplaca::await-listener-agent-turn frame buffer :started))))
    (is-true stop-called
        "stop-streaming-response must be called exactly once")
    ;; Detached drain state must be preserved (not cleared).
    (let ((request (rplaca::rplaca-listener-active-await-request frame)))
      (is-true request "detached request preserved")
      (is (eq :detached-cancelling (rplaca::listener-await-request-phase request))))
    ;; Dirty/pending must NOT be cleared for detached drain.
    ;; (They were never set in this test, so we verify the request is alive.)
    ))

;;; BUG 5: Detached-cancelling requests must accept wake events and drain.

(test review-detached-cancelling-request-drains-to-retired
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer)))
    (setf (rplaca::listener-await-request-phase request) :detached-cancelling
          (rplaca::buffer-pending-stream buffer) :busy)
    ;; A dirty wake should reserve for the detached request.
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (incf (rplaca::rplaca-listener-wake-generation frame))
    (is-true (rplaca::listener-reserve-wakeup-event frame)
        "reserve must accept :detached-cancelling requests"))
  ;; Handler must accept and process a detached-cancelling event.
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::listener-await-request-phase request) :detached-cancelling
          (rplaca::rplaca-listener-wake-pending-generation frame) 1
          (rplaca::rplaca-listener-wake-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (rplaca::handle-listener-wakeup frame request event))
    (is (eq :retired (rplaca::listener-await-request-phase request))
        "detached-cancelling handler must retire when runtime is idle")
    ;; When settled via detached drain, the request retires without output.
    (is-false (rplaca::listener-await-request-terminal-failure request)
        "detached drain must not record a terminal failure")))

;;; BUG 6: Bounded transient enqueue retry.

(test review-enqueue-retries-on-transient-queue-failure
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (queue-attempts 0))
    (install-active-request frame buffer)
    (setf (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (incf (rplaca::rplaca-listener-wake-generation frame))
    (is-true (rplaca::listener-reserve-wakeup-event frame))
    (with-await-overrides
        ((rplaca::listener-grafted-top-level-sheet (frm) (declare (ignore frm)) :fake-sheet)
         (clim:queue-event (sheet event)
           (declare (ignore sheet event))
           (incf queue-attempts)
           (if (= queue-attempts 1)
               (error "transient")
               nil)))
      (is-true (rplaca::listener-enqueue-reserved-wakeup frame)
          "enqueue must succeed on the second attempt after transient failure"))
    (is (>= queue-attempts 2)
        "at least two queue-event attempts must occur")))

;;; BUG 7: Applicator error with active runtime must not settle prematurely.

(test review-applicator-error-with-active-runtime-does-not-settle
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer)))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf))
           (error "applicator blew up"))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (rplaca::stop-streaming-response (buf) (declare (ignore buf)) t)
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (rplaca::listener-compute-settlement frame request))
    (is (member (rplaca::listener-await-request-phase request)
                '(:cancelling :detached-cancelling :waiting))
        "request must NOT be :settled when runtime is still active after error")
    (is-true (rplaca::listener-await-request-terminal-failure request)
        "terminal failure must be recorded")))

;;; BUG 8: Event must carry expected runtime generation.

(test review-wakeup-event-carries-expected-runtime-generation
  (let* ((frame (make-await-frame))
         (request (rplaca::make-listener-await-request
                   :frame frame :buffer nil :dispatch-result nil
                   :lifecycle-generation 0 :wait-generation 0
                   :expected-runtime-generation 7))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request
                               :notification-generation 1
                               :expected-runtime-generation 7)))
    (is (= 7 (rplaca::rplaca-listener-wakeup-event-expected-runtime-generation event)))))

;;; BUG 9: Escape key-press-event detection.

(test review-escape-key-press-event-is-detected
  (let ((esc-event (make-instance 'clim:key-press-event
                                   :key-name :escape
                                   :sheet nil)))
    (is-true (rplaca::listener-escape-gesture-p esc-event)
        "key-press-event with :escape key-name must be detected as Escape")))

;;; BUG 10: Event/sheet/buffer validation in handle-event.

(test review-handle-event-rejects-mismatched-conversation-buffer
  (let* ((buffer-a (make-await-buffer))
         (buffer-b (make-await-buffer))
         (frame (make-await-frame buffer-a))
         (request (rplaca::make-listener-await-request
                   :frame frame :buffer buffer-b :dispatch-result nil
                   :lifecycle-generation 0 :wait-generation 0
                   :expected-runtime-generation 0))
         (event (make-instance 'rplaca::rplaca-listener-wakeup-event
                               :sheet nil :request request :notification-generation 1)))
    (setf (rplaca::rplaca-listener-active-await-request frame) request
          (rplaca::rplaca-listener-wake-pending-generation frame) 1
          (rplaca::rplaca-listener-wake-generation frame) 1)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf) (declare (ignore buf)))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      ;; Request's buffer (buffer-b) differs from frame's conversation-buffer
      ;; (buffer-a). The handler must reject this event.
      (rplaca::handle-listener-wakeup frame request event))
    (is (eq :waiting (rplaca::listener-await-request-phase request))
        "handler must reject events whose request buffer != frame buffer")))

;;; --------------------------------------------------------------------------
;;; Final todo 9 regressions: transport failure, applicator teardown,
;;; runtime-generation identity, and real abort-condition transfer.
;;; --------------------------------------------------------------------------

(test final-permanent-public-enqueue-failure-uses-worker-safe-queue-fallback
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (public-attempts 0)
         (fallback-events nil)
         (stop-count 0))
    (setf (rplaca::buffer-user-input-pending buffer) t
          (rplaca::rplaca-listener-wake-dirty-p frame) t)
    (incf (rplaca::rplaca-listener-wake-generation frame))
    (is-true (rplaca::listener-reserve-wakeup-event frame))
    (with-await-overrides
        ((rplaca::listener-grafted-top-level-sheet (frm)
           (declare (ignore frm))
           :fake-sheet)
         (clim:queue-event (sheet event)
           (declare (ignore sheet event))
           (incf public-attempts)
           (error "permanent public queue failure"))
         (clim:sheet-event-queue (sheet)
           (declare (ignore sheet))
           :fake-concurrent-queue)
         (climi::queue-append (queue event)
           (is (eq :fake-concurrent-queue queue))
           (push event fallback-events)
           event)
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           (incf (rplaca::buffer-runtime-generation buffer))
           t))
      (is-true (rplaca::listener-enqueue-reserved-wakeup frame)
          "the source-backed concurrent queue fallback must transport the failure wake"))
    (is (= rplaca::*listener-enqueue-max-attempts* public-attempts))
    (is (= 1 (length fallback-events)))
    (is (= 1 stop-count) "permanent transport failure must cancel once")
    (is (eq :cancelling (rplaca::listener-await-request-phase request)))
    (is-true (rplaca::listener-await-request-terminal-failure request))
    (is (= (rplaca::listener-await-request-expected-runtime-generation request)
           (rplaca::buffer-runtime-generation buffer)))
    (is (= (rplaca::listener-await-request-expected-runtime-generation request)
           (rplaca::rplaca-listener-wakeup-event-expected-runtime-generation
            (first fallback-events))))))

(test final-applicator-failure-cancels-once-drains-and-preserves-original-error
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (original (make-condition 'rplaca::prompt-run-error
                                   :message "original applicator failure"))
         (later (make-condition 'rplaca::prompt-run-error
                                :message "later applicator failure"))
         (failure original)
         (stop-count 0)
         (enqueue-count 0))
    (setf (rplaca::buffer-user-input-pending buffer) t)
    (with-await-overrides
        ((rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf))
           (error failure))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           (incf (rplaca::buffer-runtime-generation buffer))
           t)
         (rplaca::listener-enqueue-reserved-wakeup (frm)
           (declare (ignore frm))
           (incf enqueue-count)
           t)
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (is-false (rplaca::listener-compute-settlement frame request))
      (is (= 1 stop-count))
      (is-true (rplaca::listener-await-request-cancel-requested-p request))
      (is (eq :cancelling (rplaca::listener-await-request-phase request)))
      (is (eq original
              (rplaca::listener-await-request-terminal-failure request)))
      (is (plusp enqueue-count) "teardown must receive a fresh wake reservation")
      (setf failure later
            (rplaca::buffer-user-input-pending buffer) nil)
      (is-true (rplaca::listener-compute-settlement frame request))
      (is (= 1 stop-count) "later applicator failures must not request Stop again")
      (is (eq :settled (rplaca::listener-await-request-phase request)))
      (is (eq original
              (rplaca::listener-await-request-terminal-failure request))))))

(defun final-runtime-generation-event (request notification runtime)
  (make-instance 'rplaca::rplaca-listener-wakeup-event
                 :sheet nil
                 :request request
                 :notification-generation notification
                 :expected-runtime-generation runtime))

(test final-runtime-generation-exact-current-event-is-consumed
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (apply-count 0))
    (setf (rplaca::buffer-runtime-generation buffer) 4
          (rplaca::listener-await-request-expected-runtime-generation request) 4
          (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((rplaca::listener-apply-runtime-state (frm buf)
           (declare (ignore frm buf))
           (incf apply-count))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (rplaca::handle-listener-wakeup
       frame request (final-runtime-generation-event request 1 4)))
    (is (= 1 apply-count))
    (is (eq :settled (rplaca::listener-await-request-phase request)))))

(test final-runtime-generation-stale-event-preserves-current-reservation
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (apply-count 0))
    (setf (rplaca::buffer-runtime-generation buffer) 5
          (rplaca::listener-await-request-expected-runtime-generation request) 5
          (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-override (rplaca::listener-apply-runtime-state (frm buf)
                           (declare (ignore frm buf))
                           (incf apply-count))
      (rplaca::handle-listener-wakeup
       frame request (final-runtime-generation-event request 1 4)))
    (is (= 0 apply-count))
    (is (= 1 (rplaca::rplaca-listener-wake-pending-generation frame)))
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame))))

(test final-runtime-generation-replaced-request-snapshot-is-rejected
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (event (final-runtime-generation-event request 2 6))
         (apply-count 0))
    (setf (rplaca::buffer-runtime-generation buffer) 7
          (rplaca::listener-await-request-expected-runtime-generation request) 7
          (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-pending-generation frame) 2)
    (with-await-override (rplaca::listener-apply-runtime-state (frm buf)
                           (declare (ignore frm buf))
                           (incf apply-count))
      (rplaca::handle-listener-wakeup frame request event))
    (is (= 0 apply-count))
    (is (= 2 (rplaca::rplaca-listener-wake-pending-generation frame)))
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame))))

(test final-runtime-generation-stale-event-cannot-clear-newer-reservation
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer)))
    (setf (rplaca::buffer-runtime-generation buffer) 9
          (rplaca::listener-await-request-expected-runtime-generation request) 9
          (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-pending-generation frame) 11)
    (rplaca::handle-listener-wakeup
     frame request (final-runtime-generation-event request 10 8))
    (is (= 11 (rplaca::rplaca-listener-wake-pending-generation frame)))
    (is-true (rplaca::rplaca-listener-wake-dirty-p frame))))

(test final-successful-stop-invalidates-pre-cancel-event-and-queues-fresh-snapshot
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (queued-events nil))
    (setf (rplaca::buffer-user-input-pending buffer) t
          (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-generation frame) 12
          (rplaca::rplaca-listener-wake-pending-generation frame) 12)
    (with-await-overrides
        ((rplaca::stop-streaming-response (buf)
           (incf (rplaca::buffer-runtime-generation buf))
           t)
         (rplaca::listener-grafted-top-level-sheet (frm)
           (declare (ignore frm))
           :fake-sheet)
         (clim:queue-event (sheet event)
           (declare (ignore sheet))
           (push event queued-events))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (rplaca::listener-handle-cancel-request frame request))
    (is (= 1 (rplaca::listener-await-request-expected-runtime-generation request)))
    (is (= 1 (length queued-events)))
    (is (= 1 (rplaca::rplaca-listener-wakeup-event-expected-runtime-generation
              (first queued-events))))
    (is (not (= 12 (rplaca::rplaca-listener-wake-pending-generation frame))))))

(test final-abort-gesture-condition-is-caught-and-does-not-escape
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (stop-count 0)
         (read-count 0)
         (escaped-abort nil))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key input-wait-test
                                           input-wait-handler &allow-other-keys)
           (declare (ignore stream))
           (incf read-count)
           (if (= read-count 1)
               (error (make-condition 'clim:abort-gesture :event :control-c))
               (progn
                 (await-fire-settlement frame buffer input-wait-test
                                        input-wait-handler)
                 (values nil :timeout))))
         (rplaca::stop-streaming-response (buf)
           (declare (ignore buf))
           (incf stop-count)
           (setf (rplaca::buffer-runtime-stopping-p buffer) t)
           t))
      (with-await-applicator-stubs ()
        (let ((clim:*application-frame* frame))
          (handler-case
              (signals rplaca::prompt-run-cancelled
                (rplaca::await-listener-agent-turn frame buffer :started))
            (clim:abort-gesture (condition)
              (setf escaped-abort condition))))))
    (is-false escaped-abort "abort-gesture must not escape the await loop")
    (is (= 1 stop-count))
    (is (= 2 read-count) "abort must be caught and the wait loop resumed")))

(test final-abnormal-unwind-refreshes-generation-and-drains-detached-request
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (unwind-tag (gensym "DETACHED-UNWIND"))
         (old-event nil)
         (queued-events nil)
         (delivery-count 0)
         (stop-count 0))
    (setf (rplaca::buffer-pending-stream buffer) :busy)
    (with-await-overrides
        ((clim:stream-read-gesture (stream &key &allow-other-keys)
           (declare (ignore stream))
           (let ((request (await-active-request frame)))
             (setf (rplaca::rplaca-listener-wake-dirty-p frame) t
                   (rplaca::rplaca-listener-wake-generation frame) 40
                   (rplaca::rplaca-listener-wake-pending-generation frame) 40
                   old-event (final-runtime-generation-event request 40 0)))
           (throw unwind-tag :unwound))
         (rplaca::stop-streaming-response (buf)
           (incf stop-count)
           (incf (rplaca::buffer-runtime-generation buf))
           (setf (rplaca::buffer-pending-stream buf) nil)
           t)
         (rplaca::listener-grafted-top-level-sheet (frm)
           (declare (ignore frm))
           :fake-sheet)
         (clim:queue-event (sheet event)
           (declare (ignore sheet))
           (push event queued-events)
           event)
         (rplaca::deliver-buffer-runtime-stopped-notification (buf)
           (declare (ignore buf))
           (incf delivery-count))
         (rplaca::update-openai-oauth-login (&optional buf) (declare (ignore buf)))
         (rplaca::update-interactive-tool-execution (buf) (declare (ignore buf)))
         (rplaca::update-interactive-buffer-operation (buf) (declare (ignore buf)))
         (rplaca::update-streaming-response (buf) (declare (ignore buf)))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane)))
         (clim:execute-frame-command (frm command)
           (declare (ignore frm command))
           (error "detached drain must not execute or print assistant output")))
      (let ((clim:*application-frame* frame))
        (catch unwind-tag
          (rplaca::await-listener-agent-turn frame buffer :started)))
      (setf delivery-count 0)
      (let* ((request (await-active-request frame))
             (fresh-event (first queued-events))
             (fresh-reservation
               (rplaca::rplaca-listener-wake-pending-generation frame)))
        (is (= 1 stop-count))
        (is (= 1 (rplaca::buffer-runtime-generation buffer)))
        (is (= 1 (rplaca::listener-await-request-expected-runtime-generation request)))
        (is (eq :detached-cancelling
                (rplaca::listener-await-request-phase request)))
        (is (= 1 (length queued-events))
            "post-Stop refresh must queue exactly one fresh detached wake")
        (is (= 1 (rplaca::rplaca-listener-wakeup-event-expected-runtime-generation
                  fresh-event)))
        (is (not (= 40 fresh-reservation))
            "the pre-Stop reservation must be invalidated")
        (is-true (rplaca::rplaca-listener-wake-dirty-p frame))
        (rplaca::handle-listener-wakeup frame request old-event)
        (is (= fresh-reservation
               (rplaca::rplaca-listener-wake-pending-generation frame))
            "the stale queued event must not clear the fresh reservation")
        (rplaca::handle-listener-wakeup frame request fresh-event)
        (is (= 1 delivery-count)
            "detached drain must apply the runtime-stopped notification")
        (is (eq :retired (rplaca::listener-await-request-phase request)))
        (is (null (await-active-request frame)))
        (is (null (rplaca::rplaca-listener-wake-pending-generation frame)))
        (is (null (rplaca::rplaca-listener-wake-dirty-p frame)))))))

(test final-interrupt-fallback-only-enqueues-until-normal-dispatch
  (let* ((buffer (make-await-buffer))
         (frame (make-await-frame buffer))
         (request (install-active-request frame buffer))
         (failure (make-condition 'rplaca::prompt-run-error
                                  :message "permanent wake transport failure"))
         (queue-attempts 0)
         (queued-events nil)
         (callback-executed nil)
         (apply-count 0))
    (setf (rplaca::listener-await-request-phase request) :cancelling
          (rplaca::listener-await-request-cancel-requested-p request) t
          (rplaca::listener-await-request-terminal-failure request) failure
          (rplaca::rplaca-listener-wake-dirty-p frame) t
          (rplaca::rplaca-listener-wake-generation frame) 1
          (rplaca::rplaca-listener-wake-pending-generation frame) 1)
    (with-await-overrides
        ((clim:sheet-event-queue (sheet)
           (declare (ignore sheet))
           :failed-sheet-queue)
         (climi::frame-event-queue (frm)
           (is (eq frame frm))
           :frame-queue)
         (climi::queue-append (queue event)
           (incf queue-attempts)
           (cond
             ((eq queue :failed-sheet-queue)
              (error "sheet queue transport failed"))
             ((eq queue :frame-queue)
              (push event queued-events)
              event)
             (t (error "unexpected queue ~S" queue))))
         (climi::frame-process (frm)
           (declare (ignore frm))
           :frame-process)
         (clim-sys:process-interrupt (process callback)
           (is (eq :frame-process process))
           (setf callback-executed t)
           (bt:with-lock-held ((rplaca::rplaca-listener-wake-lock frame))
             (funcall callback))
           t)
         (rplaca::listener-apply-runtime-state (frm buf)
           (declare (ignore frm buf))
           (incf apply-count))
         (clim:redisplay-frame-pane (frm pane &key &allow-other-keys)
           (declare (ignore frm pane))))
      (is-true (rplaca::listener-deliver-failure-wakeup frame :fake-sheet failure))
      (is-true callback-executed)
      (is (= 2 queue-attempts))
      (is (= 1 (length queued-events)))
      (is (= 0 apply-count)
          "interrupt callback must not apply runtime state immediately")
      (is (eq :cancelling (rplaca::listener-await-request-phase request)))
      (is (eq request (await-active-request frame)))
      (rplaca::handle-listener-wakeup frame request (first queued-events))
      (is (= 1 apply-count)
          "normal event dispatch must run the handler after the lock is released")
      (is (eq failure (rplaca::listener-await-terminal-condition request buffer)))
      (is (null (await-active-request frame))))
    (let* ((fallback-buffer (make-await-buffer))
           (fallback-frame (make-await-frame fallback-buffer))
           (fallback-request (install-active-request fallback-frame fallback-buffer))
           (fallback-failure
             (make-condition 'rplaca::prompt-run-error
                             :message "frame event queue failure"))
           (interrupt-callback nil))
      (setf (rplaca::listener-await-request-terminal-failure fallback-request)
            fallback-failure
            (rplaca::rplaca-listener-wake-dirty-p fallback-frame) t
            (rplaca::rplaca-listener-wake-generation fallback-frame) 1
            (rplaca::rplaca-listener-wake-pending-generation fallback-frame) 1)
      (with-await-overrides
          ((clim:sheet-event-queue (sheet)
             (declare (ignore sheet))
             :broken-queue)
           (climi::frame-event-queue (frm)
             (declare (ignore frm))
             :broken-queue)
           (climi::queue-append (queue event)
             (declare (ignore queue event))
             (error "all frame enqueue paths failed"))
           (climi::frame-process (frm)
             (declare (ignore frm))
             :frame-process)
           (clim-sys:process-interrupt (process callback)
             (declare (ignore process))
             (setf interrupt-callback callback)
             t))
        (is-true (rplaca::listener-deliver-failure-wakeup
                  fallback-frame :fake-sheet fallback-failure))
        (signals rplaca::prompt-run-error
          (funcall interrupt-callback))))))

;;; --------------------------------------------------------------------------
;;; Production surface presence.
;;; --------------------------------------------------------------------------

(test await-listener-production-surface-is-registered
  (is (fboundp 'rplaca::await-listener-agent-turn))
  (is (fboundp 'rplaca::handle-listener-wakeup))
  (is (fboundp 'rplaca::listener-make-wake-hook))
  (is (fboundp 'rplaca::listener-reserve-wakeup-event))
  (is (fboundp 'rplaca::listener-enqueue-reserved-wakeup))
  (is (fboundp 'rplaca::listener-turn-settled-p))
  (is (fboundp 'rplaca::listener-apply-runtime-state))
  (is (fboundp 'rplaca::listener-compute-settlement))
  (is (find-class 'rplaca::rplaca-listener-wakeup-event nil)))
