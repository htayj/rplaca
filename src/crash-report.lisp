(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Fatal crash report policy and portable formatting boundary
;;; --------------------------------------------------------------------------

(defparameter +crash-report-schema+ "rplaca-crash-report")
(defconstant +crash-report-schema-version+ 1)
(defconstant +crash-report-max-characters+ 32768)
(defconstant +crash-report-max-condition-characters+ 2048)
(defconstant +crash-report-max-backtrace-frames+ 64)
(defconstant +crash-report-max-threads+ 64)
(defconstant +crash-repair-max-condition-characters+ 4096)

(defvar *crash-report-frame* nil
  "The application frame active at the installed crash-report boundary.")

(defvar *crash-report-runtime-snapshot* '(:phase :not-started)
  "Immutable lock-free summary published at coarse application boundaries.")

(defvar *crash-report-hook-active-p* nil
  "Per-thread recursion guard for the fatal debugger hook.")

(defvar *crash-report-install-lock*
  (bt:make-lock "rplaca crash reporter installation"))

(defvar *crash-report-install-depth* 0)
(defvar *crash-report-original-debugger-hook* nil)
(defvar *crash-report-sequence-cell* (list 0)
  "Mutable cell used for lock-free report filename sequencing.")

(defstruct (crash-report-claim-state
            (:constructor make-crash-report-claim-state ()))
  (claimed-p nil :type boolean))

(defvar *crash-report-claim-state* (make-crash-report-claim-state)
  "Process-wide exactly-once claim for one installed application runtime.")

(defvar *crash-report-emitter-function* 'write-crash-report
  "Fatal-hook report emitter. Dynamically replaced only by deterministic tests.")

(defvar *crash-report-rename-function* 'crash-platform-atomic-publish
  "Same-directory atomic no-replace publication adapter.")

(defvar *crash-report-private-write-function*
  'crash-platform-write-private-file
  "Exclusive private temporary-file writer adapter.")

(defvar *crash-repair-request-emitter-function* 'write-crash-repair-request
  "Fatal-hook repair handoff emitter, replaceable by deterministic tests.")

(defun nonblank-environment-value (name)
  "Return trimmed NAME from the environment, or NIL when absent or blank."
  (let ((value (uiop:getenv name)))
    (when value
      (let ((trimmed
              (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
        (and (plusp (length trimmed)) trimmed)))))

(defun crash-report-directory-components ()
  "Return the report directory and the first RPLACA-owned directory."
  (let ((override (nonblank-environment-value
                   "RPLACA_CRASH_REPORT_DIR")))
    (if override
        (let ((directory
                (uiop:ensure-directory-pathname
                 (uiop:ensure-absolute-pathname (pathname override)
                                                (uiop:getcwd)))))
          (values directory directory))
        (let* ((state-home
                 (uiop:ensure-directory-pathname
                  (or (let ((xdg
                              (nonblank-environment-value
                               "XDG_STATE_HOME")))
                        (and xdg
                             (uiop:ensure-absolute-pathname
                              (pathname xdg) (uiop:getcwd))))
                      (merge-pathnames #P".local/state/"
                                       (user-homedir-pathname)))))
               (owned-root (merge-pathnames #P"rplaca/" state-home)))
          (values (merge-pathnames #P"crash-reports/" owned-root)
                  owned-root)))))

(defun crash-report-directory ()
  "Return the configured private crash-report directory.

RPLACA_CRASH_REPORT_DIR takes precedence. Otherwise use
XDG_STATE_HOME/rplaca/crash-reports, falling back to
~/.local/state/rplaca/crash-reports."
  (nth-value 0 (crash-report-directory-components)))

(defun crash-repair-request-directory ()
  "Return the configured private repair-request directory, or NIL.

The outer launcher sets RPLACA_CRASH_REPAIR_REQUEST_DIR only for supervised
interactive runs.  Fatal reports remain independent of automatic repair."
  (let ((value (nonblank-environment-value
                "RPLACA_CRASH_REPAIR_REQUEST_DIR")))
    (and value
         (uiop:ensure-directory-pathname
          (uiop:ensure-absolute-pathname (pathname value) (uiop:getcwd))))))

(defun archived-legacy-crash-report-directory ()
  "Return the archival legacy crash-report directory.

RPLACA never writes to or automatically imports this directory. It is exposed
only so migration tooling and users can locate preserved reports."
  (let* ((state-home
           (uiop:ensure-directory-pathname
            (or (let ((xdg (nonblank-environment-value "XDG_STATE_HOME")))
                  (and xdg
                       (uiop:ensure-absolute-pathname
                        (pathname xdg) (uiop:getcwd))))
                (merge-pathnames #P".local/state/"
                                 (user-homedir-pathname)))))
         (legacy-root (merge-pathnames #P"clawmacs/" state-home)))
    (merge-pathnames #P"crash-reports/" legacy-root)))

(defun replace-all-substrings (string old new)
  "Return STRING with every non-overlapping OLD occurrence replaced by NEW."
  (if (zerop (length old))
      string
      (with-output-to-string (stream)
        (loop :with start = 0
              :for position = (search old string :start2 start)
              :do
                 (if position
                     (progn
                       (write-string string stream :start start :end position)
                       (write-string new stream)
                       (setf start (+ position (length old))))
                     (progn
                       (write-string string stream :start start)
                       (return)))))))

(defun crash-report-normalize-path-text (text)
  "Normalize home and current-working-directory prefixes in TEXT."
  (let* ((home (ignore-errors (namestring (truename (user-homedir-pathname)))))
         (cwd (ignore-errors (namestring (truename (uiop:getcwd)))))
         (result text))
    (when (and cwd (plusp (length cwd)))
      (setf result (replace-all-substrings result cwd "<cwd>/")))
    (when (and home (plusp (length home)))
      (setf result (replace-all-substrings result home "~/")))
    result))

(defun crash-secret-value-terminator-p (character)
  "Return true when CHARACTER terminates a credential-shaped value."
  (find character
        '(#\Space #\Tab #\Newline #\Return #\" #\' #\, #\; #\) #\] #\})
        :test #'char=))

(defun redact-value-after-marker (string marker)
  "Redact credential-shaped values after every case-insensitive MARKER."
  (loop :with result = string
        :with start = 0
        :for position = (search marker result :start2 start :test #'char-equal)
        :while position
        :for value-start = (+ position (length marker))
        :for value-end = (or (position-if #'crash-secret-value-terminator-p
                                          result :start value-start)
                             (length result))
        :do
           (setf result
                 (concatenate 'string
                              (subseq result 0 value-start)
                              "[REDACTED]"
                              (subseq result value-end))
                 start (+ value-start (length "[REDACTED]")))
        :finally (return result)))

(defun redact-credential-shaped-text (text)
  "Return bounded TEXT with common credential forms redacted.

This is a defense after whitelist-only collection, not permission to add
environment values, payloads, transcripts, compose text, or provider stderr."
  (let ((result text))
    (dolist (marker '("authorization:" "bearer "
                      "api-key=" "api_key=" "apikey="
                      "access-token=" "access_token="
                      "refresh-token=" "refresh_token="
                      "token=" "password=" "passwd="
                      "client-secret=" "client_secret=" "secret="))
      (setf result (redact-value-after-marker result marker)))
    result))

(defun bounded-crash-string (value limit)
  "Render VALUE defensively, redact it, normalize paths, and bound its length."
  (let* ((rendered
           (handler-case
               (let ((*print-length* 16)
                     (*print-level* 6)
                     (*print-circle* t)
                     (*print-pretty* nil)
                     (*print-readably* nil))
                 (typecase value
                   (string value)
                   (condition (format nil "~A" value))
                   (t (write-to-string value :escape t))))
             (condition () "<unprintable>")))
         (safe (crash-report-normalize-path-text
                (redact-credential-shaped-text rendered))))
    (if (> (length safe) limit)
        (format nil "~A...[truncated ~D characters]"
                (subseq safe 0 (max 0 (- limit 40)))
                (- (length safe) (max 0 (- limit 40))))
        safe)))

(defun crash-report-timestamp (&optional (universal-time (get-universal-time)))
  "Return UNIVERSAL-TIME as a UTC ISO-8601 timestamp."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun crash-report-filename
    (&key (universal-time (get-universal-time))
          (pid (crash-platform-process-id))
          (sequence (crash-platform-next-sequence)))
  "Return one collision-resistant report filename."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time universal-time 0)
    (format nil
            "rplaca-crash-~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0DZ-p~D-~8,'0D.report"
            year month day hour minute second pid sequence)))

(defun crash-report-safe-value (function &rest arguments)
  "Call FUNCTION with ARGUMENTS, returning NIL if diagnostic capture fails."
  (handler-case (apply function arguments)
    (condition () nil)))

(defun crash-report-frame-buffer ()
  "Return the active frame buffer without assuming a live McCLIM backend."
  (or (and *crash-report-frame*
           (fboundp 'chat-frame-buffer)
           (crash-report-safe-value #'chat-frame-buffer
                                    *crash-report-frame*))
      (first (or *buffer-ring* nil))))

(defun publish-crash-report-runtime-snapshot (&rest state)
  "Publish a fresh whitelist-only crash diagnostic STATE without a lock."
  (setf *crash-report-runtime-snapshot* (copy-list state)))

(defun crash-report-safe-application-state ()
  "Return whitelist-only lock-free RPLACA state useful for diagnosis."
  (let* ((frame *crash-report-frame*)
         (buffer (crash-report-frame-buffer))
         (compose (and buffer
                       (crash-report-safe-value #'buffer-input-message buffer)))
         (pending-stream
           (and buffer
                (crash-report-safe-value #'buffer-pending-stream buffer))))
    (list
     :phase (getf *crash-report-runtime-snapshot* :phase)
     :buffer-count
     (crash-report-safe-value #'length (or *buffer-ring* nil))
     :frame-present-p (not (null frame))
     :frame-class (and frame (class-name (class-of frame)))
     :frame-state
     (and frame (fboundp 'clim:frame-state)
          (crash-report-safe-value #'clim:frame-state frame))
     :frame-lifecycle
     (and frame (fboundp 'chat-frame-lifecycle-state)
          (crash-report-safe-value #'chat-frame-lifecycle-state frame))
     :buffer-kind (and buffer (crash-report-safe-value #'buffer-kind buffer))
     :buffer-status (and buffer (crash-report-safe-value #'buffer-status buffer))
     :buffer-dirty-p
     (and buffer (not (null (crash-report-safe-value #'buffer-dirty-p buffer))))
     :session-persistence
     (and buffer
          (crash-report-safe-value #'buffer-session-persistence-mode buffer))
     :message-count
     (and buffer (crash-report-safe-value #'buffer-message-count buffer))
     :compose-character-count
     (and compose
          (crash-report-safe-value
           (lambda (message) (length (message-text message))) compose))
     :token-count
     (and buffer (crash-report-safe-value #'buffer-token-count buffer))
     :context-limit
     (and buffer (crash-report-safe-value #'buffer-context-limit buffer))
     :provider
     (and buffer
          (or (crash-report-safe-value #'buffer-provider-override buffer)
              *default-provider*))
     :model
     (and buffer
          (or (crash-report-safe-value #'buffer-model-override buffer)
              *default-model*))
     :think-level
     (and buffer
          (crash-report-safe-value #'buffer-think-level-override buffer))
     :stream-present-p (not (null pending-stream))
     :stream-done-p
     (and pending-stream
          (crash-report-safe-value #'stream-state-done-p pending-stream))
     :stream-cancel-requested-p
     (and pending-stream
          (crash-report-safe-value
           #'stream-state-cancel-requested-p pending-stream))
     :pending-tool-p
     (and buffer
          (not (null (crash-report-safe-value
                      #'buffer-pending-tool-execution buffer))))
     :pending-operation-p
     (and buffer
          (not (null (crash-report-safe-value
                      #'buffer-pending-interactive-operation buffer))))
     :runtime-generation
     (and buffer
          (crash-report-safe-value #'buffer-runtime-generation buffer))
     :runtime-stopping-p
     (and buffer
          (crash-report-safe-value #'buffer-runtime-stopping-p buffer))
     :disposed-p
     (and buffer (crash-report-safe-value #'buffer-disposed-p buffer)))))

(defun write-crash-report-pair (stream key value)
  "Write one bounded diagnostic KEY/VALUE pair."
  (format stream "~(~A~): ~A~%"
          key
          (bounded-crash-string value 512)))

(defun crash-report-condition-summary (condition)
  "Return a structured privacy-safe summary for standard CONDITION families.

Never include an offending datum, format arguments, operands, stream contents,
or an implementation condition's arbitrary printed report."
  (cond
    ((typep condition 'type-error)
     (let ((datum
             (crash-report-safe-value #'type-error-datum condition))
           (expected
             (crash-report-safe-value #'type-error-expected-type condition)))
       (list
        :family :type-error
        :datum-type (and datum (type-of datum))
        :datum-length
        (and (or (stringp datum) (vectorp datum))
             (length datum))
        :expected-type
        (and (symbolp expected) expected))))
    ((typep condition 'arithmetic-error)
     (let ((operation
             (crash-report-safe-value #'arithmetic-error-operation condition)))
       (list :family :arithmetic-error
             :operation (and (symbolp operation) operation))))
    ((typep condition 'stream-error)
     (let ((stream
             (crash-report-safe-value #'stream-error-stream condition)))
       (list :family :stream-error
             :stream-class (and stream (class-name (class-of stream))))))
    ((typep condition 'storage-condition)
     (list :family :storage-condition))
    (t
     (list :family :other))))

(defun build-crash-report (condition &key context)
  "Build a bounded privacy-preserving fatal report for CONDITION."
  (let ((text
          (with-output-to-string (stream)
            (format stream "schema: ~A~%" +crash-report-schema+)
            (format stream "schema_version: ~D~%"
                    +crash-report-schema-version+)
            (format stream "timestamp_utc: ~A~%" (crash-report-timestamp))
            (format stream "pid: ~D~%" (crash-platform-process-id))
            (format stream "implementation: ~A ~A~%"
                    (lisp-implementation-type)
                    (lisp-implementation-version))
            (write-crash-report-pair stream :context (or context :unknown))
            (write-crash-report-pair stream :cwd (namestring (uiop:getcwd)))
            (format stream "~%[argv]~%")
            (dolist (pair (crash-platform-argv-summary))
              (write-crash-report-pair stream (car pair) (cdr pair)))
            (format stream "~%[condition]~%")
            (write-crash-report-pair stream :type (type-of condition))
            (format stream "message: <omitted for privacy>~%")
            (loop :for (key value) :on
                    (crash-report-condition-summary condition)
                    :by #'cddr
                  :do (write-crash-report-pair stream key value))
            (format stream "~%[rplaca_state]~%")
            (loop :for (key value) :on (crash-report-safe-application-state)
                    :by #'cddr
                  :do (write-crash-report-pair stream key value))
            (format stream "~%[current_thread_backtrace]~%")
            (let ((frames
                    (crash-platform-safe-backtrace
                     +crash-report-max-backtrace-frames+)))
              (if frames
                  (loop :for frame :in frames
                        :for index :from 0
                        :do (format stream "~D: ~A~%"
                                    index
                                    (bounded-crash-string frame 512)))
                  (format stream "<unavailable>~%")))
            (format stream "~%[threads]~%")
            (let ((threads
                    (crash-platform-thread-inventory
                     +crash-report-max-threads+)))
              (if threads
                  (dolist (thread threads)
                    (format stream
                            "role=~A current=~:[no~;yes~] alive=~:[no~;yes~]~@[ tid=~D~]~%"
                            (getf thread :role)
                            (getf thread :current-p)
                            (getf thread :alive-p)
                            (getf thread :os-tid)))
                  (format stream "<unavailable>~%"))))))
    (if (> (length text) +crash-report-max-characters+)
        (concatenate
         'string
         (subseq text 0 (- +crash-report-max-characters+ 32))
         "~%[report truncated]~%")
        text)))

(defun crash-report-temporary-path (directory filename)
  "Return a same-directory private temporary path for FILENAME."
  (merge-pathnames
   (format nil ".~A.tmp-~D-~D"
           filename
           (crash-platform-process-id)
           (crash-platform-next-sequence))
   directory))

(defun write-crash-report (condition &key context)
  "Atomically write one private report for CONDITION and return its pathname."
  (multiple-value-bind (directory owned-root)
      (crash-report-directory-components)
    (let* ((content (build-crash-report condition :context context))
           (final nil)
           (temporary nil)
           (committed-p nil))
      ;; Parent state roots belong to the user/XDG configuration. Create them
      ;; if absent but never chmod them. Only RPLACA-owned descendants are
      ;; forced private by the platform adapter.
      (ensure-directories-exist
       (merge-pathnames #P".crash-report-parent"
                        (uiop:pathname-parent-directory-pathname owned-root)))
      (crash-platform-ensure-private-directory owned-root)
      (unless (equal directory owned-root)
        (crash-platform-ensure-private-directory directory))
    (loop
      :for filename = (crash-report-filename)
      :for candidate = (merge-pathnames filename directory)
      :unless (probe-file candidate)
        :do (setf final candidate
                  temporary (crash-report-temporary-path directory filename))
            (return))
      (unwind-protect
           (progn
             (funcall *crash-report-private-write-function* temporary content)
             (funcall *crash-report-rename-function* temporary final)
             (crash-platform-fsync-directory directory)
             (setf committed-p t)
             final)
        (unless committed-p
          (ignore-errors (delete-file temporary)))))))

(defun crash-repair-request-path (report-path directory)
  "Return the repair-request pathname paired with REPORT-PATH."
  (merge-pathnames
   (make-pathname :name (pathname-name report-path) :type "request")
   directory))

(defun build-crash-repair-request (condition report-path)
  "Build a private Codex handoff containing the actionable fatal condition."
  (with-output-to-string (stream)
    (format stream "schema: rplaca-crash-repair-request~%")
    (format stream "schema_version: 1~%")
    (format stream "timestamp_utc: ~A~%" (crash-report-timestamp))
    (write-crash-report-pair stream :report-path (namestring report-path))
    (write-crash-report-pair stream :condition-type (type-of condition))
    (format stream "condition-message: ~A~%"
            (bounded-crash-string
             condition +crash-repair-max-condition-characters+))
    (write-crash-report-pair
     stream :debug-log
     (or (nonblank-environment-value "RPLACA_DEBUG_LOG") "<unavailable>"))
    (write-crash-report-pair
     stream :repair-history
     (or (nonblank-environment-value "RPLACA_CRASH_REPAIR_HISTORY")
         "<unavailable>"))))

(defun write-crash-repair-request (condition report-path)
  "Atomically publish one private repair handoff and return its pathname."
  (let ((directory (crash-repair-request-directory)))
    (when directory
      (ensure-directories-exist
       (merge-pathnames #P".repair-request-parent"
                        (uiop:pathname-parent-directory-pathname directory)))
      (crash-platform-ensure-private-directory directory)
      (let* ((final (crash-repair-request-path report-path directory))
             (temporary
               (crash-report-temporary-path directory
                                            (file-namestring final)))
             (content (build-crash-repair-request condition report-path))
             (committed-p nil))
        (unwind-protect
             (progn
               (funcall *crash-report-private-write-function*
                        temporary content)
               (funcall *crash-report-rename-function* temporary final)
               (crash-platform-fsync-directory directory)
               (setf committed-p t)
               final)
          (unless committed-p
            (ignore-errors (delete-file temporary))))))))

(defun crash-report-invoke-debugger-hook (condition previous-hook)
  "Best-effort fatal reporter that always delegates to the captured hook."
  (declare (ignore previous-hook))
  ;; INVOKE-DEBUGGER binds SBCL's hook to NIL while invoking us, but a
  ;; reporter-side condition can still explicitly reach this function in
  ;; tests or through another debugger integration. Let the reporter's local
  ;; HANDLER-CASE consume that recursive condition. Delegating it to the
  ;; disabled-debugger hook would terminate on the reporter error and hide the
  ;; original application crash.
  (unless *crash-report-hook-active-p*
    (let ((*crash-report-hook-active-p* t)
          (original *crash-report-original-debugger-hook*))
      (when (crash-platform-claim-report
             *crash-report-claim-state*)
        (handler-case
            (let* ((path
                     (funcall *crash-report-emitter-function*
                              condition
                              :context
                              (if *crash-report-frame* :frame :main)))
                   (repair-request
                     (ignore-errors
                       (funcall *crash-repair-request-emitter-function*
                                condition path))))
              (ignore-errors
                (format *error-output*
                        "~&RPLACA fatal crash report: ~A~%"
                        path)
                (when repair-request
                  (format *error-output*
                          "RPLACA crash repair request: ~A~%"
                          repair-request))
                (force-output *error-output*)))
          (condition (reporter-condition)
            (declare (ignore reporter-condition))
            (ignore-errors
              (format *error-output*
                      "~&RPLACA crash reporter failed; original condition preserved.~%")
              (force-output *error-output*)))))
      (when original
        (funcall original condition original)))))

(defun call-with-installed-crash-reporter (function &key frame)
  "Call FUNCTION with the process-wide fatal hook installed and restored."
  (let ((*crash-report-frame* (or frame *crash-report-frame*))
        (installed-p nil))
    (unwind-protect
         (progn
           (bt:with-lock-held (*crash-report-install-lock*)
             (when (zerop *crash-report-install-depth*)
               (setf *crash-report-original-debugger-hook*
                     (crash-platform-current-debugger-hook)
                     (crash-report-claim-state-claimed-p
                      *crash-report-claim-state*)
                     nil)
               (crash-platform-set-debugger-hook
                'crash-report-invoke-debugger-hook))
             (incf *crash-report-install-depth*)
             (setf installed-p t))
           (funcall function))
      (when installed-p
        (bt:with-lock-held (*crash-report-install-lock*)
          (decf *crash-report-install-depth*)
          (when (zerop *crash-report-install-depth*)
            (when (eq (crash-platform-current-debugger-hook)
                      'crash-report-invoke-debugger-hook)
              (crash-platform-set-debugger-hook
               *crash-report-original-debugger-hook*))
            (setf *crash-report-original-debugger-hook* nil)))))))
