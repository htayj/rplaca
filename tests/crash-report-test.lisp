(in-package :rplaca/tests)

(in-suite crash-report-suite)

(defun crash-test-directory (label)
  (let ((directory
          (make-pathname
           :directory
           (list :absolute "tmp"
                 (format nil "rplaca-crash-test-~A-~36R-~36R-~A"
                         label
                         (get-universal-time)
                         (get-internal-real-time)
                         (gensym))))))
    (ensure-directories-exist (merge-pathnames #P".keep" directory))
    (uiop:ensure-directory-pathname directory)))

(defmacro with-crash-test-directory ((variable label) &body body)
  `(let ((,variable (crash-test-directory ,label)))
     (unwind-protect
          (progn ,@body)
       (uiop:delete-directory-tree ,variable
                                   :validate t
                                   :if-does-not-exist :ignore))))

(defmacro with-crash-environment ((name value) &body body)
  (let ((old (gensym "OLD-")))
    `(let ((,old (uiop:getenv ,name)))
       (unwind-protect
            (progn
              (setf (uiop:getenv ,name) ,value)
              ,@body)
         (if ,old
             (setf (uiop:getenv ,name) ,old)
             (setf (uiop:getenv ,name) ""))))))

(defun crash-test-read-file (path)
  (uiop:read-file-string path :external-format :utf-8))

(defun crash-test-report-files (directory)
  (directory (merge-pathnames #P"*.report" directory)))

(defun crash-test-temporary-files (directory)
  (directory (merge-pathnames #P".*.tmp-*" directory)))

(defun crash-test-request-files (directory)
  (directory (merge-pathnames #P"*.request" directory)))

#+sbcl
(defun crash-test-mode (path)
  (logand #o777
          (sb-posix:stat-mode
           (sb-posix:stat (namestring path)))))

(test crash-report-directory-honors-override-and-xdg-default
  (with-crash-test-directory (override "override")
    (with-crash-test-directory (xdg "xdg")
      (with-crash-environment
          ("RPLACA_CRASH_REPORT_DIR" (namestring override))
        (with-crash-environment ("XDG_STATE_HOME" (namestring xdg))
          (is (equal override (rplaca::crash-report-directory)))))
      (with-crash-environment ("RPLACA_CRASH_REPORT_DIR" "")
        (with-crash-environment ("XDG_STATE_HOME" (namestring xdg))
          (is (equal (merge-pathnames #P"rplaca/crash-reports/" xdg)
                     (rplaca::crash-report-directory))))))))

(test crash-report-filenames-are-bounded-unique-and-diagnostic
  (let ((first (rplaca::crash-report-filename
                :universal-time 3983724306
                :pid 42
                :sequence 7))
        (second (rplaca::crash-report-filename
                 :universal-time 3983724306
                 :pid 42
                 :sequence 8)))
    (is (not (string= first second)))
    (is (search "p42-00000007.report" first))
    (is (< (length first) 100))))

(test crash-report-content-is-whitelist-only-redacted-and-bounded
  (let* ((secret "arbitrary prompt sentinel 60ec503c")
         (environment-secret "environment sentinel b2ee70cc")
         (buffer
           (make-buffer "private session name"
                        :session-persistence-mode :ephemeral)))
    (set-buffer-provider-override buffer :openai-codex)
    (set-buffer-model-override
     buffer (make-string 10000 :initial-element #\m))
    (set-message-text (buffer-input-message buffer) secret)
    (buffer-insert-read-only-message buffer :user secret :record-p nil)
    (with-crash-environment ("OPENAI_API_KEY" environment-secret)
      (let* ((rplaca::*buffer-ring* (list buffer))
             (report
               (rplaca::build-crash-report
                (make-condition 'simple-error
                                :format-control "~A"
                                :format-arguments (list secret))
                :context :unit-test)))
        (is (search "schema: rplaca-crash-report" report))
        (is (search "schema_version: 1" report))
        (is (search "type: SIMPLE-ERROR" report))
        (is (search "message: <omitted for privacy>" report))
        (is (search "provider: :OPENAI-CODEX" report))
        (is (search "compose-character-count:" report))
        (is-false (search secret report))
        (is-false (search environment-secret report))
        (is-false (search "private session name" report))
        (is-false (search (make-string 1000 :initial-element #\m) report))
        (is (<= (length report)
                rplaca::+crash-report-max-characters+))))))

(test crash-report-type-error-summary-never-prints-the-datum
  (let* ((secret "datum sentinel 97cc61dd")
         (condition
           (handler-case
               (error 'type-error :datum secret :expected-type 'integer)
             (type-error (caught) caught)))
         (report (rplaca::build-crash-report condition :context :unit-test)))
    (is (search "family: :TYPE-ERROR" report))
    (is (search "datum-type:" report))
    (is (search "datum-length:" report))
    (is (search "expected-type: INTEGER" report))
    (is-false (search secret report))))

(test crash-report-write-is-private-atomic-and-leaves-no-temporary-file
  #+sbcl
  (with-crash-test-directory (base "atomic")
    (let ((directory (merge-pathnames #P"reports/" base)))
      (with-crash-environment
        ("RPLACA_CRASH_REPORT_DIR" (namestring directory))
      (let ((observed-pre-rename-p nil)
            (original rplaca::*crash-report-rename-function*))
        (let ((rplaca::*crash-report-rename-function*
                (lambda (source target)
                  (is (probe-file source))
                  (is-false (probe-file target))
                  (is (search "schema: rplaca-crash-report"
                              (crash-test-read-file source)))
                  (setf observed-pre-rename-p t)
                  (funcall original source target))))
          (let ((path
                  (rplaca::write-crash-report
                   (make-condition 'simple-error
                                   :format-control "atomic test")
                   :context :unit-test)))
            (is-true observed-pre-rename-p)
            (is (probe-file path))
            (is (= #o700 (crash-test-mode directory)))
            (is (= #o600 (crash-test-mode path)))
            (is (= 1 (length (crash-test-report-files directory))))
            (is (null (crash-test-temporary-files directory))))))))))

(test crash-repair-request-is-private-actionable-and-redacted
  #+sbcl
  (with-crash-test-directory (base "repair-request")
    (let ((directory (merge-pathnames #P"requests/" base))
          (report (merge-pathnames #P"rplaca-crash-example.report" base))
          (debug-log (merge-pathnames #P"debug.log" base))
          (history (merge-pathnames #P"repair-history.md" base)))
      (with-crash-environment
          ("RPLACA_CRASH_REPAIR_REQUEST_DIR" (namestring directory))
        (with-crash-environment ("RPLACA_DEBUG_LOG" (namestring debug-log))
          (with-crash-environment
              ("RPLACA_CRASH_REPAIR_HISTORY" (namestring history))
            (let* ((condition
                     (make-condition
                      'simple-error
                      :format-control "repair sentinel token=super-secret"))
                   (path
                     (rplaca::write-crash-repair-request condition report))
                   (content (crash-test-read-file path)))
              (is (probe-file path))
              (is (search "schema: rplaca-crash-repair-request" content))
              (is (search "condition-message: repair sentinel" content))
              (is (search "token=[REDACTED]" content))
              (is-false (search "super-secret" content))
              (is (search (namestring report) content))
              (is (search (namestring debug-log) content))
              (is (search (namestring history) content))
              (is (= #o700 (crash-test-mode directory)))
              (is (= #o600 (crash-test-mode path)))
              (is (= 1 (length (crash-test-request-files directory))))
              (is (null (crash-test-temporary-files directory))))))))))

(test crash-report-write-failure-publishes-no-partial-report
  (with-crash-test-directory (base "write-failure")
    (let ((directory (merge-pathnames #P"reports/" base)))
    (with-crash-environment
        ("RPLACA_CRASH_REPORT_DIR" (namestring directory))
      (let ((rplaca::*crash-report-private-write-function*
              (lambda (path content)
                (declare (ignore content))
                (with-open-file
                    (stream path :direction :output
                                 :if-does-not-exist :create)
                  (write-string "partial" stream))
                (error "simulated writer failure"))))
        (signals error
          (rplaca::write-crash-report
           (make-condition 'simple-error :format-control "writer")
           :context :unit-test))
        (is (null (crash-test-report-files directory)))
        (is (null (crash-test-temporary-files directory))))))))

(test existing-override-is-validated-without-permission-mutation
  #+sbcl
  (with-crash-test-directory (base "existing-override")
    (let ((directory (merge-pathnames #P"reports/" base)))
      (ensure-directories-exist (merge-pathnames #P".keep" directory))
      (sb-posix:chmod (namestring directory) #o755)
      (with-crash-environment
          ("RPLACA_CRASH_REPORT_DIR" (namestring directory))
        (signals error
          (rplaca::write-crash-report
           (make-condition 'simple-error :format-control "mode")
           :context :unit-test))
        (is (= #o755 (crash-test-mode directory)))
        (is (null (crash-test-report-files directory)))))))

(test override-symbolic-link-is-rejected-without-changing-target
  #+sbcl
  (with-crash-test-directory (base "symlink-override")
    (let ((target (merge-pathnames #P"target/" base))
          (link (merge-pathnames #P"link" base)))
      (ensure-directories-exist (merge-pathnames #P".keep" target))
      (sb-posix:chmod (namestring target) #o700)
      (sb-posix:symlink (namestring target) (namestring link))
      (with-crash-environment
          ("RPLACA_CRASH_REPORT_DIR" (namestring link))
        (signals error
          (rplaca::write-crash-report
           (make-condition 'simple-error :format-control "symlink")
           :context :unit-test))
        (is (= #o700 (crash-test-mode target)))
        (is (null (crash-test-report-files target)))))))

(test atomic-publication-never-replaces-a-colliding-final-report
  #+sbcl
  (with-crash-test-directory (base "collision")
    (let ((directory (merge-pathnames #P"reports/" base)))
      (with-crash-environment
          ("RPLACA_CRASH_REPORT_DIR" (namestring directory))
        (let ((original rplaca::*crash-report-rename-function*)
              (rplaca::*crash-report-rename-function* nil))
          (setf rplaca::*crash-report-rename-function*
                (lambda (source target)
                  (with-open-file
                      (stream target :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :error)
                    (write-string "preexisting" stream))
                  (funcall original source target)))
          (signals error
            (rplaca::write-crash-report
             (make-condition 'simple-error :format-control "collision")
             :context :unit-test))
          (let ((reports (crash-test-report-files directory)))
            (is (= 1 (length reports)))
            (is (string= "preexisting"
                         (crash-test-read-file (first reports)))))
          (is (null (crash-test-temporary-files directory))))))))

(test crash-reporter-failure-and-recursion-preserve-original-condition
  (let* ((original (make-condition 'simple-error
                                   :format-control "original"))
         (delegated nil)
         (emissions 0)
         (old-emitter rplaca::*crash-report-emitter-function*)
         (old-original rplaca::*crash-report-original-debugger-hook*))
    (unwind-protect
         (progn
           (setf (rplaca::crash-report-claim-state-claimed-p
                  rplaca::*crash-report-claim-state*)
                 nil
                 rplaca::*crash-report-original-debugger-hook*
                 (lambda (condition previous)
                   (is (eq previous
                           rplaca::*crash-report-original-debugger-hook*))
                   (push condition delegated))
                 rplaca::*crash-report-emitter-function*
                 (lambda (condition &key context)
                   (declare (ignore condition context))
                   (incf emissions)
                   (rplaca::crash-report-invoke-debugger-hook
                    (make-condition 'simple-error
                                    :format-control "recursive")
                    nil)
                   (error "simulated reporter failure")))
           (rplaca::crash-report-invoke-debugger-hook original nil)
           (is (= 1 emissions))
           (is (eq original (first delegated)))
           (is (= 1 (length delegated))))
      (setf rplaca::*crash-report-emitter-function* old-emitter
            rplaca::*crash-report-original-debugger-hook* old-original))))

(test crash-reporter-process-claim-is-exactly-once-under-concurrency
  #+sbcl
  (let ((emissions (list 0))
        (delegations (list 0))
        (old-emitter rplaca::*crash-report-emitter-function*)
        (old-original rplaca::*crash-report-original-debugger-hook*))
    (unwind-protect
         (progn
           (setf (rplaca::crash-report-claim-state-claimed-p
                  rplaca::*crash-report-claim-state*)
                 nil
                 rplaca::*crash-report-emitter-function*
                 (lambda (condition &key context)
                   (declare (ignore condition context))
                   (sb-ext:atomic-incf (car emissions))
                   #P"/tmp/report")
                 rplaca::*crash-report-original-debugger-hook*
                 (lambda (condition previous)
                   (declare (ignore condition previous))
                   (sb-ext:atomic-incf (car delegations))))
           (let ((threads
                   (loop :repeat 12
                         :collect
                         (bt:make-thread
                          (lambda ()
                            (rplaca::crash-report-invoke-debugger-hook
                             (make-condition 'simple-error
                                             :format-control "concurrent")
                             nil))
                          :name "rplaca crash claim test"))))
             (dolist (thread threads)
               (bt:join-thread thread)))
           (is (= 1 (car emissions)))
           (is (= 12 (car delegations))))
      (setf rplaca::*crash-report-emitter-function* old-emitter
            rplaca::*crash-report-original-debugger-hook* old-original))))

(test installed-crash-reporter-restores-hook-and-ignores-handled-errors
  (let ((before (rplaca::crash-platform-current-debugger-hook))
        (emissions 0)
        (old-emitter rplaca::*crash-report-emitter-function*))
    (unwind-protect
         (progn
           (setf rplaca::*crash-report-emitter-function*
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (incf emissions)))
           (is (eq :handled
                   (rplaca::call-with-installed-crash-reporter
                    (lambda ()
                      (handler-case
                          (error "handled")
                        (error () :handled))))))
           (is (= 0 emissions))
           (is (eq before
                   (rplaca::crash-platform-current-debugger-hook))))
      (setf rplaca::*crash-report-emitter-function* old-emitter))))

(defun run-crash-report-subprocess (mode directory)
  (with-crash-environment
      ("RPLACA_CRASH_REPORT_DIR" (namestring directory))
    (with-crash-environment ("RPLACA_CRASH_TEST_MODE" mode)
      (uiop:run-program
       (list "sbcl" "--noinform" "--disable-debugger"
             "--script" "scripts/crash-report-test-entry.lisp")
       :directory (asdf:system-source-directory :rplaca)
       :output :string
       :error-output :string
       :ignore-error-status t))))

(test disable-debugger-main-and-worker-fatals-write-one-report-each
  (dolist (spec '(("main" "86f4d337") ("worker" "34ecb3ce")))
    (destructuring-bind (mode private-sentinel) spec
      (with-crash-test-directory (directory mode)
        (let ((reports-directory (merge-pathnames #P"reports/" directory)))
          (multiple-value-bind (stdout stderr exit-code)
            (run-crash-report-subprocess mode reports-directory)
          (declare (ignore stdout))
          (is (not (zerop exit-code)))
          (is (search "RPLACA fatal crash report:" stderr))
          (let ((reports (crash-test-report-files reports-directory)))
            (is (= 1 (length reports)))
            (let ((content (crash-test-read-file (first reports))))
              (is (search "schema: rplaca-crash-report" content))
              (is (search "[current_thread_backtrace]" content))
              (is (search "[threads]" content))
              (is (search "message: <omitted for privacy>" content))
              (is-false (search private-sentinel content))
              (is-false
               (search "rplaca crash integration worker" content)))
            #+sbcl
            (progn
              (is (= #o700 (crash-test-mode reports-directory)))
              (is (= #o600 (crash-test-mode (first reports)))))
            (is (null
                 (crash-test-temporary-files reports-directory))))))))))

(test disable-debugger-handled-condition-exits-zero-without-report
  (with-crash-test-directory (base "handled-subprocess")
    (let ((directory (merge-pathnames #P"reports/" base)))
      (multiple-value-bind (stdout stderr exit-code)
          (run-crash-report-subprocess "handled" directory)
      (declare (ignore stdout stderr))
      (is (zerop exit-code))
      (is (null (crash-test-report-files directory)))
      (is (null (crash-test-temporary-files directory)))))))
