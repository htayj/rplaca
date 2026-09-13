(in-package :rplaca/tests)

(in-suite appearance-suite)

;;; Package declaration transactions ------------------------------------------------

(defun package-appearance-test-definition (&optional (name "org.example.theme"))
  (make-package-definition :name name :description "appearance test"
                           :root #P"/tmp/" :entrypoint #P"/tmp/entrypoint.lisp"))

(defun package-appearance-test-id (local &optional (owner "org.example.theme"))
  (list :package owner local))

(defun package-appearance-test-role (local &key fallback)
  (make-appearance-role-definition
   :id (package-appearance-test-id local) :kind :content :fallback-role fallback))

(defun package-appearance-test-theme (local &key parent overlays)
  (make-appearance-theme-definition
   :id (package-appearance-test-id local) :parent-theme parent
   :role-overlays overlays))

(defmacro with-package-appearance-test-state ((&key (owner "org.example.theme")
                                                     (frames nil) planner reserver
                                                     publisher finalizer)
                                               &body body)
  `(let* ((definition (package-appearance-test-definition ,owner))
          (rplaca::*package-appearance-declarations* (make-hash-table :test #'equal))
          (rplaca::*package-appearance-catalog* (make-classic-appearance-catalog))
          (rplaca::*loaded-packages* (make-hash-table :test #'equal))
          (rplaca::*current-rplaca-package* ,owner)
          (rplaca::*current-package-resource-types* '(:appearance))
          (rplaca::*package-appearance-entrypoint-staging*
            (rplaca::begin-package-appearance-entrypoint-staging definition))
          (rplaca::*appearance-package-live-frame-provider*
            (lambda () ,frames))
          (rplaca::*appearance-package-frame-transition-planner*
            (or ,planner (lambda (frame catalog)
                           (declare (ignore frame catalog)) (list :status :ready))))
          (rplaca::*appearance-package-frame-transition-reserver*
            (or ,reserver (lambda (frame plan catalog token)
                            (list frame plan catalog token))))
          (rplaca::*appearance-package-frame-transition-publisher*
            (or ,publisher (lambda (reservation) (declare (ignore reservation)) t)))
          (rplaca::*appearance-package-frame-transition-finalizer*
            (or ,finalizer
                (lambda (token reservations commit rollback)
                  (declare (ignore reservations rollback))
                  (funcall commit)
                  (setf
                   (rplaca::appearance-package-transition-token-state token)
                   :committed)
                  t))))
     ,@body))

(test package-appearance-ids-are-tagged-data-without-interning
  "Owner/local preferences remain tagged strings even if a Lisp package unloads."
  (let* ((owner "org.example.theme")
         (local "night")
         (symbol-name "ORG.EXAMPLE.THEME/NIGHT"))
    (is (null (find-symbol symbol-name :cl-user)))
    (let ((id (parse-appearance-theme-selector "org.example.theme/night")))
      (is (equal (list :package owner local) id))
      (is (string= "org.example.theme/night"
                   (rplaca::appearance-id-external-string id))))
    (is (null (find-symbol symbol-name :cl-user)))))

(test package-appearance-requires-explicit-allowlist-and-normalized-owner
  "Legacy NIL resource policy is not permission for appearance declarations."
  (with-package-appearance-test-state ()
    (let ((rplaca::*current-package-resource-types* nil))
      (signals appearance-error
        (register-package-appearance-role (package-appearance-test-role "entry"))))
    (signals appearance-error
      (register-package-appearance-role
       (make-appearance-role-definition
        :id '(:package "Org.Example.Theme" "entry") :kind :content)))))

(test package-appearance-transaction-validates-graphs-and-collisions
  "A missing same-owner dependency or duplicate catalog key publishes nothing."
  (with-package-appearance-test-state ()
    (let ((before (current-package-appearance-catalog)))
      (register-package-appearance-role
       (package-appearance-test-role
        "entry" :fallback (package-appearance-test-id "missing")))
      (signals appearance-error
        (rplaca::commit-package-appearance-entrypoint-staging
         rplaca::*package-appearance-entrypoint-staging*))
      (is (eq before (current-package-appearance-catalog)))))
  (with-package-appearance-test-state ()
    (let ((role (package-appearance-test-role "entry")))
      (register-package-appearance-declarations :roles (list role role))
      (signals appearance-error
        (rplaca::commit-package-appearance-entrypoint-staging
         rplaca::*package-appearance-entrypoint-staging*)))))

(test package-appearance-validates-every-candidate-overlay-before-publication
  "Dangling roles and unsupported axes never enter the published catalog."
  (flet ((surface-style ()
           (make-appearance-role-style
            :surface
            (make-appearance-surface-spec :background '(:rgb 0.1 0.2 0.3)))))
    (with-package-appearance-test-state ()
      (let ((before (current-package-appearance-catalog)))
        (register-package-appearance-theme
         (package-appearance-test-theme
          "dangling"
          :overlays
          (list (cons (package-appearance-test-id "missing")
                      (make-appearance-role-style
                       :foreground-ink
                       (make-appearance-ink-spec :foreground :red))))))
        (signals appearance-error
          (rplaca::commit-package-appearance-entrypoint-staging
           rplaca::*package-appearance-entrypoint-staging*))
        (is (eq before (current-package-appearance-catalog)))))
    (with-package-appearance-test-state ()
      (register-package-appearance-defaults
       (list (cons (package-appearance-test-id "missing")
                   (make-appearance-role-style
                    :foreground-ink
                    (make-appearance-ink-spec :foreground :red)))))
      (signals appearance-error
        (rplaca::commit-package-appearance-entrypoint-staging
         rplaca::*package-appearance-entrypoint-staging*)))
    (with-package-appearance-test-state ()
      (register-package-appearance-role (package-appearance-test-role "content"))
      (register-package-appearance-theme
       (package-appearance-test-theme
        "unsupported"
        :overlays
        (list (cons (package-appearance-test-id "content")
                    (surface-style)))))
      (signals unsupported-role-axis
        (rplaca::commit-package-appearance-entrypoint-staging
         rplaca::*package-appearance-entrypoint-staging*)))))

(test package-appearance-partial-style-sentinels-are-portable
  "Internal inheritance sentinels in partial typography/decoration are inert data."
  (with-package-appearance-test-state ()
    (let ((role (package-appearance-test-role "partial")))
      (register-package-appearance-role
       role
       :defaults
       (list
        (cons (package-appearance-test-id "partial")
              (make-appearance-role-style
               :typography (make-appearance-typography-spec :face :bold)
               :decoration
               (make-appearance-decoration-spec :kind :none)))))
      (is (rplaca::commit-package-appearance-entrypoint-staging
           rplaca::*package-appearance-entrypoint-staging*))
      (is (find-appearance-role-definition
           (current-package-appearance-catalog)
           (package-appearance-test-id "partial"))))))

(test package-appearance-publishes-after-two-frame-reservations
  "No frame release occurs until both live frames have admitted the catalog."
  (let ((trace nil) (frame-a (list :frame :a)) (frame-b (list :frame :b)))
    (with-package-appearance-test-state
        (:frames (list frame-a frame-b)
         :reserver (lambda (frame plan catalog token)
                      (declare (ignore plan catalog token))
                      (push (list :reserve frame) trace) frame)
         :publisher (lambda (reservation)
                      (push (list :release reservation) trace) t))
      (register-package-appearance-role (package-appearance-test-role "entry"))
      (rplaca::commit-package-appearance-entrypoint-staging
       rplaca::*package-appearance-entrypoint-staging*)
      (is (equal (reverse trace)
                 (list (list :reserve frame-a) (list :reserve frame-b)
                       (list :release frame-a) (list :release frame-b)))))))

(test package-appearance-refusal-rolls-back-catalog-and-owner-batch
  "A frame refusal preserves the exact published catalog and declarations."
  (with-package-appearance-test-state
      (:frames (list :frame)
       :planner (lambda (frame catalog)
                  (declare (ignore frame catalog)) (list :status :failed)))
    (let ((before (current-package-appearance-catalog)))
      (register-package-appearance-role (package-appearance-test-role "entry"))
      (signals appearance-error
        (rplaca::commit-package-appearance-entrypoint-staging
         rplaca::*package-appearance-entrypoint-staging*))
      (is (eq before (current-package-appearance-catalog)))
      (is (= 0 (hash-table-count rplaca::*package-appearance-declarations*))))))

(test package-appearance-release-failure-aborts-transaction
  "A failed post-publication release restores the catalog and marks its token aborted."
  (let ((token nil))
    (with-package-appearance-test-state
        (:frames (list :frame)
         :reserver (lambda (frame plan catalog candidate-token)
                      (declare (ignore frame plan catalog))
                      (setf token candidate-token)
                      (list :reservation))
         :publisher (lambda (reservation) (declare (ignore reservation)) nil))
      (let ((before (current-package-appearance-catalog)))
        (register-package-appearance-role (package-appearance-test-role "entry"))
        (signals error
          (rplaca::commit-package-appearance-entrypoint-staging
           rplaca::*package-appearance-entrypoint-staging*))
        (is (eq before (current-package-appearance-catalog)))
        (is (eq :aborted
                (rplaca::appearance-package-transition-token-state token)))))))

(test package-appearance-refused-unload-preserves-loaded-marker-and-files
  "Removal refusal happens before package runtime state or installed files change."
  (let* ((root (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "rplaca-appearance-unload-~D/"
                                         (get-internal-real-time))
                                 #P"/tmp/")))
         (entrypoint (merge-pathnames "entrypoint.lisp" root))
         (definition (make-package-definition :name "org.example.theme"
                                              :description "unload test"
                                              :root root :entrypoint entrypoint)))
    (ensure-directories-exist entrypoint)
    (with-open-file (stream entrypoint :direction :output :if-exists :supersede)
      (write-string ";; retained on refusal" stream))
    (unwind-protect
         (with-package-appearance-test-state
             (:frames (list :live-frame)
              :planner (lambda (frame catalog)
                         (declare (ignore frame catalog)) (list :status :failed)))
           (register-package-appearance-role (package-appearance-test-role "entry"))
           ;; Publish the owner batch before installing the refusing frame seam.
           (let ((rplaca::*appearance-package-live-frame-provider* (constantly nil)))
             (rplaca::commit-package-appearance-entrypoint-staging
              rplaca::*package-appearance-entrypoint-staging*))
           (let ((key (rplaca::package-install-key root))
                 (catalog (current-package-appearance-catalog)))
             (setf (gethash key rplaca::*loaded-packages*) "org.example.theme")
             (signals appearance-error (rplaca::%reset-package-runtime-state definition))
             (is (eq catalog (current-package-appearance-catalog)))
             (is (gethash key rplaca::*loaded-packages*))
             (is (probe-file entrypoint))))
      (when (probe-file root)
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(test package-appearance-unload-admission-failures-restore-ordinary-runtime
  "Reservation and event-release failures roll back marker and registrations."
  (dolist (failure '(:reserve :publish))
    (let* ((root
             (temp-package-test-directory
              (format nil "appearance-unload-~A" failure)))
           (entrypoint (merge-pathnames "entrypoint.lisp" root))
           (definition
             (make-package-definition
              :name "org.example.theme"
              :description "appearance unload rollback"
              :root root
              :entrypoint entrypoint))
           (key (rplaca::package-install-key root))
           (section
             (rplaca::make-package-prompt-section
              :name "appearance-rollback"
              :package "org.example.theme"
              :body "committed"))
           (metadata
             (rplaca::make-command-metadata
              :name 'package-appearance-rollback-command
              :docstring "committed"
              :package "org.example.theme")))
      (write-test-file entrypoint ";; retained after failed unload")
      (unwind-protect
           (with-package-appearance-test-state ()
             (register-package-appearance-role
              (package-appearance-test-role "entry"))
             (rplaca::commit-package-appearance-entrypoint-staging
              rplaca::*package-appearance-entrypoint-staging*)
             (setf (gethash key rplaca::*loaded-packages*)
                   "org.example.theme")
             (let ((rplaca::*package-prompt-sections* (list section))
                   (rplaca::*command-table* (make-hash-table :test #'eq))
                   (rplaca::*appearance-package-live-frame-provider*
                     (lambda () (list :live-frame)))
                   (rplaca::*appearance-package-frame-transition-planner*
                     (lambda (frame catalog)
                       (declare (ignore frame catalog))
                       (list :status :ready)))
                   (rplaca::*appearance-package-frame-transition-reserver*
                     (if (eq failure :reserve)
                         (lambda (&rest arguments)
                           (declare (ignore arguments)) nil)
                         (lambda (frame plan catalog token)
                           (list frame plan catalog token))))
                   (rplaca::*appearance-package-frame-transition-publisher*
                     (if (eq failure :publish)
                         (lambda (reservation)
                           (declare (ignore reservation)) nil)
                         (lambda (reservation)
                           (declare (ignore reservation)) t))))
               (setf (gethash 'package-appearance-rollback-command
                              rplaca::*command-table*)
                     metadata)
               (let ((catalog (current-package-appearance-catalog)))
                 (signals error
                   (rplaca::%reset-package-runtime-state definition))
                 (is (eq catalog (current-package-appearance-catalog)))
                 (is (eq section
                         (first rplaca::*package-prompt-sections*)))
                 (is (eq metadata
                         (gethash 'package-appearance-rollback-command
                                  rplaca::*command-table*)))
                 (is (gethash key rplaca::*loaded-packages*))
                 (is (probe-file entrypoint)))))
        (when (probe-file root)
          (uiop:delete-directory-tree root
                                      :validate t
                                      :if-does-not-exist :ignore))))))

(test failed-same-owner-reload-restores-runtime-and-propagates
  "Invalid declarations and frame refusal cannot turn a failed reload into OK."
  (let* ((root (temp-package-test-directory "appearance-reload-rollback"))
         (entrypoint (merge-pathnames "entrypoint.lisp" root))
         (definition
           (make-package-definition
            :name "org.example.theme"
            :description "reload rollback"
            :root root
            :entrypoint entrypoint))
         (key (rplaca::package-install-key root)))
    (unwind-protect
         (let ((rplaca::*package-appearance-declarations*
                 (make-hash-table :test #'equal))
               (rplaca::*package-appearance-catalog*
                 (make-classic-appearance-catalog))
               (rplaca::*loaded-packages* (make-hash-table :test #'equal))
               (rplaca::*package-prompt-sections* nil)
               (rplaca::*package-runtime-maintenance-admitted-p* t)
               (rplaca::*appearance-package-live-frame-provider*
                 (constantly nil)))
           (rplaca::write-package-install-record
            root
            (rplaca::package-install-record-plist
             definition
             :source-type :path
             :source root
             :scope :global
             :resource-types '(:appearance :prompt-section)))
           (write-test-file
            entrypoint
            "(rplaca:register-package-appearance-role
               (rplaca:make-appearance-role-definition
                :id '(:package \"org.example.theme\" \"old\")
                :kind :content))
             (rplaca:register-package-prompt-section
              \"org.example.theme\" \"committed\"
              :package \"org.example.theme\")")
           (is (rplaca::load-package-definition-entrypoint definition))
           (let ((catalog (current-package-appearance-catalog))
                 (section (first rplaca::*package-prompt-sections*)))
             (write-test-file
              entrypoint
              "(rplaca:register-package-prompt-section
                 \"org.example.theme\" \"invalid replacement\"
                 :package \"org.example.theme\")
               (rplaca:register-package-appearance-theme
                (rplaca:make-appearance-theme-definition
                 :id '(:package \"org.example.theme\" \"broken\")
                 :role-overlays
                 (list
                  (cons '(:package \"org.example.theme\" \"missing\")
                        (rplaca:make-appearance-role-style
                         :foreground-ink
                         (rplaca:make-appearance-ink-spec
                          :foreground :red))))))")
             (signals appearance-error
               (rplaca::%reload-rplaca-package definition))
             (is (eq catalog (current-package-appearance-catalog)))
             (is (eq section (first rplaca::*package-prompt-sections*)))
             (is (gethash key rplaca::*loaded-packages*)))
           ;; A valid replacement which a live frame refuses must propagate
           ;; through the same reload boundary and preserve the old state.
           (let ((catalog (current-package-appearance-catalog))
                 (section (first rplaca::*package-prompt-sections*))
                 (rplaca::*appearance-package-live-frame-provider*
                   (lambda () (list :live-frame)))
                 (rplaca::*appearance-package-frame-transition-planner*
                   (lambda (frame candidate-catalog)
                     (declare (ignore frame candidate-catalog))
                     (list :status :failed))))
             (write-test-file
              entrypoint
              "(rplaca:register-package-appearance-role
                 (rplaca:make-appearance-role-definition
                  :id '(:package \"org.example.theme\" \"new\")
                  :kind :content))")
             (signals appearance-error
               (rplaca::%reload-rplaca-package definition))
             (is (eq catalog (current-package-appearance-catalog)))
             (is (eq section (first rplaca::*package-prompt-sections*)))
             (is (gethash key rplaca::*loaded-packages*))))
      (when (probe-file root)
        (uiop:delete-directory-tree root
                                    :validate t
                                    :if-does-not-exist :ignore)))))

(test safe-reload-appearance-isolation-does-not-read-or-write-preferences
  "Runtime registration refresh never performs appearance file/init startup work."
  (let ((rplaca::*appearance-configuration-access-count* 17)
        (rplaca::*appearance-startup-resolution-count* 19)
        (rplaca::*package-channels* nil)
        (rplaca::*available-packages* nil)
        (rplaca::*package-registry-loaded-p* nil))
    (rplaca::safe-reload-refresh-runtime-registrations nil)
    (is (= 17 rplaca::*appearance-configuration-access-count*))
    (is (= 19 rplaca::*appearance-startup-resolution-count*))))

(test package-appearance-catalog-generation-and-owner-replacement
  "One owner batch increments generation once and a reload replaces its own IDs."
  (with-package-appearance-test-state ()
    (register-package-appearance-role (package-appearance-test-role "first"))
    (rplaca::commit-package-appearance-entrypoint-staging
     rplaca::*package-appearance-entrypoint-staging*)
    (let ((generation (appearance-catalog-generation (current-package-appearance-catalog))))
      (let ((rplaca::*package-appearance-entrypoint-staging*
              (rplaca::begin-package-appearance-entrypoint-staging
               (package-appearance-test-definition))))
        (register-package-appearance-role (package-appearance-test-role "second"))
        (rplaca::commit-package-appearance-entrypoint-staging
         rplaca::*package-appearance-entrypoint-staging*))
      (is (= (1+ generation)
             (appearance-catalog-generation (current-package-appearance-catalog))))
      (is (null (find-appearance-role-definition
                 (current-package-appearance-catalog)
                 (package-appearance-test-id "first"))))
      (is (find-appearance-role-definition
           (current-package-appearance-catalog)
           (package-appearance-test-id "second"))))))

(test empty-same-owner-reload-removes-the-prior-declaration-batch
  "An entrypoint which stops declaring appearance replaces its old batch with empty."
  (with-package-appearance-test-state ()
    (register-package-appearance-role (package-appearance-test-role "old"))
    (rplaca::commit-package-appearance-entrypoint-staging
     rplaca::*package-appearance-entrypoint-staging*)
    (let ((generation
            (appearance-catalog-generation
             (current-package-appearance-catalog)))
          (rplaca::*package-appearance-entrypoint-reload-p* t)
          (rplaca::*package-appearance-entrypoint-staging*
            (rplaca::begin-package-appearance-entrypoint-staging
             (package-appearance-test-definition))))
      (rplaca::commit-package-appearance-entrypoint-staging
       rplaca::*package-appearance-entrypoint-staging*)
      (is (= (1+ generation)
             (appearance-catalog-generation
              (current-package-appearance-catalog))))
      (is (null
           (find-appearance-role-definition
            (current-package-appearance-catalog)
            (package-appearance-test-id "old"))))
      (is (= 0
             (hash-table-count
              rplaca::*package-appearance-declarations*))))))

(test appearance-specifications-distinguish-unspecified-components
  "Every appearance axis has an explicit internal inheritance value."
  (let ((typography (make-appearance-typography-spec :face :bold))
        (ink (make-appearance-ink-spec :foreground '(:rgb 0.1 0.2 0.3)))
        (surface (make-appearance-surface-spec))
        (decoration (make-appearance-decoration-spec :kind :none)))
    (is (appearance-unspecified-p (appearance-typography-spec-family typography)))
    (is (eq :bold (appearance-typography-spec-face typography)))
    (is (appearance-unspecified-p (appearance-typography-spec-size typography)))
    (is (equal '(:rgb 0.1 0.2 0.3) (appearance-ink-spec-foreground ink)))
    (is (appearance-unspecified-p (appearance-surface-spec-background surface)))
    (is (eq :none (appearance-decoration-spec-kind decoration)))))

(test appearance-unspecified-sentinel-survives-source-reload
  "Reloading its source declaration preserves sentinels in existing objects."
  (let* ((source (merge-pathnames
                  #P"src/appearance.lisp"
                  (asdf:system-source-directory "rplaca")))
         (old-sentinel rplaca::*appearance-unspecified*)
         (typography (make-appearance-typography-spec :face :bold))
         (declaration
           (with-open-file (stream source :direction :input)
             (let ((*package* (find-package :rplaca)))
               (loop :for form = (read stream nil nil)
                     :while form
                     :when (and (consp form)
                                (member (first form) '(defvar defparameter))
                                (eq (second form)
                                    'rplaca::*appearance-unspecified*))
                       :return form)))))
    (is-true declaration)
    (unwind-protect
         (progn
           (eval declaration)
           (is (eq old-sentinel rplaca::*appearance-unspecified*))
           (is-true
            (appearance-unspecified-p
             (appearance-typography-spec-family typography))))
      (setf rplaca::*appearance-unspecified* old-sentinel))))

(test appearance-role-axis-validation-respects-role-kind
  "Surface-only background declarations cannot silently apply to content roles."
  (let* ((content (make-appearance-role-definition
                   :id :default-text :kind :content))
         (surface (make-appearance-role-definition
                   :id :transcript-pane :kind :surface))
         (style (make-appearance-role-style
                 :surface (make-appearance-surface-spec :background :white))))
    (signals unsupported-role-axis
      (validate-appearance-role-style content style :origin :test))
    (is (eq style (validate-appearance-role-style surface style :origin :test)))
    (is-true (appearance-role-supports-axis-p content :foreground-ink))
    (is-false (appearance-role-supports-axis-p content :surface))))

(test appearance-profile-and-theme-are-declaration-data
  "Profiles and themes retain portable data rather than resolved port objects."
  (let* ((theme (make-appearance-theme-definition
                 :id :classic
                 :role-overlays (list (cons :default-text
                                             (make-appearance-role-style)))))
         (profile (make-appearance-profile
                   :selected-theme :classic
                   :role-overrides (list (cons :default-text
                                                (make-appearance-role-style))))))
    (is (eq :classic (appearance-theme-definition-id theme)))
    (is (eq :classic (appearance-profile-selected-theme profile)))
    (is-false (appearance-profile-strict-contrast profile))
    (is (= 1 (length (appearance-profile-role-overrides profile))))))

(test appearance-declarations-defensively-copy-mutable-inputs
  "Published declarations neither retain nor expose mutable caller data."
  (let* ((parameters (list :marker (copy-seq "sample")))
         (overlays (list (cons :default-text
                               (make-appearance-role-style))))
         (theme (make-appearance-theme-definition
                 :id :classic
                 :role-overlays overlays))
         (decoration (make-appearance-decoration-spec
                      :kind :selection-marker :parameters parameters)))
    (setf (char (second parameters) 0) #\c
          (caar overlays) :error)
    (is (eq :default-text (caar (appearance-theme-definition-role-overlays theme))))
    (is (string= "sample"
                 (second (appearance-decoration-spec-parameters decoration))))
    (let ((published (appearance-theme-definition-role-overlays theme)))
      (setf (caar published) :error)
      (is (eq :default-text
              (caar (appearance-theme-definition-role-overlays theme)))))))

(test appearance-profile-validates-strict-contrast-and-style-components
  "Only profile strictness accepts NIL; axes are typed and role kinds are fixed."
  (is-true (appearance-profile-strict-contrast
            (make-appearance-profile :strict-contrast t)))
  (signals invalid-appearance-component
    (make-appearance-profile :strict-contrast :yes))
  (signals invalid-appearance-component
    (make-appearance-ink-spec :foreground nil))
  (signals invalid-appearance-component
    (make-appearance-ink-spec :foreground :none))
  (signals invalid-appearance-component
    (make-appearance-ink-spec :foreground :unspecified))
  (signals invalid-appearance-component
    (make-appearance-role-style :typography :bold))
  (signals invalid-appearance-component
    (make-appearance-role-definition :id :bad :kind :unknown)))

(test appearance-condition-factory-copies-payloads-before-signaling
  "The required factory and warning boundary preserve no caller-owned payload."
  (let* ((payload (list (copy-seq "original")))
         (condition (make-appearance-condition 'appearance-contrast-warning
                                               :value payload :role :default-text))
         (captured nil))
    (setf (char (first payload) 0) #\c)
    (handler-bind ((appearance-contrast-warning
                     (lambda (warning)
                       (setf captured warning)
                       (muffle-warning warning))))
      (warn condition))
    (is (typep captured 'warning))
    (is (string= "original" (car (appearance-condition-value captured))))
    (let ((reported (appearance-condition-value captured)))
      (setf (char (first reported) 0) #\c)
      (is (string= "original"
                   (car (appearance-condition-value captured)))))))

(test appearance-warning-factory-boundary-copies-signaled-payloads
  "WARN-APPEARANCE-CONDITION copies payloads before a handler can observe them."
  (let ((payload (list (copy-seq "original")))
        (captured nil))
    (handler-bind ((appearance-contrast-warning
                     (lambda (warning)
                       (setf captured warning)
                       (muffle-warning warning))))
      (warn-appearance-condition 'appearance-contrast-warning
                                 :value payload :role :default-text))
    (setf (char (first payload) 0) #\c)
    (is (string= "original" (car (appearance-condition-value captured))))))

(test appearance-vocabulary-accessors-return-fresh-lists
  "The internal vocabulary cannot be modified through its public accessors."
  (let ((sizes (appearance-logical-sizes))
        (kinds (appearance-role-kinds)))
    (setf (first sizes) :changed
          (first kinds) :changed)
    (is (eq :tiny (first (appearance-logical-sizes))))
    (is (eq :surface (first (appearance-role-kinds))))))

(defun test-appearance-style (&key foreground background family face size decoration)
  "Build only the explicitly requested typed style axes for appearance tests."
  (let ((arguments nil))
    (when (or family face size)
      (let ((typography-arguments nil))
        (when family
          (setf typography-arguments
                (append typography-arguments (list :family family))))
        (when face
          (setf typography-arguments
                (append typography-arguments (list :face face))))
        (when size
          (setf typography-arguments
                (append typography-arguments (list :size size))))
        (setf arguments
              (append arguments
                      (list :typography
                            (apply #'make-appearance-typography-spec
                                   typography-arguments))))))
    (when foreground
      (setf arguments
            (append arguments
                    (list :foreground-ink
                          (make-appearance-ink-spec :foreground foreground)))))
    (when background
      (setf arguments
            (append arguments
                    (list :surface
                          (make-appearance-surface-spec :background background)))))
    (when decoration
      (setf arguments
            (append arguments
                    (list :decoration
                          (make-appearance-decoration-spec :kind decoration)))))
    (apply #'make-appearance-role-style arguments)))

(defun make-test-appearance-catalog (&key role-cycle-p theme-cycle-p (generation 0))
  "Return a compact catalog exercising fallbacks, parent themes, and stacks."
  (make-appearance-catalog
   :role-definitions
   (list (make-appearance-role-definition :id :base :kind :surface)
         (make-appearance-role-definition :id :transcript :kind :surface
                                          :fallback-role :base)
         (make-appearance-role-definition :id :default-text :kind :content
                                          :fallback-role (and role-cycle-p :title))
         (make-appearance-role-definition :id :title :kind :content
                                          :fallback-role :default-text)
         (make-appearance-role-definition :id :selected :kind :state))
   :theme-definitions
   (list (make-appearance-theme-definition
          :id :root
          :parent-theme (and theme-cycle-p :child)
          :role-overlays
          (list (cons :default-text
                      (test-appearance-style :foreground :black
                                             :family :fix :size :normal))
                (cons :base (test-appearance-style :background :black))))
         (make-appearance-theme-definition
          :id :child
          :parent-theme :root
          :role-overlays
          (list (cons :title (test-appearance-style :face :bold :size :larger))
                (cons :selected (test-appearance-style :foreground :white
                                                       :face :bold)))))
   :generation generation))

(test appearance-role-resolution-obeys-the-exact-layer-order
  "Every source layer wins over lower layers while role fallback stays root-first."
  (let* ((catalog (make-test-appearance-catalog))
         (resolved
           (resolve-appearance-role
            catalog :child :title
            :file-overrides (list (cons :default-text
                                        (test-appearance-style :foreground :red)))
            :init-overrides (list (cons :title
                                        (test-appearance-style :foreground :green)))
            :environment-overrides (list (cons :title
                                               (test-appearance-style :foreground :blue)))
            :command-line-overrides (list (cons :title
                                                (test-appearance-style :foreground :cyan)))
            :unsaved-overrides (list (cons :title
                                           (test-appearance-style :foreground :white)))))
         (style (resolved-appearance-role-style resolved)))
    (is (eq :white
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink style))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography style))))
    (is (eq :large
            (appearance-typography-spec-size
             (appearance-role-style-typography style))))
    (is (equal :unsaved
               (cdr (assoc :foreground-ink
                           (resolved-appearance-role-provenance resolved)))))))

(test appearance-typography-merges-components-and-clamps-relative-sizes
  "Family/face/size cascade independently and each relative request is consumed."
  (let* ((catalog (make-test-appearance-catalog))
         (resolved (resolve-appearance-role
                    catalog :child :title
                    :file-overrides
                    (list (cons :title
                                (test-appearance-style :size :larger)))))
         (typography (appearance-role-style-typography
                      (resolved-appearance-role-style resolved))))
    (is (eq :fix (appearance-typography-spec-family typography)))
    (is (eq :bold (appearance-typography-spec-face typography)))
    (is (eq :very-large (appearance-typography-spec-size typography)))
    (is (eq :huge (resolve-appearance-relative-size :huge :larger :test)))
    (is (eq :tiny (resolve-appearance-relative-size :tiny :smaller :test)))
    (signals relative-size-base-invalid
      (resolve-appearance-relative-size :not-a-logical-size :larger :test))))

(test appearance-chain-builders-return-root-to-leaf-order
  "Fallback and parent builders independently preserve the cascade direction."
  (let ((catalog (make-test-appearance-catalog)))
    (is (equal '(:default-text :title)
               (mapcar #'appearance-role-definition-id
                       (appearance-role-fallback-chain catalog :title))))
    (is (equal '(:root :child)
               (mapcar #'appearance-theme-definition-id
                       (appearance-theme-parent-chain catalog :child))))))

(test appearance-catalog-rejects-invalid-graphs-and-configuration-roles
  "Graphs are rejected before publication; stored/configuration roles are fatal."
  (signals appearance-role-cycle
    (make-test-appearance-catalog :role-cycle-p t))
  (signals appearance-theme-cycle
    (make-test-appearance-catalog :theme-cycle-p t))
  (signals missing-appearance-parent
    (make-appearance-catalog
     :role-definitions
     (list (make-appearance-role-definition :id :missing-child :kind :content
                                             :fallback-role :absent))))
  (signals invalid-appearance-fallback
    (make-appearance-catalog
     :role-definitions
     (list (make-appearance-role-definition :id :surface :kind :surface
                                             :fallback-role :content)
           (make-appearance-role-definition :id :content :kind :content))))
  (signals unknown-appearance-role
    (resolve-appearance-role (make-test-appearance-catalog) :child :missing))
  (signals unknown-appearance-role
    (resolve-appearance-role
     (make-test-appearance-catalog) :child :title
     :file-overrides (list (cons :missing
                                 (test-appearance-style :foreground :red))))))

(test appearance-stack-composes-surface-content-and-state
  "A stack overlays surface, content, then state without sharing profile state."
  (let* ((resolved (resolve-appearance-role-stack
                    (make-test-appearance-catalog) :child
                    '(:transcript :title :selected)))
         (style (resolved-appearance-role-style resolved)))
    (is (eq :black
            (appearance-surface-spec-background
             (appearance-role-style-surface style))))
    (is (eq :white
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink style))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography style))))))

(test appearance-runtime-unknown-roles-fall-back-with-a-diagnostic
  "Runtime display data cannot fail redisplay because a wire role is unknown."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog) :child '(:unknown))))
    (is (= 1 (length (resolved-appearance-role-diagnostics resolved))))
    (is-false (appearance-condition-fatal-p
               (appearance-diagnostic-condition
                (first (resolved-appearance-role-diagnostics resolved)))))
    (is (eq :black
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink
              (resolved-appearance-role-style resolved)))))))

(test appearance-runtime-unknown-role-does-not-mask-a-later-valid-role
  "A valid same-kind wire role wins over a preceding fallback from an unknown."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog) :child '(:unknown :title))))
    (is (= 1 (length (resolved-appearance-role-diagnostics resolved))))
    (is (eq :bold
            (appearance-typography-spec-face
             (appearance-role-style-typography
              (resolved-appearance-role-style resolved)))))))

(test appearance-runtime-diagnostics-have-bounded-deduplication-keys
  "Each unknown role gets one bounded event keyed by catalog generation and role."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog :generation 17) :child
                   '(:unknown-one :unknown-two :title))))
    (is (= 2 (length (resolved-appearance-role-diagnostics resolved))))
    (is (equal '(:unknown-one :unknown-two)
               (mapcar #'appearance-diagnostic-unknown-role
                       (resolved-appearance-role-diagnostics resolved))))
    (is (equal '(17 :unknown-one)
               (appearance-diagnostic-deduplication-key
                (first (resolved-appearance-role-diagnostics resolved)))))))

(test appearance-runtime-diagnostics-bound-distinct-unknown-roles
  "The pure resolver cannot produce an unbounded diagnostic list."
  (let ((resolved (resolve-runtime-appearance-role-stack
                   (make-test-appearance-catalog) :child
                   (loop for index below 20
                         collect (intern (format nil "UNKNOWN-~D" index)
                                         :keyword)))))
    (is (= 16 (length (resolved-appearance-role-diagnostics resolved))))))

(test appearance-supported-axis-accessors-do-not-expose-constant-lists
  "Default axis vocabulary copies are fresh even when they originate in constants."
  (let* ((role (make-appearance-role-definition :id :content :kind :content))
         (axes (appearance-role-supported-axes role)))
    (setf (first axes) :changed)
    (is (eq :typography (first (appearance-role-supported-axes role))))))

(test appearance-structural-keys-and-classic-goldens-are-deterministic
  "Keys are output-only and classic retains every current literal/style golden."
  (let* ((catalog (make-test-appearance-catalog))
         (first (resolve-appearance-role catalog :child :title))
         (second (resolve-appearance-role catalog :child :title
                                          :unsaved-overrides nil))
         (different (resolve-appearance-role catalog :child :default-text))
         (classic-catalog (make-classic-appearance-catalog))
         (classic (resolve-appearance-role classic-catalog :classic
                                           :transcript-user)))
    (is (equal (resolved-appearance-role-structural-key first)
               (resolved-appearance-role-structural-key second)))
    (is-false (equal (resolved-appearance-role-structural-key first)
                     (resolved-appearance-role-structural-key different)))
    (is (equal
         (appearance-role-style-key
          (make-appearance-role-style
           :decoration (make-appearance-decoration-spec
                        :kind :selection-marker
                        :parameters (list :marker ">"))))
         (appearance-role-style-key
          (make-appearance-role-style
           :decoration (make-appearance-decoration-spec
                        :kind :selection-marker
                        :parameters (list :marker ">"))))))
    (is (equal '(:typography :unspecified
                 :foreground-ink :unspecified
                 :surface :unspecified
                 :decoration (:selection-marker (:marker ">")))
               (appearance-role-style-key
                (make-appearance-role-style
                 :decoration
                 (make-appearance-decoration-spec
                  :kind :selection-marker
                  :parameters (list :marker ">"))))))
    (is (equal '(0.10 0.25 0.55)
               (appearance-ink-spec-foreground
                (appearance-role-style-foreground-ink
                 (resolved-appearance-role-style classic)))))
    (dolist (golden '((:transcript-agent (0.10 0.10 0.10))
                      (:transcript-tool (0.12 0.34 0.18))
                      (:tool-result (0.12 0.34 0.18))
                      (:transcript-system (0.36 0.36 0.36))
                      (:transcript-empty (0.45 0.45 0.45))
                      (:selector-title (0.16 0.22 0.45))
                      (:selector-header (0.18 0.36 0.20))
                      (:selector-footer (0.35 0.35 0.35))
                      (:selector-selection (0.10 0.38 0.65))
                      (:selector-entry (0.20 0.20 0.20))
                      (:system (0.45 0.45 0.45))
                      (:disabled (0.45 0.45 0.45))
                      (:error (0.60 0.12 0.12))))
      (let* ((resolved (resolve-appearance-role classic-catalog :classic
                                                 (first golden)))
             (foreground (appearance-ink-spec-foreground
                          (appearance-role-style-foreground-ink
                           (resolved-appearance-role-style resolved)))))
        (is (equal (second golden) foreground))))
    (let* ((title (resolved-appearance-role-style
                   (resolve-appearance-role classic-catalog :classic
                                            :selector-title)))
           (selection (resolved-appearance-role-style
                       (resolve-appearance-role classic-catalog :classic
                                                :selector-selection)))
           (minibuffer-emphasis
             (resolved-appearance-role-style
              (resolve-appearance-role classic-catalog :classic
                                       :minibuffer-selection-emphasis)))
           (generic-selected
             (resolved-appearance-role-style
              (resolve-appearance-role-stack
               classic-catalog :classic
               '(:selector-entry :selector-selection))))
           (minibuffer-selected
             (resolved-appearance-role-style
              (resolve-appearance-role-stack
               classic-catalog :classic
               '(:minibuffer-pane :selector-entry :selector-selection
                 :minibuffer-selection-emphasis)))))
      (is (eq :bold (appearance-typography-spec-face
                     (appearance-role-style-typography title))))
      (is-true (appearance-unspecified-p
                (appearance-typography-spec-face
                 (appearance-role-style-typography selection))))
      (is (equal '(:marker ">")
                 (appearance-decoration-spec-parameters
                  (appearance-role-style-decoration selection))))
      (is (eq :bold (appearance-typography-spec-face
                     (appearance-role-style-typography minibuffer-emphasis))))
      (is-true (appearance-unspecified-p
                (appearance-typography-spec-face
                 (appearance-role-style-typography generic-selected))))
      (is (eq :bold (appearance-typography-spec-face
                     (appearance-role-style-typography minibuffer-selected))))
      (is (equal '(:marker ">")
                 (appearance-decoration-spec-parameters
                  (appearance-role-style-decoration minibuffer-selected)))))))

(defun appearance-test-dark-foreground (catalog role)
  "Return ROLE's exact dark foreground declaration for palette goldens."
  (appearance-ink-spec-foreground
   (appearance-role-style-foreground-ink
    (resolved-appearance-role-style
     (resolve-appearance-role catalog :dark role)))) )

(defun appearance-test-dark-surface (catalog role)
  "Return ROLE's exact dark surface declaration for palette goldens."
  (appearance-surface-spec-background
   (appearance-role-style-surface
    (resolved-appearance-role-style
     (resolve-appearance-role catalog :dark role)))))

(test appearance-dark-profile-has-the-exact-opaque-rgb-palette
  "Every Section-5 dark palette declaration is exact, opaque RGB data."
  (let ((catalog (make-classic-appearance-catalog)))
    (dolist (golden '((:base (:rgb 13/255 17/255 23/255))
                      (:transcript-pane (:rgb 13/255 17/255 23/255))
                      (:compose-pane (:rgb 13/255 17/255 23/255))
                      (:help-pane (:rgb 13/255 17/255 23/255))
                      (:info-pane (:rgb 22/255 27/255 34/255))
                      (:minibuffer-pane (:rgb 22/255 27/255 34/255))))
      (is (equal (second golden)
                 (appearance-test-dark-surface catalog (first golden)))))
    (dolist (golden '((:default-text (:rgb 230/255 237/255 243/255))
                      (:transcript-agent (:rgb 230/255 237/255 243/255))
                      (:modeline (:rgb 230/255 237/255 243/255))
                      (:selector-entry (:rgb 230/255 237/255 243/255))
                      (:transcript-user (:rgb 121/255 192/255 255/255))
                      (:transcript-tool (:rgb 126/255 231/255 135/255))
                      (:tool-result (:rgb 126/255 231/255 135/255))
                      (:selector-header (:rgb 126/255 231/255 135/255))
                      (:transcript-system (:rgb 177/255 186/255 196/255))
                      (:system (:rgb 177/255 186/255 196/255))
                      (:transcript-empty (:rgb 139/255 148/255 158/255))
                      (:selector-separator (:rgb 139/255 148/255 158/255))
                      (:selector-footer (:rgb 139/255 148/255 158/255))
                      (:error (:rgb 255/255 123/255 114/255))
                      (:selector-title (:rgb 165/255 214/255 255/255))
                      (:selector-selection (:rgb 255/255 255/255 255/255))
                      (:disabled (:rgb 139/255 148/255 158/255))))
      (is (equal (second golden)
                 (appearance-test-dark-foreground catalog (first golden)))))))

(test appearance-dark-profile-preserves-classic-typography-and-pointer-documentation
  "Dark changes no typography and leaves pointer documentation outside its base."
  (let ((catalog (make-classic-appearance-catalog)))
    (dolist (role '(:default-text :transcript-user :transcript-agent
                    :transcript-tool :transcript-system :transcript-empty
                    :system :error :tool-result :modeline :selector-title
                    :selector-header :selector-entry :selector-separator
                    :selector-footer :selector-selection
                    :minibuffer-selection-emphasis :disabled))
      (is (equal
           (appearance-role-style-key
            (make-appearance-role-style
             :typography
             (appearance-role-style-typography
              (resolved-appearance-role-style
               (resolve-appearance-role catalog :classic role)))))
           (appearance-role-style-key
            (make-appearance-role-style
             :typography
             (appearance-role-style-typography
              (resolved-appearance-role-style
               (resolve-appearance-role catalog :dark role))))))))
    (let ((pointer-style
            (resolved-appearance-role-style
             (resolve-appearance-role catalog :dark :pointer-documentation))))
      (is-true (appearance-unspecified-p
                (appearance-surface-spec-background
                 (appearance-role-style-surface pointer-style)))))))

(test appearance-dark-profile-contrast-goldens-cover-every-built-in-stack
  "All active dark stacks compose surface < content < state at 4.5:1 or better."
  (let ((catalog (make-classic-appearance-catalog)))
    (dolist (golden '(((:transcript-pane :default-text) 16.02d0)
                      ((:transcript-pane :transcript-agent) 16.02d0)
                      ((:help-pane :default-text) 16.02d0)
                      ((:compose-pane :default-text) 16.02d0)
                      ((:transcript-pane :transcript-user) 9.73d0)
                      ((:transcript-pane :transcript-tool) 12.32d0)
                      ((:transcript-pane :tool-result) 12.32d0)
                      ((:transcript-pane :transcript-system) 9.63d0)
                      ((:transcript-pane :system) 9.63d0)
                      ((:transcript-pane :transcript-empty) 6.15d0)
                      ((:transcript-pane :error) 7.51d0)
                      ((:info-pane :modeline) 14.64d0)
                      ((:minibuffer-pane :selector-title) 11.25d0)
                      ((:minibuffer-pane :selector-header) 11.26d0)
                      ((:minibuffer-pane :selector-entry) 14.64d0)
                      ((:minibuffer-pane :selector-separator) 5.62d0)
                      ((:minibuffer-pane :selector-footer) 5.62d0)
                      ((:minibuffer-pane :selector-entry :selector-selection) 17.30d0)
                      ((:minibuffer-pane :selector-entry :disabled) 5.62d0)))
      (let* ((stack (first golden))
             (style (resolved-appearance-role-style
                     (resolve-appearance-role-stack catalog :dark stack)))
             (ratio (rplaca::appearance-contrast-ratio
                     (appearance-ink-spec-foreground
                      (appearance-role-style-foreground-ink style))
                     (appearance-surface-spec-background
                      (appearance-role-style-surface style)))))
        (is (<= (abs (- ratio (second golden))) 0.01d0))
        (is (>= ratio 4.5d0))))
    (is-true (rplaca::validate-appearance-profile-contrast
              catalog (make-appearance-profile :selected-theme :dark)))))

(test appearance-dark-profile-contrast-policy-is-typed-and-never-recolors
  "User-touched low contrast warns unless strict; built-in failures are fatal."
  (let* ((catalog (make-classic-appearance-catalog))
         (override (test-appearance-style :foreground :black))
         (profile (make-appearance-profile
                   :selected-theme :dark
                   :role-overrides (list (cons :error override))))
         (captured nil))
    (handler-bind
        ((appearance-contrast-warning
           (lambda (warning)
             (setf captured warning)
             (muffle-warning warning))))
      (is-true (rplaca::validate-appearance-profile-contrast catalog profile)))
    (is (equal '(:transcript-pane :error) (appearance-condition-role captured)))
    (is (equal '((:foreground-ink . :unsaved)
                 (:surface :theme :dark :owner :builtin))
               (appearance-condition-origin captured)))
    (is (equal '(:transcript-pane :error) (appearance-condition-path captured)))
    (is (eq :contrast (appearance-condition-axis captured)))
    (is (numberp (appearance-condition-value captured)))
    (is-false (appearance-condition-fatal-p captured))
    (is (equal '(:increase-foreground-contrast :change-surface)
               (appearance-condition-suggested-repairs captured)))
    (is (eq :black
            (appearance-ink-spec-foreground
             (appearance-role-style-foreground-ink override))))
    (signals appearance-contrast-warning
      (rplaca::validate-appearance-profile-contrast
       catalog (make-appearance-profile
                :selected-theme :dark :strict-contrast t
                :role-overrides (list (cons :error override)))))
    (let* ((dark (find-appearance-theme-definition catalog :dark))
           (broken-dark
             (make-appearance-theme-definition
              :id :dark :parent-theme :classic :owner :builtin
              :role-overlays
              (cons (cons :error (test-appearance-style :foreground :black))
                    (remove :error (appearance-theme-definition-role-overlays dark)
                            :key #'car :test #'eq))))
           (broken-catalog
             (make-appearance-catalog
              :role-definitions (appearance-catalog-role-definitions catalog)
              :theme-definitions
              (cons broken-dark
                    (remove :dark (appearance-catalog-theme-definitions catalog)
                            :key #'appearance-theme-definition-id :test #'eq))
              :built-in-overlays (appearance-catalog-built-in-overlays catalog))))
      (signals appearance-contrast-warning
        (rplaca::validate-appearance-profile-contrast
         broken-catalog (make-appearance-profile :selected-theme :dark))))))

(defun appearance-test-catalog-with-package-theme (owner)
  "Return a dark-derived package theme whose error foreground has low contrast."
  (let* ((catalog (make-classic-appearance-catalog))
         (package-theme-id '(:package "org.example.appearance" "low-dark"))
         (package-theme
           (make-appearance-theme-definition
            :id package-theme-id
            :parent-theme :dark
            :owner owner
            :role-overlays
            (list (cons :error (test-appearance-style :foreground :black))))))
    (values
     (make-appearance-catalog
      :role-definitions (appearance-catalog-role-definitions catalog)
      :theme-definitions
      (append (appearance-catalog-theme-definitions catalog)
              (list package-theme))
      :built-in-overlays (appearance-catalog-built-in-overlays catalog))
     package-theme-id)))

(defun appearance-test-catalog-with-package-defaults (owner)
  "Return package-owned surface/content role defaults with low contrast."
  (let* ((catalog (make-classic-appearance-catalog))
         (surface-id '(:package "org.example.appearance" "low-surface"))
         (content-id '(:package "org.example.appearance" "low-content")))
    (values
     (make-appearance-catalog
      :role-definitions
      (append
       (appearance-catalog-role-definitions catalog)
       (list (make-appearance-role-definition
              :id surface-id :kind :surface :owner owner)
             (make-appearance-role-definition
              :id content-id :kind :content :owner owner)))
      :theme-definitions (appearance-catalog-theme-definitions catalog)
      :built-in-overlays
      (append
       (appearance-catalog-built-in-overlays catalog)
       (list (cons surface-id
                   (test-appearance-style
                    :background '(:rgb 13/255 17/255 23/255)))
             (cons content-id
                   (test-appearance-style :foreground :black)))))
     (list surface-id content-id))))

(test appearance-contrast-provenance-identifies-built-in-contributions-by-owner
  "Core origins are explicitly owned; theme names and NIL never imply built-in."
  (let* ((catalog (make-classic-appearance-catalog))
         (resolved (resolve-appearance-role-stack
                    catalog :dark '(:transcript-pane :error)))
         (provenance (resolved-appearance-role-provenance resolved)))
    (is (every (lambda (role)
                 (eq :builtin (appearance-role-definition-owner role)))
               (appearance-catalog-role-definitions catalog)))
    (is (every (lambda (theme)
                 (eq :builtin (appearance-theme-definition-owner theme)))
               (appearance-catalog-theme-definitions catalog)))
    (is (equal '((:foreground-ink :theme :dark :owner :builtin)
                 (:surface :theme :dark :owner :builtin))
               provenance))
    (is-true (rplaca::appearance-built-in-contrast-provenance-p provenance))
    (is-false
     (rplaca::appearance-built-in-contrast-provenance-p
      '((:foreground-ink :theme :dark :owner nil)
        (:surface :theme :dark :owner :builtin))))))

(test appearance-package-theme-contrast-provenance-warns-and-strictly-fails
  "A package theme contributes at the theme layer and never counts as built-in."
  (multiple-value-bind (catalog theme-id)
      (appearance-test-catalog-with-package-theme "org.example.appearance")
    (let ((captured nil))
      (handler-bind
          ((appearance-contrast-warning
             (lambda (warning)
               (setf captured warning)
               (muffle-warning warning))))
        (is-true
         (rplaca::validate-appearance-profile-contrast
          catalog (make-appearance-profile :selected-theme theme-id))))
      (is-false (appearance-condition-fatal-p captured))
      (is (equal
           `((:foreground-ink :theme ,theme-id
                              :owner "org.example.appearance")
             (:surface :theme :dark :owner :builtin))
           (appearance-condition-origin captured)))
      (is (equal '(:transcript-pane :error)
                 (appearance-condition-role captured)))
      (signals appearance-contrast-warning
        (rplaca::validate-appearance-profile-contrast
         catalog (make-appearance-profile
                  :selected-theme theme-id :strict-contrast t))))))

(test appearance-package-role-default-contrast-provenance-warns-and-strictly-fails
  "Package role defaults retain the defaults layer and owner-aware policy."
  (multiple-value-bind (catalog role-stack)
      (appearance-test-catalog-with-package-defaults "org.example.appearance")
    (let ((captured nil))
      (handler-bind
          ((appearance-contrast-warning
             (lambda (warning)
               (setf captured warning)
               (muffle-warning warning))))
        (is-true
         (rplaca::validate-appearance-profile-contrast
          catalog (make-appearance-profile :selected-theme :dark)
          :role-stacks (list role-stack))))
      (is-false (appearance-condition-fatal-p captured))
      (is (equal
           `((:foreground-ink :role-default ,(second role-stack)
                              :owner "org.example.appearance")
             (:surface :role-default ,(first role-stack)
                       :owner "org.example.appearance"))
           (appearance-condition-origin captured)))
      (signals appearance-contrast-warning
        (rplaca::validate-appearance-profile-contrast
         catalog
         (make-appearance-profile :selected-theme :dark :strict-contrast t)
         :role-stacks (list role-stack))))))

(test appearance-nil-owned-contribution-warns-and-mixed-owners-remain-visible
  "NIL ownership is non-built-in and mixed effective contributors are preserved."
  (multiple-value-bind (catalog theme-id)
      (appearance-test-catalog-with-package-theme nil)
    (let ((captured nil))
      (handler-bind
          ((appearance-contrast-warning
             (lambda (warning)
               (setf captured warning)
               (muffle-warning warning))))
        (is-true
         (rplaca::validate-appearance-profile-contrast
          catalog (make-appearance-profile :selected-theme theme-id))))
      (is-false (appearance-condition-fatal-p captured))
      (is (equal
           `((:foreground-ink :theme ,theme-id :owner nil)
             (:surface :theme :dark :owner :builtin))
           (appearance-condition-origin captured)))
      (is-false
       (rplaca::appearance-built-in-contrast-provenance-p
        (appearance-condition-origin captured))))))

(test appearance-classic-custom-contrast-is-validated-while-built-in-is-inert
  "Classic skips its unknown backend surface only until a custom stack touches it."
  (let* ((catalog (make-classic-appearance-catalog))
         (overrides
           (list (cons :base
                       (test-appearance-style
                        :background '(:rgb 13/255 17/255 23/255)))
                 (cons :default-text
                       (test-appearance-style :foreground :black))))
         (profile (make-appearance-profile
                   :selected-theme :classic :role-overrides overrides))
         (captured nil))
    (is-true
     (rplaca::validate-appearance-profile-contrast
      catalog (make-appearance-profile :selected-theme :classic)
      :role-stacks '((:transcript-pane :default-text))))
    (handler-bind
        ((appearance-contrast-warning
           (lambda (warning)
             (setf captured warning)
             (muffle-warning warning))))
      (is-true
       (rplaca::validate-appearance-profile-contrast
        catalog profile
        :role-stacks '((:transcript-pane :default-text)))))
    (is-false (appearance-condition-fatal-p captured))
    (is (equal '((:foreground-ink . :unsaved)
                 (:surface . :unsaved))
               (appearance-condition-origin captured)))
    (signals appearance-contrast-warning
      (rplaca::validate-appearance-profile-contrast
       catalog
       (make-appearance-profile
        :selected-theme :classic :strict-contrast t
        :role-overrides overrides)
       :role-stacks '((:transcript-pane :default-text))))))

(test appearance-activation-preparation-classifies-whole-bundles-without-publication
  "Only non-default foreground changes are render-boundary live in commit six."
  (let* ((catalog (make-classic-appearance-catalog))
         (active (make-appearance-profile))
         (candidate
           (make-appearance-candidate
            (make-appearance-profile
             :role-overrides
             (list
              (cons :transcript-user
                    (test-appearance-style :foreground '(0.80 0.20 0.30)))))))
         (result (rplaca::prepare-appearance-activation
                  catalog active candidate :port-identity :test-port))
         (classification (appearance-activation-result-classification result)))
    (is (eq :ready (appearance-activation-result-status result)))
    (is (eq :render-boundary-live
            (appearance-activation-classification-status classification)))
    (is-true
     (every (lambda (delta)
              (eq :render-boundary-live (getf delta :classification)))
            (appearance-activation-classification-deltas classification)))
    ;; Preparation is pure: the active profile remains the separate classic
    ;; value until a frame event decides to publish the prepared result.
    (is (eq :classic (appearance-profile-selected-theme active)))))

(test appearance-activation-classifies-dark-as-restart-required-and-noop-exactly
  "Surface/default deltas stage; structurally equal candidates do nothing."
  (let* ((catalog (make-classic-appearance-catalog))
         (active (make-appearance-profile))
         (dark (rplaca::prepare-appearance-activation
                 catalog active
                (make-appearance-candidate
                 (make-appearance-profile :selected-theme :dark))
                :port-identity :test-port))
         (noop (rplaca::prepare-appearance-activation
                catalog active
                (make-appearance-candidate (make-appearance-profile))
                :port-identity :test-port)))
    (is (eq :restart-required (appearance-activation-result-status dark)))
    (is (eq :restart-required
            (appearance-activation-classification-status
             (appearance-activation-result-classification dark))))
    (is-true
     (some (lambda (delta)
             (and (eq :surface (getf delta :axis))
                  (eq :restart-required (getf delta :classification))))
           (appearance-activation-classification-deltas
            (appearance-activation-result-classification dark))))
    (is (eq :no-op (appearance-activation-result-status noop)))
    (is (eq :no-op
            (appearance-activation-classification-status
             (appearance-activation-result-classification noop))))))

(test appearance-classification-ignores-new-unused-catalog-roles
  "Installing an unreferenced package role does not change every live axis."
  (let* ((classic (make-classic-appearance-catalog))
         (catalog
           (make-appearance-catalog
            :role-definitions
            (append
             (appearance-catalog-role-definitions classic)
             (list
              (make-appearance-role-definition
               :id '(:package "org.example.theme" "unused")
               :kind :content
               :fallback-role :default-text)))
            :theme-definitions
            (appearance-catalog-theme-definitions classic)
            :built-in-overlays
            (appearance-catalog-built-in-overlays classic)
            :generation
            (1+ (appearance-catalog-generation classic))))
         (profile (make-appearance-profile))
         (active
           (resolve-appearance-profile-bundle
            classic profile :profile-revision 0
            :font-inventory-generation 0 :port-identity :test-port))
         (candidate
           (resolve-appearance-profile-bundle
            catalog profile :profile-revision 1
            :font-inventory-generation 0 :port-identity :test-port))
         (classification
           (rplaca::classify-appearance-bundle-delta
            catalog active candidate)))
    (is (eq :no-op
            (appearance-activation-classification-status classification)))
    (is (null
         (appearance-activation-classification-deltas classification)))))

(test appearance-activation-preparation-retains-a-structured-resolution-diagnostic
  "A missing candidate theme fails before any caller could publish a bundle."
  (let* ((candidate
           (make-appearance-candidate
            (make-appearance-profile :selected-theme :missing-theme)))
         (result
           (rplaca::prepare-appearance-activation
            (make-classic-appearance-catalog)
            (make-appearance-profile) candidate :port-identity :test-port)))
    (is (eq :failed (appearance-activation-result-status result)))
    (is (null (appearance-activation-result-bundle result)))
    (is-true
     (some (lambda (diagnostic) (typep diagnostic 'appearance-condition))
           (appearance-activation-result-diagnostics result)))))

(test appearance-port-bundles-are-structural-port-local-and-immutable
  "A supplied opaque port is the sole identity-bearing bundle-key component."
  (let* ((catalog (make-classic-appearance-catalog))
         (profile (make-appearance-profile))
         (port-one (make-symbol "OPAQUE-PORT"))
         (port-two (make-symbol "OPAQUE-PORT"))
         (first (resolve-appearance-profile-bundle
                 catalog profile :profile-revision 4
                 :font-inventory-generation 7 :port-identity port-one))
         (same (resolve-appearance-profile-bundle
                catalog profile :profile-revision 4
                :font-inventory-generation 7 :port-identity port-one))
         (other-port (resolve-appearance-profile-bundle
                      catalog profile :profile-revision 4
                      :font-inventory-generation 7 :port-identity port-two))
         (other-inventory (resolve-appearance-profile-bundle
                           catalog profile :profile-revision 4
                           :font-inventory-generation 8 :port-identity port-one))
         (other-revision (resolve-appearance-profile-bundle
                          catalog profile :profile-revision 5
                          :font-inventory-generation 7 :port-identity port-one))
         (other-catalog
           (make-appearance-catalog
            :role-definitions (appearance-catalog-role-definitions catalog)
            :theme-definitions (appearance-catalog-theme-definitions catalog)
            :built-in-overlays (appearance-catalog-built-in-overlays catalog)
            :generation 9))
         (other-generation (resolve-appearance-profile-bundle
                            other-catalog profile :profile-revision 4
                            :font-inventory-generation 7 :port-identity port-one)))
    (is (equal (resolved-appearance-bundle-bundle-key first)
               (resolved-appearance-bundle-bundle-key same)))
    (is-false (equal (resolved-appearance-bundle-bundle-key first)
                     (resolved-appearance-bundle-bundle-key other-port)))
    (is-false (equal (resolved-appearance-bundle-bundle-key first)
                     (resolved-appearance-bundle-bundle-key other-inventory)))
    (is-false (equal (resolved-appearance-bundle-bundle-key first)
                     (resolved-appearance-bundle-bundle-key other-revision)))
    (is-false (equal (resolved-appearance-bundle-bundle-key first)
                     (resolved-appearance-bundle-bundle-key other-generation)))
    (is (eq port-one (resolved-appearance-bundle-port-identity first)))
    (is (= 7 (resolved-appearance-bundle-font-inventory-generation first)))
    (is-true (resolved-appearance-bundle-surface-defaults first))
    (is-true (assoc :transcript-user
                    (resolved-appearance-bundle-provenance first)
                    :test #'equal))
    (let ((keys (resolved-appearance-bundle-role-keys first)))
      (setf (caar keys) :mutated)
      (is (assoc :transcript-user
                 (resolved-appearance-bundle-role-keys first)
                 :test #'equal)))))

(test appearance-port-bundle-role-keys-change-only-with-effective-role-output
  "A transcript-user override leaves unrelated role render keys untouched."
  (let* ((catalog (make-classic-appearance-catalog))
         (port (make-symbol "OPAQUE-PORT"))
         (base (resolve-appearance-profile-bundle
                catalog (make-appearance-profile) :profile-revision 0
                :font-inventory-generation 0 :port-identity port))
         (changed (resolve-appearance-profile-bundle
                   catalog
                   (make-appearance-profile
                    :role-overrides
                    (list (cons :transcript-user
                                (test-appearance-style
                                 :foreground '(0.80 0.20 0.30)))))
                   :profile-revision 1 :font-inventory-generation 0
                   :port-identity port)))
    (is-false (equal (resolved-appearance-bundle-bundle-key base)
                     (resolved-appearance-bundle-bundle-key changed)))
    (is-false (equal (cdr (assoc :transcript-user
                                 (resolved-appearance-bundle-role-keys base)))
                     (cdr (assoc :transcript-user
                                 (resolved-appearance-bundle-role-keys changed)))))
    (is (equal (cdr (assoc :transcript-agent
                           (resolved-appearance-bundle-role-keys base)))
               (cdr (assoc :transcript-agent
                           (resolved-appearance-bundle-role-keys changed)))))))
