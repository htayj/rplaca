(in-package :rplaca)

;;;; Appearance declarations and resolution
;;;;
;;;; This file deliberately contains no drawing, frame mutation, or configuration
;;;; I/O.  It describes immutable appearance data which later UI code resolves at
;;;; a frame and port boundary.

(defvar *appearance-unspecified* (gensym "APPEARANCE-UNSPECIFIED-")
  "Process-stable sentinel retained across in-place ASDF reloads.")

(defun appearance-unspecified-p (value)
  "Return true when VALUE is the internal inheritance sentinel."
  (eq value *appearance-unspecified*))

(defparameter +appearance-logical-sizes+
  '(:tiny :very-small :small :normal :large :very-large :huge))

(defparameter +appearance-relative-sizes+ '(:smaller :larger))

(defparameter +appearance-role-kinds+ '(:surface :content :state))

(defparameter +appearance-role-axes+
  '((:surface . (:typography :foreground-ink :surface :decoration))
    (:content . (:typography :foreground-ink :decoration))
    (:state . (:typography :foreground-ink :decoration))))

(defconstant +appearance-runtime-diagnostic-limit+ 16)

(defconstant +appearance-minimum-text-contrast+ 4.5d0)

(defparameter +dark-appearance-text-stacks+
  '((:transcript-pane :default-text)
    (:transcript-pane :transcript-agent)
    (:help-pane :default-text)
    (:compose-pane :default-text)
    (:transcript-pane :transcript-user)
    (:transcript-pane :transcript-tool)
    (:transcript-pane :tool-result)
    (:transcript-pane :transcript-system)
    (:transcript-pane :system)
    (:transcript-pane :transcript-empty)
    (:transcript-pane :error)
    (:info-pane :modeline)
    (:minibuffer-pane :selector-title)
    (:minibuffer-pane :selector-header)
    (:minibuffer-pane :selector-entry)
    (:minibuffer-pane :selector-separator)
    (:minibuffer-pane :selector-footer)
    (:minibuffer-pane :selector-entry :selector-selection)
    (:minibuffer-pane :selector-entry :disabled)))

(defun appearance-logical-sizes ()
  "Return a fresh stable-order copy of supported logical text sizes."
  (copy-list +appearance-logical-sizes+))

(defun appearance-relative-sizes ()
  "Return a fresh stable-order copy of supported relative text sizes."
  (copy-list +appearance-relative-sizes+))

(defun appearance-role-kinds ()
  "Return a fresh stable-order copy of supported role kinds."
  (copy-list +appearance-role-kinds+))

(define-condition appearance-condition (condition)
  ((origin :initarg :origin :reader appearance-condition-origin :initform nil)
   (role :initarg :role :reader appearance-condition-role :initform nil)
   (axis :initarg :axis :reader appearance-condition-axis :initform nil)
   (value :initarg :value :reader appearance-condition-value :initform nil)
   (path :initarg :path :reader appearance-condition-path :initform nil)
   (port :initarg :port :reader appearance-condition-port :initform nil)
   (available-choices :initarg :available-choices
                      :reader appearance-condition-available-choices
                      :initform nil)
   (fatal-p :initarg :fatal-p :reader appearance-condition-fatal-p :initform t)
   (suggested-repairs :initarg :suggested-repairs
                      :reader appearance-condition-suggested-repairs
                      :initform nil))
  (:documentation "Base condition for an appearance diagnostic.")
  (:report (lambda (condition stream)
             (format stream "Appearance condition~@[ for role ~S~]~@[ at axis ~S~]: ~S"
                     (appearance-condition-role condition)
                     (appearance-condition-axis condition)
                     (appearance-condition-value condition)))))

(defun copy-appearance-condition-initargs (initargs)
  "Copy all diagnostic initarg values before condition initialization."
  (loop :for (key value) :on initargs :by #'cddr
        :append (list key (copy-appearance-value value))))

(defmethod appearance-condition-origin :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-role :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-axis :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-value :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-path :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-port :around ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-available-choices :around
    ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(defmethod appearance-condition-suggested-repairs :around
    ((condition appearance-condition))
  (copy-appearance-value (call-next-method)))

(define-condition appearance-error (error appearance-condition) ())

(define-condition unknown-appearance-role (appearance-error) ())
(define-condition invalid-appearance-component (appearance-error) ())
(define-condition appearance-theme-cycle (appearance-error) ())
(define-condition appearance-role-cycle (appearance-error) ())
(define-condition missing-appearance-parent (appearance-error) ())
(define-condition invalid-appearance-fallback (appearance-error) ())
(define-condition unsupported-role-axis (appearance-error) ())
(define-condition ambiguous-font-family (appearance-error) ())
(define-condition ambiguous-font-face (appearance-error) ())
(define-condition font-unavailable (appearance-error) ())
(define-condition font-size-unavailable (appearance-error) ())
(define-condition relative-size-base-invalid (appearance-error) ())
(define-condition device-font-port-mismatch (appearance-error) ())
(define-condition appearance-contrast-warning (warning appearance-condition) ())
(define-condition appearance-live-update-unsupported (appearance-error) ())
(define-condition appearance-activation-failed (appearance-error) ())

(defun copy-appearance-value (value)
  "Return a deep copy of VALUE's mutable Common Lisp containers."
  (typecase value
    (string (copy-seq value))
    (cons (cons (copy-appearance-value (car value))
                (copy-appearance-value (cdr value))))
    (vector (map 'vector #'copy-appearance-value value))
    (hash-table
     (let ((copy (make-hash-table :test (hash-table-test value)
                                  :size (hash-table-count value))))
       (maphash (lambda (key entry)
                  (setf (gethash (copy-appearance-value key) copy)
                        (copy-appearance-value entry)))
                value)
       copy))
    (t value)))

(defun make-appearance-condition (condition-class &rest initargs)
  "Construct an appearance condition with copied diagnostic payloads.

Appearance condition classes are public handling types.  Application code
must construct them through this function rather than CL:MAKE-CONDITION, whose
implementation-specific initialization path cannot provide this copy boundary."
  (unless (nth-value 0 (subtypep condition-class 'appearance-condition))
    (error "~S is not an appearance condition class." condition-class))
  (apply #'make-condition condition-class
         (copy-appearance-condition-initargs initargs)))

(defun signal-appearance-condition (condition-class &rest initargs)
  "Construct and signal an appearance condition through the mandatory boundary."
  (let ((condition (apply #'make-appearance-condition condition-class initargs)))
    (cond ((typep condition 'warning) (warn condition))
          ((typep condition 'error) (error condition))
          (t (signal condition)))))

(defun error-appearance-condition (condition-class &rest initargs)
  "Construct and signal an appearance error through the mandatory boundary."
  (error (apply #'make-appearance-condition condition-class initargs)))

(defun warn-appearance-condition (condition-class &rest initargs)
  "Construct and warn an appearance warning through the mandatory boundary."
  (let ((condition (apply #'make-appearance-condition condition-class initargs)))
    (unless (typep condition 'warning)
      (error "~S is not an appearance warning class." condition-class))
    (warn condition)))

(defun validate-appearance-component (value axis)
  "Reject NIL as an appearance style component and return VALUE."
  (when (null value)
    (error-appearance-condition 'invalid-appearance-component :axis axis :value value))
  value)

(defstruct (appearance-typography-spec
            (:constructor %make-appearance-typography-spec
                (&key (family *appearance-unspecified*)
                      (face *appearance-unspecified*)
                      (size *appearance-unspecified*)))
            (:conc-name %appearance-typography-spec-))
  "Partial typography declaration.  Each component inherits independently."
  (family *appearance-unspecified* :read-only t)
  (face *appearance-unspecified* :read-only t)
  (size *appearance-unspecified* :read-only t))

(defstruct (appearance-ink-spec
            (:constructor %make-appearance-ink-spec
                (&key (foreground *appearance-unspecified*)))
            (:conc-name %appearance-ink-spec-))
  "Partial foreground-ink declaration."
  (foreground *appearance-unspecified* :read-only t))

(defstruct (appearance-surface-spec
            (:constructor %make-appearance-surface-spec
                (&key (background *appearance-unspecified*)))
            (:conc-name %appearance-surface-spec-))
  "Partial surface/background declaration."
  (background *appearance-unspecified* :read-only t))

(defstruct (appearance-decoration-spec
            (:constructor %make-appearance-decoration-spec
                (&key (kind *appearance-unspecified*)
                      (parameters *appearance-unspecified*)))
            (:conc-name %appearance-decoration-spec-))
  "Small tagged decoration policy; arbitrary drawing recipes are not accepted."
  (kind *appearance-unspecified* :read-only t)
  (parameters *appearance-unspecified* :read-only t))

(defstruct (appearance-role-style
            (:constructor %make-appearance-role-style
                (&key (typography *appearance-unspecified*)
                      (foreground-ink *appearance-unspecified*)
                      (surface *appearance-unspecified*)
                      (decoration *appearance-unspecified*)))
            (:conc-name %appearance-role-style-))
  "Partial, orthogonal appearance axes for one semantic role."
  (typography *appearance-unspecified* :read-only t)
  (foreground-ink *appearance-unspecified* :read-only t)
  (surface *appearance-unspecified* :read-only t)
  (decoration *appearance-unspecified* :read-only t))

(defstruct (appearance-role-definition
            (:constructor %make-appearance-role-definition
                (&key id kind documentation fallback-role
                      (supported-axes *appearance-unspecified*) owner))
            (:conc-name %appearance-role-definition-))
  "Immutable semantic role declaration owned by the catalog, not a frame."
  id
  kind
  documentation
  fallback-role
  (supported-axes *appearance-unspecified* :read-only t)
  owner)

(defstruct (appearance-theme-definition
            (:constructor %make-appearance-theme-definition
                (&key id documentation parent-theme role-overlays owner))
            (:conc-name %appearance-theme-definition-))
  "Immutable coordinated collection of partial role overlays."
  id
  documentation
  parent-theme
  (role-overlays nil :type list :read-only t)
  owner)

(defstruct (appearance-profile
            (:constructor %make-appearance-profile
                (&key (selected-theme :classic) (strict-contrast nil)
                      (role-overrides nil)))
            (:copier nil)
            (:conc-name %appearance-profile-))
  "Immutable frame-selectable profile; it contains no resolved CLIM objects."
  (selected-theme :classic :read-only t)
  (strict-contrast nil :read-only t)
  (role-overrides nil :type list :read-only t))

(defun validate-appearance-leaf (value axis &key allow-none-p)
  "Return VALUE when it is a permitted explicit leaf for AXIS."
  (validate-appearance-component value axis)
  (when (eq value :unspecified)
    (error-appearance-condition 'invalid-appearance-component :axis axis :value value))
  (when (and (eq value :none) (not allow-none-p))
    (error-appearance-condition 'invalid-appearance-component :axis axis :value value))
  value)

(defun make-appearance-typography-spec
    (&key (family *appearance-unspecified*) (face *appearance-unspecified*)
       (size *appearance-unspecified*))
  "Construct an immutable partial typography declaration."
  (%make-appearance-typography-spec
   :family (if (appearance-unspecified-p family)
               *appearance-unspecified*
               (copy-appearance-value
                (validate-appearance-leaf family :typography-family)))
   :face (if (appearance-unspecified-p face)
             *appearance-unspecified*
             (copy-appearance-value
              (validate-appearance-leaf face :typography-face)))
   :size (if (appearance-unspecified-p size)
             *appearance-unspecified*
             (copy-appearance-value
              (validate-appearance-leaf size :typography-size)))))

(defun appearance-typography-spec-family (spec)
  (copy-appearance-value (%appearance-typography-spec-family spec)))

(defun appearance-typography-spec-face (spec)
  (copy-appearance-value (%appearance-typography-spec-face spec)))

(defun appearance-typography-spec-size (spec)
  (copy-appearance-value (%appearance-typography-spec-size spec)))

(defun make-appearance-ink-spec (&key (foreground *appearance-unspecified*))
  "Construct an immutable partial foreground declaration."
  (%make-appearance-ink-spec
   :foreground (if (appearance-unspecified-p foreground)
                   *appearance-unspecified*
                   (copy-appearance-value
                    (validate-appearance-leaf foreground :foreground-ink)))))

(defun appearance-ink-spec-foreground (spec)
  (copy-appearance-value (%appearance-ink-spec-foreground spec)))

(defun make-appearance-surface-spec (&key (background *appearance-unspecified*))
  "Construct an immutable partial surface declaration."
  (%make-appearance-surface-spec
   :background (if (appearance-unspecified-p background)
                   *appearance-unspecified*
                   (copy-appearance-value
                    (validate-appearance-leaf background :surface)))))

(defun appearance-surface-spec-background (spec)
  (copy-appearance-value (%appearance-surface-spec-background spec)))

(defun make-appearance-decoration-spec
    (&key (kind *appearance-unspecified*) (parameters *appearance-unspecified*))
  "Construct a version-1 deterministic decoration declaration."
  (when (and (not (appearance-unspecified-p kind))
             (not (member kind '(:none :selection-marker) :test #'eq)))
    (error-appearance-condition 'invalid-appearance-component :axis :decoration :value kind))
  (when (and (not (appearance-unspecified-p parameters))
             (or (not (and (listp parameters)
                           (= (length parameters) 2)
                           (eq (first parameters) :marker)
                           (stringp (second parameters))))
                 (not (eq kind :selection-marker))))
    (error-appearance-condition 'invalid-appearance-component
                                :axis :decoration-parameters :value parameters))
  (%make-appearance-decoration-spec
   :kind (if (appearance-unspecified-p kind)
             *appearance-unspecified*
             (copy-appearance-value
              (validate-appearance-leaf kind :decoration :allow-none-p t)))
   :parameters (if (appearance-unspecified-p parameters)
                   *appearance-unspecified*
                   (list :marker (copy-seq (second parameters))))))

(defun appearance-decoration-spec-kind (spec)
  (copy-appearance-value (%appearance-decoration-spec-kind spec)))

(defun appearance-decoration-spec-parameters (spec)
  (copy-appearance-value (%appearance-decoration-spec-parameters spec)))

(defun validate-appearance-style-axis (value expected-type axis)
  "Validate VALUE for one role-style AXIS and return an immutable copy."
  (cond
    ((appearance-unspecified-p value) value)
    ((typep value expected-type) value)
    (t (error-appearance-condition 'invalid-appearance-component :axis axis :value value))))

(defun make-appearance-role-style
    (&key (typography *appearance-unspecified*)
       (foreground-ink *appearance-unspecified*)
       (surface *appearance-unspecified*)
       (decoration *appearance-unspecified*))
  "Construct an immutable orthogonal role style from typed axis specifications."
  (%make-appearance-role-style
   :typography (validate-appearance-style-axis typography
                                              'appearance-typography-spec
                                              :typography)
   :foreground-ink (validate-appearance-style-axis foreground-ink
                                                   'appearance-ink-spec
                                                   :foreground-ink)
   :surface (validate-appearance-style-axis surface 'appearance-surface-spec
                                             :surface)
   :decoration (validate-appearance-style-axis decoration
                                                'appearance-decoration-spec
                                                :decoration)))

(defun appearance-role-style-typography (style)
  (%appearance-role-style-typography style))

(defun appearance-role-style-foreground-ink (style)
  (%appearance-role-style-foreground-ink style))

(defun appearance-role-style-surface (style)
  (%appearance-role-style-surface style))

(defun appearance-role-style-decoration (style)
  (%appearance-role-style-decoration style))

(defun validate-appearance-role-kind (kind)
  "Return KIND if it is one of the fixed semantic role kinds."
  (unless (member kind +appearance-role-kinds+ :test #'eq)
    (error-appearance-condition 'invalid-appearance-component :axis :role-kind :value kind))
  kind)

(defun validate-appearance-supported-axes (kind supported-axes)
  "Validate an explicit supported-axis set for KIND and return a copy."
  (when (and (not (appearance-unspecified-p supported-axes))
             (or (not (listp supported-axes))
                 (not (every (lambda (axis)
                               (member axis (cdr (assoc kind
                                                        +appearance-role-axes+))
                                       :test #'eq))
                             supported-axes))
                 (/= (length supported-axes)
                     (length (remove-duplicates supported-axes :test #'eq)))))
    (error-appearance-condition 'invalid-appearance-component :axis :supported-axes
                                :value supported-axes))
  (copy-appearance-value supported-axes))

(defun make-appearance-role-definition
    (&key id kind documentation fallback-role
       (supported-axes *appearance-unspecified*) owner)
  "Construct an immutable semantic role declaration with validated kind/axes."
  (validate-appearance-component id :role-id)
  (validate-appearance-role-kind kind)
  (%make-appearance-role-definition
   :id (copy-appearance-value id)
   :kind kind
   :documentation (copy-appearance-value documentation)
   :fallback-role (copy-appearance-value fallback-role)
   :supported-axes (validate-appearance-supported-axes kind supported-axes)
   :owner (copy-appearance-value owner)))

(defun appearance-role-definition-id (definition)
  (copy-appearance-value (%appearance-role-definition-id definition)))

(defun appearance-role-definition-kind (definition)
  (%appearance-role-definition-kind definition))

(defun appearance-role-definition-documentation (definition)
  (copy-appearance-value (%appearance-role-definition-documentation definition)))

(defun appearance-role-definition-fallback-role (definition)
  (copy-appearance-value (%appearance-role-definition-fallback-role definition)))

(defun appearance-role-definition-supported-axes (definition)
  (copy-appearance-value (%appearance-role-definition-supported-axes definition)))

(defun appearance-role-definition-owner (definition)
  (copy-appearance-value (%appearance-role-definition-owner definition)))

(defun validate-appearance-overlays (overlays axis)
  "Validate a role-to-style overlay alist and return a deep immutable copy."
  (unless (listp overlays)
    (error-appearance-condition 'invalid-appearance-component :axis axis :value overlays))
  (let ((seen nil))
    (dolist (entry overlays)
      (unless (and (consp entry)
                   (not (null (car entry)))
                   (typep (cdr entry) 'appearance-role-style)
                   (not (member (car entry) seen :test #'equal)))
        (error-appearance-condition 'invalid-appearance-component :axis axis :value entry))
      (push (car entry) seen)))
  (copy-appearance-value overlays))

(defun make-appearance-theme-definition
    (&key id documentation parent-theme role-overlays owner)
  "Construct an immutable theme declaration without resolving its references."
  (validate-appearance-component id :theme-id)
  (%make-appearance-theme-definition
   :id (copy-appearance-value id)
   :documentation (copy-appearance-value documentation)
   :parent-theme (copy-appearance-value parent-theme)
   :role-overlays (validate-appearance-overlays role-overlays :role-overlays)
   :owner (copy-appearance-value owner)))

(defun appearance-theme-definition-id (definition)
  (copy-appearance-value (%appearance-theme-definition-id definition)))

(defun appearance-theme-definition-documentation (definition)
  (copy-appearance-value (%appearance-theme-definition-documentation definition)))

(defun appearance-theme-definition-parent-theme (definition)
  (copy-appearance-value (%appearance-theme-definition-parent-theme definition)))

(defun appearance-theme-definition-role-overlays (definition)
  (copy-appearance-value (%appearance-theme-definition-role-overlays definition)))

(defun appearance-theme-definition-owner (definition)
  (copy-appearance-value (%appearance-theme-definition-owner definition)))

(defun make-appearance-profile
    (&key (selected-theme :classic) (strict-contrast nil) (role-overrides nil))
  "Construct an immutable frame profile with an explicit contrast policy."
  (validate-appearance-component selected-theme :selected-theme)
  (unless (typep strict-contrast 'boolean)
    (error-appearance-condition 'invalid-appearance-component :axis :strict-contrast
                                :value strict-contrast))
  (%make-appearance-profile
   :selected-theme (copy-appearance-value selected-theme)
   :strict-contrast strict-contrast
   :role-overrides (validate-appearance-overlays role-overrides :role-overrides)))

(defun appearance-profile-selected-theme (profile)
  (copy-appearance-value (%appearance-profile-selected-theme profile)))

(defun appearance-profile-strict-contrast (profile)
  (%appearance-profile-strict-contrast profile))

(defun appearance-profile-role-overrides (profile)
  (copy-appearance-value (%appearance-profile-role-overrides profile)))

(defun appearance-role-supported-axes (role-definition)
  "Return ROLE-DEFINITION's accepted axes in stable order."
  (copy-list
   (or (unless (appearance-unspecified-p
                (appearance-role-definition-supported-axes role-definition))
         (appearance-role-definition-supported-axes role-definition))
       (cdr (assoc (appearance-role-definition-kind role-definition)
                   +appearance-role-axes+)))))

(defun appearance-role-supports-axis-p (role-definition axis)
  "Return true when AXIS is meaningful for ROLE-DEFINITION."
  (not (null (member axis (appearance-role-supported-axes role-definition)
                     :test #'eq))))

(defun validate-appearance-role-style (role-definition style &key origin)
  "Validate STYLE's axis applicability for ROLE-DEFINITION and return STYLE."
  (unless (typep role-definition 'appearance-role-definition)
    (error-appearance-condition 'invalid-appearance-component
                                :origin origin :axis :role-definition
                                :value role-definition))
  (unless (typep style 'appearance-role-style)
    (error-appearance-condition 'invalid-appearance-component
                                :origin origin :axis :role-style :value style))
  (dolist (axis '((:typography . appearance-role-style-typography)
                  (:foreground-ink . appearance-role-style-foreground-ink)
                  (:surface . appearance-role-style-surface)
                  (:decoration . appearance-role-style-decoration)))
    (unless (appearance-unspecified-p
             (funcall (cdr axis) style))
      (unless (appearance-role-supports-axis-p role-definition (car axis))
        (error-appearance-condition 'unsupported-role-axis
                                    :origin origin
                                    :role (appearance-role-definition-id role-definition)
                                    :axis (car axis)
                                    :value (funcall (cdr axis) style)))))
  style)

;;;; Pure catalogs and role cascade

(defstruct (appearance-catalog
            (:constructor %make-appearance-catalog
                (&key role-definitions theme-definitions built-in-overlays generation))
            (:conc-name %appearance-catalog-))
  "Immutable declarations available to a resolver; it contains no active frame."
  (role-definitions nil :type list :read-only t)
  (theme-definitions nil :type list :read-only t)
  (built-in-overlays nil :type list :read-only t)
  (generation 0 :type (integer 0 *) :read-only t))

(defstruct (appearance-diagnostic
            (:constructor %make-appearance-diagnostic
                (&key catalog-generation unknown-role condition))
            (:conc-name %appearance-diagnostic-))
  "Pure runtime diagnostic event, keyed for frame-level deduplication."
  (catalog-generation 0 :type (integer 0 *) :read-only t)
  (unknown-role nil :read-only t)
  (condition nil :type appearance-condition :read-only t))

(defstruct (resolved-appearance-role
            (:constructor %make-resolved-appearance-role
                (&key style provenance structural-key diagnostics))
            (:conc-name %resolved-appearance-role-))
  "Pure result of resolving one surface/content/state role stack."
  (style nil :read-only t)
  (provenance nil :type list :read-only t)
  (structural-key nil :read-only t)
  (diagnostics nil :type list :read-only t))

(defun validate-appearance-definitions (definitions expected-type axis)
  "Validate a declaration list with unique IDs and return a copied list."
  (unless (and (listp definitions)
               (every (lambda (definition) (typep definition expected-type))
                      definitions))
    (error-appearance-condition 'invalid-appearance-component :axis axis :value definitions))
  (let ((seen nil))
    (dolist (definition definitions)
      (let ((id (if (eq expected-type 'appearance-role-definition)
                    (appearance-role-definition-id definition)
                    (appearance-theme-definition-id definition))))
        (when (member id seen :test #'equal)
          (error-appearance-condition 'invalid-appearance-component :axis axis :value id))
        (push id seen))))
  (copy-list definitions))

(defun validate-appearance-role-topology (role-definitions)
  "Reject missing, cross-kind, and cyclic fallback graphs before publication."
  (labels ((find-role (id)
             (find id role-definitions :test #'equal
                   :key #'appearance-role-definition-id)))
    (dolist (role role-definitions)
      (let ((seen nil)
            (current role))
        (loop
          (let ((id (appearance-role-definition-id current)))
            (when (member id seen :test #'equal)
              (error-appearance-condition 'appearance-role-cycle :role id :path (reverse seen)))
            (push id seen))
          (let ((fallback (appearance-role-definition-fallback-role current)))
            (unless fallback
              (return))
            (let ((fallback-definition (find-role fallback)))
              (unless fallback-definition
                (error-appearance-condition 'missing-appearance-parent
                                            :role (appearance-role-definition-id current)
                                            :axis :fallback-role :value fallback))
              (unless (eq (appearance-role-definition-kind current)
                          (appearance-role-definition-kind fallback-definition))
                (error-appearance-condition 'invalid-appearance-fallback
                                            :role (appearance-role-definition-id current)
                                            :axis :fallback-role :value fallback))
              (setf current fallback-definition))))))))

(defun validate-appearance-theme-topology (theme-definitions)
  "Reject missing and cyclic parent graphs before a catalog is published."
  (labels ((find-theme (id)
             (find id theme-definitions :test #'equal
                   :key #'appearance-theme-definition-id)))
    (dolist (theme theme-definitions)
      (let ((seen nil)
            (current theme))
        (loop
          (let ((id (appearance-theme-definition-id current)))
            (when (member id seen :test #'equal)
              (error-appearance-condition 'appearance-theme-cycle :value id :path (reverse seen)))
            (push id seen))
          (let ((parent (appearance-theme-definition-parent-theme current)))
            (unless parent
              (return))
            (let ((parent-definition (find-theme parent)))
              (unless parent-definition
                (error-appearance-condition 'missing-appearance-parent
                                            :axis :parent-theme :value parent))
              (setf current parent-definition))))))))

(defun make-appearance-catalog
    (&key (role-definitions nil) (theme-definitions nil) (built-in-overlays nil)
       (generation 0))
  "Construct an immutable catalog without resolving themes or roles."
  (unless (and (integerp generation) (not (minusp generation)))
    (error-appearance-condition 'invalid-appearance-component
                                :axis :catalog-generation :value generation))
  (let ((roles (validate-appearance-definitions
                role-definitions 'appearance-role-definition :role-definitions))
        (themes (validate-appearance-definitions
                 theme-definitions 'appearance-theme-definition :theme-definitions)))
    (validate-appearance-role-topology roles)
    (validate-appearance-theme-topology themes)
    (%make-appearance-catalog
     :role-definitions roles
     :theme-definitions themes
     :generation generation
     :built-in-overlays (validate-appearance-overlays
                         built-in-overlays :built-in-overlays))))

(defun appearance-catalog-role-definitions (catalog)
  (copy-list (%appearance-catalog-role-definitions catalog)))

(defun appearance-catalog-theme-definitions (catalog)
  (copy-list (%appearance-catalog-theme-definitions catalog)))

(defun appearance-catalog-built-in-overlays (catalog)
  (copy-appearance-value (%appearance-catalog-built-in-overlays catalog)))

(defun appearance-catalog-generation (catalog)
  "Return CATALOG's immutable generation used in runtime diagnostic keys."
  (%appearance-catalog-generation catalog))

(defun resolved-appearance-role-style (resolved-role)
  (%resolved-appearance-role-style resolved-role))

(defun resolved-appearance-role-provenance (resolved-role)
  (copy-appearance-value (%resolved-appearance-role-provenance resolved-role)))

(defun resolved-appearance-role-structural-key (resolved-role)
  (copy-appearance-value (%resolved-appearance-role-structural-key resolved-role)))

(defun resolved-appearance-role-diagnostics (resolved-role)
  (copy-list (%resolved-appearance-role-diagnostics resolved-role)))

(defun appearance-diagnostic-catalog-generation (diagnostic)
  (%appearance-diagnostic-catalog-generation diagnostic))

(defun appearance-diagnostic-unknown-role (diagnostic)
  (copy-appearance-value (%appearance-diagnostic-unknown-role diagnostic)))

(defun appearance-diagnostic-condition (diagnostic)
  (%appearance-diagnostic-condition diagnostic))

(defun appearance-diagnostic-deduplication-key (diagnostic)
  "Return the stable `(catalog-generation unknown-role)' key for DIAGNOSTIC."
  (list (appearance-diagnostic-catalog-generation diagnostic)
        (appearance-diagnostic-unknown-role diagnostic)))

(defun find-appearance-role-definition (catalog id)
  "Return the catalog role with ID, or NIL when it is not declared."
  (find id (%appearance-catalog-role-definitions catalog) :test #'equal
        :key #'appearance-role-definition-id))

(defun find-appearance-theme-definition (catalog id)
  "Return the catalog theme with ID, or NIL when it is not declared."
  (find id (%appearance-catalog-theme-definitions catalog) :test #'equal
        :key #'appearance-theme-definition-id))

(defun require-appearance-role-definition (catalog id origin)
  "Return a declared role or signal a configuration/declaration error."
  (or (find-appearance-role-definition catalog id)
      (error-appearance-condition 'unknown-appearance-role
                                  :origin origin :role id :value id)))

(defun require-appearance-theme-definition (catalog id origin)
  "Return a declared theme or signal a configuration/declaration error."
  (or (find-appearance-theme-definition catalog id)
      (error-appearance-condition 'missing-appearance-parent
                                  :origin origin :value id)))

(defun appearance-role-fallback-chain (catalog role-id &key (origin :catalog))
  "Return ROLE-ID's fallback chain from root to requested role, rejecting cycles."
  (let ((seen nil)
        (chain nil)
        (current role-id))
    (loop
      (when (member current seen :test #'equal)
        (error-appearance-condition 'appearance-role-cycle
                                    :origin origin :role role-id
                                    :path (nreverse (cons current seen))))
      (let ((definition (require-appearance-role-definition catalog current origin)))
        (push current seen)
        (push definition chain)
        (let ((fallback (appearance-role-definition-fallback-role definition)))
          (if fallback
              (setf current fallback)
              (return chain)))))))

(defun appearance-theme-parent-chain (catalog theme-id &key (origin :catalog))
  "Return THEME-ID's parent chain from root to selected theme, rejecting cycles."
  (let ((seen nil)
        (chain nil)
        (current theme-id))
    (loop
      (when (member current seen :test #'equal)
        (error-appearance-condition 'appearance-theme-cycle
                                    :origin origin :value theme-id
                                    :path (nreverse (cons current seen))))
      (let ((definition (require-appearance-theme-definition catalog current origin)))
        (push current seen)
        (push definition chain)
        (let ((parent (appearance-theme-definition-parent-theme definition)))
          (if parent
              (setf current parent)
              (return chain)))))))

(defun appearance-logical-size-index (size)
  "Return SIZE's logical ladder index, or NIL when it is not logical."
  (position size +appearance-logical-sizes+ :test #'eq))

(defun resolve-appearance-relative-size (base-size relative-size origin)
  "Consume RELATIVE-SIZE once against BASE-SIZE and clamp to the logical ladder."
  (let ((index (appearance-logical-size-index base-size)))
    (unless index
      (error-appearance-condition 'relative-size-base-invalid
                                  :origin origin :axis :typography-size
                                  :value relative-size))
    (nth (max 0 (min (1- (length +appearance-logical-sizes+))
                     (+ index (ecase relative-size
                                (:smaller -1)
                                (:larger 1)))))
         +appearance-logical-sizes+)))

(defun merge-appearance-typography (base overlay origin)
  "Overlay typed typography while consuming a relative size at this layer."
  (let* ((base (if (or (null base) (appearance-unspecified-p base))
                   (make-appearance-typography-spec)
                   base))
         (overlay (if (or (null overlay) (appearance-unspecified-p overlay))
                      (make-appearance-typography-spec)
                      overlay))
         (family (appearance-typography-spec-family base))
         (face (appearance-typography-spec-face base))
         (size (appearance-typography-spec-size base))
         (new-family (appearance-typography-spec-family overlay))
         (new-face (appearance-typography-spec-face overlay))
         (new-size (appearance-typography-spec-size overlay)))
    (unless (appearance-unspecified-p new-family)
      (setf family new-family))
    (unless (appearance-unspecified-p new-face)
      (setf face new-face))
    (unless (appearance-unspecified-p new-size)
      (setf size (if (member new-size +appearance-relative-sizes+ :test #'eq)
                     (resolve-appearance-relative-size size new-size origin)
                     new-size)))
    (make-appearance-typography-spec :family family :face face :size size)))

(defun merge-appearance-ink (base overlay)
  "Overlay a foreground ink component without inventing a value."
  (let ((base (unless (appearance-unspecified-p base) base))
        (overlay (unless (appearance-unspecified-p overlay) overlay)))
    (let ((value (if (and overlay
                        (not (appearance-unspecified-p
                              (appearance-ink-spec-foreground overlay))))
                   (appearance-ink-spec-foreground overlay)
                   (if base
                       (appearance-ink-spec-foreground base)
                       *appearance-unspecified*))))
      (make-appearance-ink-spec :foreground value))))

(defun merge-appearance-surface (base overlay)
  "Overlay a surface component without inventing a value."
  (let ((base (unless (appearance-unspecified-p base) base))
        (overlay (unless (appearance-unspecified-p overlay) overlay)))
    (let ((value (if (and overlay
                        (not (appearance-unspecified-p
                              (appearance-surface-spec-background overlay))))
                   (appearance-surface-spec-background overlay)
                   (if base
                       (appearance-surface-spec-background base)
                       *appearance-unspecified*))))
      (make-appearance-surface-spec :background value))))

(defun merge-appearance-decoration (base overlay)
  "Overlay decoration kind and parameters independently."
  (let ((base (unless (appearance-unspecified-p base) base))
        (overlay (unless (appearance-unspecified-p overlay) overlay)))
    (let ((kind (if (and overlay
                       (not (appearance-unspecified-p
                             (appearance-decoration-spec-kind overlay))))
                  (appearance-decoration-spec-kind overlay)
                  (if base
                      (appearance-decoration-spec-kind base)
                      *appearance-unspecified*)))
        (parameters (if (and overlay
                             (not (appearance-unspecified-p
                                   (appearance-decoration-spec-parameters overlay))))
                        (appearance-decoration-spec-parameters overlay)
                        (if base
                            (appearance-decoration-spec-parameters base)
                            *appearance-unspecified*))))
      (make-appearance-decoration-spec :kind kind :parameters parameters))))

(defun appearance-style-axis-specified-p (style axis)
  "Return whether STYLE explicitly supplies AXIS."
  (ecase axis
    (:typography
     (let ((typography (appearance-role-style-typography style)))
       (and (not (appearance-unspecified-p typography))
            (or (not (appearance-unspecified-p
                      (appearance-typography-spec-family typography)))
                (not (appearance-unspecified-p
                      (appearance-typography-spec-face typography)))
                (not (appearance-unspecified-p
                      (appearance-typography-spec-size typography)))))))
    (:foreground-ink
     (let ((ink (appearance-role-style-foreground-ink style)))
       (and (not (appearance-unspecified-p ink))
            (not (appearance-unspecified-p
                  (appearance-ink-spec-foreground ink))))))
    (:surface
     (let ((surface (appearance-role-style-surface style)))
       (and (not (appearance-unspecified-p surface))
            (not (appearance-unspecified-p
                  (appearance-surface-spec-background surface))))))
    (:decoration
     (let ((decoration (appearance-role-style-decoration style)))
       (and (not (appearance-unspecified-p decoration))
            (or (not (appearance-unspecified-p
                      (appearance-decoration-spec-kind decoration)))
                (not (appearance-unspecified-p
                      (appearance-decoration-spec-parameters decoration)))))))))

(defun merge-appearance-role-style (base overlay origin)
  "Component-wise merge of BASE and OVERLAY role styles."
  (let ((base (or base (make-appearance-role-style)))
        (overlay (or overlay (make-appearance-role-style))))
    (make-appearance-role-style
     :typography (merge-appearance-typography
                  (appearance-role-style-typography base)
                  (appearance-role-style-typography overlay) origin)
     :foreground-ink (merge-appearance-ink
                      (appearance-role-style-foreground-ink base)
                      (appearance-role-style-foreground-ink overlay))
     :surface (merge-appearance-surface
               (appearance-role-style-surface base)
               (appearance-role-style-surface overlay))
     :decoration (merge-appearance-decoration
                  (appearance-role-style-decoration base)
                  (appearance-role-style-decoration overlay)))))

(defun find-appearance-overlay (overlays role-id)
  "Return ROLE-ID's partial style from OVERLAYS, or NIL when omitted."
  (cdr (assoc role-id overlays :test #'equal)))

(defun validate-appearance-overlays-for-catalog (catalog overlays origin)
  "Reject unknown/configuration overlays before resolving any of their effects."
  (dolist (entry overlays)
    (let ((definition (require-appearance-role-definition catalog (car entry)
                                                          origin)))
      (validate-appearance-role-style definition (cdr entry) :origin origin)))
  overlays)

(defun appearance-structural-key-value (value)
  "Canonicalize internal inheritance state out of public structural keys."
  (if (appearance-unspecified-p value)
      :unspecified
      (copy-appearance-value value)))

(defun appearance-role-style-key (style)
  "Return STYLE's structural output key without catalog/profile revisions."
  (let ((typography (appearance-role-style-typography style))
        (foreground-ink (appearance-role-style-foreground-ink style))
        (surface (appearance-role-style-surface style))
        (decoration (appearance-role-style-decoration style)))
    (list :typography
          (if (appearance-unspecified-p typography)
              :unspecified
              (list (appearance-structural-key-value
                     (appearance-typography-spec-family typography))
                    (appearance-structural-key-value
                     (appearance-typography-spec-face typography))
                    (appearance-structural-key-value
                     (appearance-typography-spec-size typography))))
          :foreground-ink
          (if (appearance-unspecified-p foreground-ink)
              :unspecified
              (appearance-structural-key-value
               (appearance-ink-spec-foreground foreground-ink)))
          :surface
          (if (appearance-unspecified-p surface)
              :unspecified
              (appearance-structural-key-value
               (appearance-surface-spec-background surface)))
          :decoration
          (if (appearance-unspecified-p decoration)
              :unspecified
              (list (appearance-structural-key-value
                     (appearance-decoration-spec-kind decoration))
                    (appearance-structural-key-value
                     (appearance-decoration-spec-parameters decoration)))))))

(defun merge-provenance (provenance style origin)
  "Record ORIGIN for each explicitly supplied axis, preserving other origins."
  (let ((result (copy-appearance-value provenance)))
    (dolist (axis '(:typography :foreground-ink :surface :decoration))
      (when (appearance-style-axis-specified-p style axis)
        (setf result (acons axis (copy-appearance-value origin)
                            (remove axis result :key #'car :test #'eq)))))
    result))

(defun appearance-declaration-origin (kind id owner)
  "Return copied structured provenance for one declaration contribution."
  (list kind (copy-appearance-value id) :owner (copy-appearance-value owner)))

(defun merge-resolved-provenance (provenance style overlay-provenance)
  "Merge only STYLE's effective axes from OVERLAY-PROVENANCE into PROVENANCE."
  (let ((result (copy-appearance-value provenance)))
    (dolist (axis '(:typography :foreground-ink :surface :decoration))
      (when (appearance-style-axis-specified-p style axis)
        (let ((origin (cdr (assoc axis overlay-provenance :test #'eq))))
          (when origin
            (setf result
                  (acons axis (copy-appearance-value origin)
                         (remove axis result :key #'car :test #'eq)))))))
    result))

(defun resolve-appearance-role (catalog theme-id role-id
                                &key (file-overrides nil) (init-overrides nil)
                                  (environment-overrides nil)
                                  (command-line-overrides nil)
                                  (unsaved-overrides nil))
  "Resolve one declared role through all version-1 source layers.

Unknown ROLE-ID is a configuration/declaration error. Runtime display callers
use RESOLVE-RUNTIME-APPEARANCE-ROLE-STACK for the documented fallback behavior."
  (let ((style (merge-appearance-role-style nil nil :initial))
        (provenance nil)
        (chain (appearance-role-fallback-chain catalog role-id :origin :catalog)))
    (labels ((apply-style (definition overlay origin)
               (when overlay
                 (validate-appearance-role-style definition overlay :origin origin)
                 (setf style (merge-appearance-role-style style overlay origin)
                       provenance (merge-provenance provenance overlay origin)))))
      ;; Role definitions supply fallback topology. Default overlays belong to
      ;; the declaration which owns their role; this lets later package-owned
      ;; defaults remain at the existing lowest-precedence layer.
      (validate-appearance-overlays-for-catalog
       catalog (appearance-catalog-built-in-overlays catalog) :built-in)
      (dolist (definition chain)
        (apply-style definition
                     (find-appearance-overlay
                      (appearance-catalog-built-in-overlays catalog)
                      (appearance-role-definition-id definition))
                     (appearance-declaration-origin
                      :role-default
                      (appearance-role-definition-id definition)
                      (appearance-role-definition-owner definition))))
      (dolist (theme (appearance-theme-parent-chain catalog theme-id
                                                    :origin :theme))
        (let ((origin (appearance-declaration-origin
                       :theme
                       (appearance-theme-definition-id theme)
                       (appearance-theme-definition-owner theme))))
          (validate-appearance-overlays-for-catalog
           catalog (appearance-theme-definition-role-overlays theme) origin)
          (dolist (definition chain)
            (apply-style definition
                         (find-appearance-overlay
                          (appearance-theme-definition-role-overlays theme)
                          (appearance-role-definition-id definition))
                         origin))))
      (dolist (layer (list (cons :appearance-file file-overrides)
                           (cons :init init-overrides)
                           (cons :environment environment-overrides)
                           (cons :command-line command-line-overrides)
                           (cons :unsaved unsaved-overrides)))
        (validate-appearance-overlays-for-catalog catalog (cdr layer)
                                                 (car layer))
        (dolist (definition chain)
          (apply-style definition
                       (find-appearance-overlay (cdr layer)
                                                (appearance-role-definition-id
                                                 definition))
                       (car layer))))
      (%make-resolved-appearance-role
       :style style
       :provenance provenance
       :structural-key (appearance-role-style-key style)
       :diagnostics nil))))

(defun resolve-appearance-role-stack (catalog theme-id role-ids &rest keys)
  "Resolve surface, content, and state ROLE-IDS into one pure result.

This is the strict configuration/declaration entry point: every supplied role
must exist.  A stack has at most one surface and content role, then may layer
state roles in their supplied order."
  (let ((by-kind (make-hash-table :test #'eq))
        (style (merge-appearance-role-style nil nil :initial))
        (provenance nil))
    (dolist (role-id role-ids)
      (let* ((definition (require-appearance-role-definition catalog role-id
                                                              :configuration))
             (kind (appearance-role-definition-kind definition)))
        (when (and (not (eq kind :state)) (gethash kind by-kind))
          (error-appearance-condition 'invalid-appearance-component
                                      :axis :role-stack :value role-ids))
        (push role-id (gethash kind by-kind))))
    (dolist (kind '(:surface :content :state))
      (dolist (role-id (nreverse (gethash kind by-kind)))
        (when role-id
          (let ((resolved (apply #'resolve-appearance-role catalog theme-id
                                 role-id keys)))
            (setf style (merge-appearance-role-style
                         style (resolved-appearance-role-style resolved)
                         (list :stack kind))
                  provenance
                  (merge-resolved-provenance
                   provenance
                   (resolved-appearance-role-style resolved)
                   (resolved-appearance-role-provenance resolved)))))))
    (%make-resolved-appearance-role
     :style style
     :provenance provenance
     :structural-key (appearance-role-style-key style)
     :diagnostics nil)))

(defun resolve-runtime-appearance-role-stack (catalog theme-id role-ids &rest keys)
  "Resolve a display stack, mapping unknown runtime roles to :DEFAULT-TEXT.

Returns the resolved role and at most sixteen pure diagnostic events.  Each
event has a stable `(catalog-generation unknown-role)' deduplication key;
frame code owns any longer-lived logging policy.  Configuration parsers must
use RESOLVE-APPEARANCE-ROLE-STACK instead, so unknown stored IDs fail."
  (let ((unknown-role-ids nil)
        (surface-role nil)
        (content-role nil)
        (state-roles nil))
    (dolist (role-id role-ids)
      (let ((known (find-appearance-role-definition catalog role-id)))
        (if known
            (let ((kind (appearance-role-definition-kind known)))
              ;; A valid surface/content role supersedes a previous unknown
              ;; fallback.  State roles intentionally compose in input order.
              (ecase kind
                (:surface
                 (setf surface-role (appearance-role-definition-id known)))
                (:content
                 (setf content-role (appearance-role-definition-id known)))
                (:state
                 (push (appearance-role-definition-id known) state-roles))))
            (progn
              (push role-id unknown-role-ids)
              (unless content-role
                (setf content-role :default-text))))))
    (let* ((known-roles (append (when surface-role (list surface-role))
                                (when content-role (list content-role))
                                (nreverse state-roles)))
           (resolved (apply #'resolve-appearance-role-stack catalog theme-id
                            known-roles keys)))
      (%make-resolved-appearance-role
       :style (resolved-appearance-role-style resolved)
       :provenance (resolved-appearance-role-provenance resolved)
       :structural-key (resolved-appearance-role-structural-key resolved)
       :diagnostics
       (loop with seen = nil
             for unknown-role in (nreverse unknown-role-ids)
             unless (member unknown-role seen :test #'equal)
               do (push unknown-role seen)
               and collect
               (%make-appearance-diagnostic
                :catalog-generation (appearance-catalog-generation catalog)
                :unknown-role (copy-appearance-value unknown-role)
                :condition
                (make-appearance-condition 'unknown-appearance-role
                                           :origin :runtime
                                           :role unknown-role
                                           :value unknown-role
                                           :fatal-p nil))
             into diagnostics
             when (= (length diagnostics) +appearance-runtime-diagnostic-limit+)
               do (return diagnostics)
             finally (return diagnostics))))))

(defun appearance-rgb-components (ink)
  "Return INK's three opaque RGB components, or NIL for an unsupported ink."
  (cond ((and (listp ink)
              (= (length ink) 4)
              (eq (first ink) :rgb)
              (every (lambda (component)
                       (and (realp component)
                            (<= 0 component 1)))
                     (rest ink)))
         (rest ink))
        ((and (listp ink)
              (= (length ink) 3)
              (every (lambda (component)
                       (and (realp component)
                            (<= 0 component 1)))
                     ink))
         ink)
        ((assoc ink '((:black . (0 0 0))
                      (:white . (1 1 1))
                      (:red . (1 0 0))
                      (:green . (0 1 0))
                      (:blue . (0 0 1))
                      (:cyan . (0 1 1))
                      (:magenta . (1 0 1))
                      (:yellow . (1 1 0))
                      (:gray . (1/2 1/2 1/2)))
                :test #'eq)
         (copy-list (cdr (assoc ink '((:black . (0 0 0))
                                      (:white . (1 1 1))
                                      (:red . (1 0 0))
                                      (:green . (0 1 0))
                                      (:blue . (0 0 1))
                                      (:cyan . (0 1 1))
                                      (:magenta . (1 0 1))
                                      (:yellow . (1 1 0))
                                      (:gray . (1/2 1/2 1/2)))
                                :test #'eq))))))

(defun appearance-srgb-linear-component (component)
  "Convert one sRGB COMPONENT to its WCAG relative-luminance value."
  (let ((component (coerce component 'double-float)))
    (if (<= component 0.04045d0)
        (/ component 12.92d0)
        (expt (/ (+ component 0.055d0) 1.055d0) 2.4d0))))

(defun appearance-relative-luminance (rgb)
  "Return the WCAG relative luminance of the three-component RGB value."
  (destructuring-bind (red green blue) rgb
    (+ (* 0.2126d0 (appearance-srgb-linear-component red))
       (* 0.7152d0 (appearance-srgb-linear-component green))
       (* 0.0722d0 (appearance-srgb-linear-component blue)))))

(defun appearance-contrast-ratio (foreground background)
  "Return the WCAG contrast ratio for two opaque RGB inks.

The function returns NIL when either ink is outside version 1's opaque RGB or
standard-solid-ink subset; it never changes either color."
  (let ((foreground-rgb (appearance-rgb-components foreground))
        (background-rgb (appearance-rgb-components background)))
    (when (and foreground-rgb background-rgb)
      (let ((foreground-luminance (appearance-relative-luminance foreground-rgb))
            (background-luminance (appearance-relative-luminance background-rgb)))
        (/ (+ (max foreground-luminance background-luminance) 0.05d0)
           (+ (min foreground-luminance background-luminance) 0.05d0))))))

(defun appearance-built-in-contrast-provenance-p (provenance)
  "Return true when every effective contribution is explicitly built-in-owned."
  (every (lambda (entry)
           (let ((origin (cdr entry)))
             (and (consp origin)
                  (member (first origin) '(:role-default :theme) :test #'eq)
                  (eq (getf (cddr origin) :owner) :builtin))))
         provenance))

(defun signal-appearance-contrast-violation
    (role-stack ratio provenance fatal-p)
  "Signal the contract payload for one below-threshold resolved stack."
  (let ((initargs (list :origin provenance
                        :role role-stack
                        :axis :contrast
                        :value ratio
                        :path role-stack
                        :port nil
                        :available-choices nil
                        :fatal-p fatal-p
                        :suggested-repairs
                        '(:increase-foreground-contrast :change-surface))))
    (if fatal-p
        (apply #'error-appearance-condition 'appearance-contrast-warning initargs)
        (apply #'warn-appearance-condition 'appearance-contrast-warning initargs))))

(defun validate-appearance-profile-contrast
    (catalog profile &key (file-overrides nil) (init-overrides nil)
       (environment-overrides nil) (command-line-overrides nil)
       (unsaved-overrides nil) (role-stacks +dark-appearance-text-stacks+))
  "Validate every contrast-applicable text stack in PROFILE without recoloring it.

Untouched built-in failures are fatal.  A user, file, init, environment,
command-line, unsaved, or package-touched failure warns by default and is
fatal when PROFILE requests strict contrast.  The function returns true when
no fatal violation was signaled.  Untouched classic stacks remain inert because
their backend surface is deliberately unspecified; customized classic stacks
are validated."
  (let ((theme-id (appearance-profile-selected-theme profile))
        (profile-overrides (appearance-profile-role-overrides profile)))
    (dolist (role-stack role-stacks t)
      (let* ((resolved
               (resolve-appearance-role-stack
                catalog theme-id role-stack
                :file-overrides file-overrides
                :init-overrides init-overrides
                :environment-overrides environment-overrides
                :command-line-overrides command-line-overrides
                :unsaved-overrides
                (append profile-overrides unsaved-overrides)))
             (style (resolved-appearance-role-style resolved))
             ;; A partially customized :CLASSIC stack may still inherit an
             ;; intentionally unspecified backend surface or foreground.
             ;; Preserve that as an unavailable ratio rather than trying to
             ;; call a specification accessor on the inheritance sentinel.
             (foreground-spec (appearance-role-style-foreground-ink style))
             (background-spec (appearance-role-style-surface style))
             (foreground (unless (appearance-unspecified-p foreground-spec)
                           (appearance-ink-spec-foreground foreground-spec)))
             (background (unless (appearance-unspecified-p background-spec)
                           (appearance-surface-spec-background background-spec)))
             (ratio (appearance-contrast-ratio foreground background))
             (provenance (resolved-appearance-role-provenance resolved))
             (built-in-p
               (appearance-built-in-contrast-provenance-p provenance)))
        ;; Classic's backend surface is intentionally unspecified.  It is
        ;; inert only while every effective contribution remains built-in;
        ;; any custom contribution makes the stack contrast-applicable.
        (when (or (not (eq theme-id :classic)) (not built-in-p))
          (unless (and ratio (>= ratio +appearance-minimum-text-contrast+))
            (signal-appearance-contrast-violation
             role-stack ratio provenance
             (or built-in-p
                 (appearance-profile-strict-contrast profile)))))))))

(defun make-classic-appearance-catalog ()
  "Return the version-1 :CLASSIC declarations and exact current output goldens."
  (labels ((role (id kind &optional fallback)
             (make-appearance-role-definition :id id :kind kind
                                               :fallback-role fallback
                                               :owner :builtin))
           (foreground (value &key face marker)
             (let ((arguments
                     (list :foreground-ink
                           (make-appearance-ink-spec :foreground value))))
               (when face
                 (setf arguments
                       (append arguments
                               (list :typography
                                     (make-appearance-typography-spec
                                      :face face)))))
               (when marker
                 (setf arguments
                       (append arguments
                               (list :decoration
                                     (make-appearance-decoration-spec
                                      :kind :selection-marker
                                      :parameters (list :marker marker))))))
               (apply #'make-appearance-role-style arguments)))
           (surface (value)
             (make-appearance-role-style
              :surface (make-appearance-surface-spec :background value))))
    (make-appearance-catalog
     :role-definitions
     (list
      (role :base :surface)
      (role :transcript-pane :surface :base)
      (role :info-pane :surface :base)
      (role :compose-pane :surface :base)
      (role :minibuffer-pane :surface :base)
      (role :help-pane :surface :base)
      ;; Pointer documentation is deliberately outside the dark surface
      ;; cascade: version 1 leaves the pinned McCLIM implementation unthemed.
      (role :pointer-documentation :surface)
      (role :default-text :content)
      (role :transcript-user :content :default-text)
      (role :transcript-agent :content :default-text)
      (role :transcript-tool :content :default-text)
      (role :transcript-system :content :default-text)
      (role :transcript-empty :content :default-text)
      (role :system :content :default-text)
      (role :error :content :default-text)
      (role :tool-result :content :default-text)
      (role :modeline :content :default-text)
      (role :selector-title :content :default-text)
      (role :selector-header :content :default-text)
      (role :selector-entry :content :default-text)
      (role :selector-separator :content :default-text)
      (role :selector-footer :content :default-text)
      (role :selector-selection :state)
      (role :minibuffer-selection-emphasis :state)
      (role :disabled :state))
     :theme-definitions
     (list (make-appearance-theme-definition :id :classic :owner :builtin)
           (make-appearance-theme-definition
            :id :dark
            :parent-theme :classic
            :owner :builtin
            :role-overlays
            (list
             (cons :base (surface '(:rgb 13/255 17/255 23/255)))
             (cons :transcript-pane (surface '(:rgb 13/255 17/255 23/255)))
             (cons :compose-pane (surface '(:rgb 13/255 17/255 23/255)))
             (cons :help-pane (surface '(:rgb 13/255 17/255 23/255)))
             (cons :info-pane (surface '(:rgb 22/255 27/255 34/255)))
             (cons :minibuffer-pane (surface '(:rgb 22/255 27/255 34/255)))
             (cons :default-text (foreground '(:rgb 230/255 237/255 243/255)))
             (cons :transcript-agent (foreground '(:rgb 230/255 237/255 243/255)))
             (cons :modeline (foreground '(:rgb 230/255 237/255 243/255)))
             (cons :selector-entry (foreground '(:rgb 230/255 237/255 243/255)))
             (cons :transcript-user (foreground '(:rgb 121/255 192/255 255/255)))
             (cons :transcript-tool (foreground '(:rgb 126/255 231/255 135/255)))
             (cons :tool-result (foreground '(:rgb 126/255 231/255 135/255)))
             (cons :selector-header (foreground '(:rgb 126/255 231/255 135/255)))
             (cons :transcript-system (foreground '(:rgb 177/255 186/255 196/255)))
             (cons :system (foreground '(:rgb 177/255 186/255 196/255)))
             (cons :transcript-empty (foreground '(:rgb 139/255 148/255 158/255)))
             (cons :selector-separator (foreground '(:rgb 139/255 148/255 158/255)))
             (cons :selector-footer (foreground '(:rgb 139/255 148/255 158/255)))
             (cons :error (foreground '(:rgb 255/255 123/255 114/255)))
             (cons :selector-title (foreground '(:rgb 165/255 214/255 255/255)))
             ;; Typography and marker declarations remain those of :classic.
             (cons :selector-selection (foreground '(:rgb 255/255 255/255 255/255)))
             (cons :disabled (foreground '(:rgb 139/255 148/255 158/255))))))
     :built-in-overlays
     (list
      (cons :transcript-user (foreground '(0.10 0.25 0.55)))
      (cons :transcript-agent (foreground '(0.10 0.10 0.10)))
      (cons :transcript-tool (foreground '(0.12 0.34 0.18)))
      (cons :tool-result (foreground '(0.12 0.34 0.18)))
      (cons :transcript-system (foreground '(0.36 0.36 0.36)))
      (cons :transcript-empty (foreground '(0.45 0.45 0.45)))
      (cons :selector-title (foreground '(0.16 0.22 0.45) :face :bold))
      (cons :selector-header (foreground '(0.18 0.36 0.20)))
      (cons :selector-footer (foreground '(0.35 0.35 0.35)))
      (cons :selector-selection
            (foreground '(0.10 0.38 0.65) :marker ">"))
      (cons :minibuffer-selection-emphasis
            (make-appearance-role-style
             :typography (make-appearance-typography-spec :face :bold)))
      (cons :selector-entry (foreground '(0.20 0.20 0.20)))
      (cons :system (foreground '(0.45 0.45 0.45)))
      (cons :disabled (foreground '(0.45 0.45 0.45)))
      (cons :error (foreground '(0.60 0.12 0.12)))))))

;;;; Candidate activation planning
;;;;
;;;; These objects deliberately remain independent of application frames and
;;;; CLIM panes.  A frame event later decides whether a prepared plan is safe
;;;; to publish.  In particular, no resolver here mutates a pane, medium,
;;;; sheet, text-style mapping, or output record.

(defstruct (appearance-candidate
            (:constructor %make-appearance-candidate (&key profile))
            (:conc-name %appearance-candidate-))
  "Immutable requested profile awaiting frame-local activation."
  (profile nil :type appearance-profile :read-only t))

(defstruct (resolved-appearance-bundle
            (:constructor %make-resolved-appearance-bundle
                (&key catalog-generation profile profile-revision
                      font-inventory-generation port-identity role-table
                      surface-defaults role-keys provenance bundle-key))
            (:conc-name %resolved-appearance-bundle-))
  "Complete immutable resolution of one profile for one opaque target port."
  (catalog-generation 0 :type (integer 0 *) :read-only t)
  (profile nil :type appearance-profile :read-only t)
  (profile-revision 0 :type (integer 0 *) :read-only t)
  (font-inventory-generation 0 :type (integer 0 *) :read-only t)
  ;; This deliberately retains the opaque identity object.  No other bundle
  ;; field may rely on object identity for its structural comparison.
  port-identity
  (role-table nil :type list :read-only t)
  (surface-defaults nil :type list :read-only t)
  (role-keys nil :type list :read-only t)
  (provenance nil :type list :read-only t)
  (bundle-key nil :read-only t))

(defstruct (appearance-activation-classification
            (:constructor %make-appearance-activation-classification
                (&key status deltas))
            (:conc-name %appearance-activation-classification-))
  "Classification of every effective bundle delta before publication."
  (status :no-op :type keyword :read-only t)
  (deltas nil :type list :read-only t))

(defstruct (appearance-activation-result
            (:constructor %make-appearance-activation-result
                (&key status candidate classification diagnostics bundle))
            (:conc-name %appearance-activation-result-))
  "Structured outcome retained by the owning frame after one activation event."
  (status :failed :type keyword :read-only t)
  (candidate nil :read-only t)
  (classification nil :read-only t)
  (diagnostics nil :type list :read-only t)
  (bundle nil :read-only t))

(defun copy-appearance-profile (profile)
  "Return a fresh immutable copy of PROFILE."
  (make-appearance-profile
   :selected-theme (appearance-profile-selected-theme profile)
   :strict-contrast (appearance-profile-strict-contrast profile)
   :role-overrides (appearance-profile-role-overrides profile)))

(defun make-appearance-candidate (profile)
  "Construct a deep immutable activation candidate for PROFILE."
  (unless (typep profile 'appearance-profile)
    (error-appearance-condition 'invalid-appearance-component
                                :axis :appearance-candidate :value profile))
  (%make-appearance-candidate :profile (copy-appearance-profile profile)))

(defun appearance-candidate-profile (candidate)
  (copy-appearance-profile (%appearance-candidate-profile candidate)))

(defun resolved-appearance-bundle-profile (bundle)
  (copy-appearance-profile (%resolved-appearance-bundle-profile bundle)))

(defun resolved-appearance-bundle-catalog-generation (bundle)
  (%resolved-appearance-bundle-catalog-generation bundle))

(defun resolved-appearance-bundle-profile-revision (bundle)
  (%resolved-appearance-bundle-profile-revision bundle))

(defun resolved-appearance-bundle-font-inventory-generation (bundle)
  (%resolved-appearance-bundle-font-inventory-generation bundle))

(defun resolved-appearance-bundle-port-identity (bundle)
  "Return BUNDLE's deliberately opaque target-port identity unchanged."
  (%resolved-appearance-bundle-port-identity bundle))

(defun resolved-appearance-bundle-role-table (bundle)
  (copy-appearance-value (%resolved-appearance-bundle-role-table bundle)))

(defun resolved-appearance-bundle-roles (bundle)
  "Compatibility accessor for BUNDLE's immutable resolved role table."
  (resolved-appearance-bundle-role-table bundle))

(defun resolved-appearance-bundle-surface-defaults (bundle)
  (copy-appearance-value (%resolved-appearance-bundle-surface-defaults bundle)))

(defun resolved-appearance-bundle-role-keys (bundle)
  (copy-appearance-value (%resolved-appearance-bundle-role-keys bundle)))

(defun resolved-appearance-bundle-provenance (bundle)
  (copy-appearance-value (%resolved-appearance-bundle-provenance bundle)))

(defun resolved-appearance-bundle-bundle-key (bundle)
  (copy-appearance-value (%resolved-appearance-bundle-bundle-key bundle)))

(defun appearance-activation-classification-deltas (classification)
  (copy-appearance-value
   (%appearance-activation-classification-deltas classification)))

(defun appearance-activation-classification-status (classification)
  (%appearance-activation-classification-status classification))

(defun appearance-activation-result-status (result)
  (%appearance-activation-result-status result))

(defun appearance-activation-result-classification (result)
  (%appearance-activation-result-classification result))

(defun appearance-activation-result-candidate (result)
  (let ((candidate (%appearance-activation-result-candidate result)))
    (and candidate
         (make-appearance-candidate (appearance-candidate-profile candidate)))))

(defun appearance-activation-result-diagnostics (result)
  (copy-list (%appearance-activation-result-diagnostics result)))

(defun appearance-activation-result-bundle (result)
  (%appearance-activation-result-bundle result))

(defun appearance-profile-structural-key (profile)
  "Return PROFILE's deterministic declaration key without object identity."
  (list :selected-theme (copy-appearance-value
                         (appearance-profile-selected-theme profile))
        :strict-contrast (appearance-profile-strict-contrast profile)
        :role-overrides
        (mapcar (lambda (entry)
                  (list (copy-appearance-value (car entry))
                        (appearance-role-style-key (cdr entry))))
                (appearance-profile-role-overrides profile))))

(defun valid-appearance-generation-p (value axis)
  (unless (and (integerp value) (not (minusp value)))
    (error-appearance-condition 'invalid-appearance-component
                                :axis axis :value value))
  value)

(defun resolve-appearance-profile-bundle
    (catalog profile &key profile-revision font-inventory-generation port-identity)
  "Resolve every declared role for PROFILE against an explicit opaque port.

PROFILE-REVISION and FONT-INVENTORY-GENERATION are frame-owned inputs.  This
builder enumerates neither fonts nor ports: callers supply the port identity
only after the frame manager has adopted and engrafted the target frame."
  (unless (typep profile 'appearance-profile)
    (error-appearance-condition 'invalid-appearance-component
                                :axis :appearance-profile :value profile))
  (valid-appearance-generation-p profile-revision :profile-revision)
  (valid-appearance-generation-p font-inventory-generation
                                 :font-inventory-generation)
  (when (null port-identity)
    (error-appearance-condition 'invalid-appearance-component
                                :axis :port-identity :value port-identity))
  (let* ((theme-id (appearance-profile-selected-theme profile))
         (overrides (appearance-profile-role-overrides profile))
         (role-table
           (loop for definition in (appearance-catalog-role-definitions catalog)
                 for role-id = (appearance-role-definition-id definition)
                 collect
                 (cons role-id
                       (resolve-appearance-role catalog theme-id role-id
                                                :unsaved-overrides overrides))))
         (role-keys
           (mapcar (lambda (entry)
                     (cons (car entry)
                           (resolved-appearance-role-structural-key (cdr entry))))
                   role-table))
         (surface-defaults
           (loop for definition in (appearance-catalog-role-definitions catalog)
                 for role-id = (appearance-role-definition-id definition)
                 when (eq :surface (appearance-role-definition-kind definition))
                   collect
                   (cons (copy-appearance-value role-id)
                         (resolved-appearance-role-style
                          (cdr (assoc role-id role-table :test #'equal))))))
         (provenance
           (mapcar (lambda (entry)
                     (cons (copy-appearance-value (car entry))
                           (resolved-appearance-role-provenance (cdr entry))))
                   role-table))
         (profile-key (appearance-profile-structural-key profile)))
    (%make-resolved-appearance-bundle
     :catalog-generation (appearance-catalog-generation catalog)
     :profile (copy-appearance-profile profile)
     :profile-revision profile-revision
     :font-inventory-generation font-inventory-generation
     :port-identity port-identity
     :role-table role-table
     :surface-defaults surface-defaults
     :role-keys role-keys
     :provenance provenance
     :bundle-key
     (list :catalog-generation (appearance-catalog-generation catalog)
           :profile profile-key
           :profile-revision profile-revision
           :font-inventory-generation font-inventory-generation
           ;; The intentional opaque identity component.  It is not copied or
           ;; derived from backend/private port fields.
           :port-identity port-identity
           :roles (copy-appearance-value role-keys)
           :surface-defaults
           (mapcar (lambda (entry)
                     (list (copy-appearance-value (car entry))
                           (appearance-role-style-key (cdr entry))))
                   surface-defaults)))))

(defun appearance-role-style-axis-value (style axis)
  "Return STYLE's structural effective AXIS value for delta comparison.

Resolved style specifications are distinct immutable structures on each pass,
so object identity would incorrectly classify an unchanged profile as a delta.
The established structural role key supplies the value comparison boundary."
  (getf (appearance-role-style-key style) axis))

(defun classify-appearance-delta-axis (catalog role-id axis)
  "Return the conservative v1 activation class for one effective delta.

Only non-default content/state foreground changes are proven safe at the
output boundary in this commit.  Pane defaults, surfaces, typography, and
decorations deliberately remain outside its live scope."
  (let ((kind (appearance-role-definition-kind
               (require-appearance-role-definition catalog role-id :activation))))
    (cond
      ((eq axis :surface) :restart-required)
      ((eq axis :typography) :unsupported)
      ((eq axis :decoration) :unsupported)
      ((eq axis :foreground-ink)
       (if (or (eq kind :surface) (eq role-id :default-text))
           :restart-required
           :render-boundary-live)))))

(defun classify-appearance-bundle-delta (catalog active candidate)
  "Classify all effective differences between ACTIVE and CANDIDATE bundles."
  (let ((deltas nil))
    (dolist (candidate-entry (%resolved-appearance-bundle-role-table candidate))
      (let* ((role-id (car candidate-entry))
             (candidate-style
               (resolved-appearance-role-style (cdr candidate-entry)))
             (active-entry
               (assoc role-id (%resolved-appearance-bundle-role-table active)
                      :test #'equal))
             (active-style
               (and active-entry
                    (resolved-appearance-role-style (cdr active-entry)))))
        (dolist (axis '(:typography :foreground-ink :surface :decoration))
          ;; A newly declared semantic role has no active rendered output.
          ;; Merely adding it cannot change the existing frame, so defer delta
          ;; classification until some profile/output stack actually uses it.
          (when active-entry
            (unless (equal
                     (appearance-role-style-axis-value active-style axis)
                     (appearance-role-style-axis-value candidate-style axis))
              (push (list :role role-id
                          :axis axis
                          :classification
                          (classify-appearance-delta-axis catalog role-id axis))
                    deltas))))))
    (let ((deltas (nreverse deltas)))
      (%make-appearance-activation-classification
       :status (cond ((null deltas) :no-op)
                     ((every (lambda (delta)
                               (eq (getf delta :classification)
                                   :render-boundary-live))
                             deltas)
                      :render-boundary-live)
                     ((some (lambda (delta)
                              (eq (getf delta :classification) :unsupported))
                            deltas)
                      :unsupported)
                     (t :restart-required))
       :deltas deltas))))

(defun prepare-appearance-activation
    (catalog active-profile candidate
     &key (profile-revision 0) (font-inventory-generation 0) port-identity)
  "Resolve, contrast-validate, and classify CANDIDATE without publishing it.

The returned result is always structured.  Errors become copied appearance
diagnostics, leaving the caller free to retain its active frame state exactly."
  (let ((diagnostics nil))
    (handler-case
        (handler-bind
            ((appearance-contrast-warning
               (lambda (condition)
                 (push condition diagnostics)
                 (let ((restart (find-restart 'muffle-warning condition)))
                   (when restart (invoke-restart restart))))))
          (let* ((active
                   (resolve-appearance-profile-bundle
                    catalog active-profile
                    :profile-revision profile-revision
                    :font-inventory-generation font-inventory-generation
                    :port-identity port-identity))
                 (profile (appearance-candidate-profile candidate)))
            (validate-appearance-profile-contrast catalog profile)
            (let* ((bundle
                     (resolve-appearance-profile-bundle
                      catalog profile
                      :profile-revision (1+ profile-revision)
                      :font-inventory-generation font-inventory-generation
                      :port-identity port-identity))
                   (classification
                     (classify-appearance-bundle-delta catalog active bundle))
                   (status (case (%appearance-activation-classification-status classification)
                             (:no-op :no-op)
                             (:render-boundary-live :ready)
                             (:restart-required :restart-required)
                             (:unsupported :unsupported))))
              (%make-appearance-activation-result
               :status status :candidate candidate :classification classification
               :diagnostics (nreverse diagnostics) :bundle bundle))))
      (appearance-condition (condition)
        (%make-appearance-activation-result
         :status :failed :candidate candidate
         :diagnostics (nreverse (cons condition diagnostics))))
      (error (condition)
        (%make-appearance-activation-result
         :status :failed :candidate candidate
         :diagnostics
         (nreverse
          (cons (make-appearance-condition
                 'appearance-activation-failed
                 :axis :activation :value (format nil "~A" condition))
                diagnostics)))))))
