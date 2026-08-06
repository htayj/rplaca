(in-package :rplaca)

;;; Listener frame fixture, presentation types, and delegated acceptance.
;;; This module introduces the rplaca-listener application frame (panes,
;;; layouts, and lifecycle are completed in a later todo), the frame-generated
;;; command definer, the listener-input-token result type, and three owned
;;; presentation types at the input boundary: rplaca-package, prose, and
;;; command-form-or-prose.  command-form-or-prose classifies the submitted
;;; region and delegates acceptance to clim:form, clim:command, or prose; it
;;; never executes.  Mode is read from the active frame's listener-context at
;;; accept time, with *listener-input-mode* as a headless fallback.

(defvar *listener-input-mode* :eval
  "Headless fallback for the listener input mode when no frame is active.")

(defstruct (listener-input-token
             (:constructor make-listener-input-token
                 (&key kind value source)))
  (kind nil :type symbol)
  (value nil)
  (source "" :type string))

(defun current-listener-input-mode ()
  "Return the validated listener input mode for the current accept.

An active rplaca-listener frame's listener-context supplies the mode and always
wins; *listener-input-mode* is the fallback for the headless accept-from-string
path.  A frame whose listener-context is missing or invalid is not silently
ignored: the context accessor or require-listener-input-mode signals."
  (require-listener-input-mode
   (if (typep clim:*application-frame* 'rplaca-listener)
       (listener-context-input-mode
        (rplaca-listener-context clim:*application-frame*))
       *listener-input-mode*)))

(defun current-listener-command-table ()
  "Return the active frame's command table, or the rplaca-listener table."
  (or (when clim:*application-frame*
        (ignore-errors (clim:frame-command-table clim:*application-frame*)))
      (clim:find-command-table 'rplaca-listener)))

;;; rplaca-listener frame: single interactor, dual layouts, scoped state.

(clim:define-application-frame rplaca-listener ()
  ((listener-context
    :initarg :listener-context
    :accessor rplaca-listener-context
    :initform (make-listener-context))
   (conversation-buffer
    :initarg :conversation-buffer
    :accessor rplaca-listener-conversation-buffer)
   (pending-session-name
    :initarg :pending-session-name
    :initform nil
    :accessor rplaca-listener-pending-session-name)
   (session-label
    :initarg :session-label
    :initform nil
    :accessor rplaca-listener-session-label)
   (appearance-profile
    :initarg :appearance-profile
    :initform (make-appearance-profile)
    :reader rplaca-listener-appearance-profile)
   (pending-assistant-turn
    :initform nil
    :accessor rplaca-listener-pending-assistant-turn)
   (selected-detail
    :initform nil
    :accessor rplaca-listener-selected-detail)
   (listener-liveness
    :initform :live
    :accessor rplaca-listener-liveness)
   (listener-progress
    :initform nil
    :accessor rplaca-listener-progress)
   (listener-lifecycle-generation
    :initform 0
    :accessor rplaca-listener-lifecycle-generation)
   (listener-wait-generation
    :initform 0
    :accessor rplaca-listener-wait-generation)
   (listener-wake-generation
    :initform 0
    :accessor rplaca-listener-wake-generation)
   (listener-wake-lock
    :initform (bt:make-lock "rplaca listener wake")
    :reader rplaca-listener-wake-lock)
   (listener-active-await-request
    :initform nil
    :accessor rplaca-listener-active-await-request)
   (listener-wake-dirty-p
    :initform nil
    :accessor rplaca-listener-wake-dirty-p)
   (listener-wake-pending-generation
    :initform nil
    :accessor rplaca-listener-wake-pending-generation)
   (listener-wake-handling-p
    :initform nil
    :accessor rplaca-listener-wake-handling-p)
   (pane-appearance-snapshot
    :initform nil
    :reader rplaca-listener-pane-appearance-snapshot)
   (pane-cache
    :initform (make-hash-table :test #'eq)
    :reader rplaca-listener-pane-cache))
  (:command-table (rplaca-listener))
  (:menu-bar nil)
  (:panes
   ;; Pane spec names are container names (e.g. interactor-container).
   ;; coerce-pane-name names the wrapper with the spec name.  The factory
   ;; passes :name '<semantic> to the inner stream pane, so find-pane-named
   ;; 'interactor finds the inner stream, not the wrapper (matching the
   ;; McCLIM Listener pattern).  Layouts reference container names.
   (interactor-container (make-rplaca-listener-pane 'interactor 'clim:interactor-pane nil
                                                     clim-internals::frame))
   (pointer-doc-container (make-rplaca-listener-pane 'pointer-doc 'clim:pointer-documentation-pane nil
                                                      clim-internals::frame))
   (wholine-container (make-rplaca-listener-pane 'wholine 'clim:application-pane 'display-listener-wholine
                                                  clim-internals::frame))
   (details-container (make-rplaca-listener-pane 'details 'clim:application-pane 'display-turn-details
                                                  clim-internals::frame)))
  (:layouts
   (listener-only
    (clim:vertically ()
      interactor-container
      pointer-doc-container
      wholine-container))
   (listener+details
    (clim:horizontally ()
      details-container
      (clim:vertically ()
        interactor-container
        pointer-doc-container
        wholine-container))))
  (:top-level (listener-frame-top-level)))

;;; Output maps to the inner interactor stream pane (not the outlined wrapper).
;;; get-frame-pane finds the inner pane by name then find-pane-of-type.
(defmethod clim:frame-standard-output ((frame rplaca-listener))
  (clim:get-frame-pane frame 'interactor))

(defmethod clim:frame-error-output ((frame rplaca-listener))
  (clim:get-frame-pane frame 'interactor))

(defun listener-print-prompt (stream frame)
  "Write the package/mode-aware prompt for FRAME to STREAM."
  (let ((context (rplaca-listener-context frame)))
    (write-string
     (listener-prompt-string (listener-context-package-name context)
                             (listener-context-input-mode context))
     stream)))

(defun listener-frame-top-level (frame)
  "Bind package/directory/error-output around the whole command loop.

*application-frame* and frame-process ownership come from run-frame-top-level
and are not replaced here.  default-frame-top-level does not bind *error-output*,
so the wrapper does."
  (let* ((context (rplaca-listener-context frame))
         (buffer (rplaca-listener-conversation-buffer frame))
         (*package* (or (find-package (listener-context-package-name context))
                        (find-package :cl-user)))
         (*default-pathname-defaults*
           (if buffer
               (uiop:ensure-directory-pathname (buffer-working-directory buffer))
               *default-pathname-defaults*))
         (*error-output* (or (clim:frame-error-output frame) *error-output*)))
    (clim:default-frame-top-level frame :prompt 'listener-print-prompt)))

;;; rplaca-package presentation type.

(clim:define-presentation-type rplaca-package ()
  :inherit-from t)

(clim:define-presentation-method clim:presentation-typep
    (object (type rplaca-package))
  (typep object 'package))

(clim:define-presentation-method clim:present
    (object (type rplaca-package) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string (package-name object) stream))

(defun listener-package-completion-entries ()
  "Return deterministic sorted (display-string . package) completion entries
over every package name and nickname."
  (let ((entries '()))
    (flet ((add-entry (label package)
             (push (cons (string-upcase label) package) entries)))
      (dolist (package (list-all-packages))
        (add-entry (package-name package) package)
        (dolist (nickname (package-nicknames package))
          (add-entry nickname package))))
    (coerce
     (sort (delete-duplicates entries :test #'string= :key #'car)
           #'string< :key #'car)
     'list)))

(clim:define-presentation-method clim:accept
    ((type rplaca-package) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (let ((entries (listener-package-completion-entries)))
    (clim:completing-from-suggestions (stream)
      (dolist (entry entries)
        (clim:suggest (car entry) (cdr entry))))))

;;; prose presentation type.

(clim:define-presentation-type prose ()
  :inherit-from t)

(clim:define-presentation-method clim:present
    (object (type prose) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string object stream))

(clim:define-presentation-method clim:accept
    ((type prose) stream (view clim:textual-view) &key)
  (declare (ignore view))
  ;; Rest of line verbatim, empty allowed: no delimiter gestures, so spaces and
  ;; most punctuation accumulate; the token ends at activation or end of input.
  (values (clim:with-delimiter-gestures (nil :override t)
            (clim:read-token stream))
          'prose))

;;; command-form-or-prose presentation type (delegated acceptance).

(clim:define-presentation-type command-form-or-prose ()
  :inherit-from t)

(clim:define-presentation-method clim:present
    (object (type command-form-or-prose) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (typecase object
    (listener-input-token
     (write-string (listener-input-token-source object) stream))
    (otherwise (princ object stream))))

(defun listener-submission-end (buffer)
  "Return the submission endpoint.  McCLIM 1.0's accept-from-string stream uses
the raw string as its buffer (no fill pointer), so length is the endpoint."
  (if (array-has-fill-pointer-p buffer)
      (fill-pointer buffer)
      (length buffer)))

(defun listener-region-whitespace-p (string)
  (or (zerop (length string))
      (every #'listener-whitespace-p string)))

(defun listener-position-newline (string)
  (position #\Newline string))

(defmacro with-listener-submission ((region-var
                                     start-var end-var buffer-var stream-var)
                                    stream-form &body body)
  `(let* ((,stream-var ,stream-form)
          (,buffer-var (clim:stream-input-buffer ,stream-var))
          (,start-var (clim:stream-scan-pointer ,stream-var))
          (,end-var (listener-submission-end ,buffer-var))
          (,region-var (subseq ,buffer-var ,start-var ,end-var)))
     (declare (ignorable ,buffer-var))
     ,@body))

(defun listener-consume-to (stream submission-end)
  (setf (clim:stream-scan-pointer stream) submission-end))

(defun listener-advance-prefix (stream submission-start prefix-length)
  (setf (clim:stream-scan-pointer stream)
        (+ submission-start prefix-length)))

(defun listener-multiline-prose-error-token (region submission-end stream)
  (listener-consume-to stream submission-end)
  (make-listener-input-token
   :kind :error
   :value "Use ,Compose for multiline prose."
   :source region))

(clim:define-presentation-method clim:accept
    ((type command-form-or-prose) stream (view clim:textual-view) &key)
  (with-listener-submission
      (region submission-start submission-end buffer stream) stream
    (labels ((source-after (prefix-length)
               (subseq region prefix-length))
             (finish (kind value prefix-length)
               ;; Delegated acceptance only positions for parsing; consume the
               ;; complete submission so no semantic residual tail remains (the
               ;; CLIM accept return confirms this in tests).
               (listener-consume-to stream submission-end)
               (make-listener-input-token
                :kind kind
                :value value
                :source (source-after prefix-length))))
      (let ((mode (current-listener-input-mode)))
        (cond
          ;; Empty or whitespace-only submission: no-op before any delegation.
          ((listener-region-whitespace-p region)
           (listener-consume-to stream submission-end)
           (make-listener-input-token :kind :no-op :value nil
                                      :source region))
          ;; #! -> shell token (checked on two characters before the ! rule).
          ((and (>= (length region) 2)
                (char= (char region 0) #\#)
                (char= (char region 1) #\!))
           (let ((shell-source (source-after 2)))
              (listener-consume-to stream submission-end)
              (make-listener-input-token :kind :shell :value shell-source
                                         :source shell-source)))
          ;; , -> consume, then peek to distinguish eval-form / literal prose /
          ;; bare-comma / command.
          ((char= (char region 0) #\,)
           (listener-advance-prefix stream submission-start 1)
           (let ((rest (source-after 1)))
             (cond
               ((zerop (length rest))
                 ;; Bare comma: mode-dependent error or exit-say.
                 (listener-consume-to stream submission-end)
                 (ecase mode
                   (:eval
                    (make-listener-input-token
                     :kind :error
                     :value "did you mean ,Command or ! to talk?"
                     :source ""))
                   (:say
                    (make-listener-input-token
                     :kind :exit-say :value nil
                     :source ""))))
               ((char= (char rest 0) #\()
                ;; ,( -> one-shot eval form.
                (let ((value (clim:accept 'clim:form :stream stream
                                          :prompt nil :view view)))
                  (finish :eval-form value 1)))
               ((and (>= (length rest) 2)
                     (char= (char rest 0) #\,)
                     (char= (char rest 1) #\())
                ;; ,,( -> literal ,( prose.  Reject multiline paste first.
                (if (listener-position-newline region)
                    (listener-multiline-prose-error-token
                     region submission-end stream)
                    (progn
                      (listener-advance-prefix stream submission-start 2)
                      (let ((value (clim:accept 'prose :stream stream
                                                :prompt nil :view view)))
                        (finish :prose value 1)))))
               (t
                ;; ,X -> CLIM command in the listener command table.
                (let ((value (clim:accept
                              `(clim:command
                                :command-table ,(current-listener-command-table))
                              :stream stream :prompt nil :view view)))
                  (finish :command value 1))))))
          ;; ! -> consume, then prose; only exact bare ! toggles the mode.
          ((char= (char region 0) #\!)
           (if (listener-position-newline region)
               (listener-multiline-prose-error-token
                region submission-end stream)
               (let ((rest (source-after 1)))
                 (cond
                   ((zerop (length rest))
                     ;; Exact bare bang: mode toggle.
                     (listener-consume-to stream submission-end)
                     (make-listener-input-token
                      :kind (ecase mode (:eval :enter-say) (:say :exit-say))
                      :value nil :source ""))
                   (t
                    ;; Nonempty rest (spaces count) is force-prose.
                    (listener-advance-prefix stream submission-start 1)
                    (let ((value (clim:accept 'prose :stream stream
                                              :prompt nil :view view)))
                      (finish :prose value 1)))))))
          ;; Leading whitespace disables dispatch; payload is the full line.
          ((listener-whitespace-p (char region 0))
           (if (eq mode :say)
               (if (listener-position-newline region)
                   (listener-multiline-prose-error-token
                    region submission-end stream)
                   (let ((value (clim:accept 'prose :stream stream
                                             :prompt nil :view view)))
                     (finish :prose value 0)))
               (let ((value (clim:accept 'clim:form :stream stream
                                         :prompt nil :view view)))
                 (finish :form value 0))))
          ;; Default: mode default (form in :eval, prose in :say).
          (t
           (if (eq mode :say)
               (if (listener-position-newline region)
                   (listener-multiline-prose-error-token
                    region submission-end stream)
                   (let ((value (clim:accept 'prose :stream stream
                                             :prompt nil :view view)))
                     (finish :prose value 0)))
               (let ((value (clim:accept 'clim:form :stream stream
                                         :prompt nil :view view)))
                 (finish :form value 0)))))))))

;;; --------------------------------------------------------------------------
;;; Assistant turn and detail-surface data, presentations, and rendering.
;; --------------------------------------------------------------------------

(defstruct (assistant-turn
             (:constructor make-assistant-turn
                 (&key primary-text tool-uses reasoning metadata
                       artifact-refs media-refs inspect-payload status)))
  (primary-text "" :type string)
  (tool-uses nil :type list)
  (reasoning nil :type list)
  (metadata nil :type list)
  (artifact-refs nil :type list)
  (media-refs nil :type list)
  (inspect-payload nil)
  (status :complete :type (member :complete :error :cancelled)))

(defstruct (turn-facet
             (:constructor make-turn-facet (&key turn kind)))
  (turn nil :type (or null assistant-turn))
  (kind nil :type (member :tools :reasoning :metadata :artifacts :media :inspect)))

(clim:define-presentation-type assistant-turn () :inherit-from t)
(clim:define-presentation-type turn-facet () :inherit-from t)

(clim:define-presentation-method clim:present
    (object (type assistant-turn) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (write-string (assistant-turn-primary-text object) stream))

(clim:define-presentation-method clim:present
    (object (type turn-facet) stream (view clim:textual-view) &key)
  (declare (ignore view))
  (format stream "[~A]" (string-downcase (turn-facet-kind object))))

(defun set-rplaca-listener-selected-detail (frame turn facet-kind)
  "Select TURN/FACET-KIND as the details-pane content for FRAME."
  (setf (rplaca-listener-selected-detail frame) (cons turn facet-kind)))

(defun clear-rplaca-listener-selected-detail (frame)
  "Clear FRAME's details-pane selection."
  (setf (rplaca-listener-selected-detail frame) nil))

(defun listener-recording-stream-p (stream)
  "Return true when STREAM supports CLIM output recording (formatting-table)."
  (ignore-errors (clim:extended-output-stream-p stream)))

(defun render-listener-tool-facets (stream turn)
  (let ((tool-uses (assistant-turn-tool-uses turn)))
    (cond
      ((null tool-uses) (write-string "No tool calls." stream))
      ((listener-recording-stream-p stream)
       (clim:formatting-table (stream)
         (dolist (use tool-uses)
           (clim:formatting-row (stream)
             (clim:formatting-cell (stream)
               (let ((name (or (getf use :name) "unknown")))
                 (clim:with-output-as-presentation
                     (stream use 'turn-facet)
                   (write-string name stream))))))))
      (t (dolist (use tool-uses)
           (let ((name (or (getf use :name) "unknown")))
             (write-line name stream)))))))

(defun render-listener-reasoning-facets (stream turn)
  (let ((blocks (assistant-turn-reasoning turn)))
    (cond
      ((null blocks) (write-string "No reasoning." stream))
      ((listener-recording-stream-p stream)
       (dolist (block blocks)
         (clim:with-output-as-presentation (stream block 'turn-facet)
           (write-line (princ-to-string block) stream))))
      (t (dolist (block blocks)
           (write-line (princ-to-string block) stream))))))

(defun listener-metadata-plist-pairs (metadata)
  "Validate METADATA as an even-length proper plist and return its (key . value)
pairs.  Signal a clear error on malformed input rather than silently dropping it."
  (let ((length (list-length metadata)))
    (unless length
      (error "Malformed assistant-turn metadata (not a proper list): ~S" metadata))
    (when (oddp length)
      (error "Malformed assistant-turn metadata (odd-length plist): ~S" metadata))
    (loop :for (key value) :on metadata :by #'cddr
          :collect (cons key value))))

(defun render-listener-metadata-facets (stream turn)
  (let ((metadata (assistant-turn-metadata turn)))
    (cond
      ((null metadata) (write-string "No metadata." stream))
      (t (let ((pairs (listener-metadata-plist-pairs metadata)))
           (if (listener-recording-stream-p stream)
               (clim:formatting-table (stream)
                 (dolist (pair pairs)
                   (clim:formatting-row (stream)
                     (clim:formatting-cell (stream) (prin1 (car pair) stream))
                     (clim:formatting-cell (stream) (prin1 (cdr pair) stream)))))
               (dolist (pair pairs)
                 (format stream "~A = ~A~%" (car pair) (cdr pair)))))))))

(defun render-listener-artifact-facets (stream turn)
  (let ((refs (assistant-turn-artifact-refs turn)))
    (cond
      ((null refs) (write-string "No artifacts." stream))
      ((listener-recording-stream-p stream)
       (dolist (ref refs)
         (clim:with-output-as-presentation (stream ref 'turn-facet)
           (write-line ref stream))))
      (t (dolist (ref refs)
           (write-line ref stream))))))

(defun render-listener-media-facets (stream turn)
  (let ((refs (assistant-turn-media-refs turn)))
    (cond
      ((null refs) (write-string "No media." stream))
      ((listener-recording-stream-p stream)
       (dolist (ref refs)
         (clim:with-output-as-presentation (stream ref 'turn-facet)
           (write-line ref stream))))
      (t (dolist (ref refs)
           (write-line ref stream))))))

(defun render-listener-inspect-facet (stream turn)
  (let ((payload (assistant-turn-inspect-payload turn)))
    (if payload
        (prin1 payload stream)
        (write-string "Nothing to inspect." stream))))

(defun display-turn-details (frame pane)
  "Render the selected assistant-turn facet in the details pane.

Pure: it only reads FRAME's stored selection and writes to PANE; it performs no
domain mutation."
  (let ((detail (rplaca-listener-selected-detail frame)))
    (when detail
      (let ((turn (car detail))
            (facet (cdr detail)))
        (when (and turn facet)
          (clim:with-output-as-presentation (pane turn 'assistant-turn)
            (ecase facet
              (:tools (render-listener-tool-facets pane turn))
              (:reasoning (render-listener-reasoning-facets pane turn))
              (:metadata (render-listener-metadata-facets pane turn))
              (:artifacts (render-listener-artifact-facets pane turn))
              (:media (render-listener-media-facets pane turn))
              (:inspect (render-listener-inspect-facet pane turn)))))))))

(defun display-listener-wholine (frame pane)
  "Render a compact package/directory/liveness status line for FRAME."
  (let* ((context (rplaca-listener-context frame))
         (buffer (rplaca-listener-conversation-buffer frame))
         (package-name (listener-context-package-name context))
         (directory (if buffer
                        (namestring (buffer-working-directory buffer))
                        ""))
         (progress (rplaca-listener-progress frame)))
    (format pane "~A  pkg:~A  dir:~A~@[  ~A~]"
            (or (rplaca-listener-session-label frame) "rplaca")
            package-name directory progress)))

;;; --------------------------------------------------------------------------
;;; Construction-time pane appearance, lifecycle, and frame launcher.
;; --------------------------------------------------------------------------

(defun listener-rgb-color-from-ink (ink)
  "Translate an appearance ink value to a CLIM color, or NIL when unspecified."
  (let ((rgb (appearance-rgb-components ink)))
    (when rgb
      (apply #'clim:make-rgb-color rgb))))

(defun listener-role-style->pane-initargs (style)
  "Translate a resolved appearance role STYLE to CLIM pane initargs.

Only specified axes appear.  Foreground/background become CLIM colors via
appearance-rgb-components; typography becomes a CLIM text-style."
  (let ((initargs nil)
        (foreground-ink (appearance-role-style-foreground-ink style))
        (surface (appearance-role-style-surface style))
        (typography (appearance-role-style-typography style)))
    (unless (appearance-unspecified-p foreground-ink)
      (let ((color (listener-rgb-color-from-ink
                    (appearance-ink-spec-foreground foreground-ink))))
        (when color
          (setf (getf initargs :foreground) color))))
    (unless (appearance-unspecified-p surface)
      (let ((color (listener-rgb-color-from-ink
                    (appearance-surface-spec-background surface))))
        (when color
          (setf (getf initargs :background) color))))
    (unless (appearance-unspecified-p typography)
      (let ((family (appearance-typography-spec-family typography))
            (face (appearance-typography-spec-face typography))
            (size (appearance-typography-spec-size typography)))
        (setf (getf initargs :text-style)
              (clim:make-text-style (unless (appearance-unspecified-p family) family)
                                    (unless (appearance-unspecified-p face) face)
                                    (unless (appearance-unspecified-p size) size)))))
    initargs))

(defun listener-resolve-pane-appearance-initargs (profile catalog)
  "Resolve pane-name -> CLIM pane initarg plist once from PROFILE and CATALOG.

Each pane maps to a declared role stack; profile role overrides participate via
resolve-runtime-appearance-role-stack.  Returns a fresh alist.  Resolver or
configuration errors propagate (no swallowed fallback)."
  (flet ((resolve (roles)
           (listener-role-style->pane-initargs
            (resolved-appearance-role-style
             (resolve-runtime-appearance-role-stack
              catalog
              (appearance-profile-selected-theme profile)
              roles
              :unsaved-overrides (appearance-profile-role-overrides profile))))))
    (list (cons 'interactor (resolve '(:base :default-text)))
          (cons 'pointer-doc (resolve '(:pointer-documentation :default-text)))
          (cons 'wholine (resolve '(:info-pane :modeline)))
          (cons 'details (resolve '(:base :default-text))))))

(defmethod initialize-instance :after ((frame rplaca-listener) &key)
  "Resolve the immutable pane-appearance snapshot before pane construction."
  (setf (slot-value frame 'pane-appearance-snapshot)
        (listener-resolve-pane-appearance-initargs
         (rplaca-listener-appearance-profile frame)
         (current-package-appearance-catalog))))

(defun listener-pane-appearance-initargs (frame pane-name)
  "Return the cached initarg plist for PANE-NAME (resolved once at construction)."
  (cdr (assoc pane-name
              (rplaca-listener-pane-appearance-snapshot frame)
              :test #'eq)))

(defun make-rplaca-listener-pane (pane-name pane-type display-function frame)
  "Construct a CLIM stream pane for PANE-NAME with the cached appearance initargs.

The pane is cached per-frame so layout regeneration (which clears
frame-panes-for-layout and rebuilds) reuses the SAME pane object, preserving
interactor output history and stream state across listener-only <-> details
layout switches.  The first call constructs and caches; later calls return the
cached object."
  (or (gethash pane-name (rplaca-listener-pane-cache frame))
      (setf (gethash pane-name (rplaca-listener-pane-cache frame))
            (let ((initargs (listener-pane-appearance-initargs frame pane-name)))
              (apply #'clim:make-clim-stream-pane
                     :type pane-type
                     :name pane-name
                     (append initargs
                             (ecase pane-name
                               ((interactor) (list :scroll-bars :vertical))
                               ((pointer-doc) nil)
                               ((wholine) (list :scroll-bars nil
                                                :end-of-line-action :allow))
                               ((details) (list :scroll-bars t)))
                             (when display-function
                               (list :display-function display-function
                                     :display-time :command-loop))))))))

(defun listener-frame-cleanup (frame hook)
  "Tear down FRAME-owned runtime: mark dead, advance lifecycle generation,
retire the wake hook and any active await request/coalescing state, dispose the
owned conversation buffer, and ignore late events.  Idempotent and error-bounded
so a broken buffer/pane cannot retain a dead frame."
  (setf (rplaca-listener-liveness frame) :dead)
  (bt:with-lock-held ((rplaca-listener-wake-lock frame))
    (incf (rplaca-listener-lifecycle-generation frame))
    (let ((request (rplaca-listener-active-await-request frame)))
      (when (and request
                 (member (listener-await-request-phase request)
                         '(:waiting :cancelling :detached-cancelling)))
        (setf (listener-await-request-phase request) :retired)))
    (setf (rplaca-listener-active-await-request frame) nil
          (rplaca-listener-wake-dirty-p frame) nil
          (rplaca-listener-wake-pending-generation frame) nil
          (rplaca-listener-wake-handling-p frame) nil))
  (when hook
    (ignore-errors (remove-hook '*buffer-display-wakeup-hook* hook)))
  ;; Cancel any still-owned runtime once after retiring the request so late
  ;; events no-op.  Non-blocking: the reaper settles teardown asynchronously.
  (let ((buffer (rplaca-listener-conversation-buffer frame)))
    (when buffer
      (ignore-errors (stop-streaming-response buffer))
      (ignore-errors (dispose-buffer buffer))))
  frame)

;;; --------------------------------------------------------------------------
;;; Todo 9: command-owned agent wait and frame-process settlement.
;;; --------------------------------------------------------------------------

(defvar *suppress-listener-wake-requests* nil
  "Bound to T on the frame process while the wake handler applies runtime
state.  Prevents recursive wake enqueues from the applicators' own display
notifications without losing teardown delivery.")

;;; These functions are declaimed NOTINLINE so test seams that rebind
;;; SYMBOL-FUNCTION remain effective for same-file callers (SBCL compiles
;;; same-file calls as direct references by default).
(declaim (notinline listener-grafted-top-level-sheet
                     listener-reserve-wakeup-event
                     listener-enqueue-reserved-wakeup
                     listener-initiate-cancellation
                     listener-turn-settled-p
                     listener-apply-runtime-state
                     listener-compute-settlement))

(defstruct (listener-await-request
             (:constructor make-listener-await-request
                 (&key frame buffer dispatch-result
                       lifecycle-generation wait-generation
                       expected-runtime-generation)))
  "Exact per-turn wait state for one agent-turn settlement."
  (token (gensym "LISTENER-AWAIT-") :type symbol)
  (frame nil :type (or null rplaca-listener))
  (buffer nil)
  (dispatch-result nil)
  (lifecycle-generation 0 :type integer)
  (wait-generation 0 :type integer)
  (expected-runtime-generation 0 :type integer)
  (phase :waiting
   :type (member :waiting :cancelling :settled :detached-cancelling :retired))
  (cancel-requested-p nil :type boolean)
  (terminal-failure nil)
  (terminal-status nil :type (or null (member :complete :error :cancelled))))

(defclass rplaca-listener-wakeup-event (clim:window-manager-event)
  ((request :initarg :request
            :reader rplaca-listener-wakeup-event-request)
   (notification-generation :initarg :notification-generation
                            :reader rplaca-listener-wakeup-event-notification-generation)
    (expected-runtime-generation :initarg :expected-runtime-generation
                                 :reader rplaca-listener-wakeup-event-expected-runtime-generation)
    (terminal-failure :initarg :terminal-failure
                      :reader rplaca-listener-wakeup-event-terminal-failure))
  (:default-initargs :request nil :notification-generation 0
                      :expected-runtime-generation 0 :terminal-failure nil)
  (:documentation
   "One coalesced frame-process wakeup carrying the exact active request, the
notification generation it was reserved for, and the expected buffer runtime
generation for identity validation."))

(defun listener-grafted-top-level-sheet (frame)
  "Return FRAME's grafted top-level sheet, or NIL before FRAME is running."
  (let ((sheet (ignore-errors (clim:frame-top-level-sheet frame))))
    (and sheet
         (ignore-errors (clim:sheet-grafted-p sheet))
         sheet)))

(defun listener-turn-settled-p (buffer)
  "Return true when BUFFER's runtime is fully idle for a new turn."
  (not (safe-reload-buffer-runtime-active-p buffer)))

(defun listener-request-live-for-frame-p (request frame)
  "Return true when REQUEST is the exact active live request for FRAME."
  (and (eq :live (rplaca-listener-liveness frame))
       (eq request (rplaca-listener-active-await-request frame))
       (= (listener-await-request-lifecycle-generation request)
          (rplaca-listener-lifecycle-generation frame))
       (= (listener-await-request-wait-generation request)
          (rplaca-listener-wait-generation frame))))

(defun listener-reserve-wakeup-event (frame)
  "Reserve a single queued wakeup event for a dirty, idle, live FRAME.
Return true when the caller must enqueue the reserved event.  Accepts both
active wait phases and detached-cancelling drain."
  (bt:with-lock-held ((rplaca-listener-wake-lock frame))
    (let ((request (rplaca-listener-active-await-request frame)))
      (when (and request
                 (eq :live (rplaca-listener-liveness frame))
                 (rplaca-listener-wake-dirty-p frame)
                 (not (rplaca-listener-wake-pending-generation frame))
                 (not (rplaca-listener-wake-handling-p frame))
                 (member (listener-await-request-phase request)
                         '(:waiting :cancelling :detached-cancelling)))
        (setf (rplaca-listener-wake-pending-generation frame)
              (rplaca-listener-wake-generation frame))
        t))))

(defparameter *listener-enqueue-max-attempts* 2
  "Maximum immediate attempts to enqueue one reserved wakeup event.
Retries do not sleep, recurse, or render, so a broken event queue cannot spin
forever or block the frame process.")

(defun listener-record-terminal-failure (request condition)
  "Store CONDITION as REQUEST's first terminal failure."
  (unless (listener-await-request-terminal-failure request)
    (setf (listener-await-request-terminal-failure request) condition))
  (listener-await-request-terminal-failure request))

(defun listener-reserved-wakeup-event (frame sheet &optional terminal-failure)
  "Return an immutable snapshot event for FRAME's current reservation."
  (bt:with-lock-held ((rplaca-listener-wake-lock frame))
    (let ((request (rplaca-listener-active-await-request frame))
          (generation (rplaca-listener-wake-pending-generation frame)))
      (when (and request generation)
        (make-instance 'rplaca-listener-wakeup-event
                       :sheet sheet
                       :request request
                       :notification-generation generation
                       :expected-runtime-generation
                       (listener-await-request-expected-runtime-generation request)
                       :terminal-failure terminal-failure)))))

(defun listener-interrupt-frame-with-wakeup (frame event)
  "Ask FRAME's process to append EVENT to its normal event queue."
  (let ((process (ignore-errors (clim-internals::frame-process frame))))
    (when process
      (handler-case
          (progn
            (clim-sys:process-interrupt
             process
             (lambda ()
               (handler-case
                   (climi::queue-append
                    (climi::frame-event-queue frame)
                    event)
                 (error (condition)
                   (file-debug-log "listener-wakeup-frame-queue-failed"
                                   "~A" condition)
                   (error (or (rplaca-listener-wakeup-event-terminal-failure
                               event)
                              condition))))))
            t)
        (error (condition)
          (file-debug-log "listener-wakeup-interrupt-failed" "~A" condition)
          nil)))))

(defun listener-deliver-failure-wakeup (frame sheet failure)
  "Transport FAILURE through McCLIM's concurrent queue or frame process."
  (unless (rplaca-listener-wake-pending-generation frame)
    (listener-reserve-wakeup-event frame))
  (let ((event (listener-reserved-wakeup-event frame sheet failure)))
    (when event
      (or (handler-case
              (progn
                (climi::queue-append (clim:sheet-event-queue sheet) event)
                t)
            (error (condition)
              (file-debug-log "listener-wakeup-fallback-queue-failed"
                              "~A" condition)
              nil))
          (listener-interrupt-frame-with-wakeup frame event)))))

(defun listener-enqueue-reserved-wakeup (frame)
  "Enqueue FRAME's reserved wakeup event on the grafted top-level sheet.
Uses a bounded immediate retry following the chat-frame transport pattern.
On persistent failure, cancellation invalidates the old reservation and a fresh
failure event uses McCLIM's concurrent queue or a bounded frame-process wake."
  (let ((sheet (listener-grafted-top-level-sheet frame)))
    (unless sheet
      (bt:with-lock-held ((rplaca-listener-wake-lock frame))
        (setf (rplaca-listener-wake-pending-generation frame) nil))
      (return-from listener-enqueue-reserved-wakeup nil))
    (loop
       :with max-attempts := (max 1 *listener-enqueue-max-attempts*)
       :for attempt :from 1 :to max-attempts
       :for event := (listener-reserved-wakeup-event frame sheet)
       :for request := (and event (rplaca-listener-wakeup-event-request event))
       :for generation := (and event
                               (rplaca-listener-wakeup-event-notification-generation
                                event))
       :unless (and request generation
                    (member (listener-await-request-phase request)
                            '(:waiting :cancelling :detached-cancelling)))
         :do (return nil)
       :when (handler-case
                 (progn
                   (clim:queue-event sheet event)
                   t)
               (error (condition)
                 (file-debug-log "listener-wakeup-queue-failed"
                                 "attempt ~D: ~A" attempt condition)
                 nil))
         :return t
       :do (let ((retry-p nil))
             (bt:with-lock-held ((rplaca-listener-wake-lock frame))
               (when (and (rplaca-listener-wake-pending-generation frame)
                          (= (rplaca-listener-wake-pending-generation frame)
                             generation))
                 (setf (rplaca-listener-wake-pending-generation frame) nil))
               (when (and (eq :live (rplaca-listener-liveness frame))
                          (rplaca-listener-wake-dirty-p frame)
                          (not (rplaca-listener-wake-handling-p frame))
                          (< attempt max-attempts))
                 (incf (rplaca-listener-wake-generation frame))
                 (setf (rplaca-listener-wake-pending-generation frame)
                       (rplaca-listener-wake-generation frame)
                       retry-p t)))
             (unless retry-p
               (let ((failure
                       (make-condition
                        'prompt-run-error
                        :message "Persistent listener wakeup enqueue failure.")))
                 (listener-initiate-cancellation
                  frame request :failure failure :enqueue-p nil)
                 (return
                   (listener-deliver-failure-wakeup
                    frame sheet failure))))))))

(defun listener-make-wake-hook (frame)
  "Return a data-only wake hook for FRAME's owned buffer and active request.
Under lock: mark dirty and increment generation for the exact owned buffer and
active request.  Outside lock: reserve at most one pending event and enqueue it
to the grafted top-level sheet.  Never inspects runtime state, accesses panes,
executes commands, writes output, or sleeps."
  (lambda (buffer reason)
    (declare (ignore reason))
    (unless *suppress-listener-wake-requests*
      (bt:with-lock-held ((rplaca-listener-wake-lock frame))
        (let ((request (rplaca-listener-active-await-request frame)))
          (when (and request
                     (eq :live (rplaca-listener-liveness frame))
                     (eq buffer (listener-await-request-buffer request))
                     (member (listener-await-request-phase request)
                             '(:waiting :cancelling :detached-cancelling)))
            (setf (rplaca-listener-wake-dirty-p frame) t)
            (incf (rplaca-listener-wake-generation frame)))))
      (when (listener-reserve-wakeup-event frame)
        (listener-enqueue-reserved-wakeup frame)))))

(defun listener-apply-runtime-state (frame buffer)
  "Apply the canonical frame-process applicator order for BUFFER.
Suppressed recursive wake requests during application so the applicators' own
display notifications do not storm the handler.  Teardown delivery proceeds
normally because the applicators are called explicitly here."
  (declare (ignore frame))
  (let ((*suppress-listener-wake-requests* t))
    (deliver-buffer-runtime-stopped-notification buffer)
    (update-openai-oauth-login buffer)
    (update-interactive-tool-execution buffer)
    (update-interactive-buffer-operation buffer)
    (when (buffer-pending-stream buffer)
      (update-streaming-response buffer))))

(defun listener-progress-text (request)
  "Return a compact progress label for the current wait phase."
  (case (listener-await-request-phase request)
    (:cancelling "[cancelling]")
    (:waiting "[working]")
    (otherwise nil)))

(defun listener-initiate-cancellation
    (frame request &key failure (enqueue-p t))
  "Claim REQUEST cancellation, stop once outside the wake lock, and wake FRAME."
  (let ((stop-p nil)
        (buffer (listener-await-request-buffer request)))
    (bt:with-lock-held ((rplaca-listener-wake-lock frame))
      (when failure
        (listener-record-terminal-failure request failure))
      (unless (listener-await-request-cancel-requested-p request)
        (setf (listener-await-request-cancel-requested-p request) t
              (listener-await-request-phase request) :cancelling
              stop-p t)))
    (when stop-p
      (let ((*suppress-listener-wake-requests* t))
        (stop-streaming-response buffer))
      (bt:with-lock-held ((rplaca-listener-wake-lock frame))
        (when (listener-request-live-for-frame-p request frame)
          (setf (listener-await-request-expected-runtime-generation request)
                (buffer-runtime-generation buffer)
                (rplaca-listener-wake-pending-generation frame) nil
                (rplaca-listener-wake-dirty-p frame) t)
          (incf (rplaca-listener-wake-generation frame)))))
    (when (and enqueue-p
               (listener-reserve-wakeup-event frame))
      (listener-enqueue-reserved-wakeup frame))
    stop-p))

(defun listener-compute-settlement (frame request)
  "Apply runtime state, update progress/wholine, and determine settlement.
Returns T when the turn has settled (phase set to :settled).  Applicator
failures are recorded but do NOT settle the request while runtime ownership
remains active; instead the request moves to :cancelling for teardown."
  (handler-case
      (let ((buffer (listener-await-request-buffer request)))
        (listener-apply-runtime-state frame buffer)
        (setf (rplaca-listener-progress frame)
              (listener-progress-text request))
        (ignore-errors
          (clim:redisplay-frame-pane frame 'wholine :force-p t))
        (cond
          ((listener-turn-settled-p buffer)
           (setf (listener-await-request-phase request) :settled)
           t)
          (t nil)))
    (error (condition)
      (listener-record-terminal-failure request condition)
      (let ((buffer (listener-await-request-buffer request)))
        (cond
          ((listener-turn-settled-p buffer)
            (setf (listener-await-request-phase request) :settled)
            t)
          (t
            (listener-initiate-cancellation frame request :failure condition)
            nil))))))

(defun handle-listener-wakeup (frame request event)
  "Validate and consume one coalesced wakeup event, then apply runtime state
and compute settlement.  Rejects stale, duplicate, wrong-frame, wrong-buffer,
dead-frame, and generation-mismatched events.  Never calls
execute-frame-command or writes interactor output."
  (let ((event-gen (rplaca-listener-wakeup-event-notification-generation event))
        (event-runtime-gen (rplaca-listener-wakeup-event-expected-runtime-generation event))
        (event-failure (rplaca-listener-wakeup-event-terminal-failure event))
        (detached-p nil)
        (buffer-runtime-gen
          (buffer-runtime-generation (listener-await-request-buffer request))))
    (bt:with-lock-held ((rplaca-listener-wake-lock frame))
      ;; Validate: exact live request, generations, buffer ownership.
      (unless (listener-request-live-for-frame-p request frame)
        (return-from handle-listener-wakeup nil))
      ;; Validate: request buffer is still the frame's conversation buffer.
      (unless (eq (listener-await-request-buffer request)
                   (rplaca-listener-conversation-buffer frame))
        (return-from handle-listener-wakeup nil))
      ;; Runtime identity is validated before consuming pending/dirty state.
      ;; Rejected stale events therefore cannot erase a newer reservation.
      (unless (and (= event-runtime-gen
                      (listener-await-request-expected-runtime-generation request))
                   (= event-runtime-gen buffer-runtime-gen))
        (return-from handle-listener-wakeup nil))
      (when event-failure
        (listener-record-terminal-failure request event-failure))
      ;; Consume the matching reservation: clear pending AND dirty atomically.
      ;; A stale/duplicate event whose generation does not match is rejected
      ;; without clearing a newer reservation.
      (let ((pending (rplaca-listener-wake-pending-generation frame)))
        (cond
          ((and pending (= pending event-gen))
            (setf detached-p
                  (eq :detached-cancelling
                      (listener-await-request-phase request)))
            (setf (rplaca-listener-wake-pending-generation frame) nil
                  (rplaca-listener-wake-dirty-p frame) nil
                 (rplaca-listener-wake-handling-p frame) t))
          (t
           (return-from handle-listener-wakeup nil)))))
    (unwind-protect
         (listener-compute-settlement frame request)
      ;; Finish handling: clear the flag.
      (bt:with-lock-held ((rplaca-listener-wake-lock frame))
        (setf (rplaca-listener-wake-handling-p frame) nil))
      ;; If this was a detached-cancelling request that settled, retire it
      ;; without assistant output or waiter signaling.
      (when (and (eq :settled (listener-await-request-phase request))
                  (eq request (rplaca-listener-active-await-request frame)))
        (bt:with-lock-held ((rplaca-listener-wake-lock frame))
          (when (eq request (rplaca-listener-active-await-request frame))
            (when detached-p
              (setf (listener-await-request-phase request) :retired))
            (setf (rplaca-listener-active-await-request frame) nil
                   (rplaca-listener-wake-dirty-p frame) nil
                   (rplaca-listener-wake-pending-generation frame) nil))))
      ;; Reserve one follow-up only if a NEW notification arrived during
      ;; handling (dirty became true again after we cleared it on consume).
      (when (and (eq :live (rplaca-listener-liveness frame))
                 (rplaca-listener-active-await-request frame)
                 (rplaca-listener-wake-dirty-p frame)
                 (listener-reserve-wakeup-event frame))
        (listener-enqueue-reserved-wakeup frame)))))

(defmethod clim:handle-event
    ((sheet clime:top-level-sheet-mixin)
     (event rplaca-listener-wakeup-event))
  (let ((frame (ignore-errors (clim:pane-frame sheet))))
    (when (typep frame 'rplaca-listener)
      (let ((request (rplaca-listener-wakeup-event-request event)))
        ;; Validate: sheet is the frame's current grafted top-level sheet and
        ;; request buffer matches the conversation buffer.
        (when (and request
                   (eq sheet (listener-grafted-top-level-sheet frame))
                   (eq (listener-await-request-buffer request)
                       (rplaca-listener-conversation-buffer frame)))
          (handle-listener-wakeup frame request event))))))

(defun listener-escape-gesture-p (gesture)
  "Return true when GESTURE is an Escape keyboard gesture (character or
key-press-event with :escape key-name)."
  (or (eql gesture #\Escape)
      (eql gesture #\Esc)
      (and (typep gesture 'clim:key-press-event)
           (eq :escape
               (ignore-errors (clim:keyboard-event-key-name gesture))))))

(defun listener-handle-cancel-request (frame request)
  "Idempotently request cancellation: stop once, mark :cancelling, update the
expected runtime generation, and continue waiting until cancellation settles."
  (when (listener-initiate-cancellation frame request)
    (setf (rplaca-listener-progress frame) (listener-progress-text request))
    (ignore-errors
      (clim:redisplay-frame-pane frame 'wholine :force-p t))))

(defun listener-await-terminal-condition (request buffer)
  "Return the condition to signal after settlement, or NIL for normal return."
  (cond
    ((listener-await-request-terminal-failure request))
    ((listener-await-request-cancel-requested-p request)
     (make-condition 'prompt-run-cancelled))
    ((eq :error (buffer-status buffer))
     (make-condition 'prompt-run-error
                     :message "Agent turn ended with an error."))
    (t nil)))

(defun await-listener-agent-turn (frame buffer dispatch-result)
  "Block inside com-say until the agent turn settles, consuming gestures.

Runs inside com-say after input editing has closed.  Owns one active request
invariant.  The worker remains data-only; the frame-process wake handler
applies runtime state.  Waits via stream-read-gesture with a cheap settled
predicate and a non-local-exit wait handler.  Ordinary gestures are consumed
with beep (no typeahead).  Escape and abort-gesture call stop-streaming-response
once and continue waiting until cancellation settles.  unwind-protect cancels
on frame exit/debugger unwind."
  (declare (ignore dispatch-result))
  (let ((request nil))
    ;; Install a fresh wait request under lock.
    (bt:with-lock-held ((rplaca-listener-wake-lock frame))
      (incf (rplaca-listener-wait-generation frame))
      (setf request
            (make-listener-await-request
             :frame frame
             :buffer buffer
             :dispatch-result dispatch-result
             :lifecycle-generation (rplaca-listener-lifecycle-generation frame)
             :wait-generation (rplaca-listener-wait-generation frame)
             :expected-runtime-generation (buffer-runtime-generation buffer))
            (rplaca-listener-active-await-request frame) request))
    (unwind-protect
         (progn
           ;; Immediate-completion race: run one synchronous frame-owned
           ;; apply/check before entering the gesture wait.  Do NOT return-from
           ;; here; the common terminal-condition path below must always run.
           (unless (listener-compute-settlement frame request)
              ;; Gesture wait loop.
              (let ((interactor (clim:frame-standard-output frame)))
                (block await-settlement
                  (loop
                    (multiple-value-bind (gesture reason)
                        (handler-case
                            (clim:stream-read-gesture
                             interactor
                             :input-wait-test
                             (lambda (stream)
                               (declare (ignore stream))
                               (eq :settled (listener-await-request-phase request)))
                             :input-wait-handler
                             (lambda (stream)
                               (declare (ignore stream))
                               (return-from await-settlement))
                             :pointer-button-press-handler nil)
                          (clim:abort-gesture ()
                            (listener-handle-cancel-request frame request)
                            (values nil :abort)))
                      (declare (ignore reason))
                      (cond
                        ((null gesture)
                         nil)
                        ((listener-escape-gesture-p gesture)
                         (listener-handle-cancel-request frame request))
                        (t
                         (clim:beep interactor)))))))))
      ;; Unwind: retire/transition the request under lock, but call Stop
      ;; OUTSIDE the lock so the wake hook can reacquire it normally.
      (let ((needs-stop nil))
        (bt:with-lock-held ((rplaca-listener-wake-lock frame))
          (cond
            ((eq :settled (listener-await-request-phase request))
             ;; Normal settlement: retire cleanly and clear coalescing state.
             (setf (rplaca-listener-active-await-request frame) nil
                   (rplaca-listener-wake-dirty-p frame) nil
                   (rplaca-listener-wake-pending-generation frame) nil
                   (rplaca-listener-wake-handling-p frame) nil))
            ((member (listener-await-request-phase request)
                     '(:waiting :cancelling))
             ;; Abnormal unwind (debugger/command exit): mark detached-cancelling
             ;; and preserve coalescing state for detached drain.  Stop is
             ;; called outside the lock below.
             (setf (listener-await-request-phase request) :detached-cancelling
                   needs-stop t))))
        ;; Stop once, outside the wake lock.
        (when needs-stop
          (ignore-errors (stop-streaming-response buffer))
          (let ((refresh-p nil))
            (bt:with-lock-held ((rplaca-listener-wake-lock frame))
              (when (and (listener-request-live-for-frame-p request frame)
                         (eq :detached-cancelling
                             (listener-await-request-phase request)))
                (setf (listener-await-request-expected-runtime-generation request)
                      (buffer-runtime-generation buffer)
                      (rplaca-listener-wake-pending-generation frame) nil
                      (rplaca-listener-wake-handling-p frame) nil
                      (rplaca-listener-wake-dirty-p frame) t)
                (incf (rplaca-listener-wake-generation frame))
                (setf refresh-p t)))
            (when (and refresh-p
                       (listener-reserve-wakeup-event frame))
              (listener-enqueue-reserved-wakeup frame))))))
    ;; Signal terminal conditions after cleanup so the caller's handler-case
    ;; catches them with the frame already retired.  This path is always
    ;; reached on normal settlement (never bypassed by early return).
    (let ((condition (listener-await-terminal-condition request buffer)))
      (when condition
        (error condition)))))

(defun call-with-listener-frame-runtime (frame continuation)
  "Run CONTINUATION with FRAME's top-level runtime installed and protected."
  (let ((hook nil))
    (unwind-protect
         (progn
           (setf (rplaca-listener-liveness frame) :live)
           (setf hook (listener-make-wake-hook frame))
           (add-hook '*buffer-display-wakeup-hook* hook :append t)
           (funcall continuation))
      (listener-frame-cleanup frame hook))))

(defmethod clim:run-frame-top-level :around ((frame rplaca-listener) &key)
  (call-with-listener-frame-runtime frame (lambda () (call-next-method))))

(defun run-rplaca-listener
    (buffer &key pending-session-name window-title appearance-profile new-process)
  "Construct and run the rplaca-listener frame owning BUFFER.

Mirrors the canonical McCLIM launcher: make-application-frame, run-frame-top-
level (which owns *application-frame*/frame-process), and disown in unwind
cleanup.  When :NEW-PROCESS is non-nil, run on a fresh process and return it."
  (let* ((fm (clim:find-frame-manager :port (clim:find-port)))
         (frame (clim:make-application-frame
                 'rplaca-listener
                 :frame-manager fm
                 :conversation-buffer buffer
                 :pending-session-name pending-session-name
                 :appearance-profile
                 (or appearance-profile (make-appearance-profile))
                 :listener-context (make-listener-context)
                 :pretty-name (or window-title "RPLACA"))))
    (flet ((run ()
             (unwind-protect
                  (clim:run-frame-top-level frame)
               (ignore-errors (clim:disown-frame fm frame)))))
      (if new-process
          (values (clim-sys:make-process #'run :name "rplaca listener") frame)
          (progn (run) frame)))))
