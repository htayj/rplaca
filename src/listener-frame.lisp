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
   (pane-appearance-snapshot
    :initform nil
    :reader rplaca-listener-pane-appearance-snapshot)
   (pane-cache
    :initform (make-hash-table :test #'eq)
    :reader rplaca-listener-pane-cache))
  (:command-table (rplaca-listener))
  (:menu-bar nil)
  (:panes
   ;; define-application-frame generates the pane constructor as
   ;; (lambda (clim-internals::fm clim-internals::frame) ...), so the pane forms
   ;; must reference that exact lexical frame symbol and exported CLIM pane classes.
   (interactor (make-rplaca-listener-pane 'interactor 'clim:interactor-pane nil
                                          clim-internals::frame))
   (pointer-doc (make-rplaca-listener-pane 'pointer-doc 'clim:pointer-documentation-pane nil
                                           clim-internals::frame))
   (wholine (make-rplaca-listener-pane 'wholine 'clim:application-pane 'display-listener-wholine
                                       clim-internals::frame))
   (details (make-rplaca-listener-pane 'details 'clim:application-pane 'display-turn-details
                                       clim-internals::frame)))
  (:layouts
   (listener-only
    (clim:vertically ()
      interactor
      pointer-doc
      wholine))
   (listener+details
    (clim:horizontally ()
      details
      (clim:vertically ()
        interactor
        pointer-doc
        wholine))))
  (:top-level (listener-frame-top-level)))

;;; Output always maps to the named interactor in both layouts; McCLIM would
;;; otherwise select the details application pane after a layout recomputation.
(defmethod clim:frame-standard-output ((frame rplaca-listener))
  (clim:find-pane-named frame 'interactor))

(defmethod clim:frame-error-output ((frame rplaca-listener))
  (clim:find-pane-named frame 'interactor))

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
  "Tear down FRAME-owned runtime: mark dead, retire the wake hook, dispose the
owned conversation buffer, and ignore late events.  Idempotent and error-bounded
so a broken buffer/pane cannot retain a dead frame."
  (setf (rplaca-listener-liveness frame) :dead)
  (when hook
    (ignore-errors (remove-hook '*buffer-display-wakeup-hook* hook)))
  (let ((buffer (rplaca-listener-conversation-buffer frame)))
    (when buffer
      (ignore-errors (dispose-buffer buffer))))
  frame)

(defun call-with-listener-frame-runtime (frame continuation)
  "Run CONTINUATION with FRAME's top-level runtime installed and protected."
  (let ((hook nil))
    (unwind-protect
         (progn
           (setf (rplaca-listener-liveness frame) :live)
           (setf hook
                 (lambda (buffer reason)
                   (declare (ignore buffer reason))
                   (when (eq :live (rplaca-listener-liveness frame))
                     ;; Async applicators/wake handling are todos 8/9; the hook
                     ;; only keeps ownership so cleanup retires it.
                     (setf (rplaca-listener-liveness frame) :live))))
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
