(in-package :rplaca)

;;; --------------------------------------------------------------------------
;;; Tool Registry
;;; --------------------------------------------------------------------------

(defvar *tool-registry-lock* (bt:make-lock "rplaca tool registry")
  "Process-wide lock for the global tool definition and metadata tables.")

(defvar *tool-table* (make-hash-table :test #'equal)
  "Global table mapping tool name strings to tool-definition structs.")

(defvar *process-tool-table* *tool-table*
  "The process-global tool table, distinct from dynamic temporary bindings.")

(defstruct agent-tool-metadata
  "Metadata for a Lisp function exposed as a provider-callable agent tool."
  (symbol      (error "symbol required")     :type symbol   :read-only t)
  (name        ""                            :type string   :read-only t)
  (description ""                            :type string   :read-only t)
  (args        nil                           :type list     :read-only t)
  (input-schema nil                          :type list     :read-only t)
  (call-style  :positional                   :type keyword  :read-only t)
  (execution   :background                   :type keyword  :read-only t)
  (command-p   nil                           :type boolean  :read-only t)
  (lambda-list nil                           :type list     :read-only t)
  (package     nil                           :type (or null string) :read-only t))

(defvar *agent-tool-metadata-table* (make-hash-table :test #'eq)
  "Global table mapping Lisp symbols to AGENT-TOOL-METADATA.")

(defvar *process-agent-tool-metadata-table* *agent-tool-metadata-table*
  "The process-global metadata table, distinct from dynamic test bindings.")

(defvar *agent-tool-name-table* (make-hash-table :test #'equal)
  "Global table mapping provider tool names to owning Lisp symbols.")

(defvar *process-agent-tool-name-table* *agent-tool-name-table*
  "The process-global name table, distinct from dynamic test bindings.")

(defun process-tool-registry-table-p (table)
  "Return true when TABLE is one of the process-global tool registries."
  (or (eq table *process-tool-table*)
      (eq table *process-agent-tool-metadata-table*)
      (eq table *process-agent-tool-name-table*)))

(defun call-with-tool-registry-lock-for-tables (function &rest tables)
  "Call FUNCTION under the registry lock when TABLES include a global table.

Dynamically bound replacement tables and *TEMPORARY-TOOL-TABLE* are private to
their dynamic owner and deliberately avoid the process lock.  FUNCTION must do
only bounded registry data access: it must not invoke a tool, hook, package
loader, MCP request, or any other reentrant extension callback."
  (if (some #'process-tool-registry-table-p tables)
      (bt:with-lock-held (*tool-registry-lock*)
        (funcall function))
      (funcall function)))

(defun tool-registry-table-snapshot (table)
  "Return a stable alist snapshot of registry TABLE.

The hash traversal is protected for process-global tables.  Consumers receive
the snapshot after the lock has been released and may safely invoke extension
code while traversing it."
  (call-with-tool-registry-lock-for-tables
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (key value)
                  (push (cons key value) entries))
                table)
       entries))
   table))

(defun agent-tool-blank-string-p (value)
  "Return true when VALUE is NIL or contains only ASCII whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return)
                                  value)))))

(defun normalize-agent-tool-name (tool-name)
  "Normalize TOOL-NAME into the provider-facing tool name string."
  (let* ((raw (etypecase tool-name
                (string tool-name)
                (symbol (symbol-name tool-name))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (when (agent-tool-blank-string-p trimmed)
      (error "Tool name must be non-empty"))
    (string-downcase
     (substitute #\_ #\- trimmed))))

(defun normalize-agent-tool-schema-type (value)
  "Normalize a Lisp type designator into the provider schema type string."
  (cond
    ((null value) "string")
    ((stringp value) value)
    ((symbolp value) (string-downcase (symbol-name value)))
    (t (format nil "~(~A~)" value))))

(defun normalize-agent-tool-arg-spec (tool-symbol arg-spec)
  "Normalize one explicit argument spec for TOOL-SYMBOL."
  (unless (and (consp arg-spec)
               (not (keywordp (first arg-spec))))
    (error "Tool ~A argument spec must start with an argument name, got ~S."
           tool-symbol arg-spec))
  (let ((arg-name (first arg-spec))
        (plist (rest arg-spec)))
    (unless (evenp (length plist))
      (error "Tool ~A argument spec for ~A has an odd plist: ~S."
             tool-symbol arg-name arg-spec))
    (loop :for tail :on plist :by #'cddr
          :for key := (first tail)
          :unless (member key '(:type :description :required :items) :test #'eq)
            :do (error "Tool ~A argument spec for ~A has unsupported key ~S."
                       tool-symbol arg-name key))
    (list :name (tool-key-name arg-name)
          :type (normalize-agent-tool-schema-type (getf plist :type))
          :items (getf plist :items)
          :description (or (getf plist :description) "")
          :required (if (member :required plist :test #'eq)
                        (not (null (getf plist :required)))
                        t))))

(defun agent-tool-arg-schema-property (arg)
  "Return the provider schema property for normalized ARG metadata."
  (let ((property `((:type . ,(getf arg :type)))))
    (when (getf arg :items)
      (setf property (append property `((:items . ,(getf arg :items))))))
    (if (agent-tool-blank-string-p (getf arg :description))
        property
        (append property
                `((:description . ,(getf arg :description)))))))

(defun agent-tool-input-schema (args)
  "Return the provider input schema for normalized ARGS metadata."
  (let ((required (loop :for arg :in args
                        :when (getf arg :required)
                          :collect (getf arg :name)))
        (properties (loop :for arg :in args
                          :collect (cons (getf arg :name)
                                         (agent-tool-arg-schema-property arg)))))
    `((:type . "object")
      (:properties . ,properties)
      (:required . ,(coerce required 'vector)))))

(defun normalize-agent-tool-call-style (tool-symbol value command-p)
  "Normalize the call style for TOOL-SYMBOL."
  (let ((style (or value (if command-p :command :positional))))
    (unless (member style '(:raw-args :positional :keyword :command) :test #'eq)
      (error "Tool ~A has unsupported call style ~S." tool-symbol style))
    (when (and command-p (not (eq style :command)))
      (error "Command tool ~A must use :COMMAND call style." tool-symbol))
    style))

(defun normalize-agent-tool-execution (tool-symbol value command-p)
  "Normalize the interactive execution owner for TOOL-SYMBOL."
  (let ((execution (or value (if command-p :frame :background))))
    (unless (member execution '(:background :frame :command-only) :test #'eq)
      (error "Tool ~A has unsupported execution policy ~S."
             tool-symbol execution))
    execution))

(defun validate-agent-tool-args (tool-symbol tool-spec)
  "Return the explicit :ARGS value from TOOL-SPEC or signal a clear error."
  (unless (member :args tool-spec :test #'eq)
    (error "Tool ~A requires explicit :ARGS metadata." tool-symbol))
  (let ((args (getf tool-spec :args)))
    (unless (listp args)
      (error "Tool ~A :ARGS must be a list, got ~S." tool-symbol args))
    args))

(defun normalize-agent-tool-spec (symbol tool-spec
                                  &key command-p lambda-list docstring)
  "Normalize explicit tool metadata into AGENT-TOOL-METADATA."
  (unless (and (listp tool-spec) (or (null tool-spec) (keywordp (first tool-spec))))
    (error "Tool metadata for ~A must be a keyword plist, got ~S."
           symbol tool-spec))
  (unless (evenp (length tool-spec))
    (error "Tool metadata for ~A has an odd plist: ~S." symbol tool-spec))
  (loop :for tail :on tool-spec :by #'cddr
        :for key := (first tail)
        :unless (member key '(:name :description :args
                              :call-style :execution)
                        :test #'eq)
          :do (error "Tool metadata for ~A has unsupported key ~S."
                     symbol key))
  (let* ((raw-args (validate-agent-tool-args symbol tool-spec))
         (args (mapcar (lambda (arg)
                         (normalize-agent-tool-arg-spec symbol arg))
                       raw-args))
         (description (or (getf tool-spec :description)
                          docstring
                          (documentation symbol 'function)
                          ""))
         (call-style
           (normalize-agent-tool-call-style
            symbol (getf tool-spec :call-style) command-p))
         (execution
           (normalize-agent-tool-execution
            symbol (getf tool-spec :execution) command-p)))
    (when (and command-p lambda-list
               (/= (length args) (length (rest lambda-list))))
      (error "Command tool ~A argument count ~D does not match command parameters ~D."
             symbol (length args) (length (rest lambda-list))))
    (make-agent-tool-metadata
     :symbol symbol
     :name (normalize-agent-tool-name (or (getf tool-spec :name) symbol))
     :description description
     :args args
     :input-schema (agent-tool-input-schema args)
     :call-style call-style
     :execution execution
     :command-p (not (null command-p))
     :lambda-list lambda-list
     :package *current-rplaca-package*)))

(defun call-agent-tool-provider-bridge (metadata)
  "Register METADATA with the provider tool table when that bridge is loaded."
  (let ((bridge (and (fboundp 'register-agent-tool-provider-definition)
                     (symbol-function 'register-agent-tool-provider-definition))))
    (when bridge
      (funcall bridge metadata))))

(defun remove-agent-tool-provider-entry (name)
  "Remove NAME from the provider tool table when the table is loaded."
  (when (and (boundp '*tool-table*)
             (hash-table-p (symbol-value '*tool-table*)))
    (let ((table (symbol-value '*tool-table*)))
      (call-with-tool-registry-lock-for-tables
       (lambda () (remhash name table))
       table))))

(defun unregister-agent-tool-metadata (symbol)
  "Remove SYMBOL's agent tool metadata and provider entry, when present."
  (let ((metadata
          (call-with-tool-registry-lock-for-tables
           (lambda ()
             (let ((metadata (gethash symbol *agent-tool-metadata-table*)))
               (when metadata
                 (remhash (agent-tool-metadata-name metadata)
                          *agent-tool-name-table*)
                 (remhash symbol *agent-tool-metadata-table*))
               metadata))
           *agent-tool-metadata-table*
           *agent-tool-name-table*)))
    (when metadata
      (remove-agent-tool-provider-entry (agent-tool-metadata-name metadata)))
    metadata))

(defun register-agent-tool-metadata (symbol tool-spec
                                     &key command-p lambda-list docstring)
  "Register SYMBOL as an agent tool from explicit TOOL-SPEC metadata."
  (when (and *current-rplaca-package*
             (not (package-resource-type-allowed-p :tool)))
    (return-from register-agent-tool-metadata nil))
  (if (null tool-spec)
      (unregister-agent-tool-metadata symbol)
      (let* ((metadata (normalize-agent-tool-spec
                        symbol tool-spec
                        :command-p command-p
                        :lambda-list lambda-list
                        :docstring docstring))
             (name (agent-tool-metadata-name metadata))
             (previous-name
               (call-with-tool-registry-lock-for-tables
                (lambda ()
                  (let ((owner (gethash name *agent-tool-name-table*)))
                    (when (and owner (not (eq owner symbol)))
                      (error "Tool name ~S is already registered for ~A, cannot reuse it for ~A."
                             name owner symbol))
                    (let ((previous
                            (gethash symbol *agent-tool-metadata-table*)))
                      (when (and previous
                                 (not (string=
                                       name
                                       (agent-tool-metadata-name previous))))
                        (remhash (agent-tool-metadata-name previous)
                                 *agent-tool-name-table*))
                      (setf (gethash symbol *agent-tool-metadata-table*) metadata
                            (gethash name *agent-tool-name-table*) symbol)
                      (and previous
                           (not (string=
                                 name
                                 (agent-tool-metadata-name previous)))
                           (agent-tool-metadata-name previous)))))
                *agent-tool-metadata-table*
                *agent-tool-name-table*)))
        ;; Provider synchronization is deliberately outside the metadata lock:
        ;; the bridge is extension-facing and may reenter registry helpers.
        (when previous-name
          (remove-agent-tool-provider-entry previous-name))
        (call-agent-tool-provider-bridge metadata)
        metadata)))

(defun find-agent-tool-metadata (symbol-or-name)
  "Return agent tool metadata by Lisp symbol or provider tool name."
  (let ((normalized-name
          (and (or (symbolp symbol-or-name) (stringp symbol-or-name))
               (normalize-agent-tool-name symbol-or-name))))
    (call-with-tool-registry-lock-for-tables
     (lambda ()
       (cond
         ((symbolp symbol-or-name)
          (or (gethash symbol-or-name *agent-tool-metadata-table*)
              (let ((owner (gethash normalized-name
                                    *agent-tool-name-table*)))
                (and owner
                     (gethash owner *agent-tool-metadata-table*)))))
         ((stringp symbol-or-name)
          (let ((owner (gethash normalized-name *agent-tool-name-table*)))
            (and owner (gethash owner *agent-tool-metadata-table*))))
         (t nil)))
     *agent-tool-metadata-table*
     *agent-tool-name-table*)))

(defun list-agent-tool-metadata ()
  "Return registered agent tool metadata sorted by provider tool name."
  (sort (mapcar #'cdr
                (tool-registry-table-snapshot
                 *agent-tool-metadata-table*))
        #'string< :key #'agent-tool-metadata-name))

(defun ensure-agent-tool-function-defined (symbol)
  "Signal an error unless SYMBOL names a callable tool function."
  (unless (fboundp symbol)
    (error "deftool ~A requires a separately defined function." symbol))
  symbol)

(defmacro deftool (symbol &rest tool-spec)
  "Register an existing function SYMBOL as a provider-callable agent tool.

When SYMBOL names a registered command, command call style is inferred: provider
arguments map to the command's non-BUFFER parameters, and the current tool
buffer is supplied automatically during execution."
  (unless (symbolp symbol)
    (error "deftool requires a symbol name, got ~S." symbol))
  (unless tool-spec
    (error "deftool ~A requires explicit metadata." symbol))
  (unless (evenp (length tool-spec))
    (error "deftool ~A metadata has an odd plist: ~S." symbol tool-spec))
  (let ((metadata-var (gensym "COMMAND-METADATA")))
    `(let ((,metadata-var (find-command-metadata ',symbol)))
       (ensure-agent-tool-function-defined ',symbol)
       (when (or (null *current-rplaca-package*)
                 (package-resource-type-allowed-p :tool))
         (register-agent-tool-metadata
          ',symbol ',tool-spec
          :command-p (not (null ,metadata-var))
          :lambda-list (and ,metadata-var
                            (command-metadata-lambda-list ,metadata-var))
          :docstring (or (and ,metadata-var
                              (command-metadata-docstring ,metadata-var))
                         (documentation ',symbol 'function)))))))

(defstruct tool-definition
  "Definition of a tool that can be called by an agent."
  (name        ""              :type string   :read-only t)
  (description ""              :type string   :read-only t)
  (input-schema nil            :type list     :read-only t)
  (execution   :background     :type keyword  :read-only t)
  (execute-fn  nil             :type (or null function))
  (package nil                 :type (or null string) :read-only t))

(defstruct (subagent-tool
            (:constructor %make-subagent-tool
                (&key name description input-schema execution execute-fn)))
  "Temporary tool definition passed to a subagent run."
  (name        ""              :type string   :read-only t)
  (description ""              :type string   :read-only t)
  (input-schema nil            :type list     :read-only t)
  (execution   :background     :type keyword  :read-only t)
  (execute-fn  nil             :type (or null function)))

(defvar *active-tool-names* nil
  "Dynamic tool allowlist for the current agent run.
NIL means all tools visible to the caller are available.")

(defvar *temporary-tool-table* nil
  "Dynamic table mapping tool names to temporary tool definitions.
Temporary tools override same-named global tools for the dynamic extent.")

(defvar *current-tool-buffer* nil
  "Dynamic buffer passed to command-style provider tools during execution.")

(defvar *current-tool-execution-plan* nil
  "Immutable definition and visibility snapshot for one background tool.")

(defvar *tool-execution-preflight-completed-p* nil
  "True when the frame process already validated dispatch and the before hook.")

(defvar *tool-effect-recorder* nil
  "Dynamically bound callback recording deferred non-buffer tool effects.")

(defvar *tool-cancellation-registration-function* nil
  "Worker-local callback accepting a bounded cooperative cancellation function.")

(defun register-current-tool-cancellation-function (function)
  "Register FUNCTION to run if the owning interactive tool is cancelled.

The function is invoked outside runtime locks.  Registration returns true when
cancellation had already been requested, allowing a tool to avoid beginning a
provider request after the frame has withdrawn it."
  (unless (functionp function)
    (error "Tool cancellation handler must be a function, got ~S." function))
  (if *tool-cancellation-registration-function*
      (funcall *tool-cancellation-registration-function* function)
      nil))

(defstruct (tool-hook-effect
            (:constructor make-tool-hook-effect (phase tool-name args result)))
  "Immutable tool hook call deferred to the frame process."
  (phase :before :type keyword :read-only t)
  (tool-name "" :type string :read-only t)
  (args nil :read-only t)
  (result nil :read-only t))

(defstruct (tool-execution-plan
            (:constructor make-tool-execution-plan
                (name definition active-allowed-p visible-p)))
  "Frame-captured immutable dispatch snapshot for a background tool invocation."
  (name "" :type string :read-only t)
  (definition nil :type (or null tool-definition) :read-only t)
  (active-allowed-p nil :type boolean :read-only t)
  (visible-p nil :type boolean :read-only t))

(defvar *http-fetch-max-chars* 50000
  "Default maximum characters returned by http_fetch.")

(defvar *http-connection-timeout* 15
  "Connection timeout in seconds for HTTP requests.")

(defvar *http-user-agent* "RPLACA/0.1"
  "User-Agent header used by HTTP requests.")

(defvar *shell-exec-default-timeout* 30
  "Default timeout in seconds for shell_exec.")

(defvar *shell-exec-poll-interval* 0.05
  "Polling interval in seconds while waiting for shell_exec to finish.")

(defparameter *built-in-tool-names*
  '("lisp_eval" "live_lisp_eval" "recovery_list")
  "Names reserved for core RPLACA provider tools.
INIT-TOOLS removes these entries before re-registering tagged tools, so
user-added tools stored in *tool-table* are left intact.")

(defun make-subagent-tool (&key name description input-schema
                             ((:schema schema-arg) nil)
                             (execution :background)
                             execute-fn
                             ((:function function-arg) nil))
  "Build a temporary tool definition suitable for RUN-SUBAGENT.
SCHEMA is accepted as an alias for INPUT-SCHEMA.  EXECUTE-FN or FUNCTION must
be a function accepting one argument: the decoded tool input alist."
  (let ((fn (or execute-fn function-arg))
        (effective-schema (or input-schema schema-arg)))
    (unless name
      (error "Temporary subagent tools require :name"))
    (unless description
      (error "Temporary subagent tools require :description"))
    (unless effective-schema
      (error "Temporary subagent tools require :input-schema or :schema"))
    (unless fn
      (error "Temporary subagent tools require :execute-fn or :function"))
    (%make-subagent-tool :name (normalize-tool-name name)
                         :description description
                         :input-schema effective-schema
                         :execution (normalize-agent-tool-execution
                                     name execution nil)
                         :execute-fn fn)))

(defun subagent-tool->tool-definition (tool)
  "Convert temporary TOOL into a TOOL-DEFINITION."
  (make-tool-definition :name (subagent-tool-name tool)
                        :description (subagent-tool-description tool)
                        :input-schema (subagent-tool-input-schema tool)
                        :execution (subagent-tool-execution tool)
                        :execute-fn (subagent-tool-execute-fn tool)))

(defun plist-subagent-tool-p (tool)
  "Return true when TOOL appears to be a plist temporary tool definition."
  (and (listp tool)
       (keywordp (first tool))
       (or (getf tool :name)
           (getf tool :description)
           (getf tool :input-schema)
           (getf tool :schema)
           (getf tool :execute-fn)
           (getf tool :function))))

(defun normalize-subagent-tool (tool)
  "Normalize TOOL into a TOOL-DEFINITION.
TOOL may be a SUBAGENT-TOOL, a TOOL-DEFINITION, or a plist accepted by
MAKE-SUBAGENT-TOOL."
  (cond
    ((tool-definition-p tool)
     (make-tool-definition :name (normalize-tool-name
                                  (tool-definition-name tool))
                           :description (tool-definition-description tool)
                           :input-schema (tool-definition-input-schema tool)
                           :execution (tool-definition-execution tool)
                           :execute-fn (tool-definition-execute-fn tool)
                           :package (tool-definition-package tool)))
    ((subagent-tool-p tool)
     (subagent-tool->tool-definition tool))
    ((plist-subagent-tool-p tool)
     (subagent-tool->tool-definition
      (apply #'make-subagent-tool tool)))
    (t
     (error "Unsupported temporary subagent tool definition: ~S" tool))))

(defun normalize-subagent-tools (tools)
  "Return a list of normalized temporary tool definitions."
  (mapcar #'normalize-subagent-tool tools))

(defun make-temporary-tool-table (tools)
  "Build a temporary tool table from normalized or plist TOOLS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (definition (normalize-subagent-tools tools) table)
      (setf (gethash (tool-definition-name definition) table)
            definition))))

(defun registered-tool-definitions-snapshot ()
  "Return a stable snapshot of the currently bound registered tool table."
  (tool-registry-table-snapshot *tool-table*))

(defun find-registered-tool-definition (name)
  "Return the registered global definition for normalized tool NAME."
  (let ((normalized-name (normalize-tool-name name)))
    (call-with-tool-registry-lock-for-tables
     (lambda () (gethash normalized-name *tool-table*))
     *tool-table*)))

(defun remove-registered-tools (names)
  "Remove NAMES from the currently bound registered tool table atomically."
  (let ((normalized-names (mapcar #'normalize-tool-name names)))
    (call-with-tool-registry-lock-for-tables
     (lambda ()
       (dolist (name normalized-names)
         (remhash name *tool-table*)))
     *tool-table*))
  t)

(defun effective-tool-definition (name)
  "Return the effective tool definition for NAME.
Temporary dynamic tools override process-global registered tools."
  (let ((normalized-name (normalize-tool-name name)))
    (or (and *temporary-tool-table*
             (gethash normalized-name *temporary-tool-table*))
        (find-registered-tool-definition normalized-name))))

(defun map-effective-tool-definitions (function)
  "Call FUNCTION with every effective tool definition.
Temporary tools are visited first and same-named global tools are suppressed."
  (let ((seen (make-hash-table :test #'equal)))
    (when *temporary-tool-table*
      (maphash (lambda (name definition)
                 (setf (gethash name seen) t)
                 (funcall function name definition))
               *temporary-tool-table*))
    ;; Snapshot before invoking FUNCTION: visibility checks and prompt
    ;; rendering are extension-facing and must never run under the registry
    ;; lock.  A frame-side package or MCP refresh may safely proceed meanwhile.
    (dolist (entry (registered-tool-definitions-snapshot))
      (unless (gethash (car entry) seen)
        (funcall function (car entry) (cdr entry))))))

(defun register-tool (name description schema execute-fn
                      &key package (execution :background))
  "Register a full-trust tool in *tool-table*.
PACKAGE records the owning RPLACA package for package-scoped exposure."
  (let* ((normalized-name (normalize-tool-name name))
         (definition
           (make-tool-definition
            :name normalized-name
            :description description
            :input-schema schema
            :execution (normalize-agent-tool-execution
                        normalized-name execution nil)
            :execute-fn execute-fn
            :package package)))
    (call-with-tool-registry-lock-for-tables
     (lambda ()
       (setf (gethash normalized-name *tool-table*) definition))
     *tool-table*)
    definition))

(defun tool-argument-value (args name)
  "Return values VALUE and SUPPLIED-P for NAME in Lisp data ARGS."
  (loop :for (key . value) :in (tool-args-alist args)
        :when (tool-key= key name)
          :return (values value t)
        :finally (return (values nil nil))))

(defun agent-tool-arg-keyword (name)
  "Return NAME as a keyword symbol suitable for APPLY keyword calls."
  (intern (string-upcase name) :keyword))

(defun collect-agent-tool-positional-args (metadata args)
  "Collect provider ARGS in METADATA argument order for positional calls."
  (let ((values nil)
        (omitting-optional-tail-p nil))
    (dolist (arg (agent-tool-metadata-args metadata) (nreverse values))
      (let ((name (getf arg :name)))
        (multiple-value-bind (value supplied-p)
            (tool-argument-value args name)
          (cond
            (supplied-p
             (when omitting-optional-tail-p
               (error "Optional tool argument ~A was supplied after an omitted optional argument."
                      name))
             (push value values))
            ((getf arg :required)
             (error "Tool ~A requires argument ~A."
                    (agent-tool-metadata-name metadata) name))
            (t
             (setf omitting-optional-tail-p t))))))))

(defun collect-agent-tool-keyword-args (metadata args)
  "Collect provider ARGS as keyword/value pairs for keyword calls."
  (let ((values nil))
    (dolist (arg (agent-tool-metadata-args metadata) (nreverse values))
      (let ((name (getf arg :name)))
        (multiple-value-bind (value supplied-p)
            (tool-argument-value args name)
          (cond
            (supplied-p
             (push (agent-tool-arg-keyword name) values)
             (push value values))
            ((getf arg :required)
             (error "Tool ~A requires argument ~A."
                    (agent-tool-metadata-name metadata) name))))))))

(defun validate-agent-tool-required-args (metadata args)
  "Signal when any required argument in METADATA is absent from ARGS."
  (dolist (arg (agent-tool-metadata-args metadata))
    (when (getf arg :required)
      (multiple-value-bind (_value supplied-p)
          (tool-argument-value args (getf arg :name))
        (declare (ignore _value))
        (unless supplied-p
          (error "Tool ~A requires argument ~A."
                 (agent-tool-metadata-name metadata)
                 (getf arg :name)))))))

(defun resolve-agent-tool-function (symbol)
  "Return SYMBOL's function binding or signal a clear tool error."
  (unless (fboundp symbol)
    (error "Tool function ~A is not defined." symbol))
  (fdefinition symbol))

(defun resolve-agent-tool-function-designator (designator)
  "Resolve a tool function designator."
  (cond
    ((null designator) nil)
    ((functionp designator) designator)
    ((symbolp designator) (resolve-agent-tool-function designator))
    ((and (consp designator)
          (eq (first designator) 'function)
          (symbolp (second designator)))
     (resolve-agent-tool-function (second designator)))
    (t
     (error "Invalid tool function designator: ~S" designator))))

(defun agent-tool-values-result-string (values)
  "Convert multiple return VALUES from a tagged tool into a tool result string."
  (cond
    ((and (= 1 (length values))
          (stringp (first values)))
     (first values))
    ((= 1 (length values))
     (lisp-data-string (first values)))
    (t
     (lisp-data-string (list :values (length values)
                             :result values)))))

(defun execute-agent-tool-metadata (metadata args)
  "Execute a tagged Lisp function tool with provider ARGS."
  (let ((fn (resolve-agent-tool-function
             (agent-tool-metadata-symbol metadata))))
    (agent-tool-values-result-string
     (multiple-value-list
      (ecase (agent-tool-metadata-call-style metadata)
        (:raw-args
         (validate-agent-tool-required-args metadata args)
         (funcall fn args))
        (:positional
         (apply fn (collect-agent-tool-positional-args metadata args)))
        (:keyword
         (apply fn (collect-agent-tool-keyword-args metadata args)))
        (:command
         (unless *current-tool-buffer*
           (error "Command tool ~A requires an active buffer."
                  (agent-tool-metadata-name metadata)))
         (apply fn *current-tool-buffer*
                (collect-agent-tool-positional-args metadata args))))))))

(defun register-agent-tool-provider-definition (metadata)
  "Register AGENT-TOOL-METADATA in the provider tool table."
  (register-tool
   (agent-tool-metadata-name metadata)
   (agent-tool-metadata-description metadata)
   (agent-tool-metadata-input-schema metadata)
   (lambda (args)
     (execute-agent-tool-metadata metadata args))
   :execution (agent-tool-metadata-execution metadata)
   :package (agent-tool-metadata-package metadata)))

(defun register-agent-tool-provider-definitions ()
  "Register process-global tagged agent tools in the provider tool table.
Package-owned tools are registered when their package entrypoint is loaded."
  (dolist (metadata (list-agent-tool-metadata))
    (unless (agent-tool-metadata-package metadata)
      (register-agent-tool-provider-definition metadata))))

(defun register-package-agent-tool-provider-definitions (package-name)
  "Register provider tools owned by PACKAGE-NAME."
  (let ((name (manifest-package-name package-name)))
    (when name
      (dolist (metadata (list-agent-tool-metadata))
        (when (string= name (or (agent-tool-metadata-package metadata) ""))
          (register-agent-tool-provider-definition metadata))))))

(defun tool-allowed-for-active-run-p (name)
  "Return true when NAME is allowed by *ACTIVE-TOOL-NAMES*."
  (or (null *active-tool-names*)
      (member (normalize-tool-name name) *active-tool-names* :test #'string=)))

(defun caller-agent-name ()
  "Return an agent-name string derived from *CURRENT-CALLER*, or NIL."
  (unless (eq *current-caller* :user)
    (string-downcase (symbol-name *current-caller*))))

(defun tool-visible-in-package-context-p (definition &key buffer agent-name)
  "Return true when DEFINITION is visible for the active package context."
  (let ((package (tool-definition-package definition)))
    (or (null package)
        (package-active-p package
                          :buffer buffer
                          :agent-name (or agent-name
                                          (and buffer
                                               (buffer-agent-name buffer))
                                          (caller-agent-name))))))

(defun tool-provider-callable-p (definition)
  "Return true when DEFINITION may be invoked from a provider turn.

COMMAND-ONLY is a process-affinity/stability designation, not a permission.
The isolated lisp_eval adapter remains provider-callable because its execution
policy validates and moves that one operation into a fresh worker process."
  (or (not (eq :command-only (tool-definition-execution definition)))
      (string= "lisp_eval" (tool-definition-name definition))))

(defun tool-definitions-for-api (&key buffer agent-name)
  "Return provider-callable tool definitions for the active workflow.

Package enablement and *ACTIVE-TOOL-NAMES* compose the workflow surface; they
are not a security boundary."
  (let ((tools nil))
    (map-effective-tool-definitions
     (lambda (name def)
       (declare (ignore name))
       (when (and (tool-provider-callable-p def)
                  (tool-visible-in-package-context-p
                   def
                   :buffer buffer
                   :agent-name agent-name)
                  (tool-allowed-for-active-run-p
                   (tool-definition-name def)))
         (push `((:name . ,(tool-definition-name def))
                 (:description . ,(tool-definition-description def))
                 (:input--schema . ,(tool-definition-input-schema def)))
               tools))))
    (coerce (sort tools #'string<
                  :key (lambda (tool)
                         (cdr (assoc :name tool))))
            'vector)))

(defun rendered-tool-description (tool)
  "Return TOOL's provider name and description values."
  (values (cdr (assoc :name tool))
          (cdr (assoc :description tool))))

(defun render-agent-tools-section
    (&optional (tools (coerce (tool-definitions-for-api) 'list)))
  "Render the active provider tool discovery section for the system prompt."
  (when tools
    (let ((sorted-tools (sort (copy-list tools) #'string<
                              :key (lambda (tool)
                                     (cdr (assoc :name tool))))))
      (with-output-to-string (s)
        (format s "<tools>~%")
        (format s "## Tools~%")
        (format s "Use the provider tools for normal actions. Tool inputs and tool results use Lisp data mode with keyword arguments.~%")
        (format s "Use `lisp_eval` only with mode=isolated for Common Lisp tests or introspection when no exposed tool fits. `live_lisp_eval` runs in the active RPLACA UI image and can hang, corrupt, or terminate the user's session; use it only when live state is essential, prefer read-only forms, and never test destructive or uncertain code with it.~%")
        (format s "### Available tools~%")
        (dolist (tool sorted-tools)
          (multiple-value-bind (name description)
              (rendered-tool-description tool)
            (format s "- ~A: ~A~%" name description)))
        (format s "</tools>")))))

(defvar *tool-execution-journal-max-chars* 20000
  "Maximum result/error characters retained in durable tool execution events.")

(defun bounded-tool-execution-string (value)
  "Return VALUE as a bounded string for durable tool execution events."
  (let ((text (cond
                ((null value) "")
                ((stringp value) value)
                (t (lisp-data-string value)))))
    (if (> (length text) *tool-execution-journal-max-chars*)
        (concatenate 'string
                     (subseq text 0 *tool-execution-journal-max-chars*)
                     (format nil "~%[truncated at ~D characters]"
                             *tool-execution-journal-max-chars*))
        text)))

(defun condition-type-name (condition)
  "Return a readable type name for CONDITION."
  (let ((type (type-of condition)))
    (if (symbolp type)
        (format nil "~A" type)
        (prin1-to-string type))))

(defun tool-execution-caller-name ()
  "Return the current tool caller as a durable string."
  (if (and (boundp '*current-caller*) *current-caller*)
      (string-downcase (symbol-name *current-caller*))
      "unknown"))

(defun record-tool-execution-event (buffer event)
  "Durably record one tool execution EVENT for BUFFER when possible.
Failures while journaling are logged but never abort tool execution."
  (if *tool-effect-recorder*
      (progn
        (funcall *tool-effect-recorder*
                 :journal (copy-runtime-owned-data event))
        event)
      (handler-case
          (progn
            (when buffer
              (ensure-buffer-session buffer)
              (when (buffer-session buffer)
                (append-session-event (buffer-session buffer) event)))
            (file-debug-log "tool-execution"
                            "~A"
                            (bounded-tool-execution-string event))
            event)
        (error (condition)
          (file-debug-log "tool-execution-journal-error"
                          "failed to journal tool event: ~A"
                          condition)
          nil))))

(defun tool-execution-event (phase tool-name args
                             &key buffer tool-id status result condition)
  "Return a durable event describing one tool execution PHASE."
  (declare (ignore buffer))
  `((:event . "tool-execution")
    (:phase . ,phase)
    (:tool-name . ,(normalize-tool-name tool-name))
    ,@(when tool-id `((:tool-id . ,tool-id)))
    (:caller . ,(tool-execution-caller-name))
    (:timestamp . ,(get-universal-time))
    (:input . ,(bounded-tool-execution-string args))
    ,@(when status `((:status . ,status)))
    ,@(when result `((:result . ,(bounded-tool-execution-string result))))
    ,@(when condition
        `((:condition-type . ,(condition-type-name condition))
          (:condition-message . ,(bounded-tool-execution-string
                                  (format nil "~A" condition)))))))

(defun file-checkpoint-tool-p (tool-name)
  "Return true when TOOL-NAME is a built-in file mutation tool."
  (member (normalize-tool-name tool-name) '("write" "edit") :test #'string=))

(defun file-checkpoint-path-argument (args)
  "Return the path argument from ARGS, if any."
  (ignore-errors
    (tool-arg args :path "path")))

(defun file-checkpoint-content-hash (content)
  "Return a stable hexadecimal FNV-1a hash for CONTENT."
  (let ((hash #xCBF29CE484222325)
        (prime #x100000001B3))
    (loop :for char :across (or content "")
          :do (setf hash (logand #xffffffffffffffff
                                 (* (logxor hash (char-code char)) prime))))
    (format nil "~16,'0X" hash)))

(defun file-checkpoint-snapshot (pathname)
  "Return a compact text snapshot for PATHNAME."
  (let ((exists-p (and pathname (probe-file pathname))))
    (if exists-p
        (handler-case
            (let* ((content (uiop:read-file-string pathname))
                   (size (length content)))
              (list :exists-p t
                    :size size
                    :hash (file-checkpoint-content-hash content)
                    :content content))
          (error (condition)
            (list :exists-p t
                  :read-error (format nil "~A" condition))))
        (list :exists-p nil))))

(defun file-checkpoint-snapshot-public (snapshot)
  "Return SNAPSHOT without full file content for durable events."
  (list :exists-p (getf snapshot :exists-p)
        :size (getf snapshot :size)
        :hash (getf snapshot :hash)
        :read-error (getf snapshot :read-error)))

(defun file-checkpoint-snapshot-event-fields (prefix snapshot)
  "Return flat durable event fields for SNAPSHOT under PREFIX."
  (let ((name (string-downcase (symbol-name prefix))))
    (flet ((key (suffix)
             (intern (string-upcase (format nil "~A-~A" name suffix)) :keyword)))
      `((,(key "exists-p") . ,(getf snapshot :exists-p))
        (,(key "size") . ,(getf snapshot :size))
        (,(key "hash") . ,(getf snapshot :hash))
        (,(key "read-error") . ,(getf snapshot :read-error))))))

(defun file-checkpoint-diff (before after)
  "Return a bounded diff from BEFORE to AFTER snapshots when readable."
  (let ((before-content (and (getf before :exists-p)
                             (getf before :content)))
        (after-content (and (getf after :exists-p)
                            (getf after :content))))
    (when (or before-content after-content)
      (bounded-tool-execution-string
       (compute-simple-diff before-content (or after-content ""))))))

(defun file-checkpoint-context (tool-name args)
  "Return checkpoint context for TOOL-NAME and ARGS, or NIL."
  (when (file-checkpoint-tool-p tool-name)
    (let ((path (file-checkpoint-path-argument args)))
      (when path
        (handler-case
            (let ((resolved (lispi:resolve-tool-path path)))
              (list :path path
                    :resolved-path (namestring resolved)))
          (error (condition)
            (list :path path
                  :resolve-error (format nil "~A" condition))))))))

(defun record-file-checkpoint-event
    (buffer phase tool-name args context before
            &key tool-id after status condition)
  "Record one before/after file checkpoint event for a mutating tool."
  (declare (ignore args))
  (when context
    (record-tool-execution-event
     buffer
     `((:event . "file-checkpoint")
       (:phase . ,phase)
       (:tool-name . ,(normalize-tool-name tool-name))
       ,@(when tool-id `((:tool-id . ,tool-id)))
       (:caller . ,(tool-execution-caller-name))
       (:timestamp . ,(get-universal-time))
       (:path . ,(getf context :path))
       ,@(when (getf context :resolved-path)
           `((:resolved-path . ,(getf context :resolved-path))))
       ,@(when (getf context :resolve-error)
           `((:resolve-error . ,(getf context :resolve-error))))
       ,@(when status `((:status . ,status)))
       ,@(when before
           (file-checkpoint-snapshot-event-fields :before before))
       ,@(when after
           (file-checkpoint-snapshot-event-fields :after after))
       ,@(when (and before after)
           `((:diff . ,(file-checkpoint-diff before after))))
       ,@(when condition
           `((:condition-type . ,(condition-type-name condition))
             (:condition-message . ,(bounded-tool-execution-string
                                     (format nil "~A" condition)))))))))

(defun lisp-eval-tool-p (tool-name)
  "Return true when TOOL-NAME evaluates arbitrary Common Lisp."
  (member (normalize-tool-name tool-name)
          '("lisp_eval" "live_lisp_eval")
          :test #'string=))

(defun lisp-eval-recovery-mode (args &optional tool-name)
  "Return the Lisp evaluation mode as a durable string."
  (let ((mode (if (and tool-name
                       (string= "live_lisp_eval"
                                (normalize-tool-name tool-name)))
                  "live"
                  (or (ignore-errors (tool-arg args :mode "mode"))
                      "live"))))
    (cond
      ((symbolp mode) (string-downcase (symbol-name mode)))
      ((stringp mode) (string-downcase mode))
      (t (bounded-tool-execution-string mode)))))

(defun lisp-eval-recovery-context (tool-name args)
  "Return semantic recovery context for lisp_eval ARGS, or NIL."
  (when (lisp-eval-tool-p tool-name)
    (list :mode (lisp-eval-recovery-mode args tool-name)
          :package (or (ignore-errors (tool-arg args :package "package"))
                       *lisp-eval-default-package*)
          :code (or (ignore-errors (tool-arg args :code "code")) ""))))

(defun record-lisp-eval-recovery-event
    (buffer phase tool-name args context &key tool-id status result condition)
  "Record semantic lisp_eval recovery events around live or worker evals.
The before event is intentionally durable before evaluation starts, so startup
recovery can detect an interrupted live eval without reducing eval capability."
  (declare (ignore args))
  (when context
    (record-tool-execution-event
     buffer
     `((:event . "lisp-eval-checkpoint")
       (:phase . ,phase)
       (:tool-name . ,(normalize-tool-name tool-name))
       ,@(when tool-id `((:tool-id . ,tool-id)))
       (:caller . ,(tool-execution-caller-name))
       (:timestamp . ,(get-universal-time))
       (:mode . ,(getf context :mode))
       (:package . ,(getf context :package))
       (:code . ,(bounded-tool-execution-string (getf context :code)))
       ,@(when status `((:status . ,status)))
       ,@(when result
           `((:result . ,(bounded-tool-execution-string result))))
       ,@(when condition
           `((:condition-type . ,(condition-type-name condition))
             (:condition-message . ,(bounded-tool-execution-string
                                     (format nil "~A" condition)))))))))

(defun execute-tool-safely (name args &key buffer tool-id)
  "Execute tool NAME with ARGS and return a provider-compatible result string.
All agent tool paths should use this wrapper so tool start/result/error events
are journaled before control returns to the provider loop."
  (let* ((buf (or buffer *current-tool-buffer*))
         (lispi:*tool-working-directory*
           (or (and buf (buffer-working-directory buf))
               lispi:*tool-working-directory*))
         (eval-context (lisp-eval-recovery-context name args))
         (checkpoint-context (file-checkpoint-context name args))
         (checkpoint-before
           (and checkpoint-context
                (getf checkpoint-context :resolved-path)
                (file-checkpoint-snapshot
                 (pathname (getf checkpoint-context :resolved-path))))))
    (record-tool-execution-event
     buf
     (tool-execution-event "start" name args
                           :buffer buf
                           :tool-id tool-id))
    (when eval-context
      (record-lisp-eval-recovery-event
       buf "before" name args eval-context
       :tool-id tool-id))
    (when checkpoint-context
      (record-file-checkpoint-event
       buf "before" name args checkpoint-context checkpoint-before
       :tool-id tool-id))
    (handler-case
        (let ((result (execute-tool name args)))
          (when eval-context
            (record-lisp-eval-recovery-event
             buf "after" name args eval-context
             :tool-id tool-id
             :status "ok"
             :result result))
          (when checkpoint-context
            (record-file-checkpoint-event
             buf "after" name args checkpoint-context checkpoint-before
             :tool-id tool-id
             :status "ok"
             :after (and (getf checkpoint-context :resolved-path)
                         (file-checkpoint-snapshot
                          (pathname (getf checkpoint-context
                                          :resolved-path))))))
          (record-tool-execution-event
           buf
           (tool-execution-event "result" name args
                                 :buffer buf
                                 :tool-id tool-id
                                 :status "ok"
                                 :result result))
          result)
      (error (condition)
        (let ((result (tool-error-result-data condition)))
          (when eval-context
            (record-lisp-eval-recovery-event
             buf "after" name args eval-context
             :tool-id tool-id
             :status "error"
             :result result
             :condition condition))
          (when checkpoint-context
            (record-file-checkpoint-event
             buf "after" name args checkpoint-context checkpoint-before
             :tool-id tool-id
             :status "error"
             :after (and (getf checkpoint-context :resolved-path)
                         (file-checkpoint-snapshot
                          (pathname (getf checkpoint-context
                                          :resolved-path))))
             :condition condition))
          (record-tool-execution-event
           buf
           (tool-execution-event "result" name args
                                 :buffer buf
                                 :tool-id tool-id
                                 :status "error"
                                 :result result
                                 :condition condition))
          result)))))

(defun interactive-tool-execution-policy (name args)
  "Return the owner policy for interactive tool NAME and ARGS.

Provider-driven lisp_eval is restricted to its isolated worker mode.
live_lisp_eval is an explicit dangerous escape hatch that runs frame-owned in
the active UI image, where arbitrary code can hang, corrupt, or terminate the
user's session.  Explicit UI actions may also retain Lisp code and run it later
from a user gesture."
  (let* ((normalized-name (normalize-tool-name name))
         (definition (effective-tool-definition normalized-name))
         (declared (and definition (tool-definition-execution definition))))
    (cond
      ((string= normalized-name "live_lisp_eval")
       :frame)
      ((and (string= normalized-name "lisp_eval")
            (string= (lisp-eval-recovery-mode args) "isolated"))
       :background)
      ((string= normalized-name "lisp_eval")
       :command-only)
      (t
       (or declared :background)))))

(defun capture-tool-execution-plan (name buffer)
  "Capture an immutable tool definition and visibility snapshot for a worker."
  (let* ((normalized-name (normalize-tool-name name))
         (definition (effective-tool-definition normalized-name))
         (active-allowed-p (not (null (tool-allowed-for-active-run-p
                                       normalized-name))))
         (visible-p
           (not (null
                 (and definition
                      (tool-visible-in-package-context-p
                       definition
                       :buffer buffer
                       :agent-name (caller-agent-name)))))))
    (make-tool-execution-plan
     normalized-name
     definition
     active-allowed-p
     visible-p)))

(defun matching-current-tool-execution-plan (normalized-name)
  "Return the current immutable plan when it belongs to NORMALIZED-NAME."
  (and *current-tool-execution-plan*
       (string= normalized-name
                (tool-execution-plan-name *current-tool-execution-plan*))
       *current-tool-execution-plan*))

(defun run-or-defer-tool-hook (phase normalized-name args &optional result)
  "Run a tool hook now, or record it for frame-process application."
  (ecase phase
    (:before
     ;; Before hooks retain true veto semantics and are never deferred.  The
     ;; caller's tool wrapper contains a signaling veto as this call's error
     ;; result instead of letting it escape into CLIM.
     (dolist (hook *before-tool-hook*)
       (funcall (resolve-hook-function hook) normalized-name args)))
    (:after
     (if *tool-effect-recorder*
         (funcall *tool-effect-recorder*
                  :hook
                  (make-tool-hook-effect
                   phase
                   (copy-seq normalized-name)
                   (copy-runtime-owned-data args)
                   (copy-runtime-owned-data result)))
         (run-hook-with-args '*after-tool-hook*
                             normalized-name args result)))))

(defun current-tool-dispatch-values (name)
  "Return frame-captured or live dispatch values for tool NAME."
  (let* ((normalized-name (normalize-tool-name name))
         (plan (matching-current-tool-execution-plan normalized-name))
         (definition (if plan
                         (tool-execution-plan-definition plan)
                         (effective-tool-definition normalized-name))))
    (values
     normalized-name
     definition
     (if plan
         (tool-execution-plan-active-allowed-p plan)
         (not (null (tool-allowed-for-active-run-p normalized-name))))
     (if plan
         (tool-execution-plan-visible-p plan)
         (not (null
               (and definition
                    (tool-visible-in-package-context-p
                     definition
                     :buffer *current-tool-buffer*
                     :agent-name (caller-agent-name)))))))))

(defun validate-tool-execution-dispatch (name)
  "Validate workflow exposure for NAME and return its captured definition."
  (multiple-value-bind
        (normalized-name definition active-allowed-p visible-p)
      (current-tool-dispatch-values name)
    (unless active-allowed-p
      (error "Tool ~A is not exposed for this run" name))
    (unless definition
      (error "Unknown tool: ~A" normalized-name))
    (unless visible-p
      (error "Tool ~A belongs to an inactive package" normalized-name))
    (values definition normalized-name)))

(defun preflight-background-tool-execution
    (name args buffer caller execution-plan)
  "Validate NAME and run its veto-capable before hook on the frame process."
  (let ((*current-caller* caller)
        (*current-tool-buffer* buffer)
        (*current-tool-execution-plan* execution-plan))
    (multiple-value-bind (definition normalized-name)
        (validate-tool-execution-dispatch name)
      (declare (ignore definition))
      (run-or-defer-tool-hook :before normalized-name args)
      t)))

(defun execute-tool (name args)
  "Execute tool NAME with ARGS (an alist of parameter values).
Returns a string result or signals an error."
  (multiple-value-bind (definition normalized-name)
      (if *tool-execution-preflight-completed-p*
          (let* ((normalized (normalize-tool-name name))
                 (plan (matching-current-tool-execution-plan normalized)))
            (unless (and plan (tool-execution-plan-definition plan))
              (error "Missing preflight plan for tool ~A" normalized))
            (values (tool-execution-plan-definition plan)
                    normalized))
          (validate-tool-execution-dispatch name))
    (unless *tool-execution-preflight-completed-p*
      (run-or-defer-tool-hook :before normalized-name args))
    (let ((result (funcall (tool-definition-execute-fn definition) args)))
      (run-or-defer-tool-hook :after normalized-name args result)
      result)))

(defun format-tool-call-sexpr (name args)
  "Format a tool call as a raw s-expression string.
E.g., (lisp_eval :code \"(list-functions)\")"
  (let ((arg-alist (tool-args-alist args)))
    (with-output-to-string (s)
      (format s "(~A" name)
      (loop :for (k . v) :in arg-alist
            :do (format s " :~A ~S" (tool-key-name k) v))
      (write-char #\) s))))

(defun tool-schema-property (key schema-props)
  "Return KEY's property metadata from SCHEMA-PROPS."
  (loop :for (schema-key . property) :in schema-props
        :when (tool-key= key schema-key)
          :return property))

(defun format-tool-call-expanded (name args)
  "Format a tool call with expanded parameter descriptions.
E.g., (lisp_eval
        :code \"(list-functions)\")  ; The Common Lisp code to evaluate"
  (let ((def (effective-tool-definition name))
        (schema-props nil)
        (arg-alist (tool-args-alist args)))
    ;; Extract property descriptions from schema
    (when def
      (let ((schema (tool-definition-input-schema def)))
        (when schema
          (let ((props (cdr (assoc :properties schema))))
            (when props
              (setf schema-props props))))))
    (with-output-to-string (s)
      (format s "(~A" name)
      (loop :for (k . v) :in arg-alist
            :for param-name := (tool-key-name k)
            :for desc := (let ((prop (tool-schema-property k schema-props)))
                           (when prop (cdr (assoc :description prop))))
            :do (format s "~%  :~A ~S" param-name v)
                (when desc
                  (format s "  ; ~A" desc)))
      (write-char #\) s))))

;;; --------------------------------------------------------------------------
;;; HTTP Fetch Tool
;;; --------------------------------------------------------------------------

(defun execute-http-fetch (args)
  "Fetch content from an HTTP/HTTPS URL. Returns the response body as text."
  (let ((url (cdr (assoc :url args)))
        (max-chars (or (cdr (assoc :max--chars args)) *http-fetch-max-chars*)))
    (unless url
      (error "url parameter is required"))
    (unless (or (alexandria:starts-with-subseq "http://" url)
                (alexandria:starts-with-subseq "https://" url))
      (error "Only http:// and https:// URLs are supported, got: ~A" url))
    (multiple-value-bind (body status-code)
        (drakma:http-request url
                             :method :get
                             :want-stream nil
                             :force-binary nil
                             :connection-timeout *http-connection-timeout*
                             :user-agent *http-user-agent*)
      (let ((body-string (if (stringp body)
                             body
                             (flexi-streams:octets-to-string
                              body :external-format :utf-8))))
        (let ((truncated (if (> (length body-string) max-chars)
                             (subseq body-string 0 max-chars)
                             body-string)))
          (cl-json:encode-json-to-string
           `((:url . ,url)
             (:status . ,status-code)
             (:length . ,(length body-string))
             (:truncated . ,(> (length body-string) max-chars))
             (:content . ,truncated))))))))

;;; --------------------------------------------------------------------------
;;; Shell Exec Tool
;;; --------------------------------------------------------------------------

(defun execute-shell-exec (args)
  "Execute a bounded, cancellable shell command from the tool working directory."
  (let* ((command (cdr (assoc :command args)))
         (timeout (or (cdr (assoc :timeout args)) *shell-exec-default-timeout*))
         (working-directory (lispi:tool-working-directory-pathname)))
    (unless command
      (error "command parameter is required"))
    (let ((result
            (run-interactive-subprocess
             command
             :directory working-directory
             :timeout timeout
             :output-limit *interactive-subprocess-output-limit*)))
      (cl-json:encode-json-to-string
       `((:command . ,command)
         (:exit--code . ,(getf result :exit-code))
         (:timed--out . ,(getf result :timed-out-p))
         (:cancelled . ,(getf result :cancelled-p))
         (:stdout--truncated . ,(getf result :stdout-truncated-p))
         (:stderr--truncated . ,(getf result :stderr-truncated-p))
         (:stdout . ,(getf result :stdout))
         (:stderr . ,(getf result :stderr)))))))

;;; --------------------------------------------------------------------------
;;; Tool Registration
;;; --------------------------------------------------------------------------

(defun recovery-event-value (event key)
  "Return KEY's decoded JSON value from EVENT."
  (cdr (assoc key event)))

(defun recovery-event-kind-p (event kind)
  "Return true when EVENT matches recovery event KIND."
  (let ((event-name (recovery-event-value event :event)))
    (or (string= kind "all")
        (and (string= kind "tool")
             (string= event-name "tool-execution"))
        (and (string= kind "file")
             (string= event-name "file-checkpoint"))
        (and (string= kind "lisp-eval")
             (string= event-name "lisp-eval-checkpoint")))))

(defun recovery-event-key (event)
  "Return a stable-enough pairing key for before/after recovery events."
  (or (recovery-event-value event :tool-id)
      (format nil "~A/~A/~A"
              (recovery-event-value event :tool-name)
              (recovery-event-value event :timestamp)
              (recovery-event-value event :code))))

(defun recovery-summarize-event (event index)
  "Return a compact Lisp data summary for recovery EVENT."
  (list :index index
        :event (recovery-event-value event :event)
        :phase (recovery-event-value event :phase)
        :status (recovery-event-value event :status)
        :tool-name (recovery-event-value event :tool-name)
        :tool-id (recovery-event-value event :tool-id)
        :timestamp (recovery-event-value event :timestamp)
        :mode (recovery-event-value event :mode)
        :package (recovery-event-value event :package)
        :path (recovery-event-value event :path)
        :code (recovery-event-value event :code)
        :condition-message (recovery-event-value event :condition-message)
        :diff (recovery-event-value event :diff)))

(defun recovery-pending-lisp-evals (events)
  "Return lisp_eval before checkpoints without a matching after checkpoint."
  (let ((pending (make-hash-table :test #'equal)))
    (dolist (event events)
      (when (string= "lisp-eval-checkpoint"
                     (or (recovery-event-value event :event) ""))
        (let ((key (recovery-event-key event))
              (phase (recovery-event-value event :phase)))
          (cond
            ((string= phase "before")
             (setf (gethash key pending) event))
            ((string= phase "after")
             (remhash key pending))))))
    (let ((result nil))
      (maphash (lambda (_ event)
                 (declare (ignore _))
                 (push event result))
               pending)
      (sort result #'< :key (lambda (event)
                              (or (recovery-event-value event :timestamp) 0))))))

(defun recovery-relevant-events (events kind)
  "Return transcript EVENTS relevant to recovery KIND."
  (remove-if-not (lambda (event)
                   (recovery-event-kind-p event kind))
                 events))

(defun execute-recovery-list (args)
  "List recent recovery journal events for the current buffer session."
  (let* ((buffer (or *current-tool-buffer*
                     (ignore-errors (current-buffer))))
         (limit (or (tool-arg args :limit "limit") 20))
         (kind (string-downcase (or (tool-arg args :kind "kind") "all")))
         (pending-only (tool-arg args :pending-only "pending-only")))
    (unless buffer
      (error "recovery_list requires an active buffer"))
    (unless (member kind '("all" "tool" "file" "lisp-eval") :test #'string=)
      (error "kind must be one of all, tool, file, or lisp-eval"))
    (ensure-buffer-session buffer)
    (let* ((events (session-transcript-events (buffer-session buffer)))
           (pending (recovery-pending-lisp-evals events))
           (relevant (if pending-only
                         pending
                         (recovery-relevant-events events kind)))
           (recent (subseq (reverse relevant)
                           0
                           (min (max 0 limit) (length relevant)))))
      (lisp-data-string
       (list :session-id (session-id (buffer-session buffer))
             :kind kind
             :pending-lisp-evals
             (mapcar #'recovery-event-key pending)
             :event-count (length relevant)
             :events (loop :for event :in recent
                           :for index :from 0
                           :collect (recovery-summarize-event event index)))))))

(deftool execute-recovery-list
  :name "recovery_list"
  :description "List recent durable tool, file checkpoint, and lisp_eval recovery journal events for the current session. Use this after errors, crashes, interrupted live evals, or risky self-modification."
  :call-style :raw-args
  :args ((kind :type "string"
               :required nil
               :description "Optional filter: all, tool, file, or lisp-eval. Default: all.")
         (limit :type "integer"
                :required nil
                :description "Maximum number of recent events to return. Default: 20.")
         (pending-only :type "boolean"
                       :required nil
                       :description "If true, return only lisp_eval before checkpoints without matching after checkpoints.")))

(deftool execute-lisp-eval
  :name "lisp_eval"
  :description "Evaluate one Common Lisp form in an isolated SBCL worker. Provider calls must pass mode=isolated; use live_lisp_eval only when access to the running RPLACA image is essential."
  :call-style :raw-args
  :execution :command-only
  :args ((code :type "string"
               :description "Lisp data :code, one Common Lisp form to read and evaluate.")
         (package :type "string"
                  :required nil
                  :description "Lisp data :package, the package name used while reading and evaluating :code. Default: RPLACA.")
         (mode :type "string"
               :required nil
               :description "Provider calls must specify isolated, which evaluates in a fresh SBCL worker process. Live mode is refused; use live_lisp_eval explicitly instead.")
         (timeout :type "integer"
                  :required nil
                  :description "Timeout in seconds for isolated mode. Default: 10.")))

(defun execute-live-lisp-eval-tool (args)
  "Dangerously evaluate one form in the active RPLACA image."
  (let ((code (tool-arg args :code "code"))
        (package-name (or (tool-arg args :package "package")
                          *lisp-eval-default-package*)))
    (unless code
      (error "code parameter is required"))
    (lispi::execute-live-lisp-eval code package-name)))

(deftool execute-live-lisp-eval-tool
  :name "live_lisp_eval"
  :description "DANGER: Evaluate one Common Lisp form directly in the running RPLACA UI image and return its values/output. Use only when live application state is essential. Prefer read-only introspection and small bounded forms. This has no timeout or isolation: code can mutate or corrupt state, block the UI, deadlock, exhaust resources, terminate RPLACA, or lose the user's unsaved session. Never use it to test uncertain or destructive code; use lisp_eval first whenever possible."
  :call-style :raw-args
  :execution :frame
  :args ((code :type "string"
               :description "Arbitrary Common Lisp code to evaluate in the live RPLACA process. Keep it read-only, bounded, and non-blocking whenever possible.")
         (package :type "string"
                  :required nil
                  :description "Package used while reading and evaluating code. Default: RPLACA.")))

(defun init-tools ()
  "Register the default rplaca built-in tools.
This removes any previously registered built-in entries, then re-registers
tagged agent tools. User-added tools remain untouched."

  (remove-registered-tools *built-in-tool-names*)

  (setf *lisp-eval-default-package* "RPLACA")
  (register-agent-tool-provider-definitions))
