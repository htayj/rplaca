(in-package :rplaca)

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+sbcl
  (require :sb-bsd-sockets))

;;; --------------------------------------------------------------------------
;;; Configuration
;;; --------------------------------------------------------------------------

(defvar *default-provider* :zai
  "The default LLM provider to use when no agent-specific override is set.
Must be a keyword matching a known provider (:openai-codex, :zai, :openrouter).")

(defvar *default-model* "glm-5.2"
  "The default model to use when no agent-specific or provider-fallback model
is configured. Should be a valid model name for *default-provider*.")

(defvar *e2e-model* "e2e-model"
  "Deterministic no-network model name used only by GUI E2E runs.")

(defvar *e2e-provider-enabled-override* nil
  "Test override for enabling the deterministic E2E provider.")

(defparameter +e2e-hello-sentinel+ "RPLACA_E2E_HELLO_SENTINEL"
  "Stable text that the deterministic E2E provider emits for hello prompts.")

(defun e2e-provider-enabled-p ()
  "Return true when the deterministic no-network E2E provider is enabled."
  (or *e2e-provider-enabled-override*
      (env-truthy-p "RPLACA_E2E_PROVIDER")))

(defvar *default-max-tokens* 8192
  "Default maximum tokens for LLM responses across all providers.")

(defvar *openai-codex-model* "gpt-5.6-sol"
  "The OpenAI Codex model to use for chat completions.")

(defvar *openai-codex-api-base-url* "https://api.openai.com/v1"
  "The OpenAI API base URL used for API-key style Codex requests.")

(defvar *openai-codex-chatgpt-base-url* "https://chatgpt.com/backend-api/codex"
  "The ChatGPT backend base URL used for Codex ChatGPT OAuth requests.")

(defvar *openai-codex-reasoning-summary* "detailed"
  "Reasoning summary mode requested from OpenAI Codex Responses calls.
Set to NIL to avoid requesting provider-supplied reasoning summaries.")

(defvar *openai-codex-prompt-cache-key* nil
  "Optional prompt_cache_key sent with OpenAI Codex Responses requests.")

(defvar *openai-codex-prompt-cache-retention* nil
  "Optional prompt_cache_retention sent with OpenAI Codex Responses requests.")

;;; Z.AI (Zhipu AI) Configuration
(defvar *zai-model* "glm-5.2"
  "The Z.AI model to use for chat completions.
GLM-5.2 is the current coding model with a 1M context window.")

(defvar *zai-api-url* "https://api.z.ai/api/coding/paas/v4/chat/completions"
  "The Z.AI Chat Completions API endpoint for GLM Coding plan subscribers.
Uses the coding-specific endpoint for subscription-based access.")

;;; OpenRouter Configuration
(defvar *openrouter-model* "openai/gpt-5.6-sol"
  "The default OpenRouter model to use for chat completions.
Model names follow the 'provider/model-name' format (e.g. 'openai/gpt-5.6-sol',
'google/gemini-3.6-flash', 'z-ai/glm-5.2'). These are OpenRouter
model identifiers and do not imply direct Anthropic provider support.")

(defvar *openrouter-api-url* "https://openrouter.ai/api/v1/chat/completions"
  "The OpenRouter Chat Completions API endpoint.
OpenRouter normalizes to the OpenAI-compatible chat completions schema.")

(defvar *openrouter-models-url* "https://openrouter.ai/api/v1/models"
  "The OpenRouter models listing endpoint.
Returns the full catalog of available models.")

(defvar *openrouter-env-var* "OPENROUTER_API_KEY"
  "Environment variable name for the OpenRouter API key.
When set, this takes highest priority over the static token file.")

(defvar *openrouter-cached-models* nil
  "Cached list of OpenRouter model ID strings fetched from the API.
Populated on first call to fetch-openrouter-models.  Set to nil to force refresh.")

(defvar *provider-http-max-retries* 3
  "Maximum retries for transient provider HTTP failures.")

(defvar *provider-http-initial-backoff-seconds* 0.5
  "Initial delay before retrying a transient provider HTTP failure.")

(defvar *provider-http-backoff-multiplier* 2.0
  "Multiplier applied to provider HTTP retry delays.")

(defvar *provider-http-max-backoff-seconds* 8.0
  "Maximum delay before retrying a transient provider HTTP failure.")

(defvar *provider-http-connection-timeout-seconds* 20
  "Maximum seconds for one provider TCP connect/header wait in Drakma.")

(defvar *provider-http-error-body-max-characters* 65536
  "Maximum characters retained from a provider HTTP error response body.")

(defvar *provider-http-cancel-poll-seconds* 0.05
  "Maximum cancellation latency while using the default retry sleeper.")

(defvar *provider-http-sleep-function* #'sleep
  "Function used to sleep between provider HTTP retries.")

;;; OpenAI Codex OAuth 2.0 Configuration
(defvar *openai-oauth-client-id* "app_EMoamEEZ73f0CkXaXp7hrann"
  "OpenAI Codex OAuth 2.0 public client identifier.")

(defvar *openai-oauth-auth-url* "https://auth.openai.com/oauth/authorize"
  "OpenAI OAuth 2.0 authorization endpoint.")

(defvar *openai-oauth-token-url* "https://auth.openai.com/oauth/token"
  "OpenAI OAuth 2.0 token exchange endpoint.")

(defvar *openai-oauth-default-port* 1455
  "Preferred localhost port for the OpenAI Codex OAuth callback server.")

(defvar *openai-oauth-callback-path* "/auth/callback"
  "HTTP path served by the local OpenAI Codex OAuth callback server.")

(defvar *openai-oauth-max-rejected-callback-requests* 8
  "Maximum wrong-method/path requests served before the OAuth flow fails.

The localhost listener remains available after incidental probes such as a
favicon request, but a bounded rejection budget prevents an unrelated local
client from retaining the callback worker indefinitely.")

(defvar *openai-oauth-redirect-uri*
  (format nil "http://localhost:~D~A"
          *openai-oauth-default-port*
          *openai-oauth-callback-path*)
  "Default OAuth redirect URI used when the preferred callback port is available.")

(defvar *openai-oauth-scopes*
  "openid profile email offline_access api.connectors.read api.connectors.invoke"
  "OAuth scopes requested from OpenAI, aligned with Codex.")

(defvar *openai-oauth-originator* "codex_cli_rs"
  "Originator value sent in the OpenAI Codex OAuth browser flow and API requests.")

(defparameter +default-codex-auth-path+
  (merge-pathnames #P".codex/auth.json" (user-homedir-pathname))
  "Default path to the shared Codex auth.json credential store.")

(defvar *codex-auth-path* +default-codex-auth-path+
  "Path to the shared Codex auth.json credential store.")

(defparameter +default-personality-prompt-path+
  (merge-pathnames #P".config/rplaca/system-prompt.txt" (user-homedir-pathname))
  "Default path for the optional personality prompt file.")

(defparameter +legacy-personality-prompt-path+
  (merge-pathnames #P".config/clawmacs/system-prompt.txt" (user-homedir-pathname))
  "Legacy read-only fallback for the optional personality prompt file.")

(defvar *personality-prompt-path*
  +default-personality-prompt-path+
  "Path to an optional personality prompt file.")

(defparameter +default-agent-defaults-path+
  (merge-pathnames #P".config/rplaca/agent-defaults.json" (user-homedir-pathname))
  "Canonical path to the persisted agent defaults registry.")

(defparameter +legacy-agent-defaults-path+
  (merge-pathnames #P".config/clawmacs/agent-defaults.json" (user-homedir-pathname))
  "Legacy read-only fallback for the persisted agent defaults registry.")

(defvar *agent-defaults-path* +default-agent-defaults-path+
  "Path to the persisted agent defaults registry.")

(defvar *agent-defaults-registry* nil
  "Memoized agent defaults registry.")

(defvar *agent-definition-registry* (make-hash-table :test #'equal)
  "Programmatic agent definitions keyed by downcased agent name.")

(defvar *process-agent-definition-registry* *agent-definition-registry*
  "Process-global agent registry, distinct from dynamic test bindings.")

(defvar *agent-definition-registry-lock*
  (bt:make-lock "rplaca agent definition registry")
  "Lock guarding the process-global agent definition registry.")

(defun call-with-agent-definition-registry-lock (function &optional
                                                            (table
                                                              *agent-definition-registry*))
  "Call FUNCTION under the process registry lock, or directly for private TABLE."
  (if (eq table *process-agent-definition-registry*)
      (bt:with-lock-held (*agent-definition-registry-lock*)
        (funcall function))
      (funcall function)))

(defun agent-definition-registry-snapshot ()
  "Return a stable alist snapshot of the currently bound agent registry."
  (call-with-agent-definition-registry-lock
   (lambda ()
     (let ((entries nil))
       (maphash (lambda (name definition)
                  (push (cons name definition) entries))
                *agent-definition-registry*)
       entries))))

(defvar *agent-prompt-overrides* nil
  "Dynamic prompt overrides keyed by normalized agent name.
Each entry is (NAME-KEY . PLIST) and is intended for transient subagent runs.")

(defstruct agent-definition
  "Programmatic agent definition used for per-buffer routing and prompt composition."
  name
  provider
  model
  think-level
  core-prompt
  personality-prompt
  tool-names
  package)

(defparameter +default-core-system-prompt+
  "You are an expert coding assistant operating inside rplaca, a Lisp-native McCLIM agent workbench.
You help users by using provider tools to inspect files, edit code, write files, search projects, and verify Common Lisp changes in isolated workers.

Tool calls and tool results use Lisp data mode with keyword arguments such as :path, :content, :old-text, and :new-text.

Guidelines:
- Prefer provider tools for normal work.
- Use read to examine files before editing.
- Use find to locate files by name.
- Use grep to locate literal text in file contents.
- Use edit for precise changes when :old-text can match exactly once.
- Use write for new files or complete rewrites.
- write and edit reject content that leaves Lisp parentheses unbalanced.
- Use lisp_eval with :mode \"isolated\" for Common Lisp tests or introspection when no exposed tool fits.
- live_lisp_eval is a dangerous escape hatch into the running RPLACA UI image. Use it only when live state is essential; prefer read-only, bounded, non-blocking forms. It has no timeout or isolation and can corrupt state, hang or terminate RPLACA, and lose the user's unsaved session. Test uncertain code with isolated lisp_eval first.
- Return Lisp values from Lisp eval tools; prefer (format nil ...) over printing when you need a composed string.
- Be concise in user-facing replies.
- Show file paths clearly when working with files.
- To display a local image to the user, put a Markdown image link on its own line, such as `![alt text](relative/path.png)`."
  "Built-in rplaca operating instructions inserted ahead of the personality prompt.")

(defvar *default-core-system-prompt*
  +default-core-system-prompt+
  "Default rplaca operating instructions inserted ahead of the personality prompt.")

(defparameter +default-personality-prompt+
  "You are a helpful assistant. Keep private reasoning private. Use normal assistant text only
for direct user-facing replies and concise explanations after you have done the work."
  "Built-in default personality prompt inserted after the rplaca core system prompt.")

(defvar *default-personality-prompt*
  +default-personality-prompt+
  "Default personality prompt inserted after the rplaca core system prompt.
Users may override this via *personality-prompt-path* or init.lisp.")

(defvar *system-prompt-buffer* nil
  "Buffer whose state is being used while building a system prompt.")

(defvar *boot-file-names*
  '("AGENTS.md" "SOUL.md" "USER.md" "IDENTITY.md" "TOOLS.md")
  "Boot markdown files to load, in order. Checked in the active working
directory's ancestors and ~/.config/rplaca/. Compatible with OpenClaw
workspace conventions.")

(defparameter +default-global-boot-directory+
  (merge-pathnames #P".config/rplaca/" (user-homedir-pathname))
  "Canonical global boot-instruction directory.")

(defvar *global-boot-directory* +default-global-boot-directory+
  "Configured global boot-instruction directory.")

(defvar *legacy-global-boot-directory*
  (merge-pathnames #P".config/clawmacs/" (user-homedir-pathname))
  "Legacy read-only global boot-instruction directory.")

(defun boot-directory-has-instructions-p (directory)
  "Return true when DIRECTORY contains any relevant global boot file."
  (some (lambda (name)
          (probe-file (merge-pathnames name directory)))
        *boot-file-names*))

(defun selected-global-boot-directory ()
  "Select one global boot root based on relevant files, not directory existence."
  (let ((canonical-p
          (boot-directory-has-instructions-p *global-boot-directory*))
        (legacy-p
          (boot-directory-has-instructions-p
           *legacy-global-boot-directory*))
        (real-probe *migration-path-probe-function*))
    (let ((*migration-path-probe-function*
            (lambda (path)
              (cond
                ((equal (pathname path)
                        (pathname *global-boot-directory*))
                 canonical-p)
                ((equal (pathname path)
                        (pathname *legacy-global-boot-directory*))
                 legacy-p)
                (t (funcall real-probe path))))))
      (configured-migration-read-path
       *global-boot-directory*
       +default-global-boot-directory+
       *legacy-global-boot-directory*
       :label "global instruction directory"))))

(defun load-personality-prompt-file (&optional (path *personality-prompt-path*))
  "Load PATH into the default personality prompt when the file exists.
Returns the trimmed prompt text on success, or NIL when PATH is NIL or missing."
  (let* ((selected-path
           (and path
                (configured-migration-read-path
                 path
                 +default-personality-prompt-path+
                 +legacy-personality-prompt-path+
                 :label "personality prompt")))
         (prompt-path (and selected-path (probe-file selected-path))))
    (when prompt-path
      (let ((prompt (string-trim '(#\Space #\Tab #\Newline #\Return)
                                 (uiop:read-file-string prompt-path))))
        (setf *default-personality-prompt* prompt)
        prompt))))

(defun normalize-boot-directory (directory)
  "Return DIRECTORY as a directory pathname for boot-file discovery."
  (let* ((path (cond
                 ((pathnamep directory) directory)
                 ((stringp directory) (pathname directory))
                 ((null directory) (truename "."))
                 (t (error "Invalid boot directory: ~S" directory))))
         (directory-path (uiop:ensure-directory-pathname path)))
    (handler-case
        (uiop:ensure-directory-pathname (truename directory-path))
      (file-error ()
        directory-path))))

(defun boot-file-parent-directory (directory)
  "Return DIRECTORY's parent directory, or NIL at the filesystem root."
  (let* ((path (normalize-boot-directory directory))
         (components (pathname-directory path)))
    (when (and (consp components) (> (length components) 1))
      (make-pathname :host (pathname-host path)
                     :device (pathname-device path)
                     :directory (butlast components)
                     :name nil
                     :type nil
                     :version nil
                     :defaults path))))

(defun boot-file-ancestor-directories (directory)
  "Return DIRECTORY and its ancestors in root-to-leaf order."
  (let ((directories nil)
        (current (normalize-boot-directory directory)))
    (loop :while current
          :do (push current directories)
              (setf current (boot-file-parent-directory current)))
    directories))

(defun boot-file-paths-for-name (name directory global-directory)
  "Return instruction file paths for NAME that apply to DIRECTORY."
  (let ((local-paths
          (loop :for ancestor :in (boot-file-ancestor-directories directory)
                :for path := (probe-file (merge-pathnames name ancestor))
                :when path
                  :collect path)))
    (or local-paths
        (let ((global-path (probe-file (merge-pathnames name global-directory))))
          (and global-path (list global-path))))))

(defun boot-file-containing-directory (path)
  "Return the directory containing boot file PATH."
  (make-pathname :host (pathname-host path)
                 :device (pathname-device path)
                 :directory (pathname-directory path)
                 :name nil
                 :type nil
                 :version nil
                 :defaults path))

(defun format-boot-file-instructions (path)
  "Return PATH's contents wrapped as explicit agent instructions."
  (let ((contents (string-trim '(#\Space #\Tab #\Newline #\Return)
                               (uiop:read-file-string path))))
    (unless (blank-string-p contents)
      (format nil "# ~A instructions for ~A~%~%<INSTRUCTIONS>~%~A~%</INSTRUCTIONS>"
              (file-namestring path)
              (namestring (boot-file-containing-directory path))
              contents))))

(defun current-system-prompt-directory ()
  "Return the active buffer working directory, or the process directory."
  (normalize-boot-directory
   (if *system-prompt-buffer*
       (buffer-working-directory *system-prompt-buffer*)
       (truename "."))))

(defun load-boot-files (&key directory)
  "Load boot MD instruction files for DIRECTORY and ~/.config/rplaca/.
Returns a concatenated string, or nil if no files found.
Files are loaded in the order specified by *boot-file-names*. For each name,
project-local files are discovered from DIRECTORY's ancestors in root-to-leaf
order. The global file is used only when no project-local file of that name
applies."
  (let ((parts nil)
        (global-dir (selected-global-boot-directory))
        (local-dir (normalize-boot-directory
                    (or directory (current-system-prompt-directory)))))
    (dolist (name *boot-file-names*)
      (dolist (path (boot-file-paths-for-name name local-dir global-dir))
        (let ((instructions (format-boot-file-instructions path)))
          (when instructions
            (push instructions parts)))))
    (when parts
      (format nil "~{~A~^~%~%---~%~%~}" (nreverse parts)))))

(defun agent-prompt-override (agent-name)
  "Return the dynamic prompt override plist for AGENT-NAME, or NIL."
  (let ((name-key (normalize-agent-name-key agent-name)))
    (cdr (assoc name-key *agent-prompt-overrides* :test #'string=))))

(defun agent-prompt-override-value (agent-name key)
  "Return dynamic prompt override KEY for AGENT-NAME, or NIL."
  (let ((override (agent-prompt-override agent-name)))
    (and override (getf override key))))

(defun agent-definition-core-prompt-or-default (agent-name)
  "Return AGENT-NAME's core prompt, falling back to the default core system prompt."
  (let ((override (agent-prompt-override-value agent-name :core-prompt))
        (definition (find-agent-definition agent-name)))
    (or override
        (and definition
             (agent-definition-core-prompt definition))
        *default-core-system-prompt*)))

(defun agent-definition-personality-prompt-or-default (agent-name)
  "Return AGENT-NAME's personality prompt, falling back to the default personality prompt."
  (let ((override (agent-prompt-override-value agent-name :personality-prompt))
        (definition (find-agent-definition agent-name)))
    (or override
        (and definition
             (agent-definition-personality-prompt definition))
        *default-personality-prompt*)))

(defun current-system-prompt-date ()
  "Return the current local date as YYYY-MM-DD."
  (multiple-value-bind (_second _minute _hour day month year)
      (decode-universal-time (get-universal-time))
    (declare (ignore _second _minute _hour))
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

(defun current-system-prompt-working-directory ()
  "Return the current working directory for the system prompt."
  (namestring (current-system-prompt-directory)))

(defun system-prompt-runtime-footer ()
  "Return dynamic runtime context appended to each system prompt."
  (format nil "Current date: ~A~%Current working directory: ~A"
          (current-system-prompt-date)
          (current-system-prompt-working-directory)))

(defun render-agent-tools-section-if-loaded (&key buffer agent-name)
  "Render provider tool instructions when the tool system is loaded."
  (let ((renderer (and (fboundp 'render-agent-tools-section)
                       (symbol-function 'render-agent-tools-section)))
        (tool-provider (and (fboundp 'tool-definitions-for-api)
                            (symbol-function 'tool-definitions-for-api))))
    (and renderer tool-provider
         (funcall renderer
                  (coerce (funcall tool-provider
                                   :buffer buffer
                                   :agent-name agent-name)
                          'list)))))

(defun build-agent-system-prompt (agent-name &key buffer)
  "Build the full system prompt for AGENT-NAME.
Composition order: boot-file prefix, core prompt, package sections, active
tools section, skills section, personality prompt, then dynamic runtime footer."
  (load-active-packages :buffer buffer :agent-name agent-name)
  (let* ((agent-keyword (intern (string-upcase agent-name) :keyword))
         (*current-caller* agent-keyword)
         (*system-prompt-buffer* buffer)
         (parts (remove-if #'null
                           (list (load-boot-files)
                                 (agent-definition-core-prompt-or-default agent-name)
                                 (render-package-prompt-sections
                                  (list-package-prompt-sections)
                                  :buffer buffer
                                  :agent-name agent-name)
                                 (render-agent-tools-section-if-loaded
                                  :buffer buffer
                                  :agent-name agent-name)
                                 (render-skills-section)
                                 (agent-definition-personality-prompt-or-default agent-name)
                                 (system-prompt-runtime-footer)))))
    (format nil "~{~A~^~%~%---~%~%~}" parts)))

(defun build-system-prompt ()
  "Compatibility wrapper that builds the default agent prompt."
  (build-agent-system-prompt *default-agent-name*))

;;; --------------------------------------------------------------------------
;;; JSON Helpers (underscore-preserving round-trip)
;;; --------------------------------------------------------------------------

(defun json-name-to-lisp (name)
  "Convert a JSON key name to a Lisp identifier string, preserving underscores
as double-dashes so they round-trip correctly through cl-json encoding.
E.g., \"tool_use\" -> \"TOOL--USE\" -> (interned as :TOOL--USE) -> \"tool_use\"."
  (string-upcase
   (with-output-to-string (s)
     (loop :for c :across name
           :do (if (char= c #\_)
                   (write-string "--" s)
                   (write-char c s))))))

(defun api-json-decode (string)
  "Decode a JSON string using underscore-preserving key mapping."
  (let ((cl-json:*json-identifier-name-to-lisp* #'json-name-to-lisp))
    (cl-json:decode-json-from-string string)))

(defun api-json-encode (object)
  "Encode an object to JSON using cl-json's default encoding.
Keys with double-dashes encode as underscores (e.g., :TOOL--USE -> tool_use)."
  (cl-json:encode-json-to-string object))

(defclass json-false-literal ()
  ()
  (:documentation "Marker object that encodes as a literal JSON false value."))

(defparameter +json-false+ (make-instance 'json-false-literal)
  "Shared marker object that encodes as a literal JSON false value.")

(defclass json-empty-object-literal ()
  ()
  (:documentation "Marker object that encodes as a literal empty JSON object."))

(defparameter +json-empty-object+ (make-instance 'json-empty-object-literal)
  "Shared marker object that encodes as a literal empty JSON object.")

(defmethod cl-json:encode-json ((object json-false-literal)
                                &optional (stream cl-json:*json-output*))
  "Encode OBJECT as a literal JSON false value."
  (declare (ignore object))
  (write-string "false" stream))

(defmethod cl-json:encode-json ((object json-empty-object-literal)
                                &optional (stream cl-json:*json-output*))
  "Encode OBJECT as a literal empty JSON object."
  (declare (ignore object))
  (write-string "{}" stream))

(defun json-schema-object-slot-p (key)
  "Return true when KEY names a JSON Schema slot whose value must be an object."
  (member key '(:properties :definitions :$defs :pattern-properties
                :dependent-schemas "properties" "definitions" "$defs"
                "patternProperties" "dependentSchemas")
          :test #'equal))

(defun provider-ready-json-schema (schema)
  "Return SCHEMA normalized for provider JSON encoding.

In particular, empty object-valued schema slots such as :properties must encode
as `{}` rather than `null`."
  (labels ((walk (value &optional key)
             (cond
               ((and (null value)
                     (json-schema-object-slot-p key))
                +json-empty-object+)
               ((stringp value)
                value)
               ((vectorp value)
                (map 'vector (lambda (item) (walk item)) value))
               ((consp value)
                (mapcar (lambda (entry)
                          (if (consp entry)
                              (cons (car entry) (walk (cdr entry) (car entry)))
                              (walk entry)))
                        value))
               (t
                value))))
    (walk schema)))

(declaim (ftype (function (t) (or null list)) normalize-legacy-raw-content))

(defun canonical-text-block (text)
  "Return TEXT as a canonical text block." 
  `((:type . "text")
    (:text . ,(or text ""))))

(defun canonical-reasoning-block (text)
  "Return TEXT as a canonical provider-supplied reasoning block."
  `((:type . "reasoning")
    (:text . ,(or text ""))))

(defun canonicalize-content-block (role block)
  "Normalize BLOCK for ROLE and enforce valid role/block pairings."
  (let ((block-type (cdr (assoc :type block))))
    (cond
      ((string= "text" (or block-type ""))
       (unless (member role '("user" "assistant") :test #'string=)
         (error "text blocks are only allowed on user or assistant messages"))
       (canonical-text-block (cdr (assoc :text block))))
      ((string= "reasoning" (or block-type ""))
       (unless (string= role "assistant")
         (error "reasoning blocks are only allowed on assistant messages"))
       (canonical-reasoning-block (cdr (assoc :text block))))
      ((string= "tool_use" (or block-type ""))
       (unless (string= role "assistant")
         (error "tool_use blocks are only allowed on assistant messages"))
       `((:type . "tool_use")
         (:id . ,(cdr (assoc :id block)))
         (:name . ,(cdr (assoc :name block)))
         (:input . ,(cdr (assoc :input block)))))
      ((string= "tool_result" (or block-type ""))
       (unless (string= role "user")
         (error "tool_result blocks are only allowed on user messages"))
       (let ((tool-use-id (or (cdr (assoc :tool--use--id block))
                              (cdr (assoc :tool-use-id block)))))
         (unless tool-use-id
           (error "tool_result blocks require tool_use_id"))
        `((:type . "tool_result")
          (:tool--use--id . ,tool-use-id)
          (:content . ,(cdr (assoc :content block))))))
      (t
       (error "Unsupported content block type ~S" block-type)))))

(defun canonicalize-message-content (role content)
  "Normalize CONTENT into canonical content blocks for ROLE."
  (cond
    ((or (null content) (stringp content))
     (list (canonicalize-content-block role (canonical-text-block content))))
    ((vectorp content)
     (loop :for block :across content
           :collect (canonicalize-content-block role block)))
    ((listp content)
     (loop :for block :in content
           :collect (canonicalize-content-block role block)))
    (t
     (error "Unsupported message content ~S" content))))

(defun normalize-legacy-raw-content (raw-content)
  "Normalize persisted legacy session raw-content blocks to canonical blocks."
  (when raw-content
    (loop :for block :in (coerce raw-content 'list)
          :collect
          (let ((block-type (cdr (assoc :type block))))
            (cond
              ((string= "text" (or block-type ""))
               `((:type . "text")
                 (:text . ,(cdr (assoc :text block)))))
              ((string= "reasoning" (or block-type ""))
               `((:type . "reasoning")
                 (:text . ,(cdr (assoc :text block)))))
              ((string= "tool_use" (or block-type ""))
               `((:type . "tool_use")
                 (:id . ,(cdr (assoc :id block)))
                 (:name . ,(cdr (assoc :name block)))
                 (:input . ,(cdr (assoc :input block)))))
              ((string= "tool_result" (or block-type ""))
                `((:type . "tool_result")
                  (:tool--use--id . ,(or (cdr (assoc :tool--use--id block))
                                         (cdr (assoc :tool-use-id block))))
                  (:content . ,(cdr (assoc :content block)))))
              (t block))))))

;;; --------------------------------------------------------------------------
;;; Token Management
;;; --------------------------------------------------------------------------

(defparameter +default-provider-token-directory+
  (merge-pathnames #P".config/rplaca/" (user-homedir-pathname))
  "Canonical provider credential directory.")

(defvar *provider-token-directory* +default-provider-token-directory+
  "Configured provider credential directory.")

(defvar *legacy-provider-token-directory*
  (merge-pathnames #P".config/clawmacs/" (user-homedir-pathname))
  "Legacy read-only provider credential directory.")

(defun provider-token-path (provider)
  "Return the provider-specific token file path for PROVIDER."
  (merge-pathnames
   (case provider
     (:openai-codex #P"openai-codex-token")
     (:zai #P"zai-api-key")
     (:openrouter #P"openrouter-api-key")
     (otherwise
      (error "Unknown provider ~S. Supported providers: :OPENAI-CODEX, :ZAI, :OPENROUTER"
             provider)))
   *provider-token-directory*))

(defun legacy-provider-token-path (provider)
  "Return PROVIDER's legacy token file path for read-only migration fallback."
  (merge-pathnames
   (case provider
     (:openai-codex #P"openai-codex-token")
     (:zai #P"zai-api-key")
     (:openrouter #P"openrouter-api-key")
     (otherwise
      (error "Unknown provider ~S. Supported providers: :OPENAI-CODEX, :ZAI, :OPENROUTER"
             provider)))
   *legacy-provider-token-directory*))

(defvar *zai-env-var* "ZAI_CODING_MAX_API_KEY"
  "Environment variable name for the Z.AI Coding Max API key.
When set, this takes highest priority over the static token file.")

(defun read-env-token (env-var)
  "Read a token from the environment variable named ENV-VAR.
Returns the trimmed token string if the variable is set and non-empty, nil otherwise."
  (let ((value (uiop:getenv env-var)))
    (when (and value (stringp value))
      (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) value)))
        (when (plusp (length trimmed))
          trimmed)))))

(defun save-provider-token (provider token)
  "Save TOKEN to PROVIDER's provider-specific token file."
  (let ((token-path (provider-token-path provider)))
    (ensure-directories-exist token-path)
    (with-open-file (s token-path
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create)
      (write-string token s))
    (ignore-errors
      (uiop:run-program (list "chmod" "600" (namestring token-path))))
    token))

;;; --------------------------------------------------------------------------
;;; OpenAI Codex OAuth 2.0 (PKCE Flow)
;;; --------------------------------------------------------------------------

(defconstant +unix-to-universal-time-offset+ 2208988800
  "Seconds between the Unix epoch and Common Lisp universal time epoch.")

(defconstant +openai-oauth-refresh-staleness-days+ 8
  "Fallback staleness threshold used when JWT expiry cannot be parsed.")

(defconstant +openai-oauth-refresh-leeway-seconds+ 300
  "Refresh ChatGPT OAuth tokens this many seconds before JWT expiry.")

(defun trimmed-file-string (pathname)
  "Read PATHNAME and trim surrounding ASCII whitespace.
Returns NIL when the file is missing or blank."
  (when (probe-file pathname)
    (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return)
                                (uiop:read-file-string pathname))))
      (when (plusp (length trimmed))
        trimmed))))

(defun read-provider-file-token (provider)
  "Read PROVIDER's static token file without consulting env vars or OAuth."
  (trimmed-file-string
   (configured-migration-read-path
    (provider-token-path provider)
    (let ((*provider-token-directory* +default-provider-token-directory+))
      (provider-token-path provider))
    (legacy-provider-token-path provider)
    :label "provider credential")))

(defun url-like-string-p (string)
  "Return non-nil when STRING looks like an HTTP(S) URL."
  (and (stringp string)
       (or (and (<= 7 (length string))
                (string= "http://" string :end2 7))
           (and (<= 8 (length string))
                (string= "https://" string :end2 8)))))

(defun jwt-like-string-p (string)
  "Return non-nil when STRING looks like a JWT."
  (and (stringp string)
       (= 2 (count #\. string))
       (plusp (length string))))

(defun openai-codex-api-key-override-valid-p (token)
  "Return non-nil when TOKEN is a plausible API-key style override."
  (and (stringp token)
       (plusp (length token))
       (not (url-like-string-p token))
       (not (search "/auth/callback" token :test #'char=))
       (not (search "code=" token :test #'char=))
       (not (search "state=" token :test #'char=))
       (not (jwt-like-string-p token))))

(defun openai-codex-chatgpt-access-token-valid-p (token)
  "Return non-nil when TOKEN is a plausible ChatGPT OAuth access token."
  (and (stringp token)
       (plusp (length token))
       (not (url-like-string-p token))
       (jwt-like-string-p token)))

(defun current-codex-auth-path ()
  "Return the effective auth.json path."
  *codex-auth-path*)

(defun write-private-file (pathname contents)
  "Write CONTENTS to PATHNAME and best-effort chmod the result to 0600."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (write-string contents stream))
  (ignore-errors
    (uiop:run-program (list "chmod" "600" (namestring pathname))))
  pathname)

(defun openai-oauth-redirect-uri (&optional (port *openai-oauth-default-port*))
  "Return the localhost callback URI for PORT."
  (format nil "http://localhost:~D~A" port *openai-oauth-callback-path*))

(defun current-rfc3339-timestamp ()
  "Return the current UTC time as a simple RFC 3339 timestamp."
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ"
            year month day hour minute second)))

(defun unix-time->universal-time (unix-time)
  "Convert UNIX-TIME seconds to Common Lisp universal time."
  (+ unix-time +unix-to-universal-time-offset+))

(defun alist-string-value (alist key)
  "Return the non-blank string value for KEY in ALIST."
  (let ((value (cdr (assoc key alist))))
    (when (and (stringp value) (plusp (length value)))
      value)))

(defun openai-codex-auth-json-tokens (auth-json)
  "Return the decoded tokens object from AUTH-JSON."
  (cdr (assoc :tokens auth-json)))

(defun normalize-openai-codex-auth-mode (value)
  "Normalize a persisted auth mode VALUE to :CHATGPT or :API-KEY."
  (cond
    ((or (null value) (eq value :chatgpt)) :chatgpt)
    ((eq value :api-key) :api-key)
    ((stringp value)
     (let ((normalized (string-downcase value)))
       (cond
         ((string= normalized "chatgpt") :chatgpt)
         ((or (string= normalized "api_key")
              (string= normalized "api-key")
              (string= normalized "apikey"))
          :api-key)
         (t :chatgpt))))
    (t :chatgpt)))

(defun openai-codex-resolved-auth-mode (auth-json)
  "Resolve AUTH-JSON's effective auth mode the same way Codex does."
  (let ((explicit-mode (cdr (assoc :auth--mode auth-json)))
        (api-key (alist-string-value auth-json :openai--api--key)))
    (cond
      (explicit-mode
       (normalize-openai-codex-auth-mode explicit-mode))
      (api-key
       :api-key)
      (t
       :chatgpt))))

(defun read-openai-codex-auth-json ()
  "Read the shared Codex auth.json store."
  (let ((auth-path (current-codex-auth-path)))
    (when (probe-file auth-path)
      (handler-case
          (api-json-decode (uiop:read-file-string auth-path))
        (error () nil)))))

(defun openai-codex-auth-payload (&key auth-mode openai-api-key
                                       id-token access-token refresh-token account-id
                                       last-refresh)
  "Build an auth.json payload compatible with Codex."
  (let ((payload nil)
        (tokens nil))
    (when (or id-token access-token refresh-token account-id)
      (setf tokens `((:id--token . ,id-token)
                     (:access--token . ,access-token)
                     (:refresh--token . ,refresh-token)
                     (:account--id . ,account-id))))
    (when auth-mode
      (push `(:auth--mode . ,(ecase auth-mode
                               (:chatgpt "chatgpt")
                               (:api-key "api_key")))
            payload))
    (push `(:openai--api--key . ,openai-api-key) payload)
    (when tokens
      (push `(:tokens . ,tokens) payload))
    (push `(:last--refresh . ,(or last-refresh (current-rfc3339-timestamp))) payload)
    (nreverse payload)))

(defun save-openai-codex-auth-json (auth-json)
  "Persist AUTH-JSON to the shared Codex auth store."
  (write-private-file (current-codex-auth-path)
                      (api-json-encode auth-json))
  auth-json)

(defun base64url-char-value (char)
  "Decode one base64url CHAR to its 6-bit integer value."
  (cond
    ((and (char>= char #\A) (char<= char #\Z))
     (- (char-code char) (char-code #\A)))
    ((and (char>= char #\a) (char<= char #\z))
     (+ 26 (- (char-code char) (char-code #\a))))
    ((and (char>= char #\0) (char<= char #\9))
     (+ 52 (- (char-code char) (char-code #\0))))
    ((char= char #\-) 62)
    ((char= char #\_) 63)
    (t
     (error "Invalid base64url character ~S" char))))

(defun base64url-decode-to-octets (string)
  "Decode base64url STRING into a fresh octet vector."
  (let ((buffer 0)
        (bits 0)
        (bytes (make-array 0
                           :element-type '(unsigned-byte 8)
                           :adjustable t
                           :fill-pointer 0)))
    (loop :for char :across string
          :unless (char= char #\=)
            :do (progn
                  (setf buffer (logior (ash buffer 6)
                                       (base64url-char-value char))
                        bits (+ bits 6))
                  (loop :while (>= bits 8)
                        :do (decf bits 8)
                            (vector-push-extend (ldb (byte 8 bits) buffer) bytes)
                            (setf buffer (ldb (byte bits 0) buffer)))))
    bytes))

(defun jwt-payload-string (jwt)
  "Return JWT's decoded payload as a UTF-8 string."
  (let* ((parts (split-string-by-char jwt #\.))
         (payload (second parts)))
    (unless (and (= (length parts) 3)
                 payload
                 (plusp (length payload)))
      (error "Invalid JWT format"))
    (flexi-streams:octets-to-string
     (base64url-decode-to-octets payload)
     :external-format :utf-8)))

(defun parse-jwt-expiration (jwt)
  "Return JWT's `exp` claim as universal time, or NIL when unavailable."
  (handler-case
      (let* ((payload (api-json-decode (jwt-payload-string jwt)))
             (exp (cdr (assoc :exp payload))))
        (when (integerp exp)
          (unix-time->universal-time exp)))
    (error () nil)))

(defun json-string-field-value (json-string field-name)
  "Extract a simple quoted FIELD-NAME value from JSON-STRING."
  (let* ((needle (format nil "\"~A\"" field-name))
         (field-pos (search needle json-string)))
    (when field-pos
      (let* ((colon-pos (position #\: json-string :start (+ field-pos (length needle))))
             (value-start (and colon-pos
                               (position #\" json-string :start (1+ colon-pos)))))
        (when value-start
          (let ((value-end (position #\" json-string :start (1+ value-start))))
            (when value-end
              (subseq json-string (1+ value-start) value-end))))))))

(defun parse-rfc3339-timestamp (string)
  "Parse a simple RFC 3339 UTC or offset timestamp STRING to universal time."
  (when (and string (>= (length string) 20))
    (let* ((year (parse-integer string :start 0 :end 4))
           (month (parse-integer string :start 5 :end 7))
           (day (parse-integer string :start 8 :end 10))
           (hour (parse-integer string :start 11 :end 13))
           (minute (parse-integer string :start 14 :end 16))
           (second (parse-integer string :start 17 :end 19))
           (zone-pos (or (position #\Z string :start 19)
                         (position #\+ string :start 19)
                         (position #\- string :start 19)))
           (base (encode-universal-time second minute hour day month year 0)))
      (cond
        ((or (null zone-pos) (char= (char string zone-pos) #\Z))
         base)
        (t
         (let* ((sign (char string zone-pos))
                (offset-hour (parse-integer string :start (1+ zone-pos) :end (+ zone-pos 3)))
                (offset-minute (parse-integer string :start (+ zone-pos 4) :end (+ zone-pos 6)))
                (offset-seconds (+ (* offset-hour 3600) (* offset-minute 60))))
           (if (char= sign #\+)
               (- base offset-seconds)
               (+ base offset-seconds))))))))

(defun openai-codex-id-token-account-id (id-token)
  "Extract the ChatGPT account/workspace ID from ID-TOKEN when present."
  (handler-case
      (json-string-field-value (jwt-payload-string id-token) "chatgpt_account_id")
    (error () nil)))

(defun read-openai-codex-oauth-tokens ()
  "Read the shared OpenAI Codex auth store as a convenience plist."
  (let* ((auth-json (read-openai-codex-auth-json))
         (tokens (and auth-json (openai-codex-auth-json-tokens auth-json)))
         (access-token (and tokens (alist-string-value tokens :access--token))))
    (when auth-json
      (list :auth-mode (openai-codex-resolved-auth-mode auth-json)
            :openai-api-key (alist-string-value auth-json :openai--api--key)
            :id-token (and tokens (alist-string-value tokens :id--token))
            :access-token access-token
            :refresh-token (and tokens (alist-string-value tokens :refresh--token))
            :account-id (and tokens (alist-string-value tokens :account--id))
            :last-refresh (alist-string-value auth-json :last--refresh)
            :expires-at (and access-token (parse-jwt-expiration access-token))))))

(defun openai-codex-chatgpt-auth-stale-p (auth-json)
  "Return non-nil when AUTH-JSON should be proactively refreshed."
  (let* ((tokens (openai-codex-auth-json-tokens auth-json))
         (access-token (and tokens (alist-string-value tokens :access--token))))
    (cond
      ((and access-token
            (let ((expires-at (parse-jwt-expiration access-token)))
              (and expires-at
                   (<= expires-at
                       (+ (get-universal-time)
                          +openai-oauth-refresh-leeway-seconds+)))))
       t)
      (t
       (let ((last-refresh (parse-rfc3339-timestamp
                            (alist-string-value auth-json :last--refresh))))
         (and last-refresh
              (< last-refresh
                 (- (get-universal-time)
                    (* +openai-oauth-refresh-staleness-days+ 24 60 60)))))))))

(defun request-openai-codex-token-refresh (refresh-token)
  "Request a refreshed ChatGPT OAuth token set using REFRESH-TOKEN."
  (multiple-value-bind (body status-code)
      (provider-http-request-with-retries
       "OpenAI Codex OAuth refresh"
       (lambda ()
         (drakma:http-request
          *openai-oauth-token-url*
          :method :post
          :content-type "application/json"
          :content (api-json-encode
                    `((:client--id . ,*openai-oauth-client-id*)
                      (:grant--type . "refresh_token")
                      (:refresh--token . ,refresh-token)))
          :want-stream nil
          :force-binary nil
          :connection-timeout *provider-http-connection-timeout-seconds*)))
    (let ((body-string (http-body-string body)))
      (unless (= status-code 200)
        (error "OAuth refresh failed (~A): ~A" status-code body-string))
      (let ((response (api-json-decode body-string)))
        (list :id-token (alist-string-value response :id--token)
              :access-token (alist-string-value response :access--token)
              :refresh-token (or (alist-string-value response :refresh--token)
                                 refresh-token))))))

(defun refresh-openai-codex-auth-json (&optional auth-json)
  "Refresh AUTH-JSON in-place via the token endpoint and persist the result."
  (let* ((current-auth (or auth-json (read-openai-codex-auth-json)))
         (tokens (and current-auth (openai-codex-auth-json-tokens current-auth)))
         (refresh-token (and tokens (alist-string-value tokens :refresh--token))))
    (unless (and refresh-token (plusp (length refresh-token)))
      (return-from refresh-openai-codex-auth-json nil))
    (handler-case
        (let* ((refreshed (request-openai-codex-token-refresh refresh-token))
               (existing-id-token (alist-string-value tokens :id--token))
               (new-id-token (or (getf refreshed :id-token) existing-id-token))
               (account-id (or (and tokens (alist-string-value tokens :account--id))
                               (and new-id-token
                                    (openai-codex-id-token-account-id new-id-token))))
               (updated
                 (openai-codex-auth-payload
                  :auth-mode :chatgpt
                  :openai-api-key (alist-string-value current-auth :openai--api--key)
                  :id-token new-id-token
                  :access-token (getf refreshed :access-token)
                  :refresh-token (getf refreshed :refresh-token)
                  :account-id account-id
                  :last-refresh (current-rfc3339-timestamp))))
          (save-openai-codex-auth-json updated)
          updated)
      (error ()
        nil))))

(defun save-openai-codex-oauth-tokens (access-token refresh-token expires-in
                                        &key id-token account-id openai-api-key
                                          (auth-mode :chatgpt))
  "Persist an OpenAI Codex auth payload in the shared Codex auth.json store.
The legacy EXPIRES-IN argument is accepted for compatibility and ignored."
  (declare (ignore expires-in))
  (save-openai-codex-auth-json
   (openai-codex-auth-payload
    :auth-mode auth-mode
    :openai-api-key openai-api-key
    :id-token id-token
    :access-token access-token
    :refresh-token refresh-token
    :account-id account-id
    :last-refresh (current-rfc3339-timestamp)))
  access-token)

(defun read-openai-codex-oauth-token ()
  "Read a valid ChatGPT OAuth access token from the shared Codex auth store."
  (let* ((auth-json (read-openai-codex-auth-json))
         (tokens (and auth-json (openai-codex-auth-json-tokens auth-json)))
         (access-token (and tokens (alist-string-value tokens :access--token))))
    (cond
      ((null auth-json) nil)
      ((and (eq (openai-codex-resolved-auth-mode auth-json) :chatgpt)
            (openai-codex-chatgpt-auth-stale-p auth-json))
       (let ((refreshed (refresh-openai-codex-auth-json auth-json)))
         (or (and refreshed
                  (alist-string-value (openai-codex-auth-json-tokens refreshed)
                                      :access--token))
             access-token)))
      (t
       access-token))))

(defun openai-codex-auth-descriptor-from-auth-json (auth-json)
  "Resolve AUTH-JSON into a request descriptor."
  (let ((mode (openai-codex-resolved-auth-mode auth-json)))
    (ecase mode
      (:api-key
       (let ((api-key (alist-string-value auth-json :openai--api--key)))
         (when (openai-codex-api-key-override-valid-p api-key)
           (list :source :codex-api-key
                 :mode :api-key
                 :token api-key
                 :base-url *openai-codex-api-base-url*
                 :account-id nil
                 :refreshable-p nil
                 :auth-json auth-json))))
      (:chatgpt
       (let* ((tokens (openai-codex-auth-json-tokens auth-json))
              (access-token (and tokens (alist-string-value tokens :access--token)))
              (account-id (or (and tokens (alist-string-value tokens :account--id))
                              (let ((id-token (and tokens (alist-string-value tokens :id--token))))
                                (and id-token
                                     (openai-codex-id-token-account-id id-token))))))
         (unless (and (openai-codex-chatgpt-access-token-valid-p access-token)
                      account-id)
           (error "Codex ChatGPT auth requires a JWT access_token and account_id in auth.json"))
         (list :source :codex-chatgpt
               :mode :chatgpt
               :token access-token
               :base-url *openai-codex-chatgpt-base-url*
               :account-id account-id
               :refreshable-p t
               :auth-json auth-json))))))

(defun resolve-openai-codex-auth (&key (refresh-if-needed t))
  "Resolve the effective OpenAI Codex auth descriptor.
Precedence: rplaca override token file, then shared ~/.codex/auth.json."
  (let ((override-token (read-provider-file-token :openai-codex)))
    (when (openai-codex-api-key-override-valid-p override-token)
      (return-from resolve-openai-codex-auth
        (list :source :token-override
              :mode :api-key
              :token override-token
              :base-url *openai-codex-api-base-url*
              :account-id nil
              :refreshable-p nil))))
  (let ((auth-json (read-openai-codex-auth-json)))
    (when auth-json
      (let ((effective-auth
              (if (and refresh-if-needed
                       (eq (openai-codex-resolved-auth-mode auth-json) :chatgpt)
                       (openai-codex-chatgpt-auth-stale-p auth-json))
                  (or (refresh-openai-codex-auth-json auth-json)
                      auth-json)
                  auth-json)))
        (openai-codex-auth-descriptor-from-auth-json effective-auth)))))

(defun resolve-openai-codex-chatgpt-image-auth (&key (refresh-if-needed t))
  "Resolve ChatGPT OAuth credentials for subscription-backed Codex images.

Unlike `RESOLVE-OPENAI-CODEX-AUTH', this deliberately ignores RPLACA's
static token override and refuses auth.json API-key mode.  Image generation
through this path must consume the user's Codex/ChatGPT allowance rather than
silently switching to separately billed OpenAI API credentials."
  (let ((auth-json (read-openai-codex-auth-json)))
    (unless auth-json
      (error "Codex image auth is unavailable: ~~/.codex/auth.json was not found."))
    (unless (eq (openai-codex-resolved-auth-mode auth-json) :chatgpt)
      (error "Codex image generation requires ChatGPT subscription auth, not API-key mode."))
    (let ((effective-auth
            (if (and refresh-if-needed
                     (openai-codex-chatgpt-auth-stale-p auth-json))
                (or (refresh-openai-codex-auth-json auth-json)
                    (error "Codex image auth refresh failed."))
                auth-json)))
      ;; This descriptor performs the access-token and account-id validation.
      ;; It also fixes the base URL and header mode to the Codex ChatGPT route.
      (openai-codex-auth-descriptor-from-auth-json effective-auth))))

(defun refresh-openai-codex-chatgpt-image-auth ()
  "Force-refresh subscription image auth and return its request descriptor.

The narrow helper cannot fall back to API-key or static-token credentials."
  (let ((auth-json (read-openai-codex-auth-json)))
    (when (and auth-json
               (eq (openai-codex-resolved-auth-mode auth-json) :chatgpt))
      (let ((updated (refresh-openai-codex-auth-json auth-json)))
        (and updated
             (openai-codex-auth-descriptor-from-auth-json updated))))))

(defun refresh-openai-codex-oauth-token (refresh-token)
  "Refresh the OpenAI Codex ChatGPT OAuth token and return the new access token."
  (declare (ignore refresh-token))
  (let ((updated (refresh-openai-codex-auth-json)))
    (and updated
         (alist-string-value (openai-codex-auth-json-tokens updated)
                             :access--token))))

(defun read-provider-token (provider)
  "Read PROVIDER's token with provider-specific precedence rules.

  :OPENAI-CODEX  1) Static token override (~/.config/rplaca/openai-codex-token)
                 2) Shared Codex auth.json (~/.codex/auth.json)

  :ZAI           1) ZAI_CODING_MAX_API_KEY env var
                 2) Static token file (~/.config/rplaca/zai-api-key)

  :OPENROUTER    1) OPENROUTER_API_KEY env var
                 2) Static token file (~/.config/rplaca/openrouter-api-key)"
  (cond
    ((eq provider :e2e)
     nil)
    (t
     (or (when (eq provider :zai)
           (read-env-token *zai-env-var*))
         (when (eq provider :openrouter)
           (read-env-token *openrouter-env-var*))
         (when (eq provider :openai-codex)
           (getf (resolve-openai-codex-auth) :token))
         (read-provider-file-token provider)))))

(defun generate-random-string (length)
  "Generate a random string of LENGTH alphanumeric characters.
Uses /dev/urandom for cryptographic randomness."
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
        (num-chars 62))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (with-output-to-string (s)
        (loop :repeat length
              :for byte := (read-byte urandom)
              :do (write-char (char chars (mod byte num-chars)) s))))))

(defun generate-code-verifier ()
  "Generate a 43-character PKCE code verifier (RFC 7636)."
  (generate-random-string 43))

(defun generate-oauth-state ()
  "Generate a base64url OAuth state token for CSRF protection."
  (let ((bytes (make-array 32 :element-type '(unsigned-byte 8))))
    (with-open-file (urandom "/dev/urandom" :element-type '(unsigned-byte 8))
      (read-sequence bytes urandom))
    (with-output-to-string (out)
      (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
            (buffer 0)
            (bits 0))
        (loop :for byte :across bytes
              :do (setf buffer (logior (ash buffer 8) byte)
                        bits (+ bits 8))
                  (loop :while (>= bits 6)
                        :do (decf bits 6)
                            (write-char (char alphabet (ldb (byte 6 bits) buffer)) out)
                            (setf buffer (ldb (byte bits 0) buffer))))
        (when (plusp bits)
          (write-char (char alphabet (ash buffer (- 6 bits))) out))))))

(defun hex-string-to-bytes (hex-string)
  "Convert a HEX-STRING to a byte array."
  (let* ((len (/ (length hex-string) 2))
         (bytes (make-array len :element-type '(unsigned-byte 8))))
    (loop :for i :from 0 :below (length hex-string) :by 2
          :for j :from 0
          :do (setf (aref bytes j)
                    (parse-integer hex-string :start i :end (+ i 2) :radix 16)))
    bytes))

(defun compute-code-challenge (code-verifier)
  "Compute S256 PKCE code challenge from CODE-VERIFIER (RFC 7636 Section 4.2).
Uses openssl for SHA-256 and base64, then converts to base64url encoding."
  (string-trim
   '(#\Newline #\Space #\Return)
   (with-output-to-string (out)
     (uiop:run-program
      (format nil "printf '%s' '~A' | openssl dgst -sha256 -binary | openssl base64 -A | tr '+/' '-_' | tr -d '='"
              code-verifier)
      :output out))))

(defun url-encode-param (string)
  "Percent-encode STRING for use as a URL query parameter value."
  (with-output-to-string (s)
    (loop :for char :across string
          :do (cond
                ((or (alphanumericp char)
                     (find char "-_.~"))
                 (write-char char s))
                ((< (char-code char) 128)
                 (format s "%~2,'0X" (char-code char)))
                (t
                 ;; Multi-byte: encode each UTF-8 byte
                 (loop :for byte :across (flexi-streams:string-to-octets
                                          (string char) :external-format :utf-8)
                       :do (format s "%~2,'0X" byte)))))))

(defun split-string-by-char (string delimiter)
  "Split STRING into substrings at each occurrence of DELIMITER character."
  (loop :for start := 0 :then (1+ end)
        :for end := (position delimiter string :start start)
        :collect (subseq string start (or end (length string)))
        :while end))

(defun hex-digit-value (char)
  "Decode one hexadecimal digit CHAR to its integer value."
  (cond
    ((and (char>= char #\0) (char<= char #\9))
     (- (char-code char) (char-code #\0)))
    ((and (char>= char #\A) (char<= char #\F))
     (+ 10 (- (char-code char) (char-code #\A))))
    ((and (char>= char #\a) (char<= char #\f))
     (+ 10 (- (char-code char) (char-code #\a))))
    (t
     (error "Invalid hex digit ~S" char))))

(defun url-decode-param (string)
  "Percent-decode STRING from a URL query parameter."
  (with-output-to-string (out)
    (loop :for i :from 0 :below (length string)
          :for char := (char string i)
          :do (cond
                ((char= char #\+)
                 (write-char #\Space out))
                ((and (char= char #\%)
                      (<= (+ i 2) (1- (length string))))
                 (let ((byte (+ (* 16 (hex-digit-value (char string (1+ i))))
                                (hex-digit-value (char string (+ i 2))))))
                   (write-char (code-char byte) out)
                   (incf i 2)))
                (t
                 (write-char char out))))))

(defun parse-query-string (query)
  "Parse a URL QUERY string into an alist of (key . value) string pairs."
  (loop :for part :in (split-string-by-char query #\&)
        :for eq-pos := (position #\= part)
        :when eq-pos
          :collect (cons (url-decode-param (subseq part 0 eq-pos))
                         (url-decode-param (subseq part (1+ eq-pos))))))

(defun extract-oauth-callback-params (url)
  "Extract authorization code and state from an OAuth callback URL.
Returns (values code state). Signals an error if the code is missing."
  (let* ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) url))
         (query-start (position #\? trimmed)))
    (unless query-start
      (error "Invalid callback URL (no query parameters).~%Expected: ~A?code=...&state=..."
             (openai-oauth-redirect-uri)))
    (let* ((query (subseq trimmed (1+ query-start)))
           (params (parse-query-string query))
           (code (cdr (assoc "code" params :test #'string=)))
           (state (cdr (assoc "state" params :test #'string=))))
      (unless code
        (error "No authorization code in callback URL. Check that you copied the full URL."))
      (values code state))))

(defun build-openai-codex-authorize-url (redirect-uri code-challenge state)
  "Build a Codex-compatible browser authorization URL."
  (format nil
          "~A?response_type=code&client_id=~A&redirect_uri=~A&scope=~A&code_challenge=~A&code_challenge_method=S256&id_token_add_organizations=true&codex_cli_simplified_flow=true&state=~A&originator=~A"
          *openai-oauth-auth-url*
          *openai-oauth-client-id*
          (url-encode-param redirect-uri)
          (url-encode-param *openai-oauth-scopes*)
          (url-encode-param code-challenge)
          (url-encode-param state)
          (url-encode-param *openai-oauth-originator*)))

(defun openai-codex-oauth-start (&key (redirect-uri (openai-oauth-redirect-uri)))
  "Initiate an OpenAI Codex OAuth PKCE flow.
Generates PKCE pair and state, builds the authorization URL.
Returns (values auth-url code-verifier state)."
  (let* ((code-verifier (generate-code-verifier))
         (code-challenge (compute-code-challenge code-verifier))
         (state (generate-oauth-state))
         (auth-url (build-openai-codex-authorize-url redirect-uri code-challenge state)))
    (values auth-url code-verifier state)))

(defun exchange-openai-oauth-code (code code-verifier &key (redirect-uri (openai-oauth-redirect-uri)))
  "Exchange an OAuth authorization CODE for access/refresh tokens using CODE-VERIFIER.
Returns a plist (:id-token ... :access-token ... :refresh-token ... :account-id ...)."
  (multiple-value-bind (body status-code)
      (provider-http-request-with-retries
       "OpenAI Codex OAuth exchange"
       (lambda ()
         (drakma:http-request
          *openai-oauth-token-url*
          :method :post
          :content-type "application/x-www-form-urlencoded"
          :content (format nil "grant_type=authorization_code&client_id=~A&code=~A&redirect_uri=~A&code_verifier=~A"
                           (url-encode-param *openai-oauth-client-id*)
                           (url-encode-param code)
                           (url-encode-param redirect-uri)
                           (url-encode-param code-verifier))
          :want-stream nil
          :force-binary nil
          :connection-timeout *provider-http-connection-timeout-seconds*)))
    (let ((body-string (http-body-string body)))
      (unless (= status-code 200)
        (error "OAuth token exchange failed (~A): ~A" status-code body-string))
      (let* ((response (api-json-decode body-string))
             (id-token (alist-string-value response :id--token)))
        (list :id-token id-token
              :access-token (alist-string-value response :access--token)
              :refresh-token (alist-string-value response :refresh--token)
              :account-id (and id-token
                               (openai-codex-id-token-account-id id-token)))))))

(defun obtain-openai-codex-api-key (id-token)
  "Exchange ID-TOKEN for an API-key style token when the backend allows it."
  (multiple-value-bind (body status-code)
      (provider-http-request-with-retries
       "OpenAI Codex API-key exchange"
       (lambda ()
         (drakma:http-request
          *openai-oauth-token-url*
          :method :post
          :content-type "application/x-www-form-urlencoded"
          :content (format nil
                           "grant_type=~A&client_id=~A&requested_token=~A&subject_token=~A&subject_token_type=~A"
                           (url-encode-param "urn:ietf:params:oauth:grant-type:token-exchange")
                           (url-encode-param *openai-oauth-client-id*)
                           (url-encode-param "openai-api-key")
                           (url-encode-param id-token)
                           (url-encode-param "urn:ietf:params:oauth:token-type:id_token"))
          :want-stream nil
          :force-binary nil
          :connection-timeout *provider-http-connection-timeout-seconds*)))
    (let ((body-string (http-body-string body)))
      (when (= status-code 200)
        (alist-string-value (api-json-decode body-string) :access--token)))))

(defun openai-codex-oauth-finish (callback-url code-verifier expected-state
                                   &key (redirect-uri (openai-oauth-redirect-uri)))
  "Complete the OAuth flow by exchanging the authorization code for tokens.
CALLBACK-URL is the full localhost redirect URL received by the callback server.
CODE-VERIFIER is the PKCE verifier from the initial request.
EXPECTED-STATE is the state parameter for CSRF validation.
Returns the access token on success."
  (multiple-value-bind (code state)
      (extract-oauth-callback-params callback-url)
    (when (and expected-state state (not (string= state expected-state)))
      (error "OAuth state mismatch (possible CSRF). Expected ~A, got ~A"
             expected-state state))
    (let* ((tokens (exchange-openai-oauth-code code code-verifier
                                               :redirect-uri redirect-uri))
           (id-token (getf tokens :id-token))
           (api-key (and id-token
                         (ignore-errors
                           (obtain-openai-codex-api-key id-token)))))
      (save-openai-codex-oauth-tokens
       (getf tokens :access-token)
       (getf tokens :refresh-token)
       nil
       :id-token id-token
       :account-id (getf tokens :account-id)
       :openai-api-key api-key
       :auth-mode :chatgpt)
      (getf tokens :access-token))))

(defstruct openai-oauth-flow
  "State for an in-progress localhost OpenAI Codex OAuth login."
  buffer
  auth-url
  redirect-uri
  port
  code-verifier
  state
  listener
  client-socket
  client-stream
  thread
  settlement-thread
  (settlement-waiter-done-p nil :type boolean)
  (worker-settled-p nil :type boolean)
  (done-p nil :type boolean)
  (success-p nil :type boolean)
  (cancelled-p nil :type boolean)
  error
  token
  (lock (bt:make-lock "openai-oauth-flow")))

(defun html-escape (string)
  "Escape STRING for safe insertion into a tiny HTML page."
  (with-output-to-string (out)
    (loop :for char :across (or string "")
          :do (case char
                (#\& (write-string "&amp;" out))
                (#\< (write-string "&lt;" out))
                (#\> (write-string "&gt;" out))
                (#\" (write-string "&quot;" out))
                (#\' (write-string "&#39;" out))
                (otherwise
                 (write-char char out))))))

(defun openai-oauth-success-page ()
  "Return the minimal success HTML shown in the browser after login."
  "<!doctype html><html><head><meta charset=\"utf-8\"><title>Codex Login Complete</title></head><body><h1>Login complete</h1><p>You can return to rplaca.</p></body></html>")

(defun openai-oauth-error-page (message)
  "Return a minimal HTML error page for OAuth failures."
  (format nil "<!doctype html><html><head><meta charset=\"utf-8\"><title>Codex Login Failed</title></head><body><h1>Login failed</h1><pre>~A</pre></body></html>"
          (html-escape message)))

(defun openai-oauth-send-http-response (stream status reason body)
  "Write a small HTTP response with BODY to STREAM."
  (format stream "HTTP/1.1 ~D ~A~C~C" status reason #\Return #\Linefeed)
  (format stream "Content-Type: text/html; charset=utf-8~C~C" #\Return #\Linefeed)
  (format stream "Content-Length: ~D~C~C" (length body) #\Return #\Linefeed)
  (format stream "Connection: close~C~C~C~C" #\Return #\Linefeed #\Return #\Linefeed)
  (write-string body stream)
  (finish-output stream))

(defun openai-oauth-flow-set-result (flow &key success cancelled error token)
  "Record FLOW's completion state exactly once under its lock.
Return FLOW and a second value indicating whether this call won completion."
  (let ((completed-now-p nil))
    (bt:with-lock-held ((openai-oauth-flow-lock flow))
      (unless (openai-oauth-flow-done-p flow)
        (setf (openai-oauth-flow-done-p flow) t
              (openai-oauth-flow-success-p flow) success
              (openai-oauth-flow-cancelled-p flow) cancelled
              (openai-oauth-flow-error flow) error
              (openai-oauth-flow-token flow) token
              completed-now-p t)))
    ;; Workers only publish completion and wake the normal CLIM redisplay path.
    ;; Public display observers are deliberately deferred until the frame
    ;; process claims and applies the exact pending flow.
    (when (and completed-now-p (openai-oauth-flow-buffer flow))
      (ignore-errors
        (wake-buffer-display-change (openai-oauth-flow-buffer flow) :oauth)))
    (values flow completed-now-p)))

(defun openai-oauth-flow-snapshot (flow)
  "Return a plist snapshot of FLOW for safe polling from the UI."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (list :done-p (openai-oauth-flow-done-p flow)
          :success-p (openai-oauth-flow-success-p flow)
          :cancelled-p (openai-oauth-flow-cancelled-p flow)
          :error (openai-oauth-flow-error flow)
          :token (openai-oauth-flow-token flow)
          :auth-url (openai-oauth-flow-auth-url flow)
          :redirect-uri (openai-oauth-flow-redirect-uri flow)
          :port (openai-oauth-flow-port flow)
          :client-active-p
          (not (null (openai-oauth-flow-client-stream flow))))))

(defun openai-oauth-flow-thread-snapshot (flow)
  "Return FLOW's worker thread under the flow lock."
  (and flow
       (bt:with-lock-held ((openai-oauth-flow-lock flow))
         (openai-oauth-flow-thread flow))))

(defun register-openai-oauth-client (flow socket stream)
  "Publish FLOW's accepted client unless FLOW has already completed."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (when (openai-oauth-flow-done-p flow)
      (return-from register-openai-oauth-client nil))
    (setf (openai-oauth-flow-client-socket flow) socket
          (openai-oauth-flow-client-stream flow) stream)
    t))

(defun unregister-openai-oauth-client (flow socket stream)
  "Forget SOCKET and STREAM when they are still FLOW's active client."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (when (eq socket (openai-oauth-flow-client-socket flow))
      (setf (openai-oauth-flow-client-socket flow) nil))
    (when (eq stream (openai-oauth-flow-client-stream flow))
      (setf (openai-oauth-flow-client-stream flow) nil)))
  flow)

(defun detach-openai-oauth-client (flow)
  "Atomically detach and return FLOW's active client stream and socket."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (multiple-value-prog1
        (values (openai-oauth-flow-client-stream flow)
                (openai-oauth-flow-client-socket flow))
      (setf (openai-oauth-flow-client-stream flow) nil
            (openai-oauth-flow-client-socket flow) nil))))

#+sbcl
(defun interrupt-openai-oauth-client (stream socket)
  "Interrupt one accepted OAuth client without holding FLOW's lock.
The server worker owns STREAM's final close.  Closing an SBCL fd-stream from a
second thread can wait on its read lock, so cancellation shuts the socket down
and lets the blocked reader unwind and close both objects itself."
  (if socket
      (ignore-errors
        (sb-bsd-sockets:socket-shutdown socket :direction :io))
      (when stream
        (ignore-errors
          (close stream :abort t)))))

(defun detach-openai-oauth-listener (flow)
  "Atomically detach and return FLOW's listening socket."
  (bt:with-lock-held ((openai-oauth-flow-lock flow))
    (prog1 (openai-oauth-flow-listener flow)
      (setf (openai-oauth-flow-listener flow) nil))))

#+sbcl
(defun close-openai-oauth-listener (flow)
  "Close FLOW's listener at most once, outside the flow lock."
  (let ((listener (detach-openai-oauth-listener flow)))
    (when listener
      (ignore-errors
        (sb-bsd-sockets:socket-close listener))))
  flow)

(defun openai-oauth-flow-cancelled-p-safe (flow)
  "Return true when FLOW has been cancelled, holding its state lock."
  (and flow
       (bt:with-lock-held ((openai-oauth-flow-lock flow))
         (openai-oauth-flow-cancelled-p flow))))

#+sbcl
(defun wake-openai-oauth-listener (flow)
  "Wake FLOW's blocking localhost ACCEPT so its worker can observe cancel."
  (let ((socket nil))
    (unwind-protect
         (handler-case
             (progn
               (setf socket
                     (make-instance 'sb-bsd-sockets:inet-socket
                                    :type :stream
                                    :protocol :tcp))
               (sb-bsd-sockets:socket-connect
                socket
                #(127 0 0 1)
                (openai-oauth-flow-port flow))
               t)
           (error () nil))
      (when socket
        (ignore-errors
          (sb-bsd-sockets:socket-close socket))))))

(defun maybe-open-url-in-browser (url)
  "Best-effort browser opener for URL. Returns non-nil when a command launched."
  (or (let ((browser (uiop:getenv "BROWSER")))
        (when (and browser (plusp (length browser)))
          (ignore-errors
            (uiop:launch-program (list browser url)
                                 :input nil :output nil :error-output nil
                                 :ignore-error-status t))))
      (ignore-errors
        (uiop:launch-program (list "xdg-open" url)
                             :input nil :output nil :error-output nil
                             :ignore-error-status t))
      (ignore-errors
        (uiop:launch-program (list "open" url)
                             :input nil :output nil :error-output nil
                             :ignore-error-status t))))

(defun bind-openai-oauth-listener (&optional (preferred-port *openai-oauth-default-port*))
  "Bind a localhost TCP listener, preferring PREFERRED-PORT and falling back to an ephemeral port."
  #+sbcl
  (labels ((try-bind (port)
             (let ((listener (make-instance 'sb-bsd-sockets:inet-socket
                                            :type :stream
                                            :protocol :tcp)))
               (setf (sb-bsd-sockets:sockopt-reuse-address listener) t)
               (handler-case
                   (progn
                     (sb-bsd-sockets:socket-bind listener #(127 0 0 1) port)
                     (sb-bsd-sockets:socket-listen listener 5)
                     (multiple-value-bind (address actual-port)
                         (sb-bsd-sockets:socket-name listener)
                       (declare (ignore address))
                       (values listener actual-port)))
                 (error (e)
                   (ignore-errors (sb-bsd-sockets:socket-close listener))
                   (error e))))))
    (handler-case
        (try-bind preferred-port)
      (error ()
        (try-bind 0))))
  #-sbcl
  (error "Local OpenAI Codex OAuth login currently requires SBCL"))

(defun openai-oauth-read-request-target (stream)
  "Read a single HTTP request from STREAM and return its method and target."
  (let ((request-line (read-line stream nil nil)))
    (when request-line
      (loop :for line := (read-line stream nil nil)
            :for trimmed := (and line (string-trim '(#\Return) line))
            :while (and trimmed (plusp (length trimmed))))
      (let ((parts (split-string-by-char request-line #\Space)))
        (values (first parts) (second parts))))))

(defun openai-oauth-request-path (target)
  "Return TARGET's path portion without its query string."
  (let ((query-pos (position #\? target)))
    (subseq target 0 (or query-pos (length target)))))

(defun run-openai-oauth-server (flow)
  "Serve one localhost OAuth callback request for FLOW."
  #+sbcl
  (handler-case
      (let ((listener
              (bt:with-lock-held ((openai-oauth-flow-lock flow))
                (openai-oauth-flow-listener flow)))
            (rejected-requests 0))
        (loop
          (multiple-value-bind (client-socket peer-address)
              (sb-bsd-sockets:socket-accept listener)
            (declare (ignore peer-address))
            (unwind-protect
                 (unless (openai-oauth-flow-cancelled-p-safe flow)
                   (let ((stream (sb-bsd-sockets:socket-make-stream
                                  client-socket
                                  :input t
                                  :output t
                                  :element-type 'character
                                  :external-format :utf-8
                                  :buffering :line)))
                     (unwind-protect
                          (when (register-openai-oauth-client
                                 flow client-socket stream)
                            (multiple-value-bind (method target)
                                (openai-oauth-read-request-target stream)
                              (cond
                            ((or (null method) (null target))
                             (openai-oauth-send-http-response
                              stream 400 "Bad Request"
                              (openai-oauth-error-page "Malformed callback request"))
                             (openai-oauth-flow-set-result flow :error "Malformed callback request")
                             (return))
                            ((not (string= method "GET"))
                             (openai-oauth-send-http-response
                              stream 405 "Method Not Allowed"
                              (openai-oauth-error-page "Only GET callbacks are supported"))
                             (incf rejected-requests)
                             (when (>= rejected-requests
                                       *openai-oauth-max-rejected-callback-requests*)
                               (openai-oauth-flow-set-result
                                flow
                                :error "Too many rejected OAuth callback requests")
                               (return)))
                            ((not (string= (openai-oauth-request-path target)
                                           *openai-oauth-callback-path*))
                             (openai-oauth-send-http-response
                              stream 404 "Not Found"
                              (openai-oauth-error-page "Unknown callback path"))
                             (incf rejected-requests)
                             (when (>= rejected-requests
                                       *openai-oauth-max-rejected-callback-requests*)
                               (openai-oauth-flow-set-result
                                flow
                                :error "Too many rejected OAuth callback requests")
                               (return)))
                            (t
                             (handler-case
                                 (let* ((callback-url
                                          (format nil "~A~A"
                                                  (openai-oauth-flow-redirect-uri flow)
                                                  (let ((query-pos (position #\? target)))
                                                    (if query-pos
                                                        (subseq target query-pos)
                                                        ""))))
                                        (token
                                          (openai-codex-oauth-finish
                                           callback-url
                                           (openai-oauth-flow-code-verifier flow)
                                           (openai-oauth-flow-state flow)
                                           :redirect-uri
                                           (openai-oauth-flow-redirect-uri flow))))
                                   (openai-oauth-flow-set-result flow
                                                                 :success t
                                                                 :token token)
                                   (openai-oauth-send-http-response
                                    stream 200 "OK" (openai-oauth-success-page))
                                   (return))
                               (error (e)
                                 (openai-oauth-flow-set-result
                                  flow :error (format nil "~A" e))
                                 (openai-oauth-send-http-response
                                  stream 400 "Bad Request"
                                  (openai-oauth-error-page (format nil "~A" e)))
                                   (return)))))))
                       (unregister-openai-oauth-client
                        flow client-socket stream)
                       (ignore-errors
                         (close stream)))))
              (ignore-errors
                (sb-bsd-sockets:socket-close client-socket))))))
    (error (e)
      (unless (openai-oauth-flow-cancelled-p-safe flow)
        (openai-oauth-flow-set-result flow :error (format nil "~A" e)))))
  #-sbcl
  (declare (ignore flow)))

(defun start-openai-codex-oauth-login
    (&key buffer (open-browser-p t) (thread-constructor #'bt:make-thread))
  "Start the localhost OpenAI Codex OAuth PKCE flow and return the flow object."
  (multiple-value-bind (listener actual-port)
      (bind-openai-oauth-listener)
    (let ((flow nil)
          (worker-owns-listener-p nil))
      (unwind-protect
           (let* ((redirect-uri (openai-oauth-redirect-uri actual-port))
                  (auth-url nil)
                  (code-verifier nil)
                  (state nil))
             (multiple-value-setq (auth-url code-verifier state)
               (openai-codex-oauth-start :redirect-uri redirect-uri))
             (setf flow (make-openai-oauth-flow :buffer buffer
                                                :auth-url auth-url
                                                :redirect-uri redirect-uri
                                                :port actual-port
                                                :code-verifier code-verifier
                                                :state state
                                                :listener listener))
             (let ((thread
                     (funcall
                      thread-constructor
                      (lambda ()
                        (unwind-protect
                             (run-openai-oauth-server flow)
                          #+sbcl
                          (close-openai-oauth-listener flow)))
                      :name "rplaca-openai-oauth")))
               ;; Once the constructor returns, only the flow/worker lifecycle
               ;; closes the listener.  Publish the thread under the same lock
               ;; used by teardown snapshots.
               (setf worker-owns-listener-p t)
               (bt:with-lock-held ((openai-oauth-flow-lock flow))
                 (setf (openai-oauth-flow-thread flow) thread)))
             (when open-browser-p
               (maybe-open-url-in-browser auth-url))
             flow)
        (unless worker-owns-listener-p
          (if flow
              #+sbcl (close-openai-oauth-listener flow)
              #-sbcl flow
              (ignore-errors
                #+sbcl (sb-bsd-sockets:socket-close listener))))))))

(defun cancel-openai-codex-oauth-login (flow)
  "Cancel FLOW, interrupt its accepted client, and shut down its listener."
  (multiple-value-bind (result cancelled-now-p)
      (openai-oauth-flow-set-result flow :cancelled t)
    (declare (ignore result))
    (when cancelled-now-p
      (multiple-value-bind (stream socket)
          (detach-openai-oauth-client flow)
        #+sbcl
        (interrupt-openai-oauth-client stream socket)))
  ;; Closing a listening descriptor from another thread does not portably wake
  ;; an ACCEPT already blocked in the kernel.  Queue one localhost connection
  ;; first; the server observes CANCELLED before reading from that client.
    #+sbcl
    (wake-openai-oauth-listener flow)
    #+sbcl
    (close-openai-oauth-listener flow)
    (values flow cancelled-now-p)))

(defparameter *provider-fallback-models*
  '((:openai-codex . *openai-codex-model*)
    (:zai . *zai-model*)
    (:openrouter . *openrouter-model*)
    (:e2e . *e2e-model*))
  "Alist mapping provider keywords to the variable holding their default model.
Each cdr is a symbol naming a special variable; provider-fallback-model
dereferences it at call time so that user customizations take effect.")

(defun supported-provider-keywords ()
  "Return provider keywords available in the current runtime."
  (append '(:openai-codex :zai :openrouter)
          (when (e2e-provider-enabled-p)
            '(:e2e))))

(defun supported-provider-message ()
  "Return a human-readable provider list for validation errors."
  (format nil "~{~A~^, ~}"
          (mapcar (lambda (provider)
                    (format nil ":~A" (symbol-name provider)))
                  (supported-provider-keywords))))

(defun known-provider-p (provider)
  "Return non-nil when PROVIDER is supported locally."
  (member provider (supported-provider-keywords) :test #'eq))

(defun canonicalize-provider-name (provider-name)
  "Return a normalized comparison key for PROVIDER-NAME."
  (string-downcase
   (remove-if-not #'alphanumericp
                  (string provider-name))))

(defun normalize-provider (provider)
  "Normalize PROVIDER to a supported keyword, or nil when absent."
  (cond
    ((null provider) nil)
    ((keywordp provider)
     (if (known-provider-p provider)
         provider
         (error "Unknown provider ~S. Supported providers: ~A"
                provider (supported-provider-message))))
    ((stringp provider)
     (let ((normalized-name (canonicalize-provider-name provider)))
       (or (find normalized-name
                 (supported-provider-keywords)
                 :key (lambda (candidate)
                        (canonicalize-provider-name (symbol-name candidate)))
                 :test #'string=)
           (error "Unknown provider ~S. Supported providers: ~A"
                  provider (supported-provider-message)))))
    ((symbolp provider)
     (normalize-provider (symbol-name provider)))
    (t
     (error "Unknown provider ~S. Supported providers: ~A"
            provider (supported-provider-message)))))

(defun blank-string-p (value)
  "Return non-nil when VALUE is nil or all whitespace."
  (or (null value)
      (zerop (length (string-trim '(#\Space #\Tab #\Newline #\Return) value)))))

(defun normalize-agent-name-key (agent-name)
  "Normalize AGENT-NAME for registry lookup."
  (unless (stringp agent-name)
    (error "Agent name must be a string, got ~S" agent-name))
  (let ((trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) agent-name)))
    (when (blank-string-p trimmed)
      (error "Agent name must be a non-empty string"))
    (string-downcase trimmed)))

(defun normalize-tool-name (tool-name)
  "Normalize TOOL-NAME into the API-facing tool name string."
  (let* ((raw (etypecase tool-name
                (string tool-name)
                (symbol (symbol-name tool-name))))
         (trimmed (string-trim '(#\Space #\Tab #\Newline #\Return) raw)))
    (when (blank-string-p trimmed)
      (error "Tool name must be non-empty"))
    (string-downcase
     (substitute #\_ #\- trimmed))))

(defun normalize-tool-name-list (tool-names)
  "Normalize TOOL-NAMES into a duplicate-free list of API-facing strings.
NIL means no explicit tool allowlist."
  (when tool-names
    (let ((names (cond
                   ((or (stringp tool-names)
                        (symbolp tool-names))
                    (list tool-names))
                   ((vectorp tool-names)
                    (coerce tool-names 'list))
                   ((listp tool-names)
                    tool-names)
                   (t
                    (error "Tool names must be a string, symbol, list, vector, or NIL")))))
      (remove-duplicates
       (mapcar #'normalize-tool-name names)
       :test #'string=))))

(defun register-agent-definition (name &key provider model think-level
                                       core-prompt personality-prompt tool-names)
  "Register or update an agent definition for NAME.
NAME is stored as given for display, while lookups are keyed case-insensitively."
  (let* ((trimmed-name (string-trim '(#\Space #\Tab #\Newline #\Return) name))
         (registry-key (normalize-agent-name-key trimmed-name))
         (normalized-provider (normalize-provider provider))
         (normalized-think-level (normalize-think-level-override think-level))
         (normalized-tool-names (normalize-tool-name-list tool-names))
         (definition (make-agent-definition :name trimmed-name
                                            :provider normalized-provider
                                            :model model
                                            :think-level normalized-think-level
                                            :core-prompt core-prompt
                                            :personality-prompt personality-prompt
                                            :tool-names normalized-tool-names
                                            :package
                                            (and *current-rplaca-package*
                                                 (package-identifier-string
                                                  *current-rplaca-package*)))))
    (when (and model (blank-string-p model))
      (error "Agent model must be a non-empty string"))
    (call-with-agent-definition-registry-lock
     (lambda ()
       (setf (gethash registry-key *agent-definition-registry*) definition)))
    definition))

(defun find-agent-definition (agent-name)
  "Return the registered agent definition for AGENT-NAME, or NIL."
  (when agent-name
    (call-with-agent-definition-registry-lock
     (lambda ()
       (gethash (normalize-agent-name-key agent-name)
                *agent-definition-registry*)))))

(defun list-agent-definitions ()
  "Return all registered agent definitions sorted by name."
  (let ((definitions (mapcar #'cdr (agent-definition-registry-snapshot))))
    (sort definitions #'string<
          :key (lambda (definition)
                 (string-downcase (agent-definition-name definition))))))

(defun package-owned-agent-definitions (package-name)
  "Return agent definitions currently registered by PACKAGE-NAME."
  (let ((owner (package-identifier-string package-name)))
    (remove-if-not
     (lambda (definition)
       (string= owner (or (agent-definition-package definition) "")))
     (list-agent-definitions))))

(defun remove-agent-definitions-for-package (package-name)
  "Remove and return agent definitions currently owned by PACKAGE-NAME."
  (let ((owner (package-identifier-string package-name)))
    (call-with-agent-definition-registry-lock
     (lambda ()
       (let ((definitions nil)
             (keys nil))
         (maphash
          (lambda (key definition)
            (when (string= owner (or (agent-definition-package definition) ""))
              (push key keys)
              (push definition definitions)))
          *agent-definition-registry*)
         (dolist (key keys)
           (remhash key *agent-definition-registry*))
         definitions)))))

(defun agent-definition-provider-for-name (agent-name)
  "Return AGENT-NAME's programmatic default provider, or NIL."
  (let ((definition (find-agent-definition agent-name)))
    (and definition
         (agent-definition-provider definition))))

(defun agent-definition-model-for-name (agent-name provider)
  "Return AGENT-NAME's programmatic model when it matches PROVIDER."
  (let ((definition (find-agent-definition agent-name)))
    (when (and definition
               (eq provider (agent-definition-provider definition)))
      (agent-definition-model definition))))

(defun agent-definition-think-level-for-name (agent-name provider model)
  "Return AGENT-NAME's programmatic think level when PROVIDER/MODEL support it."
  (let ((definition (find-agent-definition agent-name)))
    (when definition
      (let ((think-level (agent-definition-think-level definition)))
        (when (and think-level
                   (think-level-supported-p provider model think-level))
          think-level)))))

(defun agent-definition-tool-names-for-name (agent-name)
  "Return AGENT-NAME's programmatic tool allowlist, or NIL for default tools."
  (let ((definition (find-agent-definition agent-name)))
    (when definition
      (copy-list (agent-definition-tool-names definition)))))

(defun provider-fallback-model (provider)
  "Return the fallback model for PROVIDER.
Looks up the provider-specific variable in *provider-fallback-models* and
returns its current value, so user customizations (e.g. via init.lisp) are
respected."
  (let ((entry (cdr (assoc provider *provider-fallback-models*))))
    (when entry (symbol-value entry))))

;;; --------------------------------------------------------------------------
;;; Known Models Per Provider
;;; --------------------------------------------------------------------------

(defparameter *provider-known-models*
  '((:openai-codex
     "gpt-5.6-sol"
     "gpt-5.6-terra"
     "gpt-5.6-luna"
     "gpt-5.5"
     "gpt-5.3-codex"
     "gpt-5.4"
     "gpt-5.4-mini"
     "gpt-5.4-nano"
     "gpt-5.2-codex"
     "gpt-5.1-codex-max"
     "gpt-5.1-codex-mini"
     "gpt-5.2")
    (:zai
     "glm-5.2"
     "glm-5.1"
     "glm-5"
     "glm-5-turbo"
     "glm-4.7"
     "glm-4.6"
     "glm-4.5"
     "glm-4.5-air")
    (:openrouter
     "openai/gpt-5.6-sol"
     "openai/gpt-5.6-terra"
     "openai/gpt-5.6-luna"
     "openai/gpt-5.5"
     "openai/gpt-5.4-mini"
     "openai/gpt-5.4-nano"
     "z-ai/glm-5.2"
     "z-ai/glm-5.1"
     "anthropic/claude-fable-5"
     "anthropic/claude-opus-5"
     "anthropic/claude-sonnet-5"
     "anthropic/claude-haiku-4.5"
     "google/gemini-3.6-flash"
     "google/gemini-3.5-flash"
     "google/gemini-3.5-flash-lite"
     "google/gemini-3.1-pro-preview"
     "openai/gpt-5.3-codex"
     "openai/gpt-5.2"
     "openai/gpt-5.1"
     "openai/gpt-4.1"
     "openai/gpt-4o"
     "google/gemini-2.5-pro"
     "google/gemini-2.5-flash"
     "z-ai/glm-4.6"
     "deepseek/deepseek-r1"
     "meta-llama/llama-4-maverick")
    (:e2e
     "e2e-model"))
  "Known model identifiers grouped by provider.
The first model in each list is the provider's default.
For :OPENROUTER, models are dynamically fetched by fetch-openrouter-models when
an API key is configured; this static list is used as a fallback.
These are used by the model selector overlay.")

(defparameter *openai-codex-model-think-levels*
  '(("gpt-5.6-sol" "none" "low" "medium" "high" "xhigh" "max")
    ("gpt-5.6-terra" "none" "low" "medium" "high" "xhigh" "max")
    ("gpt-5.6-luna" "none" "low" "medium" "high" "xhigh" "max")
    ("gpt-5.5" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.4" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.4-mini" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.4-nano" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.3-codex" "low" "medium" "high" "xhigh")
    ("gpt-5.2-codex" "low" "medium" "high" "xhigh")
    ("gpt-5.2" "none" "low" "medium" "high" "xhigh")
    ("gpt-5.1-codex" "none" "low" "medium" "high")
    ("gpt-5.1-codex-max" "none" "low" "medium" "high")
    ("gpt-5.1-codex-mini" "none" "low" "medium" "high"))
  "Supported reasoning effort values for known OpenAI-Codex models.
Values are ordered from lowest to highest effort, excluding the synthetic
\"default\" selector entry handled by the UI.")

(defparameter *zai-model-think-levels*
  '(("glm-5.2" "high" "max"))
  "Supported reasoning effort values for known Z.AI models.")

(defun provider-known-models (provider)
  "Return the list of known model names for PROVIDER.
For :OPENROUTER, returns the dynamically-fetched model list when available,
falling back to the static *provider-known-models* entry."
  (if (and (eq provider :openrouter) *openrouter-cached-models*)
      *openrouter-cached-models*
      (cdr (assoc provider *provider-known-models*))))

(defun fetch-openrouter-models ()
  "Fetch the list of available models from the OpenRouter API.
Populates *openrouter-cached-models* and returns the model ID list.
Requires a valid OpenRouter API key to be configured.
Returns the cached list on subsequent calls; set *openrouter-cached-models* to
nil to force a refresh. Returns the static fallback list on any error."
  (when *openrouter-cached-models*
    (return-from fetch-openrouter-models *openrouter-cached-models*))
  (handler-case
      (let ((token (read-provider-token :openrouter)))
        (unless token
          (return-from fetch-openrouter-models
            (cdr (assoc :openrouter *provider-known-models*))))
        (multiple-value-bind (body status-code)
            (provider-http-request-with-retries
             "OpenRouter models"
             (lambda ()
               (drakma:http-request
                *openrouter-models-url*
                :method :get
                :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token)))
                :want-stream nil
                :force-binary nil
                :connection-timeout
                *provider-http-connection-timeout-seconds*)))
          (if (= status-code 200)
              (let* ((body-string (http-body-string body))
                     (response (api-json-decode body-string))
                     (models-data (cdr (assoc :data response)))
                     (ids (loop :for m :in (coerce models-data 'list)
                                :for id := (cdr (assoc :id m))
                                :when (and id (stringp id) (plusp (length id)))
                                  :collect id)))
                (when ids
                  (setf *openrouter-cached-models* ids))
                (or ids (cdr (assoc :openrouter *provider-known-models*))))
              (cdr (assoc :openrouter *provider-known-models*)))))
    (error ()
      (cdr (assoc :openrouter *provider-known-models*)))))

(defun provider-has-token-p (provider)
  "Return non-nil when PROVIDER has a usable API key or OAuth token configured."
  (handler-case
      (cond
        ((eq provider :e2e)
         (e2e-provider-enabled-p))
        ((eq provider :openai-codex)
         (let ((auth (resolve-openai-codex-auth)))
           (and auth
                (stringp (getf auth :token))
                (plusp (length (getf auth :token))))))
        (t
         (let ((token (read-provider-token provider)))
           (and token (stringp token) (plusp (length token))))))
    (error () nil)))

(defun provider-model-supported-think-levels (provider model)
  "Return the supported think levels for PROVIDER and MODEL, or NIL."
  (when (stringp model)
    (copy-list
     (cdr
      (assoc model
             (case provider
               (:openai-codex *openai-codex-model-think-levels*)
               (:zai *zai-model-think-levels*))
             :test #'string=)))))

(defun think-level-supported-p (provider model think-level)
  "Return non-nil when THINK-LEVEL is supported by PROVIDER and MODEL."
  (member think-level
          (provider-model-supported-think-levels provider model)
          :test #'string=))

(defun resolved-buffer-think-level (buf provider model)
  "Return BUF's validated think-level override for PROVIDER and MODEL, or NIL."
  (let ((think-level (normalize-think-level-override
                      (buffer-think-level-override buf))))
    (when (and think-level
               (think-level-supported-p provider model think-level))
      think-level)))

(defun reconcile-buffer-think-level-override (buf &key provider model)
  "Reconcile BUF's think-level override against PROVIDER and MODEL.
Returns two values: status keyword and resulting think level."
  (multiple-value-bind (resolved-provider resolved-model)
      (if (and provider model)
          (values provider model)
          (resolve-buffer-provider-and-model buf))
    (let* ((old-think (normalize-think-level-override
                       (buffer-think-level-override buf)))
           (new-think (resolved-buffer-think-level buf
                                                   resolved-provider
                                                   resolved-model)))
      (setf (buffer-think-level-override buf) new-think)
      (values (cond
                ((and old-think new-think
                      (string= old-think new-think))
                 :kept)
                ((and old-think (null new-think))
                 :reset)
                (t
                 :default))
              new-think))))

(defun available-models-for-selector (buf)
  "Build the model selector entry list for BUF.
Returns a list of plists: ((:provider :zai :model \"name\" :active-p t/nil) ...)
Only includes providers that have a valid API key. The entry matching BUF's
currently resolved provider/model is marked :active-p t.
For :OPENROUTER, dynamically-fetched models are used when an API key is present."
  (multiple-value-bind (current-provider current-model)
      (handler-case (resolve-buffer-provider-and-model buf)
        (error () (values nil nil)))
    (let ((entries nil))
      (dolist (provider-models *provider-known-models*)
        (let* ((provider (car provider-models))
               (models (if (eq provider :openrouter)
                           (fetch-openrouter-models)
                           (cdr provider-models))))
          (when (provider-has-token-p provider)
            (dolist (model models)
              (push (list :provider provider
                          :model model
                          :active-p (and (eq provider current-provider)
                                         (string= model current-model)))
                    entries)))))
      (nreverse entries))))

(defun json-key-string (key)
  "Convert a decoded JSON key into a lowercase string."
  (string-downcase
   (etypecase key
     (string key)
     (symbol (symbol-name key)))))

(defun lookup-json-value (alist key)
  "Find KEY in decoded JSON ALIST, handling string and symbol keys."
  (let ((name (string-downcase key)))
    (loop :for (entry-key . value) :in alist
          :when (string= name (json-key-string entry-key))
            :do (return value))))

(defun make-agent-defaults-registry ()
  "Create an empty in-memory agent defaults registry."
  (list :agents (make-hash-table :test #'equal)))

(defun registry-agents (registry)
  "Return the agent table stored in REGISTRY."
  (getf registry :agents))

(defun agent-default-spec (agent-name)
  "Return the stored default spec for AGENT-NAME, or nil."
  (ensure-agent-defaults-loaded)
  (gethash (string-downcase agent-name)
           (registry-agents *agent-defaults-registry*)))

(defun load-agent-defaults ()
  "Load and memoize persisted agent defaults, overlaying built-in fallbacks."
  (let ((registry (make-agent-defaults-registry))
        (read-path
          (configured-migration-read-path
           *agent-defaults-path*
           +default-agent-defaults-path+
           +legacy-agent-defaults-path+
           :label "agent defaults")))
    (when (probe-file read-path)
      (let* ((json (uiop:read-file-string read-path))
             (data (cl-json:decode-json-from-string json))
             (agents (registry-agents registry)))
        (dolist (entry data)
          (let* ((agent-name (json-key-string (car entry)))
                 (spec (cdr entry))
                 (provider (normalize-provider (lookup-json-value spec "provider")))
                 (model (lookup-json-value spec "model")))
            (setf (gethash agent-name agents)
                  (list :provider provider
                        :model (and (stringp model)
                                    model)))))))
    (setf *agent-defaults-registry* registry)))

(defun save-agent-defaults ()
  "Persist the current agent defaults registry to disk."
  (ensure-agent-defaults-loaded)
  (let ((payload nil))
    (maphash
     (lambda (agent-name spec)
       (let ((provider (getf spec :provider))
             (model (getf spec :model)))
         (push `(,agent-name . ((:provider . ,(and provider
                                                   (string-downcase (symbol-name provider))))
                                ,@(when model
                                    `((:model . ,model)))))
               payload)))
     (registry-agents *agent-defaults-registry*))
    (ensure-directories-exist *agent-defaults-path*)
    (with-open-file (stream *agent-defaults-path*
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (write-string (api-json-encode (nreverse payload)) stream))
    *agent-defaults-path*))

(defun ensure-agent-defaults-loaded ()
  "Load the agent defaults registry on first use."
  (or *agent-defaults-registry*
      (load-agent-defaults)))

(defun agent-default (agent-name)
  "Return AGENT-NAME's default provider, or *default-provider*."
  (or (agent-definition-provider-for-name agent-name)
      (getf (agent-default-spec agent-name)
            :provider)
      *default-provider*))

(defun agent-default-model (agent-name provider)
  "Return AGENT-NAME's stored model when it matches PROVIDER."
  (or (agent-definition-model-for-name agent-name provider)
      (let ((spec (agent-default-spec agent-name)))
        (when (eq provider (getf spec :provider))
          (getf spec :model)))))

(defun agent-known-persisted-default-names ()
  "Return the agent names that exist in the persisted defaults registry."
  (ensure-agent-defaults-loaded)
  (let ((names nil))
    (maphash (lambda (name spec)
               (declare (ignore spec))
               (push name names))
             (registry-agents *agent-defaults-registry*))
    names))

(defun list-known-agent-names ()
  "Return known agent names from programmatic definitions and compatibility defaults."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (definition (list-agent-definitions))
      (setf (gethash (normalize-agent-name-key (agent-definition-name definition)) table)
            (agent-definition-name definition)))
    (dolist (name (agent-known-persisted-default-names))
      (setf (gethash (normalize-agent-name-key name) table) name))
    (when *default-agent-name*
      (setf (gethash (normalize-agent-name-key *default-agent-name*) table)
            *default-agent-name*))
    (let ((names nil))
      (maphash (lambda (name-key name)
                 (declare (ignore name-key))
                 (push name names))
               table)
      (sort names #'string< :key #'string-downcase))))

(defun set-agent-default (agent-name provider &key model)
  "Set AGENT-NAME's default provider and optional MODEL, then persist it."
  (ensure-agent-defaults-loaded)
  (let ((normalized-provider (normalize-provider provider)))
    (when (and model (blank-string-p model))
      (error "Resolved model must be a non-empty string"))
    (setf (gethash (string-downcase agent-name)
                   (registry-agents *agent-defaults-registry*))
          (list :provider normalized-provider
                :model model))
    (save-agent-defaults)
    normalized-provider))

(defun resolve-buffer-provider-and-model-base (buf)
  "Resolve BUF's effective provider, model, and think level.
Resolution order for provider: buffer override → agent definition → legacy agent
default → *default-provider*.
Resolution order for model: buffer override → agent definition → legacy agent
default → provider fallback → *default-model*.
Think level resolution: buffer override → agent definition → nil, with support
checked against the resolved provider/model."
  (ensure-agent-defaults-loaded)
  (let* ((provider (or (buffer-provider-override buf)
                       (agent-default (buffer-agent-name buf))
                       *default-provider*))
         (resolved-provider (normalize-provider provider))
         (model (or (buffer-model-override buf)
                    (agent-default-model (buffer-agent-name buf) resolved-provider)
                    (provider-fallback-model resolved-provider)
                    *default-model*))
         (think-level (or (resolved-buffer-think-level buf resolved-provider model)
                          (agent-definition-think-level-for-name (buffer-agent-name buf)
                                                                 resolved-provider
                                                                 model))))
    (when (blank-string-p model)
      (error "Resolved model must be a non-empty string"))
    (values resolved-provider model think-level)))

(defun resolve-buffer-provider-and-model (buf)
  "Resolve BUF's effective provider, model, and think level."
  (multiple-value-bind (provider model think-level)
      (resolve-buffer-provider-and-model-base buf)
    (apply-modelaria-routing buf provider model think-level)))

;;; --------------------------------------------------------------------------
;;; Conversation Building
;;; --------------------------------------------------------------------------

(defun build-conversation-messages (buf)
  "Build canonical provider messages from the buffer's chat history.
Uses raw-content when available (for tool_use/tool_result messages),
falls back to plain text content.
System messages (sender :system) are excluded — they are display-only
and should not be sent to the API."
  (let ((messages nil))
    (loop :for msg := (buffer-first-message buf) :then (message-next msg)
          :while (and msg (not (eq msg (buffer-input-message buf))))
          :do (let ((sender (message-sender msg)))
                ;; Skip system messages — they are display-only (shell output, etc.)
                (unless (eq sender :system)
                  (let* ((metadata (message-metadata msg))
                         (package-name
                           (and metadata
                                (message-metadata-value metadata :package-name))))
                    (unless (and package-name
                                 (not (package-active-p package-name
                                                        :buffer buf)))
                      (let* ((role (cond
                                     ((eq sender :user) "user")
                                     ((eq sender :tool-result) "user")
                                     ((eq sender :compaction-summary) "user")
                                     ((eq sender :branch-summary) "user")
                                     ((eq sender :context) "user")
                                     (t "assistant")))
                             (content (canonicalize-message-content
                                       role
                                       (or (message-raw-content msg)
                                           (message-text msg)))))
                        (when (and (eq sender :user)
                                   (null (message-raw-content msg)))
                          (dolist (skill-text
                                    (handler-case
                                        (skill-injection-messages
                                         (message-text msg))
                                      (error () nil)))
                            (let ((skill-content
                                    (canonicalize-message-content
                                     "user"
                                     skill-text)))
                              (push `((:role . "user")
                                      (:content
                                       . ,(coerce skill-content 'vector)))
                                    messages)))
                          (when (fboundp 'mcp-resource-injection-messages)
                            (dolist (resource-text
                                      (handler-case
                                          (funcall
                                           (symbol-function
                                            'mcp-resource-injection-messages)
                                           (message-text msg))
                                        (error () nil)))
                              (let ((resource-content
                                      (canonicalize-message-content
                                       "user"
                                       resource-text)))
                                (push `((:role . "user")
                                        (:content
                                         . ,(coerce resource-content 'vector)))
                                      messages)))))
                        (push `((:role . ,role)
                                (:content . ,(coerce content 'vector)))
                              messages)))))))
    (nreverse messages)))

(defun canonical-tool-use-block (id name input)
  "Return ID/NAME/INPUT as a canonical tool_use block."
  `((:type . "tool_use")
    (:id . ,id)
    (:name . ,name)
    (:input . ,input)))

(defun token-usage-value (usage &rest keys)
  "Return the first numeric value from USAGE under one of KEYS."
  (loop :for key :in keys
        :for cell := (and usage (assoc key usage))
        :for value := (and cell (cdr cell))
        :when (numberp value)
          :return value))

(defun token-usage-nested-value (usage details-key value-key)
  "Return numeric VALUE-KEY from USAGE's DETAILS-KEY object."
  (let* ((details-cell (and usage (assoc details-key usage)))
         (details (and details-cell (cdr details-cell)))
         (value-cell (and details (assoc value-key details)))
         (value (and value-cell (cdr value-cell))))
    (and (numberp value) value)))

(defun normalize-openai-token-usage (usage)
  "Normalize OpenAI token USAGE into rplaca cache telemetry.
Supports both Responses-style names (input_tokens/output_tokens) and
Chat/documented names (prompt_tokens/completion_tokens)."
  (when usage
    (let* ((input (token-usage-value usage :input--tokens :prompt--tokens))
           (output (token-usage-value usage :output--tokens
                                      :completion--tokens))
           (total (or (token-usage-value usage :total--tokens)
                      (and (or input output)
                           (+ (or input 0) (or output 0)))))
           (cached (or (token-usage-nested-value usage
                                                 :input--tokens--details
                                                 :cached--tokens)
                       (token-usage-nested-value usage
                                                 :prompt--tokens--details
                                                 :cached--tokens)
                       (and input 0)))
           (uncached (and input cached (max 0 (- input cached))))
           (hit-rate (and input
                          cached
                          (plusp input)
                          (/ (float cached) input)))
           (result nil))
      (labels ((put (key value)
                 (when value
                   (setf (getf result key) value))))
        (put :input-tokens input)
        (put :output-tokens output)
        (put :total-tokens total)
        (put :cached-input-tokens cached)
        (put :uncached-input-tokens uncached)
        (put :cache-hit-rate hit-rate)
        result))))

(defun token-usage-total-count (usage)
  "Return explicit or derived total token count for normalized USAGE."
  (or (getf usage :total-tokens)
      (let ((input (getf usage :input-tokens))
            (output (getf usage :output-tokens)))
        (and (or input output)
             (+ (or input 0) (or output 0))))))

(defun merge-token-usage (left right)
  "Return aggregate token usage for LEFT and RIGHT normalized usage plists."
  (cond
    ((null left) (copy-list right))
    ((null right) (copy-list left))
    (t
     (let* ((input (and (or (getf left :input-tokens)
                            (getf right :input-tokens))
                        (+ (or (getf left :input-tokens) 0)
                           (or (getf right :input-tokens) 0))))
            (output (and (or (getf left :output-tokens)
                             (getf right :output-tokens))
                         (+ (or (getf left :output-tokens) 0)
                            (or (getf right :output-tokens) 0))))
            (cached (and (or (getf left :cached-input-tokens)
                             (getf right :cached-input-tokens))
                         (+ (or (getf left :cached-input-tokens) 0)
                            (or (getf right :cached-input-tokens) 0))))
            (uncached (and input cached (max 0 (- input cached))))
            (total (cond
                     ((or (token-usage-total-count left)
                          (token-usage-total-count right))
                      (+ (or (token-usage-total-count left) 0)
                         (or (token-usage-total-count right) 0)))
                     ((or input output)
                      (+ (or input 0) (or output 0)))))
            (hit-rate (and input cached (plusp input)
                           (/ (float cached) input)))
            (result nil))
       (labels ((put (key value)
                  (when value
                    (setf (getf result key) value))))
         (put :input-tokens input)
         (put :output-tokens output)
         (put :total-tokens total)
         (put :cached-input-tokens cached)
         (put :uncached-input-tokens uncached)
         (put :cache-hit-rate hit-rate)
         result)))))

(defun token-usage-metadata-pairs (usage)
  "Return message metadata PAIRS for normalized token USAGE."
  (let ((pairs nil))
    (dolist (key '(:input-tokens :output-tokens :total-tokens
                   :cached-input-tokens :uncached-input-tokens
                   :cache-hit-rate)
             pairs)
      (when (member key usage :test #'eq)
        (push (getf usage key) pairs)
        (push key pairs)))))

(defun token-usage-from-metadata (metadata)
  "Return normalized token usage from message METADATA."
  (let ((usage nil))
    (dolist (key '(:input-tokens :output-tokens :total-tokens
                   :cached-input-tokens :uncached-input-tokens
                   :cache-hit-rate)
             usage)
      (let ((cell (assoc key metadata :test #'eq)))
        (when cell
          (push (cdr cell) usage)
          (push key usage))))))

(defun format-token-usage-summary (usage)
  "Return a compact one-line token/cache summary for normalized USAGE."
  (when usage
    (let ((parts nil))
      (labels ((add (label key)
                 (let ((value (getf usage key)))
                   (when value
                     (push (format nil "~A=~D" label value) parts)))))
        (add "input" :input-tokens)
        (add "cached" :cached-input-tokens)
        (add "uncached" :uncached-input-tokens)
        (add "output" :output-tokens)
        (add "total" :total-tokens)
        (let ((hit-rate (getf usage :cache-hit-rate)))
          (when hit-rate
            (push (format nil "cache-hit=~,1F%" (* 100.0 hit-rate))
                  parts)))
        (when parts
          (format nil "tokens: ~{~A~^ ~}" (nreverse parts)))))))

(defun token-usage-json (usage)
  "Return normalized token USAGE as a JSON-ready alist."
  (when usage
    (remove nil
            `((:input--tokens . ,(getf usage :input-tokens))
              (:output--tokens . ,(getf usage :output-tokens))
              (:total--tokens . ,(getf usage :total-tokens))
              (:cached--input--tokens . ,(getf usage :cached-input-tokens))
              (:uncached--input--tokens . ,(getf usage :uncached-input-tokens))
              (:cache--hit--rate . ,(getf usage :cache-hit-rate)))
            :key #'cdr)))

(defun canonical-response (stop-reason content-blocks &key usage)
  "Return a provider-agnostic response payload."
  `((:stop--reason . ,stop-reason)
    (:content . ,(coerce content-blocks 'vector))
    ,@(when usage `((:usage . ,usage)))))

(defun http-body-string (body)
  "Return BODY as a UTF-8 string."
  (if (stringp body)
      body
      (flexi-streams:octets-to-string body :external-format :utf-8)))

(defun provider-http-retryable-status-p (status-code)
  "Return true when STATUS-CODE represents a transient provider failure."
  (and (integerp status-code)
       (member status-code '(408 409 425 429 500 502 503 504) :test #'=)))

(defun provider-http-header-name (name)
  "Return NAME as a lowercase HTTP header name."
  (string-downcase
   (typecase name
     (string name)
     (symbol (symbol-name name))
     (t (princ-to-string name)))))

(defun provider-http-header-value (headers name)
  "Return HTTP header NAME from HEADERS."
  (let ((wanted (provider-http-header-name name)))
    (loop :for header :in headers
          :for header-name := (provider-http-header-name (car header))
          :when (string= wanted header-name)
            :return (cdr header))))

(defun provider-http-retry-after-seconds (headers)
  "Return Retry-After seconds from HEADERS when it is a positive integer."
  (let ((value (provider-http-header-value headers "retry-after")))
    (when (and (stringp value) (plusp (length value)))
      (handler-case
          (let ((seconds (parse-integer value :junk-allowed nil)))
            (and (plusp seconds) seconds))
        (error () nil)))))

(defun provider-http-backoff-delay (attempt headers)
  "Return the retry delay for zero-based ATTEMPT and optional HEADERS."
  (min *provider-http-max-backoff-seconds*
       (or (provider-http-retry-after-seconds headers)
           (* *provider-http-initial-backoff-seconds*
              (expt *provider-http-backoff-multiplier* attempt)))))

(defun provider-http-close-body-for-retry (body)
  "Close BODY when it is a stream abandoned due to a retry."
  (when (streamp body)
    (ignore-errors (close body))))

(defun provider-http-cancellation-requested-p (cancel-p)
  "Return true when optional provider retry cancellation predicate fires."
  (and cancel-p (funcall cancel-p)))

(defun provider-http-cancellable-sleep (delay cancel-p)
  "Wait DELAY seconds and return NIL if CANCEL-P interrupts the wait."
  (cond
    ((provider-http-cancellation-requested-p cancel-p)
     nil)
    ;; Preserve test/application-injected sleepers as one call.  The default
    ;; sleeper is sliced so Stop does not wait through an entire backoff.
    ((not (eq *provider-http-sleep-function* (symbol-function 'sleep)))
     (funcall *provider-http-sleep-function* delay)
     (not (provider-http-cancellation-requested-p cancel-p)))
    (t
     (loop :with remaining := delay
           :while (plusp remaining)
           :for slice := (min remaining *provider-http-cancel-poll-seconds*)
           :when (provider-http-cancellation-requested-p cancel-p)
             :do (return nil)
           :do (sleep slice)
               (decf remaining slice)
           :finally
              (return
                (not (provider-http-cancellation-requested-p cancel-p)))))))

(defun provider-http-sleep-before-retry
    (label attempt headers reason &key cancel-p)
  "Wait cancellably before retrying a provider HTTP request."
  (let ((delay (provider-http-backoff-delay attempt headers)))
    (file-debug-log "provider"
                    "~A transient failure on attempt ~D/~D (~A); retrying in ~,2Fs"
                    label
                    (1+ attempt)
                    (1+ *provider-http-max-retries*)
                    reason
                    delay)
    (provider-http-cancellable-sleep delay cancel-p)))

(defun provider-http-request-with-retries (label thunk &key cancel-p)
  "Call THUNK, retrying transient provider HTTP failures with backoff.

THUNK must perform one HTTP request and return the same values as
drakma:http-request. Transient status codes and connection-level errors are
retried up to *PROVIDER-HTTP-MAX-RETRIES*.  CANCEL-P interrupts retry backoff
and returns an empty response; the owning stream state supplies cancellation
semantics."
  (loop :with attempt := 0
        :do (when (provider-http-cancellation-requested-p cancel-p)
              (return (values nil nil nil)))
            (let ((result
                    (handler-case
                        (multiple-value-list (funcall thunk))
                      (error (condition)
                        (if (provider-http-cancellation-requested-p cancel-p)
                            :cancelled
                            (if (< attempt *provider-http-max-retries*)
                            (progn
                              (if (provider-http-sleep-before-retry
                                   label attempt nil condition
                                   :cancel-p cancel-p)
                                  (progn
                                    (incf attempt)
                                    :retry)
                                  :cancelled))
                            (error condition)))))))
              (when (eq result :cancelled)
                (return (values nil nil nil)))
              (unless (eq result :retry)
                (let ((status-code (second result))
                      (headers (third result)))
                  (cond
                    ((and (provider-http-retryable-status-p status-code)
                          (< attempt *provider-http-max-retries*))
                     (provider-http-close-body-for-retry (first result))
                     (if (provider-http-sleep-before-retry
                          label attempt headers
                          (format nil "HTTP ~D" status-code)
                          :cancel-p cancel-p)
                         (incf attempt)
                         (return (values nil nil nil))))
                    (t
                     (return (values-list result)))))))))

(defun utf8-character-input-stream (stream)
  "Return STREAM as a UTF-8 character input stream."
  (if (nth-value 0 (subtypep (stream-element-type stream) 'character))
      stream
      (flexi-streams:make-flexi-stream stream :external-format :utf-8)))

(defun read-stream-as-utf8-string (stream)
  "Read STREAM fully as UTF-8 text and return the resulting string."
  (let ((text-stream (utf8-character-input-stream stream)))
    (unwind-protect
         (let ((s (make-string-output-stream)))
           (loop :for c := (read-char text-stream nil nil)
                 :while c :do (write-char c s))
           (get-output-stream-string s))
      (when (not (eq text-stream stream))
        (ignore-errors (close text-stream))))))

(defun openai-finish-reason->stop-reason (finish-reason)
  "Normalize OpenAI FINISH-REASON to rplaca stop reasons."
  (cond
    ((null finish-reason) nil)
    ((string= finish-reason "tool_calls") "tool_use")
    ((string= finish-reason "length") "max_tokens")
    ((string= finish-reason "stop") "end_turn")
    (t finish-reason)))

(defun openai-tool-call->canonical-block (tool-call)
  "Convert an OpenAI TOOL-CALL object into a canonical tool_use block."
  (let* ((function (cdr (assoc :function tool-call)))
         (arguments (cdr (assoc :arguments function)))
         (input (cond
                  ((blank-string-p arguments) nil)
                  ((stringp arguments) (api-json-decode arguments))
                  (t arguments))))
    (canonical-tool-use-block
     (cdr (assoc :id tool-call))
     (cdr (assoc :name function))
     input)))

(defun openai-choice->canonical-response (choice)
  "Normalize an OpenAI completion CHOICE to canonical response shape.
Handles reasoning models (Z.AI GLM, DeepSeek R1, etc.) that return
reasoning_content alongside content. When content is blank but
reasoning_content is present, falls back to reasoning_content."
  (let* ((message (cdr (assoc :message choice)))
         (content-blocks nil)
         (text (cdr (assoc :content message)))
         (reasoning (cdr (assoc :reasoning--content message)))
         (tool-calls (cdr (assoc :tool--calls message)))
         ;; Use content if non-blank, otherwise fall back to reasoning
         (effective-text (cond
                           ((not (blank-string-p text)) text)
                           ((not (blank-string-p reasoning)) reasoning)
                           (t nil))))
    (when effective-text
      (push (canonical-text-block effective-text) content-blocks))
    (when reasoning
      (push (canonical-reasoning-block reasoning) content-blocks))
    (dolist (tool-call (coerce (or tool-calls #()) 'list))
      (push (openai-tool-call->canonical-block tool-call) content-blocks))
    (canonical-response
     (openai-finish-reason->stop-reason (cdr (assoc :finish--reason choice)))
     (nreverse content-blocks))))

(defun message-role-content-blocks (message)
  "Extract a decoded provider message into ROLE and canonical content blocks."
  (values (cdr (assoc :role message))
          (coerce (cdr (assoc :content message)) 'list)))

(defun content-blocks->openai-tool-calls (tool-uses)
  "Convert canonical TOOL-USES into OpenAI tool call objects."
  (coerce
   (loop :for tool-use :in tool-uses
         :collect `((:id . ,(cdr (assoc :id tool-use)))
                    (:type . "function")
                    (:function . ((:name . ,(cdr (assoc :name tool-use)))
                                  (:arguments . ,(api-json-encode
                                                  (or (cdr (assoc :input tool-use))
                                                      '())))))))
   'vector))

(defun tool-definitions->openai-tools (tools)
  "Translate rplaca TOOLS to OpenAI-compatible tool definitions."
  (when (and tools (plusp (length tools)))
    (coerce
     (loop :for tool :across tools
           :collect `((:type . "function")
                      (:function . ((:name . ,(cdr (assoc :name tool)))
                                    (:description . ,(cdr (assoc :description tool)))
                                    (:parameters . ,(provider-ready-json-schema
                                                     (cdr (assoc :input--schema tool))))))))
     'vector)))

(defun conversation-messages->openai-messages (messages)
  "Translate canonical conversation MESSAGES into OpenAI chat messages."
  (loop :for message :in messages
        :append
        (multiple-value-bind (role content-blocks)
            (message-role-content-blocks message)
          (let ((text (content-text-blocks content-blocks))
                (tool-uses (content-tool-use-blocks content-blocks))
                (tool-results (remove-if-not (lambda (block)
                                               (string= "tool_result"
                                                        (content-block-type block)))
                                             content-blocks)))
            (cond
              ((string= role "assistant")
               (list `((:role . "assistant")
                       (:content . ,(unless (blank-string-p text) text))
                       ,@(when tool-uses
                           `((:tool--calls . ,(content-blocks->openai-tool-calls tool-uses)))))))
              ((and (string= role "user") tool-results)
               (append
                (when (not (blank-string-p text))
                  (list `((:role . "user")
                          (:content . ,text))))
                 (loop :for block :in tool-results
                       :collect `((:role . "tool")
                                 (:tool--call--id . ,(or (cdr (assoc :tool--use--id block))
                                                         (cdr (assoc :tool-use-id block))))
                                 (:content . ,(cdr (assoc :content block)))))))
              (t
               (list `((:role . ,role)
                       (:content . ,text)))))))))

(defun openai-messages-with-system-prompt (messages &key (system-prompt (build-system-prompt)))
  "Translate MESSAGES and prepend SYSTEM-PROMPT when present."
  (let ((openai-messages (conversation-messages->openai-messages messages)))
    (if system-prompt
        (cons `((:role . "system")
                (:content . ,system-prompt))
              openai-messages)
        openai-messages)))

(defun canonical-text-content-item (role text)
  "Return a Responses API content item for ROLE/TEXT."
  `((:type . ,(if (string= role "assistant")
                  "output_text"
                  "input_text"))
    (:text . ,text)))

(defun tool-result-output-string (content)
  "Normalize tool result CONTENT to a string for function_call_output."
  (cond
    ((null content) "")
    ((stringp content) content)
    (t (api-json-encode content))))

(defun content-blocks->responses-input-items (role content-blocks)
  "Convert canonical ROLE/CONTENT-BLOCKS into Responses API input items."
  (let ((text (content-text-blocks content-blocks))
        (tool-uses (content-tool-use-blocks content-blocks))
        (tool-results (remove-if-not (lambda (block)
                                       (string= "tool_result"
                                                (content-block-type block)))
                                     content-blocks)))
    (append
     (when (not (blank-string-p text))
       (list `((:type . "message")
               (:role . ,role)
               (:content . ,(vector (canonical-text-content-item role text))))))
     (when (string= role "assistant")
       (loop :for tool-use :in tool-uses
             :collect `((:type . "function_call")
                        (:call--id . ,(cdr (assoc :id tool-use)))
                        (:name . ,(cdr (assoc :name tool-use)))
                        (:arguments . ,(api-json-encode
                                        (or (cdr (assoc :input tool-use))
                                            '()))))))
     (when (string= role "user")
       (loop :for block :in tool-results
             :collect `((:type . "function_call_output")
                        (:call--id . ,(or (cdr (assoc :tool--use--id block))
                                          (cdr (assoc :tool-use-id block))))
                        (:output . ,(tool-result-output-string
                                     (cdr (assoc :content block))))))))))

(defun conversation-messages->responses-input (messages)
  "Translate canonical conversation MESSAGES into Responses API input items."
  (loop :for message :in messages
        :append
        (multiple-value-bind (role content-blocks)
            (message-role-content-blocks message)
          (content-blocks->responses-input-items role content-blocks))))

(defun tool-definitions->responses-tools (tools)
  "Translate rplaca TOOLS to OpenAI Responses function tools."
  (when (and tools (plusp (length tools)))
    (coerce
     (loop :for tool :across tools
           :collect `((:type . "function")
                      (:name . ,(cdr (assoc :name tool)))
                      (:description . ,(cdr (assoc :description tool)))
                      (:parameters . ,(provider-ready-json-schema
                                       (cdr (assoc :input--schema tool))))))
     'vector)))

(defun responses-output-item-text (item)
  "Extract displayable assistant text from a Responses ITEM."
  (let ((content (cdr (assoc :content item))))
    (when content
      (let ((text
              (with-output-to-string (out)
                (dolist (part (coerce content 'list))
                  (let ((part-type (cdr (assoc :type part)))
                        (part-text (cdr (assoc :text part))))
                    (when (and part-text
                               (member part-type
                                       '("output_text" "text" "input_text")
                                       :test #'string=))
                      (write-string part-text out)))))))
        (unless (blank-string-p text)
          text)))))

(defun responses-summary-part-text (part)
  "Extract text from one Responses reasoning summary PART."
  (or (cdr (assoc :text part))
      (cdr (assoc :summary--text part))
      (cdr (assoc :content part))))

(defun responses-texts-from-parts (parts)
  "Extract non-blank text strings from a Responses content PARTS sequence."
  (loop :for part :in (coerce (or parts #()) 'list)
        :for text := (responses-summary-part-text part)
        :when (and text (not (blank-string-p text)))
          :collect text))

(defun responses-reasoning-summary-texts (item)
  "Extract displayable reasoning text blocks from a Responses reasoning ITEM."
  (append (responses-texts-from-parts (cdr (assoc :summary item)))
          (responses-texts-from-parts (cdr (assoc :content item)))))

(defun responses-function-call->canonical-block (item)
  "Convert a Responses API function_call ITEM to a canonical tool_use block."
  (let ((arguments (cdr (assoc :arguments item))))
    (canonical-tool-use-block
     (cdr (assoc :call--id item))
     (cdr (assoc :name item))
     (when (and arguments (not (blank-string-p arguments)))
       (handler-case
           (api-json-decode arguments)
         (error ()
           nil))))))

(defun responses-item->canonical-blocks (item)
  "Convert one Responses API output ITEM into canonical content blocks."
  (let ((item-type (cdr (assoc :type item))))
    (cond
      ((and (string= item-type "message")
            (string= (cdr (assoc :role item)) "assistant"))
       (let ((text (responses-output-item-text item)))
         (when text
           (list (canonical-text-block text)))))
      ((string= item-type "function_call")
       (list (responses-function-call->canonical-block item)))
      ((string= item-type "reasoning")
       (mapcar #'canonical-reasoning-block
               (responses-reasoning-summary-texts item)))
      (t
       nil))))

(defun openai-codex-reasoning-options (reasoning-effort)
  "Return the Responses API reasoning object, or NIL when not needed."
  (let ((summary (and *openai-codex-reasoning-summary*
                      (not (and reasoning-effort
                                (string= reasoning-effort "none")))
                      *openai-codex-reasoning-summary*))
        (options nil))
    (when reasoning-effort
      (push `(:effort . ,reasoning-effort) options))
    (when summary
      (push `(:summary . ,summary) options))
    (nreverse options)))

(defun responses-api-response->canonical-response (response)
  "Normalize an OpenAI Responses API RESPONSE to the canonical rplaca shape."
  (let ((content-blocks nil)
        (saw-tool-use nil)
        (usage (normalize-openai-token-usage (cdr (assoc :usage response)))))
    (dolist (item (coerce (or (cdr (assoc :output response)) #()) 'list))
      (dolist (block (responses-item->canonical-blocks item))
        (when (string= (cdr (assoc :type block)) "tool_use")
          (setf saw-tool-use t))
        (push block content-blocks)))
    (let ((fallback-text (cdr (assoc :output--text response))))
      (when (and (null content-blocks)
                 (stringp fallback-text)
                 (plusp (length fallback-text)))
        (push (canonical-text-block fallback-text) content-blocks)))
    (when usage
      (file-debug-log "openai-codex-response"
                      "~A"
                      (format-token-usage-summary usage)))
    (canonical-response (if saw-tool-use "tool_use" "end_turn")
                        (nreverse content-blocks)
                        :usage usage)))

(defun openai-codex-responses-request-body (messages model max-tokens tools
                                            &key stream reasoning-effort
                                                 service-tier
                                                 (system-prompt (or (build-system-prompt) "")))
  "Build the request body for an OpenAI Responses API call."
  (declare (ignore max-tokens))
  (let* ((input-items (conversation-messages->responses-input messages))
         (response-tools (or (tool-definitions->responses-tools tools) #()))
         (reasoning-options (openai-codex-reasoning-options reasoning-effort))
         (body `((:model . ,model)
                 (:instructions . ,system-prompt)
                 (:tools . ,response-tools)
                 (:tool--choice . "auto")
                 (:parallel--tool--calls . t)
                 (:store . ,+json-false+)
                 (:stream . ,(if stream t +json-false+))
                 (:include . #())
                 ,@(when service-tier
                     `((:service--tier . ,service-tier)))
                 ,@(when *openai-codex-prompt-cache-key*
                     `((:prompt--cache--key
                        . ,*openai-codex-prompt-cache-key*)))
                 ,@(when *openai-codex-prompt-cache-retention*
                     `((:prompt--cache--retention
                        . ,*openai-codex-prompt-cache-retention*))))))
    (when reasoning-options
      (setf body (append body
                         (list `(:reasoning . ,reasoning-options)))))
    (setf body (append body
                       (list `(:input . ,(coerce input-items 'vector)))))
    (let ((request-body (api-json-encode body)))
      (file-debug-log "openai-codex-request"
                      "model=~A stream=~A reasoning=~A input-items=~D tools=~D"
                      model
                      (if stream t nil)
                      (if reasoning-options
                          (api-json-encode reasoning-options)
                          "none")
                      (length input-items)
                      (length response-tools))
      (file-debug-log "openai-codex-request-body" "~A" request-body)
      request-body)))

(defun openai-codex-request-headers (auth &key stream)
  "Build request headers for AUTH, optionally enabling streaming."
  (let ((headers `(("Authorization" . ,(format nil "Bearer ~A" (getf auth :token)))
                   ("originator" . ,*openai-oauth-originator*))))
    (when (getf auth :account-id)
      (push `("ChatGPT-Account-ID" . ,(getf auth :account-id)) headers))
    (when stream
      (push '("Accept" . "text/event-stream") headers))
    headers))

(defun openai-codex-responses-endpoint (auth)
  "Return AUTH's full Responses API endpoint URL."
  (format nil "~A/responses"
          (string-right-trim "/" (getf auth :base-url))))

(defun refresh-openai-codex-auth-descriptor ()
  "Force-refresh the shared ChatGPT OAuth auth descriptor and resolve it again."
  (let ((updated (refresh-openai-codex-auth-json)))
    (and updated
         (openai-codex-auth-descriptor-from-auth-json updated))))

(defun openai-codex-http-request
    (auth request-body &key stream (allow-refresh t) cancel-p)
  "Perform one OpenAI Codex HTTP request, refreshing ChatGPT auth once on 401."
  (when (and cancel-p (funcall cancel-p))
    (return-from openai-codex-http-request
      (values nil nil nil auth)))
  (multiple-value-bind (body status-code headers)
      (provider-http-request-with-retries
       (if stream "OpenAI Codex streaming request" "OpenAI Codex request")
       (lambda ()
         (when (and cancel-p (funcall cancel-p))
           (return-from openai-codex-http-request
             (values nil nil nil auth)))
         (drakma:http-request
          (openai-codex-responses-endpoint auth)
          :method :post
          :content-type "application/json"
          :additional-headers (openai-codex-request-headers auth :stream stream)
          :external-format-in :utf-8
          :content request-body
          :want-stream stream
          :force-binary t
          :connection-timeout *provider-http-connection-timeout-seconds*))
       :cancel-p cancel-p)
    (declare (ignore headers))
    (if (and status-code
             (= status-code 401)
             allow-refresh
             (getf auth :refreshable-p))
        (let ((refreshed (refresh-openai-codex-auth-descriptor)))
          (if refreshed
              (progn
                ;; A streaming 401 returns a live Drakma response stream.  It
                ;; belongs to this failed attempt and must be retired before
                ;; the refreshed request opens a replacement connection.
                ;; Close it here, on the request worker that owns it; the
                ;; recursive call cannot otherwise expose it to normal stream
                ;; registration/cleanup.
                (when (streamp body)
                  (ignore-errors
                    (close body :abort t)))
                (openai-codex-http-request refreshed request-body
                                           :stream stream
                                           :allow-refresh nil
                                           :cancel-p cancel-p))
              (values body status-code nil auth)))
        (values body status-code nil auth))))

(defun stream-state-text-block-cell (state)
  "Return the cons cell holding STATE's text block, or NIL."
  (loop :for cell :on (stream-state-content-blocks state)
        :when (let ((block (car cell)))
                (and block
                     (string= "text" (cdr (assoc :type block)))))
          :return cell))

(defun set-stream-state-text-block (state text)
  "Set or create STATE's canonical text block to TEXT."
  (let ((cell (stream-state-text-block-cell state)))
    (if cell
        (setf (car cell) (canonical-text-block text))
        (push (canonical-text-block text) (stream-state-content-blocks state)))))

(defun stream-state-reasoning-block-cell (state)
  "Return the cons cell holding STATE's reasoning block, or NIL."
  (loop :for cell :on (stream-state-content-blocks state)
        :when (let ((block (car cell)))
                (and block
                     (string= "reasoning" (cdr (assoc :type block)))))
          :return cell))

(defun set-stream-state-reasoning-block (state text)
  "Set or create STATE's canonical reasoning block to TEXT."
  (let ((cell (stream-state-reasoning-block-cell state)))
    (if cell
        (setf (car cell) (canonical-reasoning-block text))
        (push (canonical-reasoning-block text)
              (stream-state-content-blocks state)))))

(defun append-stream-state-reasoning-delta (state delta)
  "Append a streamed reasoning DELTA to STATE."
  (when (and delta (not (zerop (length delta))))
    (setf (stream-state-reasoning-text state)
          (concatenate 'string
                       (stream-state-reasoning-text state)
                       delta))
    (set-stream-state-reasoning-block
     state
     (stream-state-reasoning-text state))))

(defun merge-stream-state-reasoning-summary (state text)
  "Merge a complete reasoning summary TEXT into STATE without duplicating deltas."
  (when (and text (not (blank-string-p text)))
    (let ((current (stream-state-reasoning-text state)))
      (unless (or (string= current text)
                  (and (plusp (length current))
                       (search text current)))
        (setf (stream-state-reasoning-text state)
              (if (blank-string-p current)
                  text
                  (format nil "~A~%~A" current text)))
        (set-stream-state-reasoning-block
         state
         (stream-state-reasoning-text state))))))

(defun e2e-message-text (messages)
  "Return a simple deterministic text view of provider MESSAGES."
  (with-output-to-string (stream)
    (dolist (message messages)
      (princ message stream)
      (terpri stream))))

(defun e2e-response-text (messages)
  "Return the deterministic no-network response for E2E MESSAGES."
  (let ((prompt (string-downcase (e2e-message-text messages))))
    (if (search "hello" prompt)
        (format nil "~A: deterministic response for hello." +e2e-hello-sentinel+)
        "RPLACA_E2E_SENTINEL: deterministic response.")))

(defun e2e-token-usage (text)
  "Return small deterministic usage metadata for E2E response TEXT."
  (let ((output (max 1 (length (split-string-by-newline text)))))
    (list :input-tokens 1
          :output-tokens output
          :total-tokens (1+ output))))

(defun e2e-request (messages &key model max-tokens tools system-prompt
                                  reasoning-effort service-tier)
  "Return a deterministic no-network provider response for GUI E2E runs."
  (declare (ignore model max-tokens tools system-prompt reasoning-effort service-tier))
  (unless (e2e-provider-enabled-p)
    (error "The deterministic E2E provider is disabled."))
  (let ((text (e2e-response-text messages)))
    (canonical-response "end_turn"
                        (list (canonical-text-block text))
                        :usage (e2e-token-usage text))))

(defun e2e-response-chunks (text)
  "Split deterministic E2E response TEXT into stable streaming chunks."
  (let ((width 18))
    (loop :for start :from 0 :below (length text) :by width
          :collect (subseq text start (min (length text) (+ start width))))))

(defun e2e-request-streaming (messages callback
                              &key model max-tokens tools system-prompt
                                   reasoning-effort service-tier)
  "Stream a deterministic no-network response through the normal stream-state path."
  (declare (ignore model max-tokens tools system-prompt reasoning-effort service-tier))
  (unless (e2e-provider-enabled-p)
    (error "The deterministic E2E provider is disabled."))
  (let* ((state (make-stream-state :callback callback))
         (text (e2e-response-text messages))
         (usage (e2e-token-usage text))
         (chunks (e2e-response-chunks text)))
    (file-debug-event "e2e-provider-start"
                      :chunks (length chunks)
                      :sentinel (if (search +e2e-hello-sentinel+ text)
                                    +e2e-hello-sentinel+
                                    "RPLACA_E2E_SENTINEL"))
    (start-stream-state-reader-worker
     state
     callback
     "rplaca-e2e-stream"
     (lambda (worker-state)
       (dolist (chunk chunks)
         (unless (call-with-active-stream-state
                  worker-state
                  (lambda (locked-state)
                    (setf (stream-state-text locked-state)
                          (concatenate 'string
                                       (stream-state-text locked-state)
                                       chunk))
                    (set-stream-state-text-block
                     locked-state
                     (stream-state-text locked-state))))
           (return))
         (maybe-call-streaming-callback callback worker-state)
         (sleep 0.03))
       (when (transition-stream-state-to-terminal
              worker-state
              :stop-reason "end_turn"
              :update
              (lambda (locked-state)
                (set-stream-state-text-block locked-state text)
                (setf (stream-state-usage locked-state) usage)))
         (file-debug-event "e2e-provider-complete"
                           :stop-reason "end_turn"
                           :sentinel
                           (if (search +e2e-hello-sentinel+ text)
                               +e2e-hello-sentinel+
                               "RPLACA_E2E_SENTINEL")))))
    state))

(defun install-e2e-agent-definition ()
  "Install the deterministic visible agent used by no-network GUI E2E runs."
  (when (e2e-provider-enabled-p)
    (register-agent-definition
     "agent"
     :provider :e2e
     :model *e2e-model*
     :core-prompt "Deterministic RPLACA GUI E2E agent."
     :personality-prompt "Return only deterministic E2E fixture text."
     :tool-names nil)))

(defun provider-request (provider messages
                         &key model
                              (max-tokens *default-max-tokens*)
                              tools
                              reasoning-effort
                              service-tier
                              (system-prompt (build-system-prompt)))
  "Dispatch a non-streaming request by resolved PROVIDER."
  (ecase provider
    (:openai-codex
     (let ((request-args (list :model model
                               :max-tokens max-tokens
                               :tools tools
                               :reasoning-effort reasoning-effort
                               :system-prompt system-prompt)))
       (when service-tier
         (setf request-args
               (append request-args
                       (list :service-tier service-tier))))
       (apply #'openai-codex-request messages request-args)))
    (:zai
     (let ((request-args (list :model model
                               :max-tokens max-tokens
                               :tools tools
                               :system-prompt system-prompt)))
       (when reasoning-effort
         (setf request-args
               (append request-args
                       (list :reasoning-effort reasoning-effort))))
       (apply #'zai-request messages request-args)))
    (:openrouter
     (openrouter-request messages
                         :model model
                         :max-tokens max-tokens
                         :tools tools
                         :system-prompt system-prompt))
    (:e2e
     (e2e-request messages
                  :model model
                  :max-tokens max-tokens
                  :tools tools
                  :reasoning-effort reasoning-effort
                  :service-tier service-tier
                  :system-prompt system-prompt))))

(defun provider-request-streaming (provider messages callback
                                   &key model
                                        (max-tokens *default-max-tokens*)
                                        tools
                                        reasoning-effort
                                        service-tier
                                        (system-prompt (build-system-prompt)))
  "Dispatch a streaming request by resolved PROVIDER."
  (ecase provider
    (:openai-codex
     (let ((request-args (list :model model
                               :max-tokens max-tokens
                               :tools tools
                               :reasoning-effort reasoning-effort
                               :system-prompt system-prompt)))
       (when service-tier
         (setf request-args
               (append request-args
                       (list :service-tier service-tier))))
       (apply #'openai-codex-request-streaming
              messages
              callback
              request-args)))
    (:zai
     (let ((request-args (list :model model
                               :max-tokens max-tokens
                               :tools tools
                               :system-prompt system-prompt)))
       (when reasoning-effort
         (setf request-args
               (append request-args
                       (list :reasoning-effort reasoning-effort))))
       (apply #'zai-request-streaming
              messages
              callback
              request-args)))
    (:openrouter
     (openrouter-request-streaming messages callback
                                   :model model
                                   :max-tokens max-tokens
                                   :tools tools
                                   :system-prompt system-prompt))
    (:e2e
     (e2e-request-streaming messages callback
                            :model model
                            :max-tokens max-tokens
                            :tools tools
                            :reasoning-effort reasoning-effort
                            :service-tier service-tier
                            :system-prompt system-prompt))))

;;; --------------------------------------------------------------------------
;;; API Call
;;; --------------------------------------------------------------------------

(defun openai-codex-request (messages &key (model *openai-codex-model*)
                                           (max-tokens *default-max-tokens*)
                                           tools
                                           reasoning-effort
                                           service-tier
                                           (system-prompt (or (build-system-prompt) "")))
  "Call the OpenAI Responses API for Codex and normalize the response shape."
  (let* ((auth (or (resolve-openai-codex-auth)
                   (error 'simple-error
                          :format-control "No OpenAI Codex auth. Save a bearer token to ~~/.config/rplaca/openai-codex-token or sign in via ~~/.codex/auth.json")))
         (request-body (openai-codex-responses-request-body
                        messages model max-tokens tools
                        :system-prompt system-prompt
                        :service-tier service-tier
                        :reasoning-effort reasoning-effort)))
    (multiple-value-bind (body status-code ignored effective-auth)
        (openai-codex-http-request auth request-body)
      (declare (ignore ignored effective-auth))
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "API error (~A): ~A" status-code body-string))
        (responses-api-response->canonical-response
         (api-json-decode body-string))))))

;;; --------------------------------------------------------------------------
;;; Streaming API Call
;;; --------------------------------------------------------------------------

(defstruct stream-state
  "Mutable state for an in-progress streaming response."
  (text ""             :type string)
  (reasoning-text ""   :type string)
  (content-blocks nil  :type list)
  (tool-input-json ""  :type string)   ; accumulates input_json_delta for tool_use
  (openai-tool-call-states (make-hash-table :test #'equal))
  (openai-tool-call-order nil :type list)
  (stop-reason nil)
  (usage nil)
  (done-p nil          :type boolean)
  (error-p nil)
  (cancel-requested-p nil :type boolean)
  (cancelled-p nil :type boolean)
  callback
  (terminal-callback-fired-p nil :type boolean)
  close-stream
  reader-thread
  reader-settlement-thread
  (reader-settlement-waiter-done-p nil :type boolean)
  (reader-settled-p nil :type boolean)
  (lock (bt:make-lock "stream-state")))

;;; --------------------------------------------------------------------------
;;; Bounded external callback delivery
;;; --------------------------------------------------------------------------

(defparameter *runtime-callback-dispatch-lane-count* 4
  "Number of independent serial lanes used for external runtime callbacks.")

(defparameter *runtime-callback-dispatch-queue-limit* 256
  "Maximum queued callback deliveries retained by one dispatcher lane.")

(defparameter *runtime-callback-dispatch-idle-timeout-seconds* 0.5
  "Seconds an idle callback lane waits before retiring its worker thread.")

(defstruct runtime-callback-delivery
  "One copied external callback invocation owned by a dispatcher lane."
  function
  arguments
  label)

(defstruct runtime-callback-dispatch-lane
  "Bounded FIFO and at most one callback worker for a hash lane."
  queue-head
  queue-tail
  (queue-count 0 :type integer)
  (dropped-count 0 :type integer)
  (reported-dropped-count 0 :type integer)
  worker
  (active-p nil :type boolean)
  (lock (bt:make-lock "rplaca runtime callback lane"))
  (condition
    (bt:make-condition-variable :name "rplaca runtime callback lane")))

(defparameter *runtime-callback-copy-node-limit* 100000
  "Maximum mutable nodes copied into one external callback delivery.")

(defparameter *runtime-callback-copy-depth-limit* 2048
  "Maximum recursive nesting copied into one external callback delivery.")

(defparameter *runtime-callback-copy-element-limit* 65536
  "Maximum aggregate container elements copied into one callback delivery.")

(defun copy-runtime-callback-data (value)
  "Copy callback VALUE with cycle preservation and bounded work budgets."
  (let ((seen (make-hash-table :test #'eq))
        (nodes 0)
        (elements 0))
    (labels ((claim-node ()
               (when (> (incf nodes) *runtime-callback-copy-node-limit*)
                 (error "Runtime callback payload exceeds the ~D node limit."
                        *runtime-callback-copy-node-limit*)))
             (claim-elements (count)
               (when (> (+ elements count)
                        *runtime-callback-copy-element-limit*)
                 (error
                  "Runtime callback payload exceeds the ~D element limit."
                  *runtime-callback-copy-element-limit*))
               (incf elements count))
             (claim-depth (depth)
               (when (> depth *runtime-callback-copy-depth-limit*)
                 (error "Runtime callback payload exceeds the ~D depth limit."
                        *runtime-callback-copy-depth-limit*)))
             (copy-value (item depth)
               (typecase item
                 (string
                  (or (gethash item seen)
                      (progn
                        (claim-elements (length item))
                        (let ((copy (copy-seq item)))
                          (setf (gethash item seen) copy)
                          copy))))
                 (cons
                  (or (gethash item seen)
                      (progn
                        (claim-depth depth)
                        (claim-node)
                        (claim-elements 1)
                        (let ((copy (cons nil nil)))
                          (setf (gethash item seen) copy
                                (car copy) (copy-value (car item) (1+ depth))
                                (cdr copy) (copy-value (cdr item) (1+ depth)))
                          copy))))
                 (vector
                  (or (gethash item seen)
                      (progn
                        (claim-depth depth)
                        (claim-node)
                        (claim-elements (length item))
                        (let ((copy (make-array (length item))))
                          (setf (gethash item seen) copy)
                          (loop :for index :below (length item)
                                :do (setf (aref copy index)
                                          (copy-value (aref item index)
                                                      (1+ depth))))
                          copy))))
                 (hash-table
                  (or (gethash item seen)
                      (progn
                        (claim-depth depth)
                        (claim-node)
                        (claim-elements (* 2 (hash-table-count item)))
                        (let ((copy (make-hash-table
                                     :test (hash-table-test item)
                                     ;; Do not reproduce a sparse table's
                                     ;; potentially enormous reserved size.
                                     :size (hash-table-count item))))
                          (setf (gethash item seen) copy)
                          (maphash
                           (lambda (key entry)
                             (setf (gethash (copy-value key (1+ depth)) copy)
                                   (copy-value entry (1+ depth))))
                           item)
                          copy))))
                 (t item))))
      (copy-value value 0))))

(defvar *runtime-callback-dispatch-lanes*
  (let ((count (max 1 *runtime-callback-dispatch-lane-count*)))
    (make-array count
                :initial-contents
                (loop :repeat count
                      :collect (make-runtime-callback-dispatch-lane))))
  "Fixed process-wide lanes that bound external callback threads and queues.")

(defun runtime-callback-dispatch-lane-for (function)
  "Return the stable serial dispatcher lane for FUNCTION."
  (let ((lanes *runtime-callback-dispatch-lanes*))
    (aref lanes (mod (sxhash function) (length lanes)))))

(defun runtime-callback-dispatch-pop-locked (lane)
  "Pop one delivery from LANE with its lock held."
  (let ((cell (runtime-callback-dispatch-lane-queue-head lane)))
    (when cell
      (setf (runtime-callback-dispatch-lane-queue-head lane) (cdr cell))
      (when (null (runtime-callback-dispatch-lane-queue-head lane))
        (setf (runtime-callback-dispatch-lane-queue-tail lane) nil))
      (decf (runtime-callback-dispatch-lane-queue-count lane))
      (car cell))))

(defun runtime-callback-dispatch-next (lane)
  "Return LANE's next delivery, or NIL after retiring an idle worker."
  (bt:with-lock-held ((runtime-callback-dispatch-lane-lock lane))
    (labels ((claim-next ()
               (let ((delivery (runtime-callback-dispatch-pop-locked lane)))
                 (when delivery
                   ;; Queue removal and active publication are one transaction;
                   ;; safe reload can never observe a callback between owners.
                   (setf (runtime-callback-dispatch-lane-active-p lane) t))
                 delivery)))
      (or (claim-next)
          (progn
            (bt:condition-wait
             (runtime-callback-dispatch-lane-condition lane)
             (runtime-callback-dispatch-lane-lock lane)
             :timeout *runtime-callback-dispatch-idle-timeout-seconds*)
            (or (claim-next)
                (progn
                  (when (eq (bt:current-thread)
                            (runtime-callback-dispatch-lane-worker lane))
                    (setf (runtime-callback-dispatch-lane-worker lane) nil))
                  nil)))))))

(defun report-runtime-callback-error (delivery condition)
  "Best-effort report CONDITION raised by external callback DELIVERY."
  (ignore-errors
    (file-debug-event
     "runtime-callback-error"
     :callback (or (runtime-callback-delivery-label delivery) "external")
     :condition (format nil "~A" condition))))

(defun report-runtime-callback-drops (lane)
  "Report newly refused callback deliveries from LANE on its owned worker."
  (let ((dropped nil))
    (bt:with-lock-held ((runtime-callback-dispatch-lane-lock lane))
      (when (> (runtime-callback-dispatch-lane-dropped-count lane)
               (runtime-callback-dispatch-lane-reported-dropped-count lane))
        (setf dropped (runtime-callback-dispatch-lane-dropped-count lane)
              (runtime-callback-dispatch-lane-reported-dropped-count lane)
              dropped)))
    (when dropped
      (ignore-errors
        (file-debug-event "runtime-callback-dropped"
                          :dropped-count dropped)))))

(defun run-runtime-callback-dispatch-lane (lane)
  "Deliver LANE's callbacks serially without owning provider/interop workers."
  (unwind-protect
       (loop :for delivery := (runtime-callback-dispatch-next lane)
             :while delivery
             :do
                (unwind-protect
                     (handler-case
                         (apply (runtime-callback-delivery-function delivery)
                                (runtime-callback-delivery-arguments delivery))
                       (error (condition)
                         (report-runtime-callback-error delivery condition)))
                  ;; Diagnostics execute on the callback-owned worker too.  A
                  ;; stuck diagnostic therefore remains visible to safe reload
                  ;; instead of migrating back onto a provider/runner thread.
                  (report-runtime-callback-drops lane)
                  (bt:with-lock-held
                      ((runtime-callback-dispatch-lane-lock lane))
                    (setf (runtime-callback-dispatch-lane-active-p lane) nil)
                    (bt:condition-notify
                     (runtime-callback-dispatch-lane-condition lane)))))
    ;; An implementation/runtime failure outside the contained callback must
    ;; not leave a dead worker handle that prevents a later enqueue from
    ;; restarting this lane.
    (bt:with-lock-held ((runtime-callback-dispatch-lane-lock lane))
      (when (eq (bt:current-thread)
                (runtime-callback-dispatch-lane-worker lane))
        (setf (runtime-callback-dispatch-lane-worker lane) nil
              (runtime-callback-dispatch-lane-active-p lane) nil)
        (bt:condition-notify
         (runtime-callback-dispatch-lane-condition lane))))))

(defun ensure-runtime-callback-dispatch-worker-locked (lane)
  "Ensure LANE has a live worker, with its lock already held."
  (let ((worker (runtime-callback-dispatch-lane-worker lane)))
    (when (or (null worker) (not (bt:thread-alive-p worker)))
      (setf (runtime-callback-dispatch-lane-worker lane)
            (bt:make-thread
             (lambda () (run-runtime-callback-dispatch-lane lane))
             :name "rplaca-runtime-callback"))))
  (runtime-callback-dispatch-lane-worker lane))

(defun enqueue-runtime-callback (function arguments &key label)
  "Queue copied ARGUMENTS for FUNCTION and return true when accepted.

Each callback is assigned to one serial lane, preserving its event order.  The
fixed lane and queue counts bound damage from a callback that never returns.
Queue saturation drops the newest delivery rather than blocking a provider,
subagent, interop runner, or CLIM process."
  (unless function
    (return-from enqueue-runtime-callback nil))
  (handler-case
      (let* ((lane (runtime-callback-dispatch-lane-for function))
             (delivery
               (make-runtime-callback-delivery
                :function function
                :arguments (copy-runtime-callback-data arguments)
                :label label)))
        (bt:with-lock-held ((runtime-callback-dispatch-lane-lock lane))
          (when (>= (runtime-callback-dispatch-lane-queue-count lane)
                    *runtime-callback-dispatch-queue-limit*)
            (incf (runtime-callback-dispatch-lane-dropped-count lane))
            (return-from enqueue-runtime-callback nil))
          (ensure-runtime-callback-dispatch-worker-locked lane)
          (let ((cell (list delivery)))
            (if (runtime-callback-dispatch-lane-queue-tail lane)
                (setf (cdr (runtime-callback-dispatch-lane-queue-tail lane)) cell
                      (runtime-callback-dispatch-lane-queue-tail lane) cell)
                (setf (runtime-callback-dispatch-lane-queue-head lane) cell
                      (runtime-callback-dispatch-lane-queue-tail lane) cell)))
          (incf (runtime-callback-dispatch-lane-queue-count lane))
          (bt:condition-notify
           (runtime-callback-dispatch-lane-condition lane))
          t))
    (error (_condition)
      (declare (ignore _condition))
      ;; Copy/constructor failure is itself a refused delivery.  Do not run a
      ;; diagnostic file write on the provider/interop caller being protected.
      (let ((lane (ignore-errors
                    (runtime-callback-dispatch-lane-for function))))
        (when lane
          (ignore-errors
            (bt:with-lock-held ((runtime-callback-dispatch-lane-lock lane))
              (incf (runtime-callback-dispatch-lane-dropped-count lane))))))
      nil)))

(defun make-bounded-runtime-callback (function &key label)
  "Return a non-blocking ordered proxy for external callback FUNCTION."
  (and function
       (lambda (&rest arguments)
         (enqueue-runtime-callback function arguments :label label))))

(defun runtime-callback-dispatch-pending-count ()
  "Return queued plus currently executing external callback deliveries."
  (loop :for lane :across *runtime-callback-dispatch-lanes*
        :sum (bt:with-lock-held
                 ((runtime-callback-dispatch-lane-lock lane))
               (+ (runtime-callback-dispatch-lane-queue-count lane)
                  (if (runtime-callback-dispatch-lane-active-p lane) 1 0)))))

(defun runtime-callback-dispatch-dropped-count ()
  "Return the total number of refused external callback deliveries."
  (loop :for lane :across *runtime-callback-dispatch-lanes*
        :sum (bt:with-lock-held
                 ((runtime-callback-dispatch-lane-lock lane))
               (runtime-callback-dispatch-lane-dropped-count lane))))

(defun wait-for-runtime-callback-dispatch-idle
    (&key (timeout 2.0) (poll-interval 0.005))
  "Wait boundedly until no external callback is queued or executing."
  (let ((deadline (+ (get-internal-real-time)
                     (round (* timeout internal-time-units-per-second)))))
    (loop :when (zerop (runtime-callback-dispatch-pending-count))
            :return t
          :when (>= (get-internal-real-time) deadline)
            :return nil
          :do (sleep poll-interval))))

(defun provider-stream-wrapper-value
    (stream package-name class-name accessor-name)
  "Return STREAM's wrapped value for one known provider stream layer.

PACKAGE-NAME, CLASS-NAME, and ACCESSOR-NAME are strings so optional transport
packages do not become reader-time dependencies of this file."
  (let* ((package (find-package package-name))
         (class-symbol (and package (find-symbol class-name package)))
         (accessor-symbol (and package (find-symbol accessor-name package))))
    (when (and class-symbol
               accessor-symbol
               (find-class class-symbol nil)
               (fboundp accessor-symbol)
               (typep stream class-symbol))
      (funcall accessor-symbol stream))))

(defun provider-stream-transport-file-descriptor (stream)
  "Return STREAM's underlying socket descriptor when it is discoverable.

Drakma response bodies are Flexi streams layered over Chunga and either a
native socket stream or a CL+SSL stream.  Walk only those known ownership
layers; do not guess at arbitrary Gray stream internals.  Provider streaming
leaves Drakma's DECODE-CONTENT at NIL, so no Chipz layer is expected here.

This descriptor fast path is SBCL-specific.  It applies after Drakma has
returned a body stream; cancellation during connect or response headers remains
bounded by the configured Drakma connection timeout."
  #+sbcl
  (labels ((walk (current seen)
             (when (or (null current)
                       (member current seen :test #'eq))
               (return-from walk nil))
             (cond
               ((and (integerp current) (not (minusp current)))
                current)
               ((typep current 'sb-sys:fd-stream)
                (sb-sys:fd-stream-fd current))
               (t
                (let ((wrapped
                        (or
                         (and (typep current 'flexi-streams:flexi-stream)
                              (flexi-streams:flexi-stream-stream current))
                         (provider-stream-wrapper-value
                          current "CHUNGA" "CHUNKED-STREAM"
                          "CHUNKED-STREAM-STREAM")
                         (provider-stream-wrapper-value
                          current "CL+SSL" "SSL-STREAM"
                          "SSL-STREAM-SOCKET"))))
                  (and wrapped
                       (walk wrapped (cons current seen))))))))
    (walk stream nil))
  #-sbcl
  (declare (ignore stream))
  #-sbcl
  nil)

#+sbcl
(defun shutdown-provider-socket-file-descriptor (file-descriptor)
  "Shut down FILE-DESCRIPTOR without taking ownership of or closing it."
  (handler-case
      (let ((socket
              (make-instance 'sb-bsd-sockets:inet-socket
                             :type :stream
                             :protocol :tcp
                             :descriptor file-descriptor)))
        ;; The temporary socket object is only an exported SB-BSD-SOCKETS view
        ;; over Drakma's descriptor.  Its original stream remains the sole
        ;; close owner, so the view must never finalize or close the descriptor.
        (sb-ext:cancel-finalization socket)
        (sb-bsd-sockets:socket-shutdown socket :direction :io)
        t)
    (error ()
      nil)))

(defgeneric interrupt-provider-stream-read (stream)
  (:documentation
   "Wake a blocked provider read without closing STREAM from another thread."))

(defmethod interrupt-provider-stream-read ((stream t))
  #+sbcl
  (let ((file-descriptor
          (provider-stream-transport-file-descriptor stream)))
    (and file-descriptor
         (shutdown-provider-socket-file-descriptor file-descriptor)))
  #-sbcl
  (declare (ignore stream))
  #-sbcl
  nil)

(defun stream-state-active-p-locked (state)
  "Return true when STATE may still accept streamed mutations.
The caller must hold STATE's lock."
  (and (not (stream-state-done-p state))
       (not (stream-state-cancel-requested-p state))))

(defun stream-state-active-p-safe (state)
  "Return true when STATE may still accept streamed mutations."
  (and state
       (bt:with-lock-held ((stream-state-lock state))
         (stream-state-active-p-locked state))))

(defun call-with-active-stream-state (state function)
  "Call FUNCTION with STATE under its lock only while STATE is active.
Return true exactly when FUNCTION ran."
  (bt:with-lock-held ((stream-state-lock state))
    (when (stream-state-active-p-locked state)
      (funcall function state)
      t)))

(defun register-stream-state-callback (state callback)
  "Register CALLBACK while STATE is active."
  (bt:with-lock-held ((stream-state-lock state))
    (when (and callback (stream-state-active-p-locked state))
      (setf (stream-state-callback state) callback)))
  state)

(defun maybe-call-streaming-callback (callback state &key terminal)
  "Invoke STATE's streaming callback outside its lock.
Terminal notification is claimed under the lock and can fire at most once.
Reader threads never fail because UI notification raised."
  (let ((function nil))
    (bt:with-lock-held ((stream-state-lock state))
      (if (or terminal (stream-state-done-p state))
          (unless (stream-state-terminal-callback-fired-p state)
            (setf (stream-state-terminal-callback-fired-p state) t
                  function (or callback (stream-state-callback state))
                  (stream-state-callback state) nil))
          (when (stream-state-active-p-locked state)
            (setf function (or callback (stream-state-callback state))))))
    (when function
      (ignore-errors
        (funcall function state)))
    (not (null function))))

(defun transition-stream-state-to-terminal
    (state &key stop-reason (error nil error-supplied-p) cancelled-p update
                detach-stream-p)
  "Atomically make active STATE terminal and optionally detach its stream.
UPDATE, when supplied, runs under STATE's lock immediately before the terminal
fields are committed.  Return the transition flag and detached stream."
  (let ((transitioned-p nil)
        (stream nil))
    (bt:with-lock-held ((stream-state-lock state))
      (when (stream-state-active-p-locked state)
        (when update
          (funcall update state))
        (when stop-reason
          (setf (stream-state-stop-reason state) stop-reason))
        (when error-supplied-p
          (setf (stream-state-error-p state) error))
        (when cancelled-p
          (setf (stream-state-cancel-requested-p state) t
                (stream-state-cancelled-p state) t))
        (when detach-stream-p
          (setf stream (stream-state-close-stream state)
                (stream-state-close-stream state) nil))
        (setf (stream-state-done-p state) t
              transitioned-p t)))
    (values transitioned-p stream)))

(defun register-stream-state-stream (state stream)
  "Attach STREAM to active STATE, closing it outside the lock if too late.
Return true when STATE accepted ownership of STREAM."
  (let ((accepted-p nil))
    (bt:with-lock-held ((stream-state-lock state))
      (when (stream-state-active-p-locked state)
        (setf (stream-state-close-stream state) stream
              accepted-p t)))
    (unless accepted-p
      (ignore-errors
        (close stream :abort t)))
    accepted-p))

(defun release-stream-state-stream (state &optional expected-stream)
  "Detach and close STATE's stream outside the lock.
When EXPECTED-STREAM is non-nil, detach only that exact stream.  Return true
when this call claimed the close operation."
  (let ((stream nil)
        (abort-p nil))
    (bt:with-lock-held ((stream-state-lock state))
      (let ((current (stream-state-close-stream state)))
        (when (and current
                   (or (null expected-stream)
                       (eq current expected-stream)))
          (setf stream current
                abort-p (stream-state-cancelled-p state)
                (stream-state-close-stream state) nil))))
    (when stream
      (ignore-errors
        (close stream :abort abort-p)))
    (not (null stream))))

(defun register-stream-state-reader-thread (state thread)
  "Attach THREAD to STATE."
  (bt:with-lock-held ((stream-state-lock state))
    (setf (stream-state-reader-thread state) thread
          (stream-state-reader-settlement-thread state) nil
          (stream-state-reader-settlement-waiter-done-p state) nil
          (stream-state-reader-settled-p state) nil))
  state)

(defun clear-stream-state-reader-thread (state thread)
  "Clear STATE's joined reader and settlement-waiter references."
  (bt:with-lock-held ((stream-state-lock state))
    (when (eq thread (stream-state-reader-thread state))
      (setf (stream-state-reader-thread state) nil
            (stream-state-reader-settlement-thread state) nil
            (stream-state-reader-settled-p state) t)
      t)))

(defun start-stream-state-reader-worker (state callback name function)
  "Run FUNCTION asynchronously as STATE's managed reader worker.
The worker blocks on a start gate until its thread reference and callback are
registered.  Errors and ordinary return both settle STATE; cleanup detaches the
active stream and sends one terminal callback.  The exact thread reference is
retained until its buffer/prompt owner joins it, so DONE-P never masquerades as
worker settlement."
  (let ((start-gate (bt:make-lock "stream-state-reader-start"))
        (thread nil))
    (bt:with-lock-held (start-gate)
      (register-stream-state-callback state callback)
      (setf thread
            (bt:make-thread
             (lambda ()
               ;; The creating thread holds START-GATE until THREAD is stored
               ;; in STATE, so a fast worker cannot finish before registration.
               (bt:with-lock-held (start-gate))
               (unwind-protect
                    (handler-case
                        (when (stream-state-active-p-safe state)
                          (funcall function state))
                      (error (condition)
                        (transition-stream-state-to-terminal
                         state
                         :error (format nil "~A" condition))))
                 ;; Final transport/state mutation is settlement, not a new
                 ;; start.  If a visibility defect ever let reload claim first,
                 ;; wait until live redefinition ends before touching methods.
                 (call-with-runtime-settlement-admission
                  (lambda ()
                    (transition-stream-state-to-terminal state)
                    (release-stream-state-stream state))
                  :operation "provider reader settlement")
                 (maybe-call-streaming-callback
                  callback state :terminal t)))
             :name name
             ;; A provider can be started while the frame process suppresses
             ;; its own nested notifications.  Caller-local binding inheritance
             ;; is implementation-defined, so readers explicitly re-enable
             ;; terminal/update wakeups.
             :initial-bindings
             (acons '*suppress-chat-redisplay-requests*
                    nil
                    bt:*default-special-bindings*)))
      (register-stream-state-reader-thread state thread))
    state))

(defun stream-state-cancel-requested-p-safe (state)
  "Return true when STATE has been cancelled, holding its lock."
  (and state
       (bt:with-lock-held ((stream-state-lock state))
         (stream-state-cancel-requested-p state))))

(defun register-stream-state-reader (state stream &optional thread)
  "Attach STREAM and THREAD to STATE so cancellation can interrupt reads."
  (when stream
    (register-stream-state-stream state stream))
  (when thread
    (register-stream-state-reader-thread state thread))
  state)

(defun cancel-stream-state (state &key (stop-reason "cancelled"))
  "Request cancellation of STATE and interrupt its provider transport.
Returns true when cancellation changed an active stream."
  (let ((cancelled nil)
        (orphan-stream nil))
    (when state
      (setf cancelled
            (transition-stream-state-to-terminal
             state
             :stop-reason stop-reason
             :cancelled-p t
             :update
             (lambda (locked-state)
               ;; Keep the stream attached for same-thread cleanup by the
               ;; reader.  Shutdown is performed under the ownership lock so
               ;; the reader cannot close and recycle the descriptor between
               ;; descriptor discovery and the shutdown system call.
               (let ((stream (stream-state-close-stream locked-state)))
                 (when stream
                   (if (stream-state-reader-thread locked-state)
                       (ignore-errors
                         (interrupt-provider-stream-read stream))
                       ;; A manually attached stream with no managed reader has
                       ;; no future unwind that can release it.  Detach it under
                       ;; the state lock, then close it below without holding
                       ;; that lock.
                       (setf orphan-stream stream
                             (stream-state-close-stream locked-state) nil)))))))
      (when orphan-stream
        (ignore-errors
          (close orphan-stream :abort t)))
      (when cancelled
        (maybe-call-streaming-callback nil state :terminal t)))
    cancelled))

(defun stream-state-final-content-blocks (state)
  "Return STATE's canonical content blocks, flushing accumulated text first."
  (bt:with-lock-held ((stream-state-lock state))
    (when (plusp (length (stream-state-text state)))
      (set-stream-state-text-block state (stream-state-text state)))
    (nreverse (copy-list (stream-state-content-blocks state)))))

(defun parse-sse-line (line)
  "Parse a single SSE line. Returns (values field value) or nil.
SSE format: 'field: value' or just 'data: {...}'."
  (let ((colon-pos (position #\: line)))
    (when (and colon-pos (plusp colon-pos))
      (let ((field (subseq line 0 colon-pos))
            (value (if (< (1+ colon-pos) (length line))
                       (string-left-trim " " (subseq line (1+ colon-pos)))
                       "")))
        (values field value)))))

(defun stream-state-first-block (state)
  "Return STATE's current leading content block."
  (first (stream-state-content-blocks state)))

(defun ensure-openai-stream-text-block (state)
  "Ensure STATE has a current canonical text block."
  (let ((block (stream-state-first-block state)))
    (unless (and block (string= "text" (cdr (assoc :type block))))
      (push (canonical-text-block "") (stream-state-content-blocks state)))))

(defun openai-stream-tool-call-key (tool-call)
  "Return a stable key for a streamed OpenAI TOOL-CALL delta."
  (or (cdr (assoc :index tool-call))
      (cdr (assoc :id tool-call))
      (error "OpenAI streaming tool call missing index/id: ~S" tool-call)))

(defun upsert-openai-stream-tool-call (state tool-call)
  "Merge TOOL-CALL delta into STATE's OpenAI tool-call assembly tables."
  (let* ((key (openai-stream-tool-call-key tool-call))
         (function (cdr (assoc :function tool-call)))
         (existing (or (gethash key (stream-state-openai-tool-call-states state))
                       (list :id nil :name nil :arguments ""))))
    (unless (gethash key (stream-state-openai-tool-call-states state))
      (setf (stream-state-openai-tool-call-order state)
            (append (stream-state-openai-tool-call-order state) (list key))))
    (let ((updated (list :id (or (cdr (assoc :id tool-call))
                                 (getf existing :id))
                         :name (or (cdr (assoc :name function))
                                   (getf existing :name))
                         :arguments (let ((arguments (cdr (assoc :arguments function))))
                                      (if arguments
                                          (concatenate 'string
                                                       (getf existing :arguments)
                                                       arguments)
                                          (getf existing :arguments))))))
      (setf (gethash key (stream-state-openai-tool-call-states state)) updated)
      updated)))

(defun finalize-openai-stream-tool-blocks (state)
  "Finalize all assembled OpenAI streaming tool calls into canonical blocks."
  (let ((non-tool-blocks (remove-if (lambda (block)
                                      (string= "tool_use" (cdr (assoc :type block))))
                                    (stream-state-content-blocks state)))
        (tool-blocks
          (loop :for key :in (stream-state-openai-tool-call-order state)
                :for call-state := (gethash key (stream-state-openai-tool-call-states state))
                :collect (canonical-tool-use-block
                          (getf call-state :id)
                          (getf call-state :name)
                          (let ((arguments (getf call-state :arguments)))
                            (when (plusp (length arguments))
                              (handler-case
                                  (api-json-decode arguments)
                                (error () nil))))))))
    (let ((display-blocks (append (nreverse (copy-list non-tool-blocks))
                                  tool-blocks)))
      (setf (stream-state-content-blocks state)
            (nreverse display-blocks)))))

(defun process-openai-sse-event (data state)
  "Process a single OpenAI SSE DATA payload into STATE."
  (unless (stream-state-active-p-safe state)
    (return-from process-openai-sse-event nil))
  (cond
    ((string= data "[DONE]")
     (when (transition-stream-state-to-terminal
            state
            :update
            (lambda (locked-state)
              (finalize-openai-stream-tool-blocks locked-state)
              (when (plusp (length (stream-state-text locked-state)))
                (set-stream-state-text-block
                 locked-state
                 (stream-state-text locked-state)))
              (unless (stream-state-stop-reason locked-state)
                (setf (stream-state-stop-reason locked-state) "end_turn"))))
       :terminal))
    (t
     (let* ((event (api-json-decode data))
            (choice (first (coerce (cdr (assoc :choices event)) 'list)))
            (delta (and choice (cdr (assoc :delta choice))))
            (text (and delta (cdr (assoc :content delta))))
            (reasoning (and delta (cdr (assoc :reasoning--content delta))))
            (tool-calls (and delta (cdr (assoc :tool--calls delta))))
            (finish-reason (and choice (cdr (assoc :finish--reason choice)))))
        ;; Reasoning models (Z.AI GLM, DeepSeek R1) stream reasoning_content
        ;; first, then content. Keep those channels separate so UI rendering
        ;; can hide or show reasoning output.
        (when (call-with-active-stream-state
               state
               (lambda (locked-state)
                 (append-stream-state-reasoning-delta
                  locked-state reasoning)
                 (when text
                   (setf (stream-state-text locked-state)
                         (concatenate 'string
                                      (stream-state-text locked-state)
                                      text))
                   (set-stream-state-text-block
                    locked-state
                    (stream-state-text locked-state)))
                 (dolist (tool-call (coerce (or tool-calls #()) 'list))
                   (upsert-openai-stream-tool-call locked-state tool-call))
                 (when finish-reason
                   (when (or tool-calls
                             (stream-state-openai-tool-call-order
                              locked-state))
                     (finalize-openai-stream-tool-blocks locked-state))
                   (setf (stream-state-stop-reason locked-state)
                         (openai-finish-reason->stop-reason
                          finish-reason)))))
          :updated)))))

(defun read-openai-sse-stream (stream state callback
                               &key defer-terminal-callback)
  "Read OpenAI SSE events from STREAM into STATE."
  (register-stream-state-callback state callback)
  (handler-case
      (loop :with data-buffer := nil
            :while (stream-state-active-p-safe state)
            :for line := (read-line stream nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Return) line)))
                  (cond
                    ((zerop (length trimmed))
                     (when data-buffer
                       (let ((result
                               (process-openai-sse-event
                                (format nil "~{~A~}" (nreverse data-buffer))
                                state)))
                         (setf data-buffer nil)
                         (case result
                           (:updated
                            (maybe-call-streaming-callback callback state))
                           (:terminal
                            (loop-finish))))))
                    (t
                     (multiple-value-bind (field value) (parse-sse-line trimmed)
                       (when (and field (string= "data" field))
                         (push value data-buffer)))))))
    (error (e)
      (transition-stream-state-to-terminal
       state
       :error (format nil "~A" e))))
  (transition-stream-state-to-terminal state)
  (unless defer-terminal-callback
    (maybe-call-streaming-callback callback state :terminal t)))

(defun responses-stream-tool-use-present-p (state)
  "Return non-nil when STATE already contains a tool_use block."
  (some (lambda (block)
          (string= "tool_use" (cdr (assoc :type block))))
        (stream-state-content-blocks state)))

(defun process-openai-codex-responses-sse-event (data state)
  "Process one Responses API SSE DATA payload into STATE."
  (unless (stream-state-active-p-safe state)
    (return-from process-openai-codex-responses-sse-event nil))
  (let* ((event (api-json-decode data))
         (event-type (cdr (assoc :type event))))
    (unless (string= event-type "response.output_text.delta")
      (let* ((item (cdr (assoc :item event)))
             (item-type (and item (cdr (assoc :type item))))
             (summary-count
               (and item (length (coerce (or (cdr (assoc :summary item))
                                              #())
                                          'list))))
             (content-count
               (and item (length (coerce (or (cdr (assoc :content item))
                                              #())
                                          'list)))))
        (file-debug-log "openai-codex-sse"
                        "type=~A~@[ item-type=~A~]~@[ summary-parts=~D~]~@[ content-parts=~D~]"
                        event-type
                        item-type
                        summary-count
                        content-count)))
    (cond
      ((string= event-type "response.output_text.delta")
       (let ((delta (cdr (assoc :delta event))))
         (and delta
              (call-with-active-stream-state
               state
               (lambda (locked-state)
                 (setf (stream-state-text locked-state)
                       (concatenate 'string
                                    (stream-state-text locked-state)
                                    delta))
                 (set-stream-state-text-block
                  locked-state
                  (stream-state-text locked-state))))
              :updated)))
      ((member event-type
               '("response.reasoning_summary_text.delta"
                 "response.reasoning_text.delta")
               :test #'string=)
       (let ((delta (or (cdr (assoc :delta event))
                        (cdr (assoc :text event)))))
         (and delta
              (call-with-active-stream-state
               state
               (lambda (locked-state)
                 (append-stream-state-reasoning-delta
                  locked-state delta)))
              :updated)))
      ((member event-type
               '("response.reasoning_summary_text.done"
                 "response.reasoning_text.done")
               :test #'string=)
       (let ((text (cdr (assoc :text event))))
         (and text
              (call-with-active-stream-state
               state
               (lambda (locked-state)
                 (merge-stream-state-reasoning-summary
                  locked-state text)))
              :updated)))
      ((string= event-type "response.output_item.done")
       (let ((item (cdr (assoc :item event))))
         (when item
           (let ((item-type (cdr (assoc :type item))))
             (and
              (call-with-active-stream-state
               state
               (lambda (locked-state)
                 (cond
                   ((and (string= item-type "message")
                         (string= (cdr (assoc :role item)) "assistant"))
                    (let ((text (responses-output-item-text item)))
                      (when text
                        (setf (stream-state-text locked-state) text)
                        (set-stream-state-text-block locked-state text))))
                   ((string= item-type "function_call")
                    (push (responses-function-call->canonical-block item)
                          (stream-state-content-blocks locked-state))
                    (setf (stream-state-stop-reason locked-state)
                          "tool_use"))
                   ((string= item-type "reasoning")
                    (dolist (text (responses-reasoning-summary-texts item))
                      (merge-stream-state-reasoning-summary
                       locked-state text))))))
              :updated)))))
      ((string= event-type "response.completed")
       (let* ((response (cdr (assoc :response event)))
              (usage (normalize-openai-token-usage
                      (or (and response (cdr (assoc :usage response)))
                          (cdr (assoc :usage event))))))
         (and
          (transition-stream-state-to-terminal
           state
           :update
           (lambda (locked-state)
             (when usage
               (setf (stream-state-usage locked-state) usage)
               (file-debug-log "openai-codex-sse"
                               "~A"
                               (format-token-usage-summary usage)))
             (unless (stream-state-stop-reason locked-state)
               (setf (stream-state-stop-reason locked-state)
                     (if (responses-stream-tool-use-present-p locked-state)
                         "tool_use"
                         "end_turn")))))
          :terminal)))
      ((or (string= event-type "response.failed")
           (string= event-type "error"))
       (let ((message (or (cdr (assoc :message event))
                          (cdr (assoc :error event))
                          "Unknown Responses API error")))
         (and (transition-stream-state-to-terminal
               state
               :error message)
              :terminal))))))

(defun read-openai-codex-responses-sse-stream
    (stream state callback &key defer-terminal-callback)
  "Read Responses API SSE events from STREAM into STATE."
  (register-stream-state-callback state callback)
  (handler-case
      (loop :with data-buffer := nil
            :while (stream-state-active-p-safe state)
            :for line := (read-line stream nil nil)
            :while line
            :do (let ((trimmed (string-trim '(#\Return) line)))
                  (cond
                    ((zerop (length trimmed))
                     (when data-buffer
                       (let ((result
                               (process-openai-codex-responses-sse-event
                                (format nil "~{~A~}" (nreverse data-buffer))
                                state)))
                         (setf data-buffer nil)
                         (case result
                           (:updated
                            (maybe-call-streaming-callback callback state))
                           (:terminal
                            (loop-finish))))))
                    (t
                     (multiple-value-bind (field value) (parse-sse-line trimmed)
                       (when (and field (string= "data" field))
                         (push value data-buffer)))))))
    (error (e)
      (transition-stream-state-to-terminal
       state
       :error (format nil "~A" e))))
  (transition-stream-state-to-terminal state)
  (unless defer-terminal-callback
    (maybe-call-streaming-callback callback state :terminal t)))

(defun read-stream-prefix (stream maximum-characters)
  "Read at most MAXIMUM-CHARACTERS from STREAM and return them as text."
  (let ((output (make-string-output-stream)))
    (loop :repeat maximum-characters
          :for character := (read-char stream nil nil)
          :while character
          :do (write-char character output))
    (get-output-stream-string output)))

(defun streaming-http-error-body (state body)
  "Read a bounded printable error BODY under STATE's cancellable ownership.

Streaming error bodies can block just like successful SSE bodies.  Convert the
body to a character stream, register it before the first read, and leave final
close ownership to the managed reader unwind.  Return NIL when registration
loses to cancellation."
  (if (streamp body)
      (let ((text-stream nil)
            ;; Until registration succeeds, this scope owns BODY or its wrapper.
            (locally-owned-stream body))
        (unwind-protect
             (progn
               (setf text-stream (utf8-character-input-stream body)
                     locally-owned-stream text-stream)
               (if (register-stream-state-stream state text-stream)
                   (progn
                     (setf locally-owned-stream nil)
                     (read-stream-prefix
                      text-stream
                      *provider-http-error-body-max-characters*))
                   ;; REGISTER closed the rejected stream outside the state
                   ;; lock, so this scope no longer owns it either.
                   (progn
                     (setf locally-owned-stream nil)
                     nil)))
          (when locally-owned-stream
            (ignore-errors
              (close locally-owned-stream :abort t)))))
      (format nil "~A" body)))

(defun openai-codex-request-streaming (messages callback
                                       &key (model *openai-codex-model*)
                                            (max-tokens *default-max-tokens*)
                                            tools
                                            reasoning-effort
                                            service-tier
                                            (system-prompt (or (build-system-prompt) "")))
  "Start an asynchronous OpenAI Responses SSE request and return its STATE."
  (let ((state (make-stream-state :callback callback)))
    (start-stream-state-reader-worker
     state
     callback
     "rplaca-openai-codex-responses"
     (lambda (worker-state)
       (block request
         (unless (stream-state-active-p-safe worker-state)
           (return-from request))
         (let* ((auth
                  (or (resolve-openai-codex-auth)
                      (error 'simple-error
                             :format-control "No OpenAI Codex auth. Save a bearer token to ~~/.config/rplaca/openai-codex-token or sign in via ~~/.codex/auth.json")))
                (request-body
                  (openai-codex-responses-request-body
                   messages model max-tokens tools
                   :stream t
                   :system-prompt system-prompt
                   :service-tier service-tier
                   :reasoning-effort reasoning-effort)))
           (unless (stream-state-active-p-safe worker-state)
             (return-from request))
           (multiple-value-bind
                 (body-stream status-code ignored effective-auth)
               (openai-codex-http-request
                auth request-body
                :stream t
                :cancel-p
                (lambda ()
                  (not (stream-state-active-p-safe worker-state))))
             (declare (ignore ignored effective-auth))
             (unless (stream-state-active-p-safe worker-state)
               (when (streamp body-stream)
                 (ignore-errors
                   (close body-stream)))
               (return-from request))
             (unless (= status-code 200)
               (let ((detail
                       (streaming-http-error-body
                        worker-state body-stream)))
                 (when (stream-state-active-p-safe worker-state)
                   (error "API error (~A): ~A" status-code detail)))
               (return-from request))
             (let ((owned-body body-stream)
                   (sse-stream nil))
               (unwind-protect
                    (progn
                      (setf sse-stream
                            (utf8-character-input-stream body-stream))
                      (if (register-stream-state-stream
                           worker-state sse-stream)
                          (progn
                            ;; The state now owns SSE-STREAM and its underlying
                            ;; BODY-STREAM until cancellation or worker cleanup.
                            (setf owned-body nil)
                            (read-openai-codex-responses-sse-stream
                             sse-stream worker-state callback
                             :defer-terminal-callback t))
                          ;; Registration rejected a concurrent cancellation and
                          ;; closed SSE-STREAM outside the state lock.
                          (setf owned-body nil
                                sse-stream nil)))
                 (when owned-body
                   (ignore-errors
                     (close owned-body))))))))))
    state))

;;; --------------------------------------------------------------------------
;;; OpenRouter API — OpenAI-compatible
;;; --------------------------------------------------------------------------
;;; OpenRouter proxies requests to 300+ models (OpenAI, Anthropic, Google,
;;; Meta, DeepSeek, etc.) through a single OpenAI-compatible endpoint.
;;; Authentication uses a Bearer API key obtained from openrouter.ai/keys.
;;; Model names follow the 'provider/model-name' format.

(defun openrouter-request (messages &key (model *openrouter-model*)
                                          (max-tokens *default-max-tokens*)
                                          tools
                                          (system-prompt (build-system-prompt)))
  "Call the OpenRouter Chat Completions API and normalize the response shape.
Uses the OpenAI-compatible chat completions protocol."
  (let* ((token (or (read-provider-token :openrouter)
                    (error 'simple-error
                           :format-control "No OpenRouter API key. Set OPENROUTER_API_KEY env var or save to ~/.config/rplaca/openrouter-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt
                                               messages
                                               :system-prompt system-prompt)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(tool-definitions->openai-tools tools)) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (provider-http-request-with-retries
         "OpenRouter request"
         (lambda ()
           (drakma:http-request
            *openrouter-api-url*
            :method :post
            :content-type "application/json"
            :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                                  ("HTTP-Referer" . "https://github.com/htayj/rplaca")
                                  ("X-Title" . "RPLACA"))
            :content request-body
            :want-stream nil
            :force-binary nil
            :connection-timeout *provider-http-connection-timeout-seconds*)))
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "OpenRouter API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "OpenRouter response did not include a choice"))
          (openai-choice->canonical-response choice))))))

(defun openrouter-request-streaming (messages callback
                                     &key (model *openrouter-model*)
                                          (max-tokens *default-max-tokens*)
                                          tools
                                          (system-prompt (build-system-prompt)))
  "Start an asynchronous OpenRouter SSE request and return its STATE."
  (let ((state (make-stream-state :callback callback)))
    (start-stream-state-reader-worker
     state
     callback
     "rplaca-openrouter-sse-reader"
     (lambda (worker-state)
       (block request
         (unless (stream-state-active-p-safe worker-state)
           (return-from request))
         (let* ((token
                  (or (read-provider-token :openrouter)
                      (error 'simple-error
                             :format-control "No OpenRouter API key. Set OPENROUTER_API_KEY env var or save to ~/.config/rplaca/openrouter-api-key")))
                (request-body
                  (let ((body
                          `((:model . ,model)
                            (:max--tokens . ,max-tokens)
                            (:stream . t)
                            (:messages
                             . ,(coerce
                                 (openai-messages-with-system-prompt
                                  messages
                                  :system-prompt system-prompt)
                                 'vector)))))
                    (when (and tools (plusp (length tools)))
                      (push `(:tools
                              . ,(tool-definitions->openai-tools tools))
                            body))
                    (api-json-encode body))))
           (unless (stream-state-active-p-safe worker-state)
             (return-from request))
           (multiple-value-bind (body-stream status-code headers)
               (provider-http-request-with-retries
                "OpenRouter streaming request"
                (lambda ()
                  (unless (stream-state-active-p-safe worker-state)
                    (return-from request))
                  (drakma:http-request
                   *openrouter-api-url*
                   :method :post
                   :content-type "application/json"
                   :additional-headers
                   `(("Authorization"
                      . ,(format nil "Bearer ~A" token))
                     ("HTTP-Referer"
                      . "https://github.com/htayj/rplaca")
                     ("X-Title" . "RPLACA"))
                   :content request-body
                   :want-stream t
                   :connection-timeout
                   *provider-http-connection-timeout-seconds*))
                :cancel-p
                (lambda ()
                  (not (stream-state-active-p-safe worker-state))))
             (declare (ignore headers))
             (unless (stream-state-active-p-safe worker-state)
               (when (streamp body-stream)
                 (ignore-errors
                   (close body-stream)))
               (return-from request))
             (unless (= status-code 200)
               (let ((detail
                       (streaming-http-error-body
                        worker-state body-stream)))
                 (when (stream-state-active-p-safe worker-state)
                   (error "OpenRouter API error (~A): ~A"
                          status-code detail)))
               (return-from request))
             (when (register-stream-state-stream
                    worker-state body-stream)
               (read-openai-sse-stream
                body-stream worker-state callback
                :defer-terminal-callback t)))))))
    state))

;;; --------------------------------------------------------------------------
;;; Z.AI (Zhipu AI) API — OpenAI-compatible
;;; --------------------------------------------------------------------------

(defun zai-request (messages &key (model *zai-model*)
                                   (max-tokens *default-max-tokens*)
                                   tools
                                   reasoning-effort
                                   (system-prompt (build-system-prompt)))
  "Call Z.AI Chat Completions API and normalize the response shape.
Uses the coding plan endpoint (api.z.ai/api/coding/paas/v4) which is
compatible with the GLM Coding Max-Monthly subscription.
The API follows the OpenAI Chat Completions format."
  (let* ((token (or (read-provider-token :zai)
                    (error 'simple-error
                           :format-control "No Z.AI API key. Set ZAI_CODING_MAX_API_KEY env var or save to ~/.config/rplaca/zai-api-key")))
         (request-body
            (let ((body `((:model . ,model)
                          (:max--tokens . ,max-tokens)
                         (:messages . ,(coerce (openai-messages-with-system-prompt
                                               messages
                                               :system-prompt system-prompt)
                                               'vector)))))
              (when (and tools (plusp (length tools)))
                (push `(:tools . ,(tool-definitions->openai-tools tools)) body))
              (when reasoning-effort
                (push `(:reasoning--effort . ,reasoning-effort) body))
              (api-json-encode body))))
    (multiple-value-bind (body status-code)
        (provider-http-request-with-retries
         "Z.AI request"
         (lambda ()
           (drakma:http-request
            *zai-api-url*
            :method :post
            :content-type "application/json"
            :additional-headers `(("Authorization" . ,(format nil "Bearer ~A" token))
                                  ("Accept-Language" . "en-US,en"))
            :content request-body
            :want-stream nil
            :force-binary nil
            :connection-timeout *provider-http-connection-timeout-seconds*)))
      (let ((body-string (http-body-string body)))
        (unless (= status-code 200)
          (error "Z.AI API error (~A): ~A" status-code body-string))
        (let* ((response (api-json-decode body-string))
               (choices (cdr (assoc :choices response)))
               (choice (first (coerce choices 'list))))
          (unless choice
            (error "Z.AI response did not include a choice"))
          (openai-choice->canonical-response choice))))))

(defun zai-request-streaming (messages callback
                               &key (model *zai-model*)
                                    (max-tokens *default-max-tokens*)
                                    tools
                                    reasoning-effort
                                    (system-prompt (build-system-prompt)))
  "Start an asynchronous Z.AI SSE request and return its STATE."
  (let ((state (make-stream-state :callback callback)))
    (start-stream-state-reader-worker
     state
     callback
     "rplaca-zai-sse-reader"
     (lambda (worker-state)
       (block request
         (unless (stream-state-active-p-safe worker-state)
           (return-from request))
         (let* ((token
                  (or (read-provider-token :zai)
                      (error 'simple-error
                             :format-control "No Z.AI API key. Set ZAI_CODING_MAX_API_KEY env var or save to ~/.config/rplaca/zai-api-key")))
                (request-body
                  (let ((body
                          `((:model . ,model)
                            (:max--tokens . ,max-tokens)
                            (:stream . t)
                            (:messages
                             . ,(coerce
                                 (openai-messages-with-system-prompt
                                  messages
                                  :system-prompt system-prompt)
                                 'vector)))))
                    (when (and tools (plusp (length tools)))
                      (push `(:tools
                              . ,(tool-definitions->openai-tools tools))
                            body)
                      (push '(:tool--stream . t) body))
                    (when reasoning-effort
                      (push `(:reasoning--effort . ,reasoning-effort) body))
                    (api-json-encode body))))
           (unless (stream-state-active-p-safe worker-state)
             (return-from request))
           (multiple-value-bind (body-stream status-code headers)
               (provider-http-request-with-retries
                "Z.AI streaming request"
                (lambda ()
                  (unless (stream-state-active-p-safe worker-state)
                    (return-from request))
                  (drakma:http-request
                   *zai-api-url*
                   :method :post
                   :content-type "application/json"
                   :additional-headers
                   `(("Authorization"
                      . ,(format nil "Bearer ~A" token))
                     ("Accept-Language" . "en-US,en"))
                   :content request-body
                   :want-stream t
                   :connection-timeout
                   *provider-http-connection-timeout-seconds*))
                :cancel-p
                (lambda ()
                  (not (stream-state-active-p-safe worker-state))))
             (declare (ignore headers))
             (unless (stream-state-active-p-safe worker-state)
               (when (streamp body-stream)
                 (ignore-errors
                   (close body-stream)))
               (return-from request))
             (unless (= status-code 200)
               (let ((detail
                       (streaming-http-error-body
                        worker-state body-stream)))
                 (when (stream-state-active-p-safe worker-state)
                   (error "Z.AI API error (~A): ~A"
                          status-code detail)))
               (return-from request))
             (when (register-stream-state-stream
                    worker-state body-stream)
               (read-openai-sse-stream
                body-stream worker-state callback
                :defer-terminal-callback t)))))))
    state))

;;; --------------------------------------------------------------------------
;;; Response Parsing Helpers
;;; --------------------------------------------------------------------------

(defun response-stop-reason (response)
  "Extract stop_reason from an API response."
  (cdr (assoc :stop--reason response)))

(defun response-content (response)
  "Extract content blocks from an API response as a list."
  (let ((content (cdr (assoc :content response))))
    (coerce content 'list)))

(defun response-usage (response)
  "Extract normalized token usage from an API response."
  (cdr (assoc :usage response)))

(defun content-block-type (block)
  "Return the type string of a content block."
  (cdr (assoc :type block)))

(defun content-text-blocks (content-blocks)
  "Extract and concatenate all text from text-type content blocks."
  (with-output-to-string (s)
    (let ((first t))
      (dolist (block content-blocks)
        (when (string= "text" (content-block-type block))
          (unless first (write-char #\Newline s))
          (write-string (cdr (assoc :text block)) s)
          (setf first nil))))))

(defun content-reasoning-blocks (content-blocks)
  "Extract provider-supplied reasoning text blocks."
  (loop :for block :in content-blocks
        :when (string= "reasoning" (content-block-type block))
          :collect (or (cdr (assoc :text block)) "")))

(defun content-tool-use-blocks (content-blocks)
  "Extract tool_use blocks from content."
  (remove-if-not (lambda (b) (string= "tool_use" (content-block-type b)))
                 content-blocks))

(defun format-tool-call-display (tool-use-block)
  "Format a tool_use block for display in the chat."
  (let ((name (cdr (assoc :name tool-use-block)))
        (input (cdr (assoc :input tool-use-block))))
    (format-tool-call-sexpr name input)))

(defun format-lisp-eval-result-display (result-text)
  "Format RESULT-TEXT from lisp_eval as a Lisp-friendly display block."
  (handler-case
      (let* ((payload (lisp-data-read result-text))
             (code (getf payload :code))
             (values-count (or (getf payload :values) 0))
             (result (getf payload :result))
             (output (getf payload :output))
             (error-output (getf payload :error-output))
             (error-text (getf payload :error))
             (denied-p (getf payload :denied))
             (reason (getf payload :reason)))
        (with-output-to-string (s)
          (write-string ";; lisp_eval" s)
          (when code
            (format s "~%~A" code))
          (cond
            (denied-p
             (format s "~%;; denied~%~S" (or reason "User denied")))
            (error-text
             (format s "~%;; error~%~A"
                     (format nil "(error ~S)" error-text)))
            (t
             (format s "~%;; => ~D value~:P" values-count)
             (when result
               (format s "~%~A" result))
             (when (and output (plusp (length output)))
               (format s "~%;; output~%~A" output))
             (when (and error-output (plusp (length error-output)))
               (format s "~%;; error-output~%~A" error-output))))))
    (error ()
      nil)))

(defun format-tool-result-display (tool-name result-text)
  "Format a tool result for display in the chat."
  (or (and (string= tool-name "lisp_eval")
           (format-lisp-eval-result-display result-text))
      (let ((preview (if (> (length result-text) 200)
                         (concatenate 'string (subseq result-text 0 200) "...")
                         result-text)))
        (format nil "[~A result: ~A]" tool-name preview))))
