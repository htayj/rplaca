(defpackage :rplaca/tests
  (:use :cl :fiveam :rplaca))

(in-package :rplaca/tests)

(def-suite rplaca-suite
  :description "All rplaca tests")

(def-suite listener-dispatch-suite
  :description "Listener line dispatch specification tests"
  :in rplaca-suite)

(def-suite legacy-path-suite
  :description "Legacy path migration tests"
  :in rplaca-suite)

(def-suite migration-integration-suite
  :description "RPLACA persistent-surface migration integration tests"
  :in rplaca-suite)

(def-suite message-suite
  :description "Message and line tests"
  :in rplaca-suite)

(def-suite buffer-suite
  :description "Buffer tests"
  :in rplaca-suite)

(def-suite info-suite
  :description "Info/manual browser tests"
  :in rplaca-suite)

(def-suite windows-suite
  :description "Logical window tree tests"
  :in rplaca-suite)

(def-suite commands-suite
  :description "Command system tests"
  :in rplaca-suite)

(def-suite safe-reload-suite
  :description "Safe in-place reload tests"
  :in rplaca-suite)

(def-suite matching-suite
  :description "Minibuffer matching tests"
  :in rplaca-suite)

(def-suite projects-suite
  :description "Project resource abstraction tests"
  :in rplaca-suite)

(def-suite skills-suite
  :description "Skill discovery and prompt injection tests"
  :in rplaca-suite)

(def-suite font-editor-suite
  :description "Bitmap font editor tests"
  :in rplaca-suite)

(def-suite appearance-suite
  :description "Appearance declaration and resolution tests"
  :in rplaca-suite)

(def-suite appearance-config-suite
  :description "Persisted appearance configuration and startup selector tests"
  :in rplaca-suite)

(def-suite crash-report-suite
  :description "Private bounded fatal crash reports"
  :in rplaca-suite)

(def-suite mcclim-interface-suite
  :description "Frame-local McCLIM interface and lifecycle tests"
  :in rplaca-suite)

(def-suite sexed-suite
  :description "Agent-oriented s-expression editing tests"
  :in rplaca-suite)

(def-suite slop-suite
  :description "Agent-oriented Common Lisp symbol lookup tests"
  :in rplaca-suite)

(def-suite git-package-suite
  :description "Agent-oriented git package tests"
  :in rplaca-suite)

(def-suite organa-package-suite
  :description "Org-mode TODO project management package tests"
  :in rplaca-suite)

(def-suite netcons-suite
  :description "Agent-oriented web lookup package tests"
  :in rplaca-suite)

(def-suite prove-package-suite
  :description "Agent-oriented self-testing package tests"
  :in rplaca-suite)

(def-suite speculum-package-suite
  :description "Agent-oriented McCLIM self-visibility package tests"
  :in rplaca-suite)

(def-suite subagent-package-suite
  :description "Agent-oriented subagent package tests"
  :in rplaca-suite)

(def-suite templata-package-suite
  :description "Slash command and prompt template package tests"
  :in rplaca-suite)

(def-suite quaestor-package-suite
  :description "Structured user-question and queued delivery package tests"
  :in rplaca-suite)

(def-suite modelaria-package-suite
  :description "Scoped model-role and usage package tests"
  :in rplaca-suite)

(def-suite artifactum-package-suite
  :description "Attachment and durable artifact package tests"
  :in rplaca-suite)

(def-suite media-package-suite
  :description "Provider-neutral generated media package tests"
  :in rplaca-suite)

(def-suite interop-suite
  :description "App-server, JSONL, and structured-output interop tests"
  :in rplaca-suite)

(def-suite mcp-bridge-package-suite
  :description "External MCP server bridge package tests"
  :in rplaca-suite)

(def-suite package-manager-suite
  :description "Package loader tests"
  :in rplaca-suite)

(def-suite reference-suite
  :description "Common Lisp spec and local library discovery tests"
  :in rplaca-suite)

(def-suite llm-suite
  :description "LLM helper tests"
  :in rplaca-suite)

(def-suite interactive-operation-suite
  :description "Managed CLIM-frame operation lifecycle tests"
  :in rplaca-suite)

(def-suite gui-e2e-suite
  :description "GUI E2E primitive tests"
  :in rplaca-suite)
