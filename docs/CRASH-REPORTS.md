# Fatal crash reports

Recoverable errors in a running chat frame use
[graphical error recovery](ERROR-RECOVERY.md). Its live restarts run before
the stack unwinds. The fatal reporting path below remains the fallback for
unhandled failures outside those UI boundaries.

RPLACA writes one private diagnostic report when an unhandled condition
reaches SBCL's debugger while `rplaca-main` owns the application runtime.
This includes a fatal condition on the main/frame thread and an unhandled fatal
condition on a named runtime worker. Conditions consumed by `handler-case`,
restarts, or other ordinary application handlers do not produce reports.

The launcher uses SBCL's `--disable-debugger` path. RPLACA installs its fatal
hook before runtime or frame workers start, writes the report, prints the
resulting pathname to standard error, and then delegates to SBCL's original
disabled-debugger hook. The original condition therefore still terminates the
process with a nonzero status. Embedded Lisp sessions are unaffected outside
the dynamic extent of `rplaca-main`; normal return restores the prior hook.

## Location and permissions

The default directory follows the XDG state convention:

```text
${XDG_STATE_HOME:-$HOME/.local/state}/rplaca/crash-reports/
```

Set `RPLACA_CRASH_REPORT_DIR` to an absolute or working-directory-relative
directory to override it. If that directory already exists, it must be a real
(not symbolic-link) directory owned by the current user with mode `0700`;
RPLACA validates it without changing its permissions. RPLACA creates only
missing owned state descendants with mode `0700`. Each report is created
through an exclusive same-directory temporary file with mode `0600`, flushed,
atomically linked into its final no-replace name, and followed by a directory
flush. A write or publication failure publishes no partial report.

Report filenames contain a UTC timestamp, PID, and process-local atomic
sequence. Existing reports are retained; RPLACA does not prune them
automatically.

## Privacy boundary

Reports are whitelist-only diagnostics. They contain:

- report schema/version, UTC time, PID, Lisp implementation/version;
- normalized working directory and allowlisted argv metadata, never raw
  argument values;
- condition class and a small structured summary for selected standard
  condition families;
- a bounded current-thread backtrace containing allowlisted function names and
  source basenames/form numbers, never frame arguments or locals;
- a bounded thread inventory classified by role, never raw thread names;
- safe counts, booleans, provider/model identifiers, and coarse
  RPLACA/frame/runtime phase data when they are available without locks.

The arbitrary printed text of a condition is deliberately omitted: an error
message can contain prompt, compose, conversation, tool, or provider data.
Reports also exclude environment-variable values, API keys and OAuth tokens,
session and buffer names, conversation and compose text, tool calls/results and
payloads, HTTP bodies/headers, provider stderr, package secrets, and debug-log
contents. Collected scalar metadata is still bounded, credential-shaped text is
redacted defensively, and home/current-directory prefixes are normalized.

The report pathname itself can reveal the configured state-directory path when
printed to standard error. SBCL then prints its original fatal diagnostic to
standard error; that original SBCL output is not part of the sanitized report
and should be handled as sensitive terminal/log data.

## Diagnostic workflow

1. Preserve the report before retrying, together with the application version
   or commit and the exact action that preceded the exit.
2. Check `schema_version`, `condition/type`, `rplaca_state/phase`, and the
   first non-reporter RPLACA frame in `current_thread_backtrace`.
3. Compare thread roles and runtime booleans to distinguish frame, provider,
   tool, subagent, and other worker failures.
4. Reproduce with the same build and configuration. Do not attach raw debug
   logs, credentials, session files, or transcripts merely because the crash
   report does not contain them.
5. Review the report before sharing it. Its collection policy is deliberately
   conservative, but provider/model identifiers and normalized paths may still
   describe private local configuration.

If report creation fails, RPLACA prints a short reporter-failure notice and
continues into the original SBCL fatal hook. Reporter recursion is suppressed,
and a process-wide atomic claim prevents concurrent fatal threads from
publishing more than one report for the same application runtime.

## Automatic Codex repair

Interactive `./run.sh` and `./run-native.sh` launches are supervised. A fatal
crash publishes the normal bounded report plus a private `*.request` handoff
containing the exact bounded, credential-redacted condition message and paths
to the report, debug log, and repair history. Launcher or container failures
that do not publish a fresh request never invoke Codex.

The outer supervisor starts a fresh `codex exec` session inside a dedicated Guix
repair container with the repository mounted at `/workspace`. The fixer must
read `AGENTS.md`, the crash artifacts, and
`.cache/crash-repair/repair-history.md`; preserve unrelated work; implement and
test the smallest repair; and return a schema-validated `fixed` or `blocked`
result. It does not commit or push.

Per-attempt JSONL events, stderr, and the final structured result are retained
under `.cache/crash-repair/attempts/`. The supervisor appends both JSONL and
human-readable histories. While Codex runs, its JSONL event stream, progress,
and errors are also echoed to the terminal that launched RPLACA. RPLACA is
relaunched only after Codex exits zero with
`status: fixed`. The default cap is two repairs in one launch chain, preventing
an endlessly changing crash/relaunch cycle.

- `RPLACA_CRASH_REPAIR=0` disables automatic repair.
- `RPLACA_CRASH_REPAIR_MAX_ATTEMPTS=1..10` changes the chain cap.
- `RPLACA_CODEX_BUNDLE=/absolute/path` overrides native Codex bundle discovery.
