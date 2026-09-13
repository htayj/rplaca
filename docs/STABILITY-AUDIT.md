# Redisplay and stability audit, 2026-09-07

The audit reproduced an application-level lost refresh: the old scheduler
cleared its pending and dirty flags before applying results and displaying
them. A display exception could consume the final notification. In the live
reproduction, the message remained absent until another message was submitted.
This establishes a failure mechanism, not the cause of an unrecorded earlier
user incident.

The custom dirty/pending/generation/retry scheduler has been removed. Worker
notifications now enter CLIM's event queue individually; the owning frame
applies results and requests native pane redisplay. Direct pane requests also
avoid ESA deferring background output during an incomplete key sequence.
The existing application panes, presentations, and `updating-output` remain
the rendering mechanism.

Unexpected UI errors now offer the native McCLIM graphical debugger while the
original stack and restarts remain live. The debugger is restricted to the
frame's owning process, has a nonrecursive fallback, and provides an abort
without automatic action replay. Diagnostic printing handles circular data.
See [Graphical error recovery](ERROR-RECOVERY.md) for behavior, configuration,
and the Genera investigation that informed this choice.

The audit also repaired test infrastructure that had become stale: expected
built-in tool lists, the keybinding test's native Listener interaction, and
the migration test's container OpenSSL and guest-PID assumptions. GUI stability
checks now fail on contained UI errors and unexpected debugger entry.

## Validation

All Lisp and graphical checks used the repository's Guix container wrappers
and the pinned McCLIM 1.0.0. Results apply to the tested working tree, which
already contained unrelated uncommitted changes when the audit began.

| Check | Result |
| --- | --- |
| Guix launch preflight | Passed |
| Fresh ASDF build in an isolated cache | Passed; 48 source components |
| Full FiveAM suite | 6,012 checks passed; zero failed or skipped |
| Python regression tests | 32 passed |
| Launcher, wrapper, and GUI harness shell suites | Eight passed |
| Independent-container session migration | Passed; colliding guest PID 2 |
| Legacy-name allowlist and diff whitespace check | Passed |
| Native graphical recovery probe | All four scenarios passed |
| GUI suites | All 11 passed |

The GUI suites were smoke, M-x, features, keybindings, compose geometry,
Organa, Quaestor, reload, appearance persistence, menu boundaries, and
stability. Each used real CLX/ESA/Drei under private Xvfb, captured screenshots,
and verified natural application exit. The Listener check evaluated a typed
Lisp form in the separate native window and closed it through its Quit command.

The recovery probe verified final provider output without input while an ESA
key prefix was pending; recovery from an injected display failure by selecting
CLIM's native retry without submitting a message; draft preservation after
command abort; and subsequent keyboard input in the composer.

Run `sh scripts/probe-ui-recovery.sh` to repeat the graphical recovery check.
Use `scripts/run-gui-e2e.sh` for the existing GUI suites, starting each suite
through its own container wrapper.

Local evidence is retained in `.cache/stability-audit/validation.json`, the
adjacent test logs, `.artifacts/stability-audit/`, and
`.artifacts/ui-recovery.7TGFLA/`. The original reproduction is retained in
`.cache/redisplay-investigation/`; its report and probe describe the pre-fix
implementation. Earlier failed test attempts remain in the audit directories;
the validation manifest identifies the successful final reruns.

## Coverage limits

Provider responses were deterministic. Live provider APIs, production OAuth
services, and a prolonged real-desktop session were not exercised. Genera's
local documented debugger analysis was reviewed; Genera itself was not rerun
and no licensed implementation was copied. These checks establish the tested
behaviors, not an absence of every possible application or backend bug.
