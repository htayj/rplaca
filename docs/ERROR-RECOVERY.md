# Graphical error recovery

Unexpected errors in a running chat frame open McCLIM's graphical debugger.
The failing computation remains suspended: the displayed restart choices refer
to the live condition and stack. Background provider and tool failures that are
already handled as application results retain their existing behavior.

For a transcript display error, choose `CLEAR-PANE-TRY-AGAIN` to ask CLIM to
rebuild that pane's output. The new messages can then appear without sending
another chat message. `SKIP-REDISPLAY` skips the current pane update.

Restart choices are clickable presentations. Number keys `0` through `9`
invoke the corresponding restart. `q` closes the debugger and invokes the
nearest abort restart; RPLACA's abort choice returns to the application without
automatically retrying the failed action. Aborting does not undo effects that
the action completed before it failed. The compose draft is retained.

The native debugger also provides stack-frame selection, inspection of local
values, and evaluation in a selected frame. These are live debugging tools;
their display may include application data. They are separate from the bounded
on-disk [fatal crash reports](CRASH-REPORTS.md).

For unattended embedding, set
`(setf rplaca:*chat-graphical-debugger-enabled-p* nil)`. Ordinary error
containment and diagnostic messages remain active. An unavailable or failing
graphical debugger falls back to that containment instead of recursively
opening debuggers. Fatal errors outside these UI boundaries still use the
existing crash-report and launcher-supervision path.

## Redisplay ownership

Worker notifications enter McCLIM's event queue and application results are
applied on the owning frame process. RPLACA has no custom redisplay dirty flags,
generation reservations, pending latch, or retry scheduler. Frame startup
requests an update after CLIM has adopted the panes.

The transcript remains an application pane using presentations and
`updating-output`. CLIM owns output records, incremental redisplay, output
buffering, clearing, and repainting. Async updates call the native pane
redisplay protocol directly because ESA's whole-frame redisplay deliberately
waits while an incomplete key sequence is pending.

Error handlers use `handler-bind` so that CLIM's native pane recovery restarts
are still available. The fallback for a display failure does not enqueue
another display of its own error, preventing a persistent failure from making
an endless notification loop. Diagnostic printing handles circular Lisp objects
and limits printed list depth and length. Domain actions are never automatically
retried.

## Genera influence and references

The local `genera-emu` knowledge base's *The Genera Debugger and Display
Debugger* describes a graphical view of suspended execution with
condition-specific proceed choices, stack frames, locals, and inspection.
It also describes fallback behavior when the graphical debugger itself is
unusable. That analysis distinguishes Genera's Dynamic Windows implementation
from CLIM, and records its manual, licensed-source, and earlier runtime
evidence. Genera was not rerun for this change and no licensed implementation
was copied.

RPLACA applies those ideas using the existing McCLIM debugger rather than
building a separate debugger or rendering layer. The implementation was
checked against the Guix-pinned McCLIM 1.0.0 `Apps/Debugger/clim-debugger.lisp`,
the native frame/pane redisplay methods, and ESA's command methods.

Public references:

- [McCLIM manual: Debugger](https://mcclim.common-lisp.dev/static/manual/mcclim.html#Debugger)
- [McCLIM manual: Using incremental redisplay](https://mcclim.common-lisp.dev/static/manual/mcclim.html#Using-incremental-redisplay)
- [Symbolics, Program Development Utilities, Genera 8](https://bitsavers.org/pdf/symbolics/software/genera_8/Program_Development_Utilities.pdf)

## Verification

The FiveAM suite covers live restart extent, abort without replaying effects,
draft preservation, debugger failure, notification delivery, and suppression
of error-notification loops. Graphical verification additionally uses real
CLX/ESA/Drei frames under private Xvfb with the deterministic provider; it must
exercise native restart selection and return to the composer.

Run the graphical recovery probe with `sh scripts/probe-ui-recovery.sh`. It
enters the Guix container, runs the deterministic provider with a pending ESA
key prefix, injects a transient display error, selects the native retry with a
number key, and checks command abort, draft preservation, and subsequent
typing. Screenshots and the probe log are retained under the printed
`.artifacts/ui-recovery.*` directory.
