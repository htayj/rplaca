#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)

# Debug log written inside the container at /workspace/debug.log,
# which maps to $REPO_ROOT/debug.log on the host.
export RPLACA_DEBUG_LOG=/workspace/debug.log

clean_build=${RPLACA_RUN_CLEAN_BUILD:-0}

exec python3 "$SCRIPT_DIR/scripts/crash-repair-supervisor.py" -- \
  "$SCRIPT_DIR/scripts/guix-container.sh" --mode run -- \
  sh -lc 'RPLACA_RUN_CLEAN_BUILD="$1"; export RPLACA_RUN_CLEAN_BUILD; shift; exec sbcl --noinform --script scripts/run.lisp "$@"' sh "$clean_build" "$@"
