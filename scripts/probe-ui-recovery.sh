#!/bin/sh
# Exercise native CLIM restarts with real X input and synthetic provider data.
set -eu
script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
cd "$repo_root"

if [ "${RPLACA_IN_GUIX_CONTAINER:-0}" != 1 ]; then
  export RPLACA_CONTAINER_DISABLE_HOST_X=1
  exec ./scripts/guix-container.sh --mode e2e -- sh scripts/probe-ui-recovery.sh
fi

mkdir -p /workspace/.artifacts
artifact_dir=$(mktemp -d /workspace/.artifacts/ui-recovery.XXXXXX)
export RPLACA_RECOVERY_ARTIFACT_DIR="$artifact_dir" RPLACA_E2E_PROVIDER=1
xvfb_pid=''
cleanup() {
  if [ -n "$xvfb_pid" ]; then
    kill "$xvfb_pid" 2>/dev/null || true
    wait "$xvfb_pid" 2>/dev/null || true
  fi
}
trap cleanup EXIT INT TERM
Xvfb -displayfd 3 -screen 0 1280x1000x24 -nolisten tcp -ac \
  3>"$artifact_dir/display" >"$artifact_dir/xvfb.log" 2>&1 &
xvfb_pid=$!
for attempt in $(seq 1 100); do
  test -s "$artifact_dir/display" && break
  sleep 0.05
done
test -s "$artifact_dir/display"
export DISPLAY=":$(cat "$artifact_dir/display")"
printf 'UI recovery artifacts: %s\n' "$artifact_dir"
status=0
timeout --kill-after=5 120 sbcl --noinform --non-interactive \
  --load "$RPLACA_QUICKLISP_SETUP" \
  --load scripts/assert-mcclim-provenance.lisp \
  --eval '(push (truename ".") asdf:*central-registry*)' \
  --load scripts/probe-ui-recovery.lisp >"$artifact_dir/probe.log" 2>&1 || status=$?
cat "$artifact_dir/probe.log"
test "$status" -eq 0
for marker in idle-stream-with-prefix native-retry-without-message \
              abort-preserves-draft input-focus-after-abort; do
  grep -q "RECOVERY $marker=PASS" "$artifact_dir/probe.log"
done
printf 'UI_RECOVERY_OK\n'
