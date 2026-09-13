#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)
LAUNCHER="$SCRIPT_DIR/guix-container.sh"
TEST_NAME="session-migration-container-test-$$"
HOST_ROOT="${TMPDIR:-/tmp}/$TEST_NAME"
CONTAINER_ROOT="$HOST_ROOT"
HOST_LEGACY="$HOST_ROOT/clawmacs/sessions"
HOST_CANONICAL="$HOST_ROOT/rplaca/sessions"
HOST_BARRIER="$HOST_ROOT/barrier"
CONTAINER_LEGACY="$CONTAINER_ROOT/clawmacs/sessions/"
CONTAINER_CANONICAL="$CONTAINER_ROOT/rplaca/sessions/"
CONTAINER_BARRIER="$CONTAINER_ROOT/barrier/"
ENTRY=/workspace/tests/session-migration-subprocess.lisp
ONE_LOG="$HOST_ROOT/one.log"
TWO_LOG="$HOST_ROOT/two.log"
one_pid=''
two_pid=''

# Let the launcher resolve OpenSSL from its actual container environment.
# An arbitrary host store item need not belong to that container's closure.

cleanup() {
  status=$?
  [ -n "$one_pid" ] && kill "$one_pid" >/dev/null 2>&1 || true
  [ -n "$two_pid" ] && kill "$two_pid" >/dev/null 2>&1 || true
  if [ "$status" -ne 0 ]; then
    [ -f "$ONE_LOG" ] && tail -80 "$ONE_LOG" >&2 || true
    [ -f "$TWO_LOG" ] && tail -80 "$TWO_LOG" >&2 || true
  fi
  chmod -R u+w "$HOST_ROOT" >/dev/null 2>&1 || true
  rm -rf "$HOST_ROOT"
  return "$status"
}
trap cleanup EXIT HUP INT TERM

wait_for_file() {
  path="$1"
  count=0
  while [ ! -f "$path" ]; do
    for worker_pid in "$one_pid" "$two_pid"; do
      if [ -n "$worker_pid" ] && ! kill -0 "$worker_pid" 2>/dev/null; then
        printf 'publisher %s exited before %s\n' "$worker_pid" "$path" >&2
        return 1
      fi
    done
    count=$((count + 1))
    if [ "$count" -ge 2400 ]; then
      printf 'timed out waiting for %s\n' "$path" >&2
      return 1
    fi
    sleep 0.05
  done
}

mkdir -p "$HOST_LEGACY/locked/nested" "$HOST_BARRIER"
printf 'cross-container\n' >"$HOST_LEGACY/locked/nested/payload.json"
chmod 400 "$HOST_LEGACY/locked/nested/payload.json"
chmod 555 "$HOST_LEGACY/locked/nested" "$HOST_LEGACY/locked" "$HOST_LEGACY"

XDG_STATE_HOME="$HOST_ROOT" "$LAUNCHER" --mode run -- \
  env \
  "RPLACA_TEST_REPO_ROOT=/workspace/" \
  "RPLACA_TEST_CANONICAL_SESSIONS=$CONTAINER_CANONICAL" \
  "RPLACA_TEST_LEGACY_SESSIONS=$CONTAINER_LEGACY" \
  "RPLACA_TEST_SESSION_BARRIER=$CONTAINER_BARRIER" \
  "RPLACA_TEST_SESSION_BARRIER_COUNT=1" \
  "RPLACA_TEST_SESSION_WORKER_ID=one" \
  "RPLACA_TEST_SESSION_HOLD_BEFORE_PUBLISH=1" \
  sbcl --noinform --disable-debugger --script "$ENTRY" \
  >"$ONE_LOG" 2>&1 &
one_pid=$!

wait_for_file "$HOST_BARRIER/holding-one"
first_stage=$(find "$HOST_ROOT/rplaca" -maxdepth 1 -type d \
  -name '.sessions-migration-*' -print -quit)
[ -n "$first_stage" ]

XDG_STATE_HOME="$HOST_ROOT" "$LAUNCHER" --mode run -- \
  env \
  "RPLACA_TEST_REPO_ROOT=/workspace/" \
  "RPLACA_TEST_CANONICAL_SESSIONS=$CONTAINER_CANONICAL" \
  "RPLACA_TEST_LEGACY_SESSIONS=$CONTAINER_LEGACY" \
  "RPLACA_TEST_SESSION_BARRIER=$CONTAINER_BARRIER" \
  "RPLACA_TEST_SESSION_BARRIER_COUNT=1" \
  "RPLACA_TEST_SESSION_WORKER_ID=two" \
  sbcl --noinform --disable-debugger --script "$ENTRY" \
  >"$TWO_LOG" 2>&1 &
two_pid=$!

wait_for_file "$HOST_BARRIER/ready-two"
sleep 1
kill -0 "$one_pid"
[ -d "$first_stage" ]

touch "$HOST_BARRIER/release-one"
wait "$one_pid"
one_pid=''
wait "$two_pid"
two_pid=''

# Guix may reserve PID 1 for its container init. The migration invariant is
# that independent publishers can have the same guest PID, not that it is 1.
guest_pid_one=$(sed -n 's/^pid=//p' "$HOST_BARRIER/started-one")
guest_pid_two=$(sed -n 's/^pid=//p' "$HOST_BARRIER/started-two")
case "$guest_pid_one:$guest_pid_two" in
  *[!0-9:]*|:*|*:) printf 'invalid publisher PIDs\n' >&2; exit 1 ;;
esac
if [ "$guest_pid_one" -le 0 ] || [ "$guest_pid_one" != "$guest_pid_two" ]; then
  printf 'expected identical positive guest PIDs, got %s and %s\n' \
    "$guest_pid_one" "$guest_pid_two" >&2
  exit 1
fi
if cmp -s "$HOST_BARRIER/started-one" "$HOST_BARRIER/started-two"; then
  printf 'independent containers unexpectedly reported identical proc state\n' >&2
  exit 1
fi

grep -q '^rplaca-session-migration-v1$' \
  "$HOST_CANONICAL/.rplaca-session-migration-complete"
[ "$(cat "$HOST_CANONICAL/locked/nested/payload.json")" = cross-container ]
[ "$(stat -c %a "$HOST_CANONICAL")" = 555 ]
[ "$(stat -c %a "$HOST_CANONICAL/locked")" = 555 ]
[ "$(stat -c %a "$HOST_CANONICAL/locked/nested")" = 555 ]
[ "$(stat -c %a "$HOST_CANONICAL/locked/nested/payload.json")" = 400 ]
if find "$HOST_ROOT/rplaca" -maxdepth 1 -type d \
    -name '.sessions-migration-*' | grep -q .; then
  printf 'session migration staging tree remained after publication\n' >&2
  exit 1
fi

printf 'session-migration-containers: two independent publishers passed (guest PID %s)\n' \
  "$guest_pid_one"
