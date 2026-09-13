#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
REPO_ROOT=$(CDPATH= cd -- "$SCRIPT_DIR/.." && pwd)
LAUNCHER="$REPO_ROOT/run-native.sh"

TMP_DIR=$(mktemp -d)
TMP_BIN="$TMP_DIR/bin"
SETUP_FILE="$TMP_DIR/setup.lisp"

cleanup() {
  rm -rf "$TMP_DIR"
}

trap cleanup EXIT INT TERM

mkdir -p "$TMP_BIN"
printf '%s\n' '(in-package :cl-user)' > "$SETUP_FILE"

cat > "$TMP_BIN/sbcl" <<'EOF'
#!/bin/sh
{
  printf 'clean-build=<%s>\n' "${RPLACA_RUN_CLEAN_BUILD:-}"
  printf 'repair-request-dir=<%s>\n' "${RPLACA_CRASH_REPAIR_REQUEST_DIR:-}"
  printf 'repair-history=<%s>\n' "${RPLACA_CRASH_REPAIR_HISTORY:-}"
  index=0
  for argument in "$@"; do
    printf 'argument[%s]=<%s>\n' "$index" "$argument"
    index=$((index + 1))
  done
} > "$RPLACA_TEST_SBCL_LOG"
EOF
chmod +x "$TMP_BIN/sbcl"

assert_log() {
  label="$1"
  expected="$2"
  if ! diff -u "$expected" "$RPLACA_TEST_SBCL_LOG"; then
    printf 'FAIL %s: native launcher arguments differ\n' "$label" >&2
    exit 1
  fi
}

run_case() {
  label="$1"
  clean_build="$2"
  shift 2
  RPLACA_TEST_SBCL_LOG="$TMP_DIR/$label.log"
  export RPLACA_TEST_SBCL_LOG
  if [ "$clean_build" = unset ]; then
    (
      unset RPLACA_RUN_CLEAN_BUILD
      PATH="$TMP_BIN:$PATH"
      RPLACA_ULTRALISP_SETUP="$SETUP_FILE"
      export PATH RPLACA_ULTRALISP_SETUP
      "$LAUNCHER" "$@"
    )
  else
    RPLACA_RUN_CLEAN_BUILD="$clean_build" \
      PATH="$TMP_BIN:$PATH" \
      RPLACA_ULTRALISP_SETUP="$SETUP_FILE" \
      "$LAUNCHER" "$@"
  fi
}

write_expected() {
  path="$1"
  clean_build="$2"
  shift 2
  {
    printf 'clean-build=<%s>\n' "$clean_build"
    printf 'repair-request-dir=<%s>\n' "$REPO_ROOT/.cache/crash-repair/requests"
    printf 'repair-history=<%s>\n' "$REPO_ROOT/.cache/crash-repair/repair-history.md"
    printf '%s\n' \
      'argument[0]=<--dynamic-space-size>' \
      'argument[1]=<2048>' \
      'argument[2]=<--noinform>' \
      'argument[3]=<--script>' \
      'argument[4]=<scripts/run-ultralisp.lisp>'
    index=5
    for argument in "$@"; do
      printf 'argument[%s]=<%s>\n' "$index" "$argument"
      index=$((index + 1))
    done
  } > "$path"
}

run_case default unset
write_expected "$TMP_DIR/default.expected" 0
assert_log default "$TMP_DIR/default.expected"

run_case environment yes
write_expected "$TMP_DIR/environment.expected" yes
assert_log environment "$TMP_DIR/environment.expected"

run_case explicit unset --clean-build
write_expected "$TMP_DIR/explicit.expected" 0 --clean-build
assert_log explicit "$TMP_DIR/explicit.expected"

run_case unrelated unset --session native-test --debug-log /tmp/native.log
write_expected "$TMP_DIR/unrelated.expected" 0 \
  --session native-test --debug-log /tmp/native.log
assert_log unrelated "$TMP_DIR/unrelated.expected"

printf '%s\n' 'run-native launcher argument tests: ok'
