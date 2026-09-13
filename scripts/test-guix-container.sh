#!/bin/sh
set -eu

SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
LAUNCHER="$SCRIPT_DIR/guix-container.sh"
REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel)

TMP_DIR=$(mktemp -d)
TEST_CACHE_RELATIVE=".cache/launcher-test-$$"
TEST_CACHE_ROOT="$REPO_ROOT/$TEST_CACHE_RELATIVE"
QUICKLISP_HOME="$TEST_CACHE_ROOT/home/quicklisp"
BOOTSTRAP_ENV="$TEST_CACHE_ROOT/quicklisp-bootstrap.env"
PRODUCTION_BOOTSTRAP_ENV="$SCRIPT_DIR/quicklisp-bootstrap.env"
PRODUCTION_QUICKLISP_SETUP="$REPO_ROOT/.cache/home/quicklisp/setup.lisp"
mkdir -p "$TEST_CACHE_ROOT"
cp "$PRODUCTION_BOOTSTRAP_ENV" "$BOOTSTRAP_ENV"
PRODUCTION_BOOTSTRAP_DIGEST=$(sha256sum "$PRODUCTION_BOOTSTRAP_ENV" | cut -d' ' -f1)
if [ -f "$PRODUCTION_QUICKLISP_SETUP" ]; then
  PRODUCTION_SETUP_STATE=present
  PRODUCTION_SETUP_DIGEST=$(sha256sum "$PRODUCTION_QUICKLISP_SETUP" | cut -d' ' -f1)
  PRODUCTION_SETUP_INODE=$(stat -c '%d:%i' "$PRODUCTION_QUICKLISP_SETUP")
else
  PRODUCTION_SETUP_STATE=absent
  PRODUCTION_SETUP_DIGEST=''
  PRODUCTION_SETUP_INODE=''
fi
export RPLACA_ENABLE_TEST_TOGGLES=1
export RPLACA_TEST_CACHE_ROOT_SET=1
export RPLACA_TEST_CACHE_RELATIVE="$TEST_CACHE_RELATIVE"
export RPLACA_TEST_QUICKLISP_ENV_PATH_SET=1
export RPLACA_TEST_QUICKLISP_ENV_PATH="$BOOTSTRAP_ENV"

cleanup() {
  rm -rf "$TEST_CACHE_ROOT"
  rm -rf "$TMP_DIR"
}

trap cleanup EXIT INT TERM

TMP_BIN="$TMP_DIR/bin"
mkdir -p "$TMP_BIN"
TMP_CONTAINER_BIN="$TMP_DIR/container-bin"
mkdir -p "$TMP_CONTAINER_BIN"
TMP_SSL_LIB="$TMP_DIR/ssl/lib"
mkdir -p "$TMP_SSL_LIB"
export TMP_SSL_LIB
touch "$TMP_SSL_LIB/libssl.so.3"
TMP_FONT_DIR="$TMP_DIR/fonts"
mkdir -p "$TMP_FONT_DIR"
TMP_FONT_FILE="$TMP_FONT_DIR/ok.ttf"
printf 'font\n' > "$TMP_FONT_FILE"
TMP_FONT_UNREADABLE="$TMP_FONT_DIR/unreadable.ttf"
cp "$TMP_FONT_FILE" "$TMP_FONT_UNREADABLE"
chmod 000 "$TMP_FONT_UNREADABLE"
TMP_CODEX_BUNDLE="$TMP_DIR/codex-bundle/@openai/codex-linux-x64/vendor/x86_64-unknown-linux-musl"
mkdir -p "$TMP_CODEX_BUNDLE/bin"
printf '#!/bin/sh\nexit 0\n' > "$TMP_CODEX_BUNDLE/bin/codex"
chmod +x "$TMP_CODEX_BUNDLE/bin/codex"
REAL_SHA256SUM=$(command -v sha256sum)
REAL_MKDIR=$(command -v mkdir)
EXPECTED_SHA=$(printf 'quicklisp\n' | "$REAL_SHA256SUM" | cut -d' ' -f1)
export REAL_SHA256SUM REAL_MKDIR
TEST_CONTAINER_PATH="$TMP_CONTAINER_BIN"
export TEST_CONTAINER_PATH

write_bootstrap_env() {
  sha="$1"
  printf '%s\n' \
    'QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp' \
    "QUICKLISP_BOOTSTRAP_SHA256=$sha" \
    'QUICKLISP_BOOTSTRAP_TIMEOUT_SECS=20' \
    'QUICKLISP_BOOTSTRAP_RETRIES=2' > "$BOOTSTRAP_ENV"
}

write_bootstrap_env "$EXPECTED_SHA"
cat > "$TMP_BIN/mkdir" <<'EOF'
#!/bin/sh
case ":${LD_LIBRARY_PATH:-}:" in
  *:"$TMP_SSL_LIB":*)
    exit 97
    ;;
esac
exec "$REAL_MKDIR" "$@"
EOF
chmod +x "$TMP_BIN/mkdir"


cat > "$TMP_BIN/sbcl" <<'EOF'
#!/bin/sh
if [ "${RPLACA_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$TEST_CONTAINER_PATH/sbcl" "$@"
EOF
chmod +x "$TMP_BIN/sbcl"

cat > "$TMP_CONTAINER_BIN/sbcl" <<'EOF'
#!/bin/sh
setup=''
is_probe=0
install_path=''
is_warm=0
is_provenance_check=0
while [ "$#" -gt 0 ]; do
  case "$1" in
    --load)
      shift
      setup="$1"
      is_probe=1
      case "$setup" in
        */quicklisp.lisp)
          mkdir -p "$HOME/quicklisp"
          printf '(load "ok")\n' > "$HOME/quicklisp/setup.lisp"
          ;;
        */scripts/assert-mcclim-provenance.lisp)
          is_provenance_check=1
          ;;
      esac
      ;;
    --eval)
      shift
      eval_arg="$1"
      case "$eval_arg" in
        *'(ql:quickload :rplaca)'*)
          is_warm=1
          ;;
        *quicklisp-quickstart:install*':path "'*'"'*)
          install_path=$(printf '%s' "$eval_arg" | sed -n 's/.*:path "\([^"]*\)".*/\1/p')
          ;;
      esac
      ;;
    */quicklisp.lisp)
      mkdir -p "$HOME/quicklisp"
      printf '(load "ok")\n' > "$HOME/quicklisp/setup.lisp"
      ;;
  esac
  shift
done

if [ -n "$install_path" ]; then
  mkdir -p "$install_path"
  printf '(load "ok")\n' > "$install_path/setup.lisp"
  if [ -n "${RPLACA_TEST_BOOTSTRAP_LOG:-}" ]; then
    printf 'bootstrap %s\n' "$$" >> "$RPLACA_TEST_BOOTSTRAP_LOG"
  fi
fi

if [ "$is_probe" -eq 1 ]; then
  if [ "${RPLACA_TEST_SBCL_FAIL_PROBE:-0}" = "1" ]; then
    exit 1
  fi
  if [ ! -f "$setup" ]; then
    exit 1
  fi
fi

if [ "$is_provenance_check" -eq 1 ]; then
  expected_registry="${GUIX_ENVIRONMENT:?missing mock Guix environment}/share/common-lisp/systems/"
  if [ "${CL_SOURCE_REGISTRY:-}" != "$expected_registry" ]; then
    exit 95
  fi
fi

if [ "${RPLACA_TEST_SBCL_FAIL_BOOTSTRAP:-0}" = "1" ]; then
  exit 1
fi

if [ "$is_warm" -eq 1 ] && [ "${RPLACA_TEST_SBCL_FAIL_WARM:-0}" = "1" ]; then
  exit 1
fi

if [ "$is_warm" -eq 1 ] && [ -n "${RPLACA_TEST_WARM_LOG:-}" ]; then
  guard="${RPLACA_TEST_WARM_GUARD:?missing warm guard}"
  if ! "$REAL_MKDIR" "$guard" 2>/dev/null; then
    echo "mock warmup overlap" >&2
    exit 96
  fi
  trap 'rmdir "$guard" >/dev/null 2>&1 || true' EXIT INT TERM
  printf 'warm %s\n' "$$" >> "$RPLACA_TEST_WARM_LOG"
  sleep 0.2
  rmdir "$guard"
  trap - EXIT INT TERM
fi

exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/sbcl"

cat > "$TMP_BIN/python3" <<'EOF'
#!/bin/sh
if [ "${RPLACA_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$TEST_CONTAINER_PATH/python3" "$@"
EOF
chmod +x "$TMP_BIN/python3"

cat > "$TMP_CONTAINER_BIN/python3" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/python3"

cat > "$TMP_CONTAINER_BIN/Xvfb" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/Xvfb"

cat > "$TMP_CONTAINER_BIN/xauth" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/xauth"

cat > "$TMP_CONTAINER_BIN/xdotool" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/xdotool"

cat > "$TMP_CONTAINER_BIN/setsid" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/setsid"

cat > "$TMP_CONTAINER_BIN/import" <<'EOF'
#!/bin/sh
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/import"

cat > "$TMP_BIN/guix" <<'EOF'
#!/bin/sh
if [ -n "${RPLACA_GUIX_ARGS_LOG:-}" ]; then
  printf 'CALL\n' >> "$RPLACA_GUIX_ARGS_LOG"
  printf '%s\n' "$@" >> "$RPLACA_GUIX_ARGS_LOG"
fi
share=''
mount_mappings=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    --share=*=/workspace)
      share=${1#--share=}
      ;;
    --share=*|--expose=*)
      mapping=${1#*=}
      mount_mappings="$mount_mappings
$mapping"
      ;;
  esac
  if [ "$1" = "--" ]; then
    shift
    break
  fi
  shift
done
if [ "$#" -ge 3 ] && [ "$1" = "bash" ] && [ "$2" = "-lc" ]; then
  command="$3"
  workspace=${share%=/workspace}
  if [ -n "$workspace" ]; then
    command=$(printf '%s' "$command" | sed "s|/workspace|$workspace|g")
  fi
  if [ "$#" -ge 4 ] && [ "$4" = "bash" ]; then
    shift 4
    if [ -n "$workspace" ]; then
      mapped_args=''
      while [ "$#" -gt 0 ]; do
        arg="$1"
        shift
        old_ifs=$IFS
        IFS='
'
        for mapping in $mount_mappings; do
          source_path=${mapping%%=*}
          target_path=${mapping#*=}
          case "$arg" in
            "$target_path")
              arg="$source_path"
              ;;
            "$target_path"/*)
              arg="$source_path${arg#"$target_path"}"
              ;;
          esac
        done
        IFS=$old_ifs
        case "$arg" in
          /workspace)
            arg="$workspace"
            ;;
          /workspace/*)
            arg="$workspace${arg#/workspace}"
            ;;
        esac
        mapped_args="$mapped_args
$arg"
      done
      set --
      old_ifs=$IFS
      IFS='
'
      for arg in $mapped_args; do
        set -- "$@" "$arg"
      done
      IFS=$old_ifs
    fi
    if [ -n "${TEST_CONTAINER_PATH:-}" ]; then
      PATH="$TEST_CONTAINER_PATH:$PATH"
      export PATH
    fi
    GUIX_ENVIRONMENT="$workspace/.guix-profile"
    export GUIX_ENVIRONMENT
    sh -c "$command" sh "$@"
    exit $?
  fi
  shift 3
  if [ -n "${TEST_CONTAINER_PATH:-}" ]; then
    PATH="$TEST_CONTAINER_PATH:$PATH"
    export PATH
  fi
  GUIX_ENVIRONMENT="$workspace/.guix-profile"
  export GUIX_ENVIRONMENT
  sh -c "$command"
  exit $?
fi
"$@"
EOF
chmod +x "$TMP_BIN/guix"

cat > "$TMP_BIN/curl" <<'EOF'
#!/bin/sh
if [ "${RPLACA_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$TEST_CONTAINER_PATH/curl" "$@"
EOF
chmod +x "$TMP_BIN/curl"

cat > "$TMP_CONTAINER_BIN/curl" <<'EOF'
#!/bin/sh
if [ "${RPLACA_TEST_CURL_FAIL:-0}" = "1" ]; then
  exit 22
fi

outfile=''
seen_timeout=''
seen_retries=''
while [ "$#" -gt 0 ]; do
  case "$1" in
    --max-time)
      shift
      seen_timeout="$1"
      ;;
    --retry)
      shift
      seen_retries="$1"
      ;;
    -o)
      shift
      outfile="$1"
      ;;
  esac
  shift
done

if [ -n "${RPLACA_EXPECT_CURL_TIMEOUT:-}" ] && [ "$seen_timeout" != "$RPLACA_EXPECT_CURL_TIMEOUT" ]; then
  exit 99
fi
if [ -n "${RPLACA_EXPECT_CURL_RETRIES:-}" ] && [ "$seen_retries" != "$RPLACA_EXPECT_CURL_RETRIES" ]; then
  exit 99
fi

printf '%s\n' "${RPLACA_TEST_CURL_BODY:-quicklisp}" > "$outfile"
exit 0
EOF
chmod +x "$TMP_CONTAINER_BIN/curl"

cat > "$TMP_BIN/sha256sum" <<'EOF'
#!/bin/sh
if [ "${RPLACA_FAIL_HOST_TOOL_USE:-0}" = "1" ]; then
  exit 98
fi
exec "$TEST_CONTAINER_PATH/sha256sum" "$@"
EOF
chmod +x "$TMP_BIN/sha256sum"

cat > "$TMP_CONTAINER_BIN/sha256sum" <<'EOF'
#!/bin/sh
exec "$REAL_SHA256SUM" "$@"
EOF
chmod +x "$TMP_CONTAINER_BIN/sha256sum"

cat > "$TMP_BIN/ldconfig" <<EOF
#!/bin/sh
if [ "\${RPLACA_TEST_LDCONFIG_EMPTY:-0}" = "1" ]; then
  exit 0
fi
if [ "\${RPLACA_TEST_LDCONFIG_MODE:-ssl}" = "crypto" ]; then
  echo "libcrypto.so.3 (libc6,x86-64) => $TMP_SSL_LIB/libcrypto.so.3"
  exit 0
fi
echo "libssl.so.3 (libc6,x86-64) => $TMP_SSL_LIB/libssl.so.3"
EOF
chmod +x "$TMP_BIN/ldconfig"

run_case() {
  name="$1"
  expected_code="$2"
  expect_prefix="$3"
  toggle_var="$4"
  shift 4

  stderr_file="$TMP_DIR/$name.stderr"
  set +e
  if [ "$toggle_var" = "-" ]; then
    env PATH="$TMP_BIN:$PATH" \
      RPLACA_ENABLE_TEST_TOGGLES=1 \
      RPLACA_SSL_LIB="$TMP_SSL_LIB" \
      "$LAUNCHER" "$@" 2>"$stderr_file"
  else
    env PATH="$TMP_BIN:$PATH" \
      RPLACA_ENABLE_TEST_TOGGLES=1 \
      RPLACA_SSL_LIB="$TMP_SSL_LIB" \
      "$toggle_var=1" \
      "$LAUNCHER" "$@" 2>"$stderr_file"
  fi
  actual_code=$?
  set -e

  if [ "$actual_code" -ne "$expected_code" ]; then
    echo "FAIL $name: expected exit $expected_code got $actual_code" >&2
    cat "$stderr_file" >&2
    exit 1
  fi

  if [ "$expect_prefix" = "yes" ] && ! grep -q '^\[rplaca-env\]' "$stderr_file"; then
    echo "FAIL $name: missing stderr prefix" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
}

run_override_case() {
  name="$1"
  shift

  stderr_file="$TMP_DIR/$name.stderr"
  set +e
  env PATH="$TMP_BIN:$PATH" \
    RPLACA_ENABLE_TEST_TOGGLES=1 \
    RPLACA_SSL_LIB="$TMP_SSL_LIB" \
    "$@" \
    "$LAUNCHER" --mode run --preflight-only 2>"$stderr_file"
  actual_code=$?
  set -e

  if [ "$actual_code" -ne 117 ]; then
    echo "FAIL $name: expected exit 117 got $actual_code" >&2
    cat "$stderr_file" >&2
    exit 1
  fi

  if ! grep -q '^\[rplaca-env\]' "$stderr_file"; then
    echo "FAIL $name: missing stderr prefix" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
}

run_env_case() {
  name="$1"
  expected_code="$2"
  shift 2

  stderr_file="$TMP_DIR/$name.stderr"
  set +e
  env PATH="$TMP_BIN:$PATH" \
    RPLACA_ENABLE_TEST_TOGGLES=1 \
    RPLACA_SSL_LIB="$TMP_SSL_LIB" \
    OPENAI_API_KEY= \
    ZAI_CODING_MAX_API_KEY= \
    OPENROUTER_API_KEY= \
    "$@" 2>"$stderr_file"
  actual_code=$?
  set -e

  if [ "$actual_code" -ne "$expected_code" ]; then
    echo "FAIL $name: expected exit $expected_code got $actual_code" >&2
    cat "$stderr_file" >&2
    exit 1
  fi

  if [ "$expected_code" -ne 0 ] && ! grep -q '^\[rplaca-env\]' "$stderr_file"; then
    echo "FAIL $name: missing stderr prefix" >&2
    cat "$stderr_file" >&2
    exit 1
  fi
}

run_case invalid-mode 118 yes - --mode nope -- true
run_case missing-mode 118 yes - -- true
run_case missing-payload 119 yes - --mode run --
run_case missing-guix-toggle 110 yes RPLACA_TEST_MISSING_GUIX --mode run --preflight-only
run_case repo-root-failure-toggle 120 yes RPLACA_TEST_FAIL_REPO_ROOT --mode run --preflight-only
run_case missing-mount-toggle 111 yes RPLACA_TEST_MISSING_MOUNT --mode run --preflight-only
run_case hidden-sbcl-run 113 yes RPLACA_TEST_HIDE_SBCL --mode run --preflight-only
run_case hidden-sbcl-e2e 113 yes RPLACA_TEST_HIDE_SBCL --mode e2e --preflight-only
run_case hidden-python3-e2e 114 yes RPLACA_TEST_HIDE_PYTHON3 --mode e2e --preflight-only
run_case hidden-xvfb-e2e 114 yes RPLACA_TEST_HIDE_XVFB --mode e2e --preflight-only
run_case hidden-setsid-e2e 114 yes RPLACA_TEST_HIDE_SETSID --mode e2e --preflight-only
run_case hidden-xdotool-e2e 114 yes RPLACA_TEST_HIDE_XDOTOOL --mode e2e --preflight-only
run_case hidden-screenshot-e2e 114 yes RPLACA_TEST_HIDE_SCREENSHOT --mode e2e --preflight-only
run_case quicklisp-bootstrap-toggle 112 yes RPLACA_TEST_QUICKLISP_BOOTSTRAP_FAIL --mode run --preflight-only
run_case missing-flock-toggle 123 yes RPLACA_TEST_MISSING_FLOCK --mode run --preflight-only
run_case missing-provider-credential-toggle 116 yes RPLACA_TEST_MISSING_PROVIDER_CREDENTIAL --mode run --preflight-only
run_case invalid-override-path-toggle 117 yes RPLACA_TEST_INVALID_OVERRIDE_PATH --mode run --preflight-only
run_case invalid-e2e-args-toggle 122 yes RPLACA_TEST_INVALID_E2E_ARGS --mode e2e --preflight-only
run_env_case preflight-precedence-credential-over-bootstrap 116 RPLACA_TEST_MISSING_PROVIDER_CREDENTIAL=1 RPLACA_TEST_QUICKLISP_BOOTSTRAP_FAIL=1 "$LAUNCHER" --mode run --preflight-only
run_env_case preflight-precedence-override-over-bootstrap 117 RPLACA_TEST_INVALID_OVERRIDE_PATH=1 RPLACA_TEST_QUICKLISP_BOOTSTRAP_FAIL=1 "$LAUNCHER" --mode run --preflight-only
run_env_case preflight-precedence-openssl-over-bootstrap 121 RPLACA_TEST_MISSING_OPENSSL_PATH=1 RPLACA_TEST_QUICKLISP_BOOTSTRAP_FAIL=1 "$LAUNCHER" --mode run --preflight-only
run_env_case e2e-credential-generic-command-optional 0 "$LAUNCHER" --mode e2e --preflight-only -- python3 -c 'print("ok")'
run_env_case host-tools-not-required-for-run-preflight 0 RPLACA_FAIL_HOST_TOOL_USE=1 "$LAUNCHER" --mode run --preflight-only
run_env_case host-tools-not-required-for-e2e-preflight 0 RPLACA_FAIL_HOST_TOOL_USE=1 OPENAI_API_KEY=dummy "$LAUNCHER" --mode e2e --preflight-only -- python3 --version
run_case payload-exit-code-passthrough 37 no - --mode run -- sh -c 'exit 37'

run_override_case ssl-override-missing-path RPLACA_SSL_LIB="$TMP_DIR/ssl/missing"
run_override_case ssl-override-non-directory RPLACA_SSL_LIB="$TMP_SSL_LIB/libssl.so.3"
run_override_case ssl-override-traversal RPLACA_SSL_LIB="$TMP_DIR/ssl/../ssl/lib"
run_override_case ssl-override-disallowed-prefix RPLACA_SSL_LIB="/usr/lib"

run_override_case font-override-missing-path RPLACA_FONT_PATH="$TMP_FONT_DIR/missing.ttf"
run_override_case font-override-unreadable RPLACA_FONT_PATH="$TMP_FONT_UNREADABLE"
run_override_case font-override-non-file RPLACA_FONT_PATH="$TMP_FONT_DIR"
run_override_case font-override-traversal RPLACA_FONT_PATH="$TMP_FONT_DIR/../fonts/ok.ttf"
run_override_case font-override-disallowed-prefix RPLACA_FONT_PATH="/etc/passwd"

write_bootstrap_env "$EXPECTED_SHA"
printf '%s\n' \
  'QUICKLISP_BOOTSTRAP_URL=https://beta.quicklisp.org/quicklisp.lisp' \
  'QUICKLISP_BOOTSTRAP_SHA256=REPLACE_ME' \
  'QUICKLISP_BOOTSTRAP_TIMEOUT_SECS=20' \
  'QUICKLISP_BOOTSTRAP_RETRIES=2' > "$BOOTSTRAP_ENV"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/pin-sentinel.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 115 ]; then
  echo "FAIL quicklisp-pin-values-sentinel: expected exit 115 got $actual_code" >&2
  cat "$TMP_DIR/pin-sentinel.stderr" >&2
  exit 1
fi

write_bootstrap_env "$EXPECTED_SHA"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_EXPECT_CURL_TIMEOUT=27 \
  RPLACA_EXPECT_CURL_RETRIES=4 \
  QUICKLISP_BOOTSTRAP_TIMEOUT_SECS=27 \
  QUICKLISP_BOOTSTRAP_RETRIES=4 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-timeout.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL bootstrap-timeout-retries: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-timeout.stderr" >&2
  exit 1
fi

set +e
rm -rf "$QUICKLISP_HOME"
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  QUICKLISP_BOOTSTRAP_URL=https://invalid.example/quicklisp.lisp \
  QUICKLISP_BOOTSTRAP_SHA256=0000000000000000000000000000000000000000000000000000000000000000 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-pin-override.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL quicklisp-pin-override-ignored: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-pin-override.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_SBCL_FAIL_PROBE=1 \
  RPLACA_TEST_CURL_FAIL=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-fail.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 112 ]; then
  echo "FAIL bootstrap-failure: expected exit 112 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-fail.stderr" >&2
  exit 1
fi

mkdir -p "$QUICKLISP_HOME"
printf '(load "existing")\n' > "$QUICKLISP_HOME/setup.lisp"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_SBCL_FAIL_PROBE=1 \
  RPLACA_TEST_CURL_FAIL=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/bootstrap-preserve.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 112 ]; then
  echo "FAIL bootstrap-failure-preserves-existing-cache: expected exit 112 got $actual_code" >&2
  cat "$TMP_DIR/bootstrap-preserve.stderr" >&2
  exit 1
fi
if [ ! -f "$QUICKLISP_HOME/setup.lisp" ]; then
  echo "FAIL bootstrap-failure-preserves-existing-cache: expected existing setup to remain" >&2
  cat "$TMP_DIR/bootstrap-preserve.stderr" >&2
  exit 1
fi
if ! grep -q 'existing' "$QUICKLISP_HOME/setup.lisp"; then
  echo "FAIL bootstrap-failure-preserves-existing-cache: expected existing setup content" >&2
  cat "$TMP_DIR/bootstrap-preserve.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_TEST_MISSING_OPENSSL_PATH=1 \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/openssl-run.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 121 ]; then
  echo "FAIL missing-openssl-runtime-path: expected exit 121 got $actual_code" >&2
  cat "$TMP_DIR/openssl-run.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_DIR/does-not-exist" \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/openssl-override-invalid.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 117 ]; then
  echo "FAIL openssl-override-invalid: expected exit 117 got $actual_code" >&2
  cat "$TMP_DIR/openssl-override-invalid.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB= \
  RPLACA_TEST_LDCONFIG_EMPTY=1 \
  OPENAI_API_KEY=dummy \
  "$LAUNCHER" --mode e2e --preflight-only -- python3 --version 2>"$TMP_DIR/openssl-e2e.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL e2e-openssl-unresolved-allowed: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/openssl-e2e.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB= \
  RPLACA_TEST_LDCONFIG_MODE=crypto \
  "$LAUNCHER" --mode run --preflight-only 2>"$TMP_DIR/openssl-run-libcrypto.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL openssl-runtime-path-libcrypto-soname: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/openssl-run-libcrypto.stderr" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_APPEARANCE_THEME=dark \
  RPLACA_CRASH_REPORT_DIR=/tmp/rplaca-crash-test \
  RPLACA_CRASH_REPAIR_REQUEST_DIR=/workspace/.cache/crash-repair/requests \
  RPLACA_CRASH_REPAIR_HISTORY=/workspace/.cache/crash-repair/repair-history.md \
  RPLACA_E2E_PROVIDER=echo \
  RPLACA_E2E_EVENTS=/tmp/rplaca-e2e-events \
  RPLACA_GUI_E2E_INITIAL_INPUT_FOCUS=1 \
  XDG_STATE_HOME=/tmp/rplaca-state-test \
  CL_SOURCE_REGISTRY=/hostile/host/registry/ \
  "$LAUNCHER" --mode run -- sh -c 'case "$HOME" in /workspace/.cache/launcher-test-*/home) ;; *) exit 1 ;; esac; test "$RPLACA_QUICKLISP_SETUP" = "$HOME/quicklisp/setup.lisp" && test "$XDG_CACHE_HOME" = "${HOME%/home}" && test "$RPLACA_PROMPT_PROJECT_ROOT" = "/workspace" && test "$CL_SOURCE_REGISTRY" = "$GUIX_ENVIRONMENT/share/common-lisp/systems/" && test "$RPLACA_APPEARANCE_THEME" = dark && test "$RPLACA_CRASH_REPORT_DIR" = /tmp/rplaca-crash-test && test "$RPLACA_CRASH_REPAIR_REQUEST_DIR" = /workspace/.cache/crash-repair/requests && test "$RPLACA_CRASH_REPAIR_HISTORY" = /workspace/.cache/crash-repair/repair-history.md && test "$RPLACA_E2E_PROVIDER" = echo && test "$RPLACA_E2E_EVENTS" = /tmp/rplaca-e2e-events && test "$RPLACA_GUI_E2E_INITIAL_INPUT_FOCUS" = 1 && test "$XDG_STATE_HOME" = /tmp/rplaca-state-test && test -f "${RPLACA_QUICKLISP_SETUP#/workspace/}"' 2>"$TMP_DIR/runtime-env.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL runtime-env-and-setup: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/runtime-env.stderr" >&2
  exit 1
fi

guix_args_log="$TMP_DIR/guix-args.log"
canonical_home="$TMP_DIR/canonical-home"
canonical_guest_home="/workspace/$TEST_CACHE_RELATIVE/home"
mkdir -p "$canonical_home"
rm -f "$guix_args_log"
set +e
env PATH="$TMP_BIN:$PATH" \
  HOME="$canonical_home" \
  XDG_STATE_HOME= \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_GUIX_ARGS_LOG="$guix_args_log" \
  "$LAUNCHER" --mode run -- \
  sh -c 'printf "user\n" > "$1/visible"; printf "project\n" > "$2/visible"; printf "state\n" > "$3/visible"' \
  sh \
  "$canonical_guest_home/.rplaca.d" \
  "$canonical_guest_home/.rplaca.projects.d" \
  "$canonical_guest_home/.local/state/rplaca" \
  2>"$TMP_DIR/canonical-persistence.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ] ||
   [ ! -f "$canonical_home/.rplaca.d/visible" ] ||
   [ ! -f "$canonical_home/.rplaca.projects.d/visible" ] ||
   [ ! -f "$canonical_home/.local/state/rplaca/visible" ]; then
  echo "FAIL canonical-host-persistence: canonical storage was not host-visible" >&2
  cat "$TMP_DIR/canonical-persistence.stderr" >&2
  cat "$guix_args_log" >&2
  exit 1
fi
for mapping in \
  "$canonical_home/.rplaca.d=$canonical_guest_home/.rplaca.d" \
  "$canonical_home/.rplaca.projects.d=$canonical_guest_home/.rplaca.projects.d" \
  "$canonical_home/.local/state/rplaca=$canonical_guest_home/.local/state/rplaca"; do
  if ! grep -F -- "--share=$mapping" "$guix_args_log" >/dev/null; then
    echo "FAIL canonical-host-persistence: missing mapping $mapping" >&2
    cat "$guix_args_log" >&2
    exit 1
  fi
done

override_home="$TMP_DIR/override-home"
override_crashes="$override_home/crashes"
override_state="$override_home/state"
mkdir -p "$override_home"
rm -f "$guix_args_log"
set +e
env PATH="$TMP_BIN:$PATH" \
  HOME="$override_home" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_GUIX_ARGS_LOG="$guix_args_log" \
  RPLACA_CRASH_REPORT_DIR="$override_crashes" \
  XDG_STATE_HOME="$override_state" \
  "$LAUNCHER" --mode run -- \
  sh -c 'printf "crash\n" > "$1/report"; printf "state\n" > "$2/visible"' \
  sh "$override_crashes" "$override_state" \
  2>"$TMP_DIR/override-persistence.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ] ||
   [ ! -f "$override_crashes/report" ] ||
   [ ! -f "$override_state/visible" ]; then
  echo "FAIL override-host-persistence: explicit storage was not host-visible" >&2
  cat "$TMP_DIR/override-persistence.stderr" >&2
  cat "$guix_args_log" >&2
  exit 1
fi
for directory in "$override_crashes" "$override_state"; do
  if ! grep -F -- "--share=$directory=$directory" "$guix_args_log" >/dev/null; then
    echo "FAIL override-host-persistence: missing same-path mapping $directory" >&2
    cat "$guix_args_log" >&2
    exit 1
  fi
done

set +e
env PATH="$TMP_BIN:$PATH" \
  HOME="$override_home" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_CRASH_REPORT_DIR=relative-crashes \
  XDG_STATE_HOME= \
  "$LAUNCHER" --mode run -- true \
  2>"$TMP_DIR/relative-crash-storage.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 124 ]; then
  echo "FAIL relative-crash-storage: expected exit 124 got $actual_code" >&2
  cat "$TMP_DIR/relative-crash-storage.stderr" >&2
  exit 1
fi

rm -f "$guix_args_log"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_GUIX_ARGS_LOG="$guix_args_log" \
  "$LAUNCHER" --mode e2e -- true 2>"$TMP_DIR/preserved-gui-env.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL preserved-gui-env: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/preserved-gui-env.stderr" >&2
  exit 1
fi
for variable in \
  RPLACA_CRASH_REPAIR_REQUEST_DIR \
  RPLACA_CRASH_REPAIR_HISTORY \
  RPLACA_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS \
  RPLACA_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS \
  RPLACA_GUI_E2E_STABILITY_MENU_ITERATIONS \
  RPLACA_GUI_E2E_STABILITY_EXPOSE_ITERATIONS \
  RPLACA_GUI_E2E_COLD_CACHE; do
  if ! grep -q "$variable" "$guix_args_log"; then
    echo "FAIL preserved-gui-env: missing $variable from guix arguments" >&2
    cat "$guix_args_log" >&2
    exit 1
  fi
done

rm -f "$guix_args_log"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_GUIX_ARGS_LOG="$guix_args_log" \
  RPLACA_CODEX_BUNDLE="$TMP_CODEX_BUNDLE" \
  "$LAUNCHER" --mode run -- /run/rplaca-codex/bin/codex \
  2>"$TMP_DIR/codex-bundle.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ] ||
   ! grep -F -- "--expose=$TMP_CODEX_BUNDLE=/run/rplaca-codex" "$guix_args_log" >/dev/null; then
  echo "FAIL codex-bundle-expose: expected read-only Codex bundle mapping" >&2
  cat "$TMP_DIR/codex-bundle.stderr" >&2
  cat "$guix_args_log" >&2
  exit 1
fi

set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_CODEX_BUNDLE="$TMP_DIR/missing-codex-bundle" \
  "$LAUNCHER" --mode run -- true \
  2>"$TMP_DIR/codex-bundle-missing.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 125 ]; then
  echo "FAIL codex-bundle-missing: expected exit 125 got $actual_code" >&2
  cat "$TMP_DIR/codex-bundle-missing.stderr" >&2
  exit 1
fi

legacy_home="$TMP_DIR/legacy-home"
mkdir -p \
  "$legacy_home/.config/clawmacs" \
  "$legacy_home/.clawmacs.d" \
  "$legacy_home/.clawmacs.projects.d"
rm -f "$guix_args_log"
set +e
env PATH="$TMP_BIN:$PATH" \
  HOME="$legacy_home" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_GUIX_ARGS_LOG="$guix_args_log" \
  "$LAUNCHER" --mode run -- true 2>"$TMP_DIR/legacy-exposes.stderr"
actual_code=$?
set -e
if [ "$actual_code" -ne 0 ]; then
  echo "FAIL legacy-read-only-exposes: expected exit 0 got $actual_code" >&2
  cat "$TMP_DIR/legacy-exposes.stderr" >&2
  exit 1
fi
for mapping in \
  "$legacy_home/.config/clawmacs=/workspace/.cache/launcher-test-" \
  "$legacy_home/.clawmacs.d=/workspace/.cache/launcher-test-" \
  "$legacy_home/.clawmacs.projects.d=/workspace/.cache/launcher-test-"; do
  if ! grep -F -- "--expose=$mapping" "$guix_args_log" >/dev/null; then
    echo "FAIL legacy-read-only-exposes: missing mapping prefix $mapping" >&2
    cat "$guix_args_log" >&2
    exit 1
  fi
done

warm_log="$TMP_DIR/warm.log"
warm_guard="$TMP_DIR/warm.guard"
warm_one_stderr="$TMP_DIR/warm-one.stderr"
warm_two_stderr="$TMP_DIR/warm-two.stderr"
rm -f "$warm_log"
rm -rf "$warm_guard"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_WARM_LOG="$warm_log" \
  RPLACA_TEST_WARM_GUARD="$warm_guard" \
  "$LAUNCHER" --mode e2e -- true 2>"$warm_one_stderr" &
warm_one_pid=$!
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_WARM_LOG="$warm_log" \
  RPLACA_TEST_WARM_GUARD="$warm_guard" \
  "$LAUNCHER" --mode e2e -- true 2>"$warm_two_stderr" &
warm_two_pid=$!
wait "$warm_one_pid"
warm_one_status=$?
wait "$warm_two_pid"
warm_two_status=$?
set -e
if [ "$warm_one_status" -ne 0 ] || [ "$warm_two_status" -ne 0 ]; then
  echo "FAIL concurrent-quicklisp-warmup: statuses $warm_one_status/$warm_two_status" >&2
  cat "$warm_one_stderr" "$warm_two_stderr" >&2
  [ -f "$TEST_CACHE_ROOT/quicklisp-warmup.log" ] && \
    cat "$TEST_CACHE_ROOT/quicklisp-warmup.log" >&2
  exit 1
fi
if [ ! -f "$warm_log" ] || [ "$(wc -l < "$warm_log" | tr -d ' ')" -ne 2 ]; then
  echo "FAIL concurrent-quicklisp-warmup: expected two serialized warmups" >&2
  [ -f "$warm_log" ] && cat "$warm_log" >&2
  exit 1
fi

warm_failure_sentinel="$TMP_DIR/warm-failure-payload-ran"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_SBCL_FAIL_WARM=1 \
  "$LAUNCHER" --mode run -- sh -c 'touch "$1"' sh "$warm_failure_sentinel" \
  2>"$TMP_DIR/warm-failure.stderr"
warm_failure_status=$?
set -e
if [ "$warm_failure_status" -ne 123 ] || [ -e "$warm_failure_sentinel" ]; then
  echo "FAIL quicklisp-warmup-failure-gate: status $warm_failure_status" >&2
  cat "$TMP_DIR/warm-failure.stderr" >&2
  exit 1
fi

lock_ready="$TMP_DIR/lock-owner.ready"
setsid sh -c '
  exec 9>"$1"
  flock -x 9
  printf "ready\n" > "$2"
  exec sleep 300
' sh "$TEST_CACHE_ROOT/quicklisp.lock" "$lock_ready" &
lock_owner_pid=$!
lock_ready_attempt=0
while [ ! -s "$lock_ready" ] && [ "$lock_ready_attempt" -lt 100 ]; do
  lock_ready_attempt=$((lock_ready_attempt + 1))
  sleep 0.01
done
if [ ! -s "$lock_ready" ]; then
  echo "FAIL quicklisp-lock-owner-death: owner did not acquire lock" >&2
  kill -KILL -- "-$lock_owner_pid" >/dev/null 2>&1 || true
  wait "$lock_owner_pid" >/dev/null 2>&1 || true
  exit 1
fi
kill -KILL -- "-$lock_owner_pid" >/dev/null 2>&1 || true
wait "$lock_owner_pid" >/dev/null 2>&1 || true
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  "$LAUNCHER" --mode run --preflight-only \
  2>"$TMP_DIR/lock-owner-death.stderr"
lock_recovery_status=$?
set -e
if [ "$lock_recovery_status" -ne 0 ]; then
  echo "FAIL quicklisp-lock-owner-death: status $lock_recovery_status" >&2
  cat "$TMP_DIR/lock-owner-death.stderr" >&2
  exit 1
fi

cold_bootstrap_log="$TMP_DIR/cold-bootstrap.log"
cold_one_stderr="$TMP_DIR/cold-one.stderr"
cold_two_stderr="$TMP_DIR/cold-two.stderr"
rm -rf "$QUICKLISP_HOME"
rm -f "$cold_bootstrap_log"
set +e
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_BOOTSTRAP_LOG="$cold_bootstrap_log" \
  "$LAUNCHER" --mode run --preflight-only 2>"$cold_one_stderr" &
cold_one_pid=$!
env PATH="$TMP_BIN:$PATH" \
  RPLACA_ENABLE_TEST_TOGGLES=1 \
  RPLACA_SSL_LIB="$TMP_SSL_LIB" \
  RPLACA_TEST_BOOTSTRAP_LOG="$cold_bootstrap_log" \
  "$LAUNCHER" --mode run --preflight-only 2>"$cold_two_stderr" &
cold_two_pid=$!
wait "$cold_one_pid"
cold_one_status=$?
wait "$cold_two_pid"
cold_two_status=$?
set -e
if [ "$cold_one_status" -ne 0 ] || [ "$cold_two_status" -ne 0 ]; then
  echo "FAIL concurrent-cold-quicklisp-bootstrap: statuses $cold_one_status/$cold_two_status" >&2
  cat "$cold_one_stderr" "$cold_two_stderr" >&2
  exit 1
fi
if [ ! -f "$cold_bootstrap_log" ] || \
   [ "$(wc -l < "$cold_bootstrap_log" | tr -d ' ')" -ne 1 ]; then
  echo "FAIL concurrent-cold-quicklisp-bootstrap: expected one bootstrap" >&2
  [ -f "$cold_bootstrap_log" ] && cat "$cold_bootstrap_log" >&2
  exit 1
fi
if find "$TEST_CACHE_ROOT/home" -maxdepth 1 \
     \( -name 'quicklisp.backup.*' -o -name 'quicklisp.bootstrap.*' \) \
     -print -quit | grep -q .; then
  echo "FAIL concurrent-cold-quicklisp-bootstrap: temporary cache tree remains" >&2
  exit 1
fi

if [ "$(sha256sum "$PRODUCTION_BOOTSTRAP_ENV" | cut -d' ' -f1)" != "$PRODUCTION_BOOTSTRAP_DIGEST" ]; then
  echo "FAIL test-isolation: production bootstrap environment changed" >&2
  exit 1
fi
case "$PRODUCTION_SETUP_STATE" in
  present)
    if [ ! -f "$PRODUCTION_QUICKLISP_SETUP" ] || \
       [ "$(sha256sum "$PRODUCTION_QUICKLISP_SETUP" | cut -d' ' -f1)" != "$PRODUCTION_SETUP_DIGEST" ] || \
       [ "$(stat -c '%d:%i' "$PRODUCTION_QUICKLISP_SETUP")" != "$PRODUCTION_SETUP_INODE" ]; then
      echo "FAIL test-isolation: production Quicklisp setup changed" >&2
      exit 1
    fi
    ;;
  absent)
    if [ -e "$PRODUCTION_QUICKLISP_SETUP" ]; then
      echo "FAIL test-isolation: production Quicklisp setup was created" >&2
      exit 1
    fi
    ;;
esac

"$SCRIPT_DIR/test-legacy-name-allowlist.sh"

echo "ok"
