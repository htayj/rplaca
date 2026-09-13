#!/bin/sh
set -eu

LAUNCHER_PREFIX='[rplaca-env]'
SCRIPT_DIR=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
MODE=''
PREFLIGHT_ONLY=0
PAYLOAD_SHIFT_COUNT=0
QUICKLISP_ENV_LOADED=0
WORKSPACE_HOME=/workspace/.cache/home
WORKSPACE_QUICKLISP_SETUP=/workspace/.cache/home/quicklisp/setup.lisp
WORKSPACE_XDG_CACHE=/workspace/.cache
HOST_HOME=''
HOST_QUICKLISP_SETUP=''
HOST_CACHE_ROOT=''
HOST_CONFIG_DIR=''
RESOLVED_SSL_LIB_PATH=''
RUNTIME_LD_LIBRARY_PATH=''
CONTAINER_LAUNCH_DIR=/tmp
GUIX_MANIFEST_PATH=''
HOST_USER_HOME=${HOME:-}
QUICKLISP_CACHE_LOCK_TIMEOUT_SECS=600
PRESERVED_ENV_PATTERN='TERM|DISPLAY|XAUTHORITY|OPENAI_API_KEY|ZAI_CODING_MAX_API_KEY|OPENROUTER_API_KEY|RPLACA_SSL_LIB|RPLACA_FONT_PATH|RPLACA_DEBUG_LOG|RPLACA_PROMPT_PROJECT_ROOT|RPLACA_APPEARANCE_THEME|RPLACA_CRASH_REPORT_DIR|RPLACA_CRASH_REPAIR_REQUEST_DIR|RPLACA_CRASH_REPAIR_HISTORY|RPLACA_E2E_PROVIDER|RPLACA_E2E_EVENTS|RPLACA_GUI_E2E_INITIAL_INPUT_FOCUS|RPLACA_GUI_E2E_FRAME_READY_TIMEOUT_SECONDS|RPLACA_GUI_E2E_APP_EXIT_TIMEOUT_SECONDS|RPLACA_GUI_E2E_STABILITY_MENU_ITERATIONS|RPLACA_GUI_E2E_STABILITY_EXPOSE_ITERATIONS|RPLACA_GUI_E2E_COLD_CACHE|HOME|RPLACA_QUICKLISP_SETUP|XDG_CACHE_HOME|XDG_STATE_HOME|RUNTIME_LD_LIBRARY_PATH'

stderr() {
  printf '%s %s\n' "$LAUNCHER_PREFIX" "$*" >&2
}

fail() {
  code="$1"
  shift
  stderr "$*"
  exit "$code"
}

is_test_toggle_enabled() {
  key="$1"
  if [ "${RPLACA_ENABLE_TEST_TOGGLES:-0}" != "1" ]; then
    return 1
  fi
  eval "value=\${$key:-0}"
  [ "$value" = "1" ]
}

is_sensitive_key() {
  key="$1"
  case "$key" in
    OPENAI_API_KEY|*_KEY|*_TOKEN|*_SECRET)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

redact_value() {
  key="$1"
  value="$2"
  if is_sensitive_key "$key"; then
    printf '[REDACTED]'
  else
    printf '%s' "$value"
  fi
}

diagnostic_env() {
  key="$1"
  eval "raw=\${$key-}"
  safe_value=$(redact_value "$key" "$raw")
  stderr "diag $key=$safe_value"
}

validate_launcher_cli() {
  MODE=''
  PREFLIGHT_ONLY=0
  PAYLOAD_SHIFT_COUNT=0

  if [ "$#" -eq 0 ]; then
    fail 118 "invalid launcher mode: missing --mode <run|e2e>"
  fi

  while [ "$#" -gt 0 ]; do
    PAYLOAD_SHIFT_COUNT=$((PAYLOAD_SHIFT_COUNT + 1))
    case "$1" in
      --preflight-only)
        PREFLIGHT_ONLY=1
        shift
        ;;
      --mode)
        if [ "$#" -lt 2 ]; then
          fail 118 "invalid launcher mode: missing --mode value"
        fi
        shift
        MODE="$1"
        PAYLOAD_SHIFT_COUNT=$((PAYLOAD_SHIFT_COUNT + 1))
        shift
        ;;
      --)
        shift
        break
        ;;
      *)
        fail 118 "invalid launcher mode: unexpected argument '$1'"
        ;;
    esac
  done

  case "$MODE" in
    run|e2e)
      ;;
    '')
      fail 118 "invalid launcher mode: missing --mode <run|e2e>"
      ;;
    *)
      fail 118 "invalid launcher mode: '$MODE'"
      ;;
  esac

  if [ "$#" -eq 0 ] && [ "$PREFLIGHT_ONLY" -ne 1 ]; then
    fail 119 "missing launcher command payload"
  fi

}

resolve_repo_root() {
  if is_test_toggle_enabled RPLACA_TEST_FAIL_REPO_ROOT; then
    fail 120 "failed to resolve repository root"
  fi

  if ! REPO_ROOT=$(git -C "$SCRIPT_DIR" rev-parse --show-toplevel 2>/dev/null); then
    fail 120 "failed to resolve repository root"
  fi

  if [ -z "$REPO_ROOT" ] || [ ! -d "$REPO_ROOT" ]; then
    fail 120 "failed to resolve repository root"
  fi

  GUIX_MANIFEST_PATH="$REPO_ROOT/guix.scm"
}

validate_guix_available() {
  if is_test_toggle_enabled RPLACA_TEST_MISSING_GUIX; then
    fail 110 "missing guix"
  fi

  if ! command -v guix >/dev/null 2>&1; then
    fail 110 "missing guix"
  fi
}

validate_project_mount() {
  if is_test_toggle_enabled RPLACA_TEST_MISSING_MOUNT; then
    fail 111 "missing project mount"
  fi

  if [ ! -d "$REPO_ROOT" ]; then
    fail 111 "missing project mount"
  fi
}

load_quicklisp_bootstrap_env() {
  if [ "$QUICKLISP_ENV_LOADED" -eq 1 ]; then
    return 0
  fi

  override_timeout=${QUICKLISP_BOOTSTRAP_TIMEOUT_SECS-__UNSET__}
  override_retries=${QUICKLISP_BOOTSTRAP_RETRIES-__UNSET__}

  QUICKLISP_BOOTSTRAP_URL='https://beta.quicklisp.org/quicklisp.lisp'
  QUICKLISP_BOOTSTRAP_SHA256='REPLACE_ME'
  QUICKLISP_BOOTSTRAP_TIMEOUT_SECS='20'
  QUICKLISP_BOOTSTRAP_RETRIES='2'

  env_path="$SCRIPT_DIR/quicklisp-bootstrap.env"
  if is_test_toggle_enabled RPLACA_TEST_QUICKLISP_ENV_PATH_SET; then
    env_path=${RPLACA_TEST_QUICKLISP_ENV_PATH:-}
    case "$env_path" in
      "$REPO_ROOT"/.cache/launcher-test-*/quicklisp-bootstrap.env)
        ;;
      *)
        fail 115 "quicklisp bootstrap pin values missing"
        ;;
    esac
  fi
  if [ ! -f "$env_path" ]; then
    fail 115 "quicklisp bootstrap pin values missing"
  fi

  # shellcheck disable=SC1090
  . "$env_path"

  if [ "$override_timeout" != "__UNSET__" ]; then
    QUICKLISP_BOOTSTRAP_TIMEOUT_SECS="$override_timeout"
  fi
  if [ "$override_retries" != "__UNSET__" ]; then
    QUICKLISP_BOOTSTRAP_RETRIES="$override_retries"
  fi

  QUICKLISP_ENV_LOADED=1
}

validate_quicklisp_pin_values() {
  if is_test_toggle_enabled RPLACA_TEST_PIN_VALUES_MISSING; then
    fail 115 "quicklisp bootstrap pin values missing"
  fi

  load_quicklisp_bootstrap_env

  for value in "$QUICKLISP_BOOTSTRAP_URL" "$QUICKLISP_BOOTSTRAP_SHA256" "$QUICKLISP_BOOTSTRAP_TIMEOUT_SECS" "$QUICKLISP_BOOTSTRAP_RETRIES"; do
    case "$value" in
      ''|REPLACE_ME|*'<'*|*'>'*)
        fail 115 "quicklisp bootstrap pin values missing"
        ;;
    esac
  done
}

set_quicklisp_runtime_env() {
  cache_relative='.cache'
  if is_test_toggle_enabled RPLACA_TEST_CACHE_ROOT_SET; then
    cache_relative=${RPLACA_TEST_CACHE_RELATIVE:-}
    case "$cache_relative" in
      .cache/launcher-test-*)
        cache_suffix=${cache_relative#.cache/launcher-test-}
        case "$cache_suffix" in
          ''|*[!A-Za-z0-9._-]*)
            fail 123 "quicklisp cache preparation failed: invalid test cache root"
            ;;
        esac
        ;;
      *)
        fail 123 "quicklisp cache preparation failed: invalid test cache root"
        ;;
    esac
  fi

  HOST_CACHE_ROOT="$REPO_ROOT/$cache_relative"
  HOST_HOME="$HOST_CACHE_ROOT/home"
  HOST_QUICKLISP_SETUP="$HOST_HOME/quicklisp/setup.lisp"
  WORKSPACE_HOME="/workspace/$cache_relative/home"
  WORKSPACE_QUICKLISP_SETUP="$WORKSPACE_HOME/quicklisp/setup.lisp"
  WORKSPACE_XDG_CACHE="/workspace/$cache_relative"
  HOST_CONFIG_DIR=''

  mkdir -p "$HOST_HOME"
  mkdir -p "$HOST_HOME/.config"

  if [ -n "$HOST_USER_HOME" ]; then
    HOST_CONFIG_DIR="$HOST_USER_HOME/.config/rplaca"
  fi

  export HOME="$WORKSPACE_HOME"
  export RPLACA_QUICKLISP_SETUP="$WORKSPACE_QUICKLISP_SETUP"
  export XDG_CACHE_HOME="$WORKSPACE_XDG_CACHE"
  export RPLACA_PROMPT_PROJECT_ROOT="${RPLACA_PROMPT_PROJECT_ROOT:-/workspace}"
}

run_in_container() {
  container_script="$1"
  shift

  cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --no-cwd --network --share="$REPO_ROOT=/workspace" -- bash -lc "$container_script" bash "$@"
}

probe_quicklisp_setup() {
  host_setup_path="$1"
  container_setup_path="$2"

  if [ ! -f "$host_setup_path" ]; then
    return 1
  fi

  run_in_container 'set -eu; setup_path="$1"; home_path="$2"; xdg_cache_path="$3"; HOME="$home_path" XDG_CACHE_HOME="$xdg_cache_path" sbcl --noinform --non-interactive --disable-debugger --load "$setup_path" --eval "(quit)" >/dev/null 2>&1' "$container_setup_path" "$WORKSPACE_HOME" "$WORKSPACE_XDG_CACHE"
}

bootstrap_quicklisp_once() {
  bootstrap_target_home="$1"
  download_path="$WORKSPACE_HOME/quicklisp.lisp"

  if is_test_toggle_enabled RPLACA_TEST_QUICKLISP_BOOTSTRAP_FAIL; then
    return 1
  fi

  run_in_container 'set -eu; bootstrap_url="$1"; expected_sha="$2"; timeout_secs="$3"; retry_count="$4"; download_path="$5"; bootstrap_home="$6"; runtime_home="$7"; xdg_cache_path="$8"; mkdir -p "$runtime_home"; mkdir -p "$xdg_cache_path"; curl --fail --location --silent --show-error --max-time "$timeout_secs" --retry "$retry_count" -o "$download_path" "$bootstrap_url"; actual_sha=$(sha256sum "$download_path" | cut -d" " -f1); [ "$actual_sha" = "$expected_sha" ]; mkdir -p "$bootstrap_home"; HOME="$runtime_home" XDG_CACHE_HOME="$xdg_cache_path" sbcl --noinform --non-interactive --disable-debugger --load "$download_path" --eval "(quicklisp-quickstart:install :path \"$bootstrap_home\")" --eval "(quit)" >/dev/null 2>&1' "$QUICKLISP_BOOTSTRAP_URL" "$QUICKLISP_BOOTSTRAP_SHA256" "$QUICKLISP_BOOTSTRAP_TIMEOUT_SECS" "$QUICKLISP_BOOTSTRAP_RETRIES" "$download_path" "$bootstrap_target_home" "$WORKSPACE_HOME" "$WORKSPACE_XDG_CACHE"
}

validate_quicklisp_bootstrap() {
  if is_test_toggle_enabled RPLACA_TEST_QUICKLISP_BOOTSTRAP_FAIL; then
    fail 112 "quicklisp bootstrap failed"
  fi

  set_quicklisp_runtime_env

  quicklisp_home="$HOST_HOME/quicklisp"
  quicklisp_backup_home="$HOST_HOME/quicklisp.backup.$$"
  quicklisp_bootstrap_home="$HOST_HOME/quicklisp.bootstrap.$$"
  container_quicklisp_setup="$WORKSPACE_QUICKLISP_SETUP"
  container_bootstrap_setup="$WORKSPACE_HOME/quicklisp.bootstrap.$$/setup.lisp"
  container_bootstrap_home="$WORKSPACE_HOME/quicklisp.bootstrap.$$"

  if probe_quicklisp_setup "$HOST_QUICKLISP_SETUP" "$container_quicklisp_setup"; then
    return 0
  fi

  rm -rf "$quicklisp_backup_home" "$quicklisp_bootstrap_home"

  if [ -e "$quicklisp_home" ]; then
    if ! mv "$quicklisp_home" "$quicklisp_backup_home"; then
      fail 112 "quicklisp bootstrap failed"
    fi
  fi

  if ! bootstrap_quicklisp_once "$container_bootstrap_home"; then
    if [ -e "$quicklisp_backup_home" ] && [ ! -e "$quicklisp_home" ]; then
      mv "$quicklisp_backup_home" "$quicklisp_home" >/dev/null 2>&1 || true
    fi
    rm -rf "$quicklisp_bootstrap_home"
    fail 112 "quicklisp bootstrap failed"
  fi

  if [ ! -f "$quicklisp_bootstrap_home/setup.lisp" ] || ! probe_quicklisp_setup "$quicklisp_bootstrap_home/setup.lisp" "$container_bootstrap_setup"; then
    if [ -e "$quicklisp_backup_home" ] && [ ! -e "$quicklisp_home" ]; then
      mv "$quicklisp_backup_home" "$quicklisp_home" >/dev/null 2>&1 || true
    fi
    rm -rf "$quicklisp_bootstrap_home"
    fail 112 "quicklisp bootstrap failed"
  fi

  rm -rf "$quicklisp_home"
  if ! mv "$quicklisp_bootstrap_home" "$quicklisp_home"; then
    if [ -e "$quicklisp_backup_home" ] && [ ! -e "$quicklisp_home" ]; then
      mv "$quicklisp_backup_home" "$quicklisp_home" >/dev/null 2>&1 || true
    fi
    rm -rf "$quicklisp_bootstrap_home"
    fail 112 "quicklisp bootstrap failed"
  fi

  rm -rf "$quicklisp_backup_home"

  if ! probe_quicklisp_setup "$HOST_QUICKLISP_SETUP" "$container_quicklisp_setup"; then
    fail 112 "quicklisp bootstrap failed"
  fi

  if [ ! -f "$HOST_QUICKLISP_SETUP" ]; then
    fail 112 "quicklisp bootstrap failed"
  fi
}

validate_quicklisp_cache_lock_tool() {
  if is_test_toggle_enabled RPLACA_TEST_MISSING_FLOCK; then
    fail 123 "quicklisp cache preparation failed: missing flock"
  fi

  if ! command -v flock >/dev/null 2>&1; then
    fail 123 "quicklisp cache preparation failed: missing flock"
  fi
}

warm_quicklisp_for_payload() {
  if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
    return 0
  fi

  host_warmup_log="$HOST_CACHE_ROOT/quicklisp-warmup.log"
  container_warmup_log="$WORKSPACE_XDG_CACHE/quicklisp-warmup.log"
  rm -f "$host_warmup_log"
  if ! run_in_container 'set -eu; cd /workspace; HOME="$1" XDG_CACHE_HOME="$2"; CL_SOURCE_REGISTRY="${GUIX_ENVIRONMENT:?missing Guix environment}/share/common-lisp/systems/"; export HOME XDG_CACHE_HOME CL_SOURCE_REGISTRY; if [ -n "$4" ]; then LD_LIBRARY_PATH="$4"; export LD_LIBRARY_PATH; else unset LD_LIBRARY_PATH; fi; sbcl --noinform --non-interactive --disable-debugger --load "$3" --load "/workspace/scripts/assert-mcclim-provenance.lisp" --eval "(push (truename \".\") asdf:*central-registry*)" --eval "(ql:quickload :rplaca)" --eval "(quit)" >"$5" 2>&1' "$WORKSPACE_HOME" "$WORKSPACE_XDG_CACHE" "$WORKSPACE_QUICKLISP_SETUP" "$RUNTIME_LD_LIBRARY_PATH" "$container_warmup_log"; then
    [ -f "$host_warmup_log" ] && tail -80 "$host_warmup_log" >&2
    fail 123 "quicklisp cache preparation failed: rplaca warmup"
  fi
}

prepare_quicklisp_cache() {
  validate_quicklisp_cache_lock_tool
  # These exported runtime paths must survive the lock-owning subshell for the
  # eventual payload launch.
  set_quicklisp_runtime_env
  mkdir -p "$HOST_CACHE_ROOT"

  # Quicklisp installs releases through shared temporary names and is not safe
  # for concurrent writers.  Hold one host-side advisory lock across cache
  # validation/bootstrap and the payload dependency warmup.  Later isolated
  # ASDF compilations can then read Quicklisp without installing releases.
  (
    if ! flock -x -w "$QUICKLISP_CACHE_LOCK_TIMEOUT_SECS" 9; then
      fail 123 "quicklisp cache preparation failed: lock timeout"
    fi
    validate_quicklisp_bootstrap
    warm_quicklisp_for_payload
  ) 9>"$HOST_CACHE_ROOT/quicklisp.lock"
}

e2e_invocation_requires_credential() {
  shift_count="$PAYLOAD_SHIFT_COUNT"
  command_name=''
  command_target=''
  only_target=''

  while [ "$shift_count" -gt 0 ]; do
    if [ "$#" -eq 0 ]; then
      fail 122 "invalid e2e arguments"
    fi
    shift
    shift_count=$((shift_count - 1))
  done

  if [ "$#" -gt 0 ]; then
    command_name="$1"
  fi
  if [ "$#" -gt 1 ]; then
    command_target="$2"
  fi

  return 1

  while [ "$#" -gt 0 ]; do
    case "$1" in
      --only)
        if [ "$#" -lt 2 ]; then
          fail 122 "invalid e2e arguments"
        fi
        shift
        only_target="$1"
        ;;
    esac
    shift
  done

  case "$only_target" in
    online|online-zai|online-openai-codex)
      return 0
      ;;
    *)
      return 1
      ;;
  esac
}

validate_provider_credential() {
  if is_test_toggle_enabled RPLACA_TEST_MISSING_PROVIDER_CREDENTIAL; then
    fail 116 "missing required provider credential"
  fi

  if [ "$MODE" != "e2e" ]; then
    return 0
  fi

  if ! e2e_invocation_requires_credential "$@"; then
    return 0
  fi

  if [ -n "${OPENAI_API_KEY:-}" ] || [ -n "${ZAI_CODING_MAX_API_KEY:-}" ] || [ -n "${OPENROUTER_API_KEY:-}" ]; then
    return 0
  fi

  fail 116 "missing required provider credential"
}

validate_override_path() {
  if is_test_toggle_enabled RPLACA_TEST_INVALID_OVERRIDE_PATH; then
    fail 117 "invalid override path"
  fi

  validate_canonical_override RPLACA_SSL_LIB directory
  validate_canonical_override RPLACA_FONT_PATH readable-file
}

resolve_canonical_path() {
  path="$1"

  if command -v realpath >/dev/null 2>&1; then
    realpath "$path" 2>/dev/null || return 1
    return 0
  fi

  if command -v readlink >/dev/null 2>&1; then
    readlink -f "$path" 2>/dev/null || return 1
    return 0
  fi

  return 1
}

override_path_has_allowed_prefix() {
  canonical_path="$1"

  case "$canonical_path" in
    "$REPO_ROOT"|"$REPO_ROOT"/*|/tmp|/tmp/*|/gnu/store|/gnu/store/*|/run/current-system/profile|/run/current-system/profile/*)
      return 0
      ;;
  esac

  return 1
}

validate_canonical_override() {
  key="$1"
  expected_type="$2"
  eval "raw_value=\${$key:-}"

  if [ -z "$raw_value" ]; then
    return 0
  fi

  case "$expected_type" in
    directory)
      [ -d "$raw_value" ] || fail 117 "invalid override path"
      ;;
    readable-file)
      [ -f "$raw_value" ] && [ -r "$raw_value" ] || fail 117 "invalid override path"
      ;;
    executable-file)
      [ -f "$raw_value" ] && [ -x "$raw_value" ] || fail 117 "invalid override path"
      ;;
    *)
      fail 117 "invalid override path"
      ;;
  esac

  canonical_path=$(resolve_canonical_path "$raw_value") || fail 117 "invalid override path"

  if [ "$canonical_path" != "$raw_value" ]; then
    fail 117 "invalid override path"
  fi

  override_path_has_allowed_prefix "$canonical_path" || fail 117 "invalid override path"
}

add_runtime_library_path() {
  lib_dir="$1"

  if [ -z "$lib_dir" ] || [ ! -d "$lib_dir" ]; then
    return 0
  fi
  if [ -e "$lib_dir/libc.so" ] || [ -e "$lib_dir/libc.so.6" ]; then
    return 0
  fi

  case ":${RUNTIME_LD_LIBRARY_PATH:-}:" in
    *:"$lib_dir":*)
      return 0
      ;;
  esac

  if [ -n "${RUNTIME_LD_LIBRARY_PATH:-}" ]; then
    RUNTIME_LD_LIBRARY_PATH="$lib_dir:$RUNTIME_LD_LIBRARY_PATH"
  else
    RUNTIME_LD_LIBRARY_PATH="$lib_dir"
  fi
}

validate_runtime_openssl_path() {
  if is_test_toggle_enabled RPLACA_TEST_MISSING_OPENSSL_PATH; then
    fail 121 "missing required runtime OpenSSL path"
  fi

  RESOLVED_SSL_LIB_PATH=''

  if [ -n "${RPLACA_SSL_LIB:-}" ]; then
    RESOLVED_SSL_LIB_PATH=$(resolve_canonical_path "$RPLACA_SSL_LIB") || fail 117 "invalid override path"
  fi

  if [ -z "$RESOLVED_SSL_LIB_PATH" ]; then
    resolved_ssl_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --no-cwd --network --share="$REPO_ROOT=/workspace" -- bash -lc 'ldconfig -p 2>/dev/null | while IFS= read -r line; do case "$line" in *" => "*) lib=${line%% *}; case "$lib" in libssl.so*|libcrypto.so*) printf "%s\n" "${line##* => }"; break ;; esac ;; esac; done' 2>/dev/null || true)

    if [ -z "$resolved_ssl_file" ]; then
      resolved_ssl_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --no-cwd --network --share="$REPO_ROOT=/workspace" -- bash -lc 'for lib in /run/current-system/profile/lib/libssl.so* /run/current-system/profile/lib/libcrypto.so* /run/current-system/profile/lib64/libssl.so* /run/current-system/profile/lib64/libcrypto.so* /gnu/store/*/lib/libssl.so* /gnu/store/*/lib/libcrypto.so* /gnu/store/*/lib64/libssl.so* /gnu/store/*/lib64/libcrypto.so* /lib/libssl.so* /lib/libcrypto.so* /lib64/libssl.so* /lib64/libcrypto.so* /usr/lib/libssl.so* /usr/lib/libcrypto.so* /usr/lib64/libssl.so* /usr/lib64/libcrypto.so*; do if [ -e "$lib" ]; then printf "%s\n" "$lib"; break; fi; done' 2>/dev/null || true)
    fi

    if [ -n "$resolved_ssl_file" ]; then
      resolved_ssl_file=$(resolve_canonical_path "$resolved_ssl_file" || printf '%s\n' "$resolved_ssl_file")
      RESOLVED_SSL_LIB_PATH=${resolved_ssl_file%/*}
    fi
  fi

  if [ -z "$RESOLVED_SSL_LIB_PATH" ] || [ ! -d "$RESOLVED_SSL_LIB_PATH" ]; then
    if [ "$MODE" = "run" ]; then
      fail 121 "missing required runtime OpenSSL path"
    fi
    return 0
  fi

  add_runtime_library_path "$RESOLVED_SSL_LIB_PATH"
}

resolve_runtime_libgcc_path() {
  resolved_libgcc_file=''

  resolved_libgcc_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --no-cwd --network --share="$REPO_ROOT=/workspace" -- bash -lc 'ldconfig -p 2>/dev/null | while IFS= read -r line; do case "$line" in *" => "*) lib=${line%% *}; case "$lib" in libgcc_s.so*) printf "%s\n" "${line##* => }"; break ;; esac ;; esac; done' 2>/dev/null || true)

  if [ -z "$resolved_libgcc_file" ]; then
    resolved_libgcc_file=$(cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --no-cwd --network --share="$REPO_ROOT=/workspace" -- bash -lc 'for lib in /run/current-system/profile/lib/libgcc_s.so* /run/current-system/profile/lib64/libgcc_s.so* /gnu/store/*/lib/libgcc_s.so* /gnu/store/*/lib64/libgcc_s.so* /lib/libgcc_s.so* /lib64/libgcc_s.so* /usr/lib/libgcc_s.so* /usr/lib64/libgcc_s.so*; do if [ -e "$lib" ]; then printf "%s\n" "$lib"; break; fi; done' 2>/dev/null || true)
  fi

  if [ -n "$resolved_libgcc_file" ]; then
    resolved_libgcc_file=$(resolve_canonical_path "$resolved_libgcc_file" || printf '%s\n' "$resolved_libgcc_file")
    add_runtime_library_path "${resolved_libgcc_file%/*}"
  fi
}

validate_e2e_args() {
  if [ "$MODE" != "e2e" ]; then
    return 0
  fi

  if is_test_toggle_enabled RPLACA_TEST_INVALID_E2E_ARGS; then
    fail 122 "invalid e2e arguments"
  fi

  if e2e_invocation_requires_credential "$@"; then
    :
  fi
}

binary_visible() {
  binary="$1"
  toggle="$2"
  if is_test_toggle_enabled "$toggle"; then
    return 1
  fi
  run_in_container 'set -eu; command -v "$1" >/dev/null 2>&1' "$binary"
}

screenshot_command_visible() {
  if is_test_toggle_enabled RPLACA_TEST_HIDE_SCREENSHOT; then
    return 1
  fi
  run_in_container 'set -eu; command -v import >/dev/null 2>&1 || command -v magick >/dev/null 2>&1 || command -v xwd >/dev/null 2>&1'
}

validate_mode_binaries() {
  if ! binary_visible sbcl RPLACA_TEST_HIDE_SBCL; then
    fail 113 "missing required binary: sbcl"
  fi

  if [ "$MODE" = "e2e" ]; then
    if ! binary_visible python3 RPLACA_TEST_HIDE_PYTHON3; then
      fail 114 "missing required binary: python3"
    fi
    if ! binary_visible Xvfb RPLACA_TEST_HIDE_XVFB; then
      fail 114 "missing required binary: Xvfb"
    fi
    if ! binary_visible xdotool RPLACA_TEST_HIDE_XDOTOOL; then
      fail 114 "missing required binary: xdotool"
    fi
    if ! binary_visible setsid RPLACA_TEST_HIDE_SETSID; then
      fail 114 "missing required binary: setsid"
    fi
    if ! screenshot_command_visible; then
      fail 114 "missing screenshot command: import, magick, or xwd"
    fi
  fi
}

run_preflight() {
  validate_launcher_cli "$@"
  resolve_repo_root
  validate_guix_available
  validate_project_mount
  validate_mode_binaries
  validate_quicklisp_pin_values
  validate_e2e_args "$@"
  validate_provider_credential "$@"
  validate_override_path
  validate_runtime_openssl_path
  resolve_runtime_libgcc_path
  prepare_quicklisp_cache
}

clear_test_toggles() {
  names=$(env | grep '^RPLACA_TEST_' | cut -d= -f1 || true)
  for name in $names; do
    unset "$name"
  done
  unset RPLACA_ENABLE_TEST_TOGGLES
}

validate_persistent_directory_path() {
  variable_name="$1"
  directory="$2"
  case "$directory" in
    /*) ;;
    *)
      fail 124 "$variable_name must be an absolute path for persistent container storage"
      ;;
  esac
  case "$directory" in
    /|*[!A-Za-z0-9_./-]*)
      fail 124 "$variable_name contains characters unsupported by persistent container storage"
      ;;
  esac
}

validate_codex_bundle_path() {
  directory="$1"
  case "$directory" in
    /*) ;;
    *) fail 125 "RPLACA_CODEX_BUNDLE must be an absolute path" ;;
  esac
  case "$directory" in
    /|*[!A-Za-z0-9_./@+-]*)
      fail 125 "RPLACA_CODEX_BUNDLE contains unsupported characters"
      ;;
  esac
}

launch_payload() {
  shift_count="$1"
  shift

  while [ "$shift_count" -gt 0 ]; do
    shift
    shift_count=$((shift_count - 1))
  done

  if [ ! -f "$HOST_QUICKLISP_SETUP" ]; then
    fail 112 "quicklisp bootstrap failed"
  fi

  # Canonical state is writable. Legacy state is exposed read-only at its
  # same-shape path so inert migration fallbacks can see it while executable
  # init, package, skill, project, and MCP stores remain refusal-only.
  extra_container_args=""
  if [ -n "$HOST_CONFIG_DIR" ]; then
    mkdir -p "$HOST_CONFIG_DIR"
    extra_container_args="$extra_container_args --share=$HOST_CONFIG_DIR=$WORKSPACE_HOME/.config/rplaca"
  fi
  if [ -n "$HOST_USER_HOME" ]; then
    host_user_config_dir="$HOST_USER_HOME/.rplaca.d"
    host_projects_dir="$HOST_USER_HOME/.rplaca.projects.d"
    host_default_state_dir="$HOST_USER_HOME/.local/state/rplaca"
    mkdir -p \
      "$host_user_config_dir" \
      "$host_projects_dir" \
      "$host_default_state_dir"
    extra_container_args="$extra_container_args --share=$host_user_config_dir=$WORKSPACE_HOME/.rplaca.d"
    extra_container_args="$extra_container_args --share=$host_projects_dir=$WORKSPACE_HOME/.rplaca.projects.d"
    extra_container_args="$extra_container_args --share=$host_default_state_dir=$WORKSPACE_HOME/.local/state/rplaca"
  fi
  if [ -n "${XDG_STATE_HOME:-}" ]; then
    validate_persistent_directory_path XDG_STATE_HOME "$XDG_STATE_HOME"
    mkdir -p "$XDG_STATE_HOME"
    extra_container_args="$extra_container_args --share=$XDG_STATE_HOME=$XDG_STATE_HOME"
  fi
  if [ -n "${RPLACA_CRASH_REPORT_DIR:-}" ]; then
    validate_persistent_directory_path \
      RPLACA_CRASH_REPORT_DIR "$RPLACA_CRASH_REPORT_DIR"
    mkdir -p "$RPLACA_CRASH_REPORT_DIR"
    extra_container_args="$extra_container_args --share=$RPLACA_CRASH_REPORT_DIR=$RPLACA_CRASH_REPORT_DIR"
  fi
  if [ -n "${RPLACA_CODEX_BUNDLE:-}" ]; then
    validate_codex_bundle_path "$RPLACA_CODEX_BUNDLE"
    if [ ! -x "$RPLACA_CODEX_BUNDLE/bin/codex" ]; then
      fail 125 "RPLACA_CODEX_BUNDLE has no executable bin/codex"
    fi
    extra_container_args="$extra_container_args --expose=$RPLACA_CODEX_BUNDLE=/run/rplaca-codex"
  fi
  if [ -n "$HOST_USER_HOME" ] && [ -d "$HOST_USER_HOME/.config/clawmacs" ]; then
    extra_container_args="$extra_container_args --expose=$HOST_USER_HOME/.config/clawmacs=$WORKSPACE_HOME/.config/clawmacs"
  fi
  if [ -n "$HOST_USER_HOME" ] && [ -d "$HOST_USER_HOME/.clawmacs.d" ]; then
    extra_container_args="$extra_container_args --expose=$HOST_USER_HOME/.clawmacs.d=$WORKSPACE_HOME/.clawmacs.d"
  fi
  if [ -n "$HOST_USER_HOME" ] && [ -d "$HOST_USER_HOME/.clawmacs.projects.d" ]; then
    extra_container_args="$extra_container_args --expose=$HOST_USER_HOME/.clawmacs.projects.d=$WORKSPACE_HOME/.clawmacs.projects.d"
  fi
  if [ -n "$HOST_USER_HOME" ] && [ -d "$HOST_USER_HOME/.codex" ]; then
    extra_container_args="$extra_container_args --share=$HOST_USER_HOME/.codex=$WORKSPACE_HOME/.codex"
  fi
  # X11 forwarding: expose the X socket and Xauthority so McCLIM (and any
  # other graphical toolkit) can connect to the host display server. McCLIM
  # E2E starts Xvfb inside the container, so it needs a private writable X
  # socket directory instead of the host socket exposed read-only.
  if [ "${RPLACA_CONTAINER_DISABLE_HOST_X:-0}" != "1" ]; then
    if [ -d "/tmp/.X11-unix" ]; then
      extra_container_args="$extra_container_args --expose=/tmp/.X11-unix"
    fi
    if [ -n "${XAUTHORITY:-}" ] && [ -f "$XAUTHORITY" ]; then
      extra_container_args="$extra_container_args --expose=$XAUTHORITY"
    fi
  fi

  export RUNTIME_LD_LIBRARY_PATH
  # shellcheck disable=SC2086
  cd "$CONTAINER_LAUNCH_DIR" && guix shell -f "$GUIX_MANIFEST_PATH" --container --no-cwd --network --preserve="$PRESERVED_ENV_PATTERN" --share="$REPO_ROOT=/workspace" $extra_container_args -- bash -lc 'cd /workspace && export RPLACA_IN_GUIX_CONTAINER=1 HOME="${HOME:-/workspace/.cache/home}" RPLACA_QUICKLISP_SETUP="${RPLACA_QUICKLISP_SETUP:-/workspace/.cache/home/quicklisp/setup.lisp}" XDG_CACHE_HOME="${XDG_CACHE_HOME:-/workspace/.cache}" RPLACA_PROMPT_PROJECT_ROOT="${RPLACA_PROMPT_PROJECT_ROOT:-/workspace}" CL_SOURCE_REGISTRY="${GUIX_ENVIRONMENT:?missing Guix environment}/share/common-lisp/systems/"; if [ -n "${RUNTIME_LD_LIBRARY_PATH:-}" ]; then export LD_LIBRARY_PATH="$RUNTIME_LD_LIBRARY_PATH"; else unset LD_LIBRARY_PATH; fi; exec "$@"' bash "$@"
}

main() {
  run_preflight "$@"

  if [ "${RPLACA_DEBUG:-0}" = "1" ]; then
    diagnostic_env OPENAI_API_KEY
    diagnostic_env ZAI_CODING_MAX_API_KEY
    diagnostic_env OPENROUTER_API_KEY
  fi

  if [ "$PREFLIGHT_ONLY" -eq 1 ]; then
    exit 0
  fi

  clear_test_toggles

  launch_payload "$PAYLOAD_SHIFT_COUNT" "$@"
}

main "$@"
