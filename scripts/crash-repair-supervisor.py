#!/usr/bin/env python3
"""Supervise RPLACA, repair genuine fatal crashes with Codex, and relaunch."""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import subprocess
import sys
import threading
import time
import uuid
from typing import Any, Iterable, TextIO


PREFIX = "[crash-repair]"
CONTAINER_ROOT = Path("/workspace")
DEFAULT_MAX_ATTEMPTS = 2


def log(message: str) -> None:
    print(f"{PREFIX} {message}", file=sys.stderr, flush=True)


def private_directory(path: Path) -> Path:
    path.mkdir(parents=True, exist_ok=True)
    path.chmod(0o700)
    return path


def append_private(path: Path, text: str) -> None:
    with path.open("a", encoding="utf-8") as stream:
        stream.write(text)
        stream.flush()
        os.fsync(stream.fileno())
    path.chmod(0o600)


def container_path(repo_root: Path, host_path: Path) -> Path:
    return CONTAINER_ROOT / host_path.resolve().relative_to(repo_root.resolve())


def snapshot_requests(directory: Path) -> set[Path]:
    return set(directory.glob("*.request"))


def newest_new_request(directory: Path, before: set[Path]) -> Path | None:
    candidates = [path for path in directory.glob("*.request") if path not in before]
    return max(candidates, key=lambda path: path.stat().st_mtime_ns, default=None)


def request_value(text: str, key: str) -> str | None:
    marker = f"{key}:"
    for line in text.splitlines():
        if line.lower().startswith(marker.lower()):
            return line.split(":", 1)[1].strip()
    return None


def host_path_from_container(repo_root: Path, value: str | None) -> Path | None:
    if not value:
        return None
    path = Path(value)
    try:
        relative = path.relative_to(CONTAINER_ROOT)
    except ValueError:
        return path
    return repo_root / relative


def host_evidence_path(repo_root: Path, value: str | None) -> Path | None:
    """Resolve a private crash-request path as seen by the host supervisor."""
    if not value or value == "<unavailable>":
        return None
    if value == "<cwd>" or value.startswith("<cwd>/"):
        return repo_root / value[len("<cwd>"):].lstrip("/")
    if value == "<home>" or value.startswith("<home>/"):
        return Path.home() / value[len("<home>"):].lstrip("/")
    return host_path_from_container(repo_root, value)


def container_evidence_path(repo_root: Path, value: str | None) -> Path | None:
    """Translate a host/native request path to the fixer container namespace."""
    host_path = host_evidence_path(repo_root, value)
    if not host_path:
        return None
    try:
        return container_path(repo_root, host_path)
    except ValueError:
        try:
            relative = host_path.resolve().relative_to(Path.home().resolve())
        except ValueError:
            return host_path
        return CONTAINER_ROOT / ".cache" / "home" / relative


def normalized_crash_fingerprint(repo_root: Path, request_path: Path) -> str:
    request_text = request_path.read_text(encoding="utf-8", errors="replace")
    stable_request = "\n".join(
        line
        for line in request_text.splitlines()
        if not line.startswith(("timestamp_utc:", "report-path:"))
    )
    report_path = host_evidence_path(
        repo_root, request_value(request_text, "report-path")
    )
    stable_report = ""
    if report_path and report_path.is_file():
        report_lines = report_path.read_text(
            encoding="utf-8", errors="replace"
        ).splitlines()
        stable_report = "\n".join(
            line
            for line in report_lines
            if not line.startswith(("timestamp_utc:", "pid:"))
            and " tid=" not in line
        )
    payload = f"{stable_request}\n{stable_report}".encode("utf-8")
    return hashlib.sha256(payload).hexdigest()


def resolve_codex_bundle() -> Path:
    override = os.environ.get("RPLACA_CODEX_BUNDLE")
    if override:
        bundle = Path(override).expanduser().resolve()
        if (bundle / "bin" / "codex").is_file():
            return bundle
        raise RuntimeError(f"RPLACA_CODEX_BUNDLE has no bin/codex: {bundle}")

    command = shutil.which("codex")
    if not command:
        raise RuntimeError("codex is not installed or not on PATH")
    entry = Path(command).resolve()
    roots = [entry.parent, *entry.parents]
    candidates: list[Path] = []
    for root in roots:
        candidates.extend(root.glob(
            "node_modules/@openai/codex-*/vendor/*/bin/codex"
        ))
    machine = platform.machine().lower()
    architecture = "x86_64" if machine in {"x86_64", "amd64"} else machine
    matching = [path for path in candidates if architecture in str(path)]
    executable = (matching or candidates)
    if not executable:
        raise RuntimeError(f"unable to locate the native Codex bundle from {entry}")
    return executable[0].resolve().parent.parent


def extract_thread_id(events_path: Path) -> str | None:
    if not events_path.is_file():
        return None
    for line in events_path.read_text(encoding="utf-8", errors="replace").splitlines():
        try:
            event = json.loads(line)
        except json.JSONDecodeError:
            continue
        for key in ("thread_id", "threadId", "session_id", "sessionId"):
            value = event.get(key)
            if isinstance(value, str) and value:
                return value
        thread = event.get("thread")
        if isinstance(thread, dict) and isinstance(thread.get("id"), str):
            return thread["id"]
    return None


def build_prompt(
    *, request_path: Path, history_path: Path, attempt: int, max_attempts: int,
    evidence_paths: Iterable[tuple[str, Path]] = (),
) -> str:
    evidence = "\n".join(f"- {label}: {path}" for label, path in evidence_paths)
    if evidence:
        evidence = f"\nContainer-visible paths translated from the request:\n{evidence}\n"
    return f"""RPLACA has crashed and this is a fresh automatic repair session.

You are running inside RPLACA's dedicated Guix repair container. Work directly
in /workspace. Read and obey /workspace/AGENTS.md before changing anything.

Evidence:
- private crash repair request: {request_path}
- append-only prior repair history: {history_path}
- this is repair attempt {attempt} of {max_attempts} in the current launch chain
{evidence}

The request names the bounded crash report, debug log, exact redacted condition,
and history. Inspect every available artifact before diagnosing the failure. For
large logs, inspect metadata and bounded relevant tails instead of loading the
entire file. Check the history for previous attempted fixes and avoid repeating
an ineffective change. Inspect the current dirty worktree and preserve unrelated
user changes.

Fix the underlying RPLACA bug with the smallest coherent change. Add a regression
test that reproduces the failure when feasible. Run focused tests and the full
suite in the repository's Guix wrapper in proportion to the change. Do not
commit, push, reset, discard, or overwrite unrelated work. Do not relaunch
RPLACA yourself; the supervisor will relaunch only after a successful result.

Return status "fixed" only when the bug is actually repaired and validation
supports relaunch. Return "blocked" if evidence is insufficient or repair is
unsafe. Your structured final result is itself appended to the repair history,
so make diagnosis, changes, validation, and recurrence prevention concrete.
"""


def tee_stream(source: TextIO, log_stream: TextIO, terminal: TextIO) -> None:
    """Copy SOURCE to its private log and the launching terminal."""
    terminal_available = True
    try:
        for line in source:
            log_stream.write(line)
            log_stream.flush()
            if terminal_available:
                try:
                    terminal.write(line)
                    terminal.flush()
                except (OSError, ValueError):
                    terminal_available = False
    finally:
        source.close()


def run_streamed_process(
    command: list[str], *, cwd: Path, environment: dict[str, str], prompt: str,
    events_stream: TextIO, stderr_stream: TextIO,
    terminal_stdout: TextIO = sys.stdout, terminal_stderr: TextIO = sys.stderr,
) -> int:
    """Run Codex while teeing both output streams to durable logs and the TTY."""
    process = subprocess.Popen(
        command,
        cwd=cwd,
        env=environment,
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        bufsize=1,
    )
    assert process.stdin and process.stdout and process.stderr
    stdout_thread = threading.Thread(
        target=tee_stream,
        args=(process.stdout, events_stream, terminal_stdout),
        name="rplaca-codex-stdout",
    )
    stderr_thread = threading.Thread(
        target=tee_stream,
        args=(process.stderr, stderr_stream, terminal_stderr),
        name="rplaca-codex-stderr",
    )
    stdout_thread.start()
    stderr_thread.start()
    try:
        process.stdin.write(prompt)
    except BrokenPipeError:
        pass
    finally:
        try:
            process.stdin.close()
        except BrokenPipeError:
            pass
    returncode = process.wait()
    stdout_thread.join()
    stderr_thread.join()
    return returncode


def load_result(path: Path) -> dict[str, Any] | None:
    try:
        value = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None
    required = {
        "status", "diagnosis", "changes", "validation", "recurrence_prevention"
    }
    if not isinstance(value, dict) or not required.issubset(value):
        return None
    if value.get("status") not in {"fixed", "blocked"}:
        return None
    if not isinstance(value.get("changes"), list) or not isinstance(
        value.get("validation"), list
    ):
        return None
    return value


def markdown_history_entry(record: dict[str, Any]) -> str:
    result = record.get("result") or {}
    changes = result.get("changes") or []
    validation = result.get("validation") or []
    lines = [
        f"## {record['finished_at']} - chain {record['chain_id']} attempt {record['attempt']}",
        "",
        f"- Fingerprint: `{record['fingerprint']}`",
        f"- Status: `{record['status']}`",
        f"- Codex thread: `{record.get('codex_thread_id') or 'unavailable'}`",
        f"- Crash request: `{record['request']}`",
        f"- Diagnosis: {result.get('diagnosis', 'No validated Codex result.')}",
        f"- Recurrence prevention: {result.get('recurrence_prevention', 'unavailable')}",
        "",
        "Changes:",
        *[f"- {item}" for item in changes],
        "",
        "Validation:",
        *[f"- {item}" for item in validation],
        "",
    ]
    return "\n".join(lines) + "\n"


def run_codex_repair(
    *, repo_root: Path, state_root: Path, request_path: Path,
    history_container_path: Path, chain_id: str, attempt: int,
    max_attempts: int
) -> tuple[int, dict[str, Any] | None, str | None, Path]:
    attempt_dir = private_directory(
        state_root / "attempts" / chain_id / f"attempt-{attempt}"
    )
    events_path = attempt_dir / "codex-events.jsonl"
    stderr_path = attempt_dir / "codex-stderr.log"
    result_path = attempt_dir / "result.json"
    bundle = resolve_codex_bundle()
    guix_wrapper = repo_root / "scripts" / "guix-container.sh"
    result_container = container_path(repo_root, result_path)
    schema_container = CONTAINER_ROOT / "scripts" / "crash-repair-result.schema.json"
    request_container = container_path(repo_root, request_path)
    request_text = request_path.read_text(encoding="utf-8", errors="replace")
    evidence_paths = []
    for label, key in (
        ("crash report", "report-path"),
        ("debug log", "debug-log"),
        ("repair history", "repair-history"),
    ):
        path = container_evidence_path(repo_root, request_value(request_text, key))
        if path:
            evidence_paths.append((label, path))
    prompt = build_prompt(
        request_path=request_container,
        history_path=history_container_path,
        attempt=attempt,
        max_attempts=max_attempts,
        evidence_paths=evidence_paths,
    )
    command = [
        str(guix_wrapper), "--mode", "run", "--",
        "/run/rplaca-codex/bin/codex", "exec",
        "--dangerously-bypass-approvals-and-sandbox",
        "--cd", str(CONTAINER_ROOT),
        "--json",
        "--output-schema", str(schema_container),
        "--output-last-message", str(result_container),
        "-",
    ]
    environment = os.environ.copy()
    environment["RPLACA_CODEX_BUNDLE"] = str(bundle)
    log(f"starting fresh Codex repair attempt {attempt}/{max_attempts}")
    with events_path.open("w", encoding="utf-8") as stdout, stderr_path.open(
        "w", encoding="utf-8"
    ) as stderr:
        returncode = run_streamed_process(
            command,
            cwd=repo_root,
            environment=environment,
            prompt=prompt,
            events_stream=stdout,
            stderr_stream=stderr,
        )
    events_path.chmod(0o600)
    stderr_path.chmod(0o600)
    if result_path.exists():
        result_path.chmod(0o600)
    return (
        returncode,
        load_result(result_path),
        extract_thread_id(events_path),
        attempt_dir,
    )


def positive_attempt_limit() -> int:
    raw = os.environ.get("RPLACA_CRASH_REPAIR_MAX_ATTEMPTS", "")
    if not raw:
        return DEFAULT_MAX_ATTEMPTS
    try:
        value = int(raw)
    except ValueError as error:
        raise RuntimeError("RPLACA_CRASH_REPAIR_MAX_ATTEMPTS must be an integer") from error
    if not 1 <= value <= 10:
        raise RuntimeError("RPLACA_CRASH_REPAIR_MAX_ATTEMPTS must be between 1 and 10")
    return value


def automatic_repair_enabled() -> bool:
    return os.environ.get("RPLACA_CRASH_REPAIR", "1").lower() not in {
        "0", "false", "no", "off"
    }


def supervise(
    application_command: Iterable[str], *, repo_root: Path | None = None,
    state_root: Path | None = None, max_attempts: int | None = None,
    application_path_mode: str = "container",
) -> int:
    command = list(application_command)
    if not command:
        raise RuntimeError("missing supervised RPLACA command")
    if application_path_mode not in {"container", "host"}:
        raise RuntimeError("application_path_mode must be 'container' or 'host'")
    repo_root = (repo_root or Path(__file__).resolve().parent.parent).resolve()
    state_root = private_directory(
        state_root or repo_root / ".cache" / "crash-repair"
    )
    requests = private_directory(state_root / "requests")
    history_jsonl = state_root / "repair-history.jsonl"
    history_markdown = state_root / "repair-history.md"
    history_container = container_path(repo_root, history_markdown)
    max_attempts = max_attempts or positive_attempt_limit()
    chain_id = uuid.uuid4().hex
    attempt = 0

    while True:
        before = snapshot_requests(requests)
        environment = os.environ.copy()
        if application_path_mode == "host":
            environment["RPLACA_CRASH_REPAIR_REQUEST_DIR"] = str(requests)
            environment["RPLACA_CRASH_REPAIR_HISTORY"] = str(history_markdown)
        else:
            environment["RPLACA_CRASH_REPAIR_REQUEST_DIR"] = str(
                container_path(repo_root, requests)
            )
            environment["RPLACA_CRASH_REPAIR_HISTORY"] = str(history_container)
        try:
            completed = subprocess.run(
                command, cwd=repo_root, env=environment, check=False
            )
        except KeyboardInterrupt:
            log("interrupted by user; automatic repair not started")
            return 130
        if completed.returncode == 0:
            return 0
        request = newest_new_request(requests, before)
        if not request:
            log(
                f"RPLACA exited with status {completed.returncode} without a new "
                "fatal repair request; not invoking Codex"
            )
            return completed.returncode
        if not automatic_repair_enabled():
            log(f"automatic repair disabled; request retained at {request}")
            return completed.returncode
        attempt += 1
        fingerprint = normalized_crash_fingerprint(repo_root, request)
        if attempt > max_attempts:
            record = {
                "chain_id": chain_id,
                "attempt": attempt,
                "fingerprint": fingerprint,
                "request": str(container_path(repo_root, request)),
                "status": "attempt-limit",
                "finished_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
                "result": None,
            }
            append_private(history_jsonl, json.dumps(record, sort_keys=True) + "\n")
            append_private(history_markdown, markdown_history_entry(record))
            log(f"repair attempt limit reached; history: {history_markdown}")
            return completed.returncode
        started_at = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        try:
            codex_code, result, thread_id, attempt_dir = run_codex_repair(
                repo_root=repo_root,
                state_root=state_root,
                request_path=request,
                history_container_path=history_container,
                chain_id=chain_id,
                attempt=attempt,
                max_attempts=max_attempts,
            )
        except Exception as error:  # supervisor failure must not loop or hide evidence
            log(f"unable to start Codex repair: {error}")
            codex_code, result, thread_id = 127, None, None
            attempt_dir = state_root / "attempts" / chain_id / f"attempt-{attempt}"
        fixed = bool(codex_code == 0 and result and result.get("status") == "fixed")
        record = {
            "chain_id": chain_id,
            "attempt": attempt,
            "fingerprint": fingerprint,
            "request": str(container_path(repo_root, request)),
            "started_at": started_at,
            "finished_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "codex_exit_code": codex_code,
            "codex_thread_id": thread_id,
            "attempt_directory": str(container_path(repo_root, attempt_dir)),
            "status": "fixed" if fixed else "blocked",
            "result": result,
        }
        append_private(history_jsonl, json.dumps(record, sort_keys=True) + "\n")
        append_private(history_markdown, markdown_history_entry(record))
        if not fixed:
            log(f"Codex did not produce a validated fix; history: {history_markdown}")
            return completed.returncode
        log(f"Codex reported a validated fix; relaunching RPLACA (history: {history_markdown})")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--application-path-mode",
        choices=("container", "host"),
        default="container",
    )
    parser.add_argument("application", nargs=argparse.REMAINDER)
    arguments = parser.parse_args()
    application = arguments.application
    if application and application[0] == "--":
        application = application[1:]
    try:
        return supervise(
            application,
            application_path_mode=arguments.application_path_mode,
        )
    except RuntimeError as error:
        log(str(error))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
