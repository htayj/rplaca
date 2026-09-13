#!/usr/bin/env python3
"""Deterministic unit tests for the outer crash-repair supervisor."""

from __future__ import annotations

import importlib.util
import io
import json
import os
from pathlib import Path
import tempfile
import unittest
from unittest import mock


SCRIPT = Path(__file__).with_name("crash-repair-supervisor.py")
SPEC = importlib.util.spec_from_file_location("crash_repair_supervisor", SCRIPT)
assert SPEC and SPEC.loader
SUPERVISOR = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(SUPERVISOR)


class CrashRepairSupervisorTests(unittest.TestCase):
    @staticmethod
    def fixed_result() -> dict[str, object]:
        return {
            "status": "fixed",
            "diagnosis": "cause",
            "changes": ["change"],
            "validation": ["test"],
            "recurrence_prevention": "regression test",
        }

    def test_newest_new_request_ignores_preexisting_files(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            directory = Path(raw)
            old = directory / "old.request"
            old.write_text("old", encoding="utf-8")
            before = SUPERVISOR.snapshot_requests(directory)
            new = directory / "new.request"
            new.write_text("new", encoding="utf-8")
            self.assertEqual(new, SUPERVISOR.newest_new_request(directory, before))

    def test_fingerprint_ignores_timestamp_pid_and_thread_ids(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            report = root / "crash.report"
            request = root / "crash.request"

            def write(timestamp: str, pid: int, tid: int) -> None:
                report.write_text(
                    f"timestamp_utc: {timestamp}\npid: {pid}\n"
                    "[condition]\ntype: SIMPLE-ERROR\n"
                    f"role=:MAIN current=yes alive=yes tid={tid}\n",
                    encoding="utf-8",
                )
                request.write_text(
                    "schema: rplaca-crash-repair-request\n"
                    f"timestamp_utc: {timestamp}\n"
                    f"report-path: {report}\n"
                    "condition-message: stable failure\n",
                    encoding="utf-8",
                )

            write("2026-01-01T00:00:00Z", 10, 20)
            first = SUPERVISOR.normalized_crash_fingerprint(root, request)
            write("2026-01-02T00:00:00Z", 30, 40)
            second = SUPERVISOR.normalized_crash_fingerprint(root, request)
            self.assertEqual(first, second)

    def test_prompt_requires_history_review_and_preserves_unrelated_work(self) -> None:
        prompt = SUPERVISOR.build_prompt(
            request_path=Path("/workspace/request"),
            history_path=Path("/workspace/history"),
            attempt=1,
            max_attempts=2,
        )
        self.assertIn("Inspect every available artifact", prompt)
        self.assertIn("bounded relevant tails", prompt)
        self.assertIn("ineffective change", prompt)
        self.assertIn("preserve unrelated", prompt)
        self.assertIn("commit, push, reset", prompt)
        self.assertIn("attempt 1 of 2", prompt)

    def test_native_evidence_paths_translate_to_repair_container(self) -> None:
        repo = Path.home() / "projects" / "rplaca"
        self.assertEqual(
            Path("/workspace/debug.log"),
            SUPERVISOR.container_evidence_path(repo, str(repo / "debug.log")),
        )
        self.assertEqual(
            Path("/workspace/.cache/home/.local/state/rplaca/crash.report"),
            SUPERVISOR.container_evidence_path(
                repo, "<home>/.local/state/rplaca/crash.report"
            ),
        )

    def test_streamed_process_tees_stdout_and_stderr(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            events = io.StringIO()
            errors = io.StringIO()
            terminal_out = io.StringIO()
            terminal_err = io.StringIO()
            code = SUPERVISOR.run_streamed_process(
                [
                    os.sys.executable,
                    "-c",
                    "import sys; data=sys.stdin.read(); "
                    "print('event:' + data); print('progress', file=sys.stderr)",
                ],
                cwd=Path(raw),
                environment=os.environ.copy(),
                prompt="repair",
                events_stream=events,
                stderr_stream=errors,
                terminal_stdout=terminal_out,
                terminal_stderr=terminal_err,
            )
        self.assertEqual(0, code)
        self.assertEqual("event:repair\n", events.getvalue())
        self.assertEqual(events.getvalue(), terminal_out.getvalue())
        self.assertEqual("progress\n", errors.getvalue())
        self.assertEqual(errors.getvalue(), terminal_err.getvalue())

    def test_native_application_receives_host_repair_paths(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            repo = Path(raw) / "repo"
            state = repo / ".cache" / "crash-repair"
            repo.mkdir()
            observed: dict[str, str] = {}

            def application_run(*args: object, **kwargs: object) -> mock.Mock:
                environment = kwargs["env"]
                observed["request"] = environment["RPLACA_CRASH_REPAIR_REQUEST_DIR"]
                observed["history"] = environment["RPLACA_CRASH_REPAIR_HISTORY"]
                return mock.Mock(returncode=0)

            with mock.patch.object(SUPERVISOR.subprocess, "run", application_run):
                code = SUPERVISOR.supervise(
                    ["fake-rplaca"],
                    repo_root=repo,
                    state_root=state,
                    max_attempts=1,
                    application_path_mode="host",
                )
            self.assertEqual(0, code)
            self.assertEqual(str(state / "requests"), observed["request"])
            self.assertEqual(str(state / "repair-history.md"), observed["history"])

    def test_result_must_be_complete_and_explicitly_fixed_or_blocked(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            result = Path(raw) / "result.json"
            result.write_text("{}", encoding="utf-8")
            self.assertIsNone(SUPERVISOR.load_result(result))
            payload = self.fixed_result()
            result.write_text(json.dumps(payload), encoding="utf-8")
            self.assertEqual(payload, SUPERVISOR.load_result(result))

    def test_codex_bundle_override_requires_native_binary(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            bundle = Path(raw)
            (bundle / "bin").mkdir()
            binary = bundle / "bin" / "codex"
            binary.write_text("binary", encoding="utf-8")
            with mock.patch.dict(os.environ, {"RPLACA_CODEX_BUNDLE": str(bundle)}):
                self.assertEqual(bundle.resolve(), SUPERVISOR.resolve_codex_bundle())

    def test_validated_fix_relaunches_and_records_history(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            repo = Path(raw) / "repo"
            state = repo / ".cache" / "crash-repair"
            repo.mkdir()
            calls = 0

            def application_run(*args: object, **kwargs: object) -> mock.Mock:
                nonlocal calls
                calls += 1
                if calls == 1:
                    requests = state / "requests"
                    requests.mkdir(parents=True, exist_ok=True)
                    report = repo / "crash.report"
                    report.write_text("[condition]\ntype: SIMPLE-ERROR\n", encoding="utf-8")
                    (requests / "crash.request").write_text(
                        "condition-message: boom\nreport-path: /workspace/crash.report\n",
                        encoding="utf-8",
                    )
                    return mock.Mock(returncode=1)
                return mock.Mock(returncode=0)

            with mock.patch.object(SUPERVISOR.subprocess, "run", application_run), \
                 mock.patch.object(
                     SUPERVISOR,
                     "run_codex_repair",
                     return_value=(0, self.fixed_result(), "thread-1", state / "attempt"),
                 ):
                code = SUPERVISOR.supervise(
                    ["fake-rplaca"], repo_root=repo, state_root=state,
                    max_attempts=2,
                )
            self.assertEqual(0, code)
            self.assertEqual(2, calls)
            history = (state / "repair-history.jsonl").read_text(encoding="utf-8")
            self.assertIn('"status": "fixed"', history)
            self.assertIn('"codex_thread_id": "thread-1"', history)

    def test_attempt_cap_stops_second_crash_without_second_fixer(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            repo = Path(raw) / "repo"
            state = repo / ".cache" / "crash-repair"
            repo.mkdir()
            launches = 0

            def crashing_run(*args: object, **kwargs: object) -> mock.Mock:
                nonlocal launches
                launches += 1
                requests = state / "requests"
                requests.mkdir(parents=True, exist_ok=True)
                report = repo / f"crash-{launches}.report"
                report.write_text("[condition]\ntype: SIMPLE-ERROR\n", encoding="utf-8")
                (requests / f"crash-{launches}.request").write_text(
                    f"condition-message: boom\nreport-path: /workspace/{report.name}\n",
                    encoding="utf-8",
                )
                return mock.Mock(returncode=1)

            fixer = mock.Mock(
                return_value=(0, self.fixed_result(), "thread-1", state / "attempt")
            )
            with mock.patch.object(SUPERVISOR.subprocess, "run", crashing_run), \
                 mock.patch.object(SUPERVISOR, "run_codex_repair", fixer):
                code = SUPERVISOR.supervise(
                    ["fake-rplaca"], repo_root=repo, state_root=state,
                    max_attempts=1,
                )
            self.assertEqual(1, code)
            self.assertEqual(2, launches)
            self.assertEqual(1, fixer.call_count)
            history = (state / "repair-history.jsonl").read_text(encoding="utf-8")
            self.assertIn('"status": "attempt-limit"', history)


if __name__ == "__main__":
    unittest.main()
