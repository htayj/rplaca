#!/usr/bin/env python3
"""Dependency-free unit tests for the external GUI E2E driver."""

from __future__ import annotations

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


DRIVER_PATH = Path(__file__).with_name("gui-e2e-driver.py")
SPEC = importlib.util.spec_from_file_location("rplaca_gui_e2e_driver",
                                              DRIVER_PATH)
if SPEC is None or SPEC.loader is None:
    raise RuntimeError(f"cannot load GUI E2E driver from {DRIVER_PATH}")
DRIVER = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = DRIVER
SPEC.loader.exec_module(DRIVER)


def event_line(event: dict[str, object], *, newline: bool = True) -> bytes:
    """Return one debug-log event line."""
    suffix = "\n" if newline else ""
    return (f"debug-prefix {DRIVER.EVENT_MARKER}"
            f"{json.dumps(event, sort_keys=True)}{suffix}").encode("utf-8")


class EventLogReaderTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        root = Path(self.temporary_directory.name)
        self.debug_log = root / "debug.log"
        self.session = DRIVER.McCLIMGuiSession(
            window_id="1",
            window_title="test",
            debug_log=self.debug_log,
            artifact_dir=root / "artifacts",
        )

    def append(self, data: bytes) -> None:
        with self.debug_log.open("ab") as stream:
            stream.write(data)

    def test_reads_new_append_bytes_without_duplicating_cached_events(self) -> None:
        self.assertEqual([], self.session._events())

        self.append(event_line({"event": "ready", "sequence": 1}))
        self.assertEqual([1], [event["sequence"]
                              for event in self.session._events()])
        self.assertEqual([1], [event["sequence"]
                              for event in self.session._events()])

        self.append(event_line({"event": "ready", "sequence": 2}))
        self.assertEqual([1, 2], [event["sequence"]
                                 for event in self.session._events()])
        self.assertEqual(self.debug_log.stat().st_size,
                         self.session._debug_log_offset)

    def test_buffers_partial_lines_until_their_terminating_newline(self) -> None:
        self.append(
            f"{DRIVER.EVENT_MARKER}{{\"event\":\"partial\",\"sequence\":".encode(
                "utf-8"))
        self.assertEqual([], self.session._events())

        self.append(b"7}")
        self.assertEqual([], self.session._events())

        self.append(b"\n")
        self.assertEqual([{"event": "partial", "sequence": 7}],
                         self.session._events())
        self.assertEqual([{"event": "partial", "sequence": 7}],
                         self.session._events())

    def test_skips_malformed_lines_and_non_object_json(self) -> None:
        self.append(
            b"ordinary debug output\n"
            + f"{DRIVER.EVENT_MARKER}{{not-json}}\n".encode("utf-8")
            + f"{DRIVER.EVENT_MARKER}[1, 2, 3]\n".encode("utf-8")
            + event_line({"event": "valid", "sequence": 8})
        )
        self.assertEqual([{"event": "valid", "sequence": 8}],
                         self.session._events())

    def test_same_inode_truncate_and_regrow_resets_cached_events(self) -> None:
        old_line = event_line({"event": "old", "sequence": 10,
                               "padding": "x" * 400})
        self.debug_log.write_bytes(old_line)
        self.assertEqual(["old"], [event["event"]
                                   for event in self.session._events()])

        new_line = event_line({"event": "new", "sequence": 1,
                               "padding": "y" * 800})
        self.debug_log.write_bytes(new_line)
        self.assertGreater(len(new_line), len(old_line))
        self.assertEqual(["new"], [event["event"]
                                   for event in self.session._events()])

    def test_inode_replacement_resets_cached_events(self) -> None:
        self.debug_log.write_bytes(
            event_line({"event": "old", "sequence": 10}))
        self.assertEqual(["old"], [event["event"]
                                   for event in self.session._events()])

        replacement = self.debug_log.with_suffix(".replacement")
        replacement.write_bytes(
            event_line({"event": "replacement", "sequence": 1}))
        os.replace(replacement, self.debug_log)
        self.assertEqual(["replacement"], [event["event"]
                                           for event in self.session._events()])

    def test_latest_helpers_and_wait_after_use_incremental_cache(self) -> None:
        self.append(event_line({"event": "target", "sequence": 2}))
        self.append(event_line({"event": "other", "sequence": 5}))
        self.assertEqual(2,
                         self.session.latest_event("target")["sequence"])
        self.assertEqual(5, self.session.latest_sequence())

        appended = False

        def append_after_first_poll(_seconds: float) -> None:
            nonlocal appended
            if not appended:
                appended = True
                self.append(event_line({"event": "target", "sequence": 6,
                                        "accepted": True}))

        with mock.patch.object(DRIVER.time, "sleep",
                               side_effect=append_after_first_poll):
            event = self.session.wait_event_after(
                "target", 2,
                lambda candidate: candidate.get("accepted") is True,
                timeout=1.0)
        self.assertEqual(6, event["sequence"])
        self.assertEqual(6, self.session.latest_sequence())

    def test_polls_read_log_bytes_once_plus_small_append_anchor(self) -> None:
        initial = b"".join(
            event_line({"event": "sample", "sequence": sequence,
                        "text": "x" * 80})
            for sequence in range(100)
        )
        self.debug_log.write_bytes(initial)
        bytes_read = 0
        real_open = Path.open

        class CountingReader:
            def __init__(self, stream: object) -> None:
                self.stream = stream

            def __enter__(self) -> "CountingReader":
                self.stream.__enter__()
                return self

            def __exit__(self, *args: object) -> object:
                return self.stream.__exit__(*args)

            def __getattr__(self, name: str) -> object:
                return getattr(self.stream, name)

            def read(self, *args: object, **kwargs: object) -> bytes:
                nonlocal bytes_read
                data = self.stream.read(*args, **kwargs)
                bytes_read += len(data)
                return data

        def counting_open(path: Path, *args: object,
                          **kwargs: object) -> object:
            stream = real_open(path, *args, **kwargs)
            mode = str(args[0] if args else kwargs.get("mode", "r"))
            if path == self.debug_log and mode == "rb":
                return CountingReader(stream)
            return stream

        with mock.patch.object(Path, "open", new=counting_open):
            for _ in range(25):
                self.session._events()
            appended = event_line({"event": "sample", "sequence": 100})
            with real_open(self.debug_log, "ab") as stream:
                stream.write(appended)
            for _ in range(25):
                self.session._events()

        expected = (self.debug_log.stat().st_size
                    + min(len(initial), DRIVER.DEBUG_LOG_ANCHOR_BYTES))
        self.assertEqual(expected, bytes_read)
        self.assertEqual(101, len(self.session._events()))


class StabilitySignatureTests(unittest.TestCase):
    def test_contained_ui_errors_and_graphical_debuggers_fail_stability(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            log = root / "debug.log"
            session = DRIVER.McCLIMGuiSession(
                window_id="1", window_title="test", debug_log=log,
                artifact_dir=root)
            for signature in ("ui-action-error", "graphical-debugger-entered",
                              "graphical-debugger-failed"):
                with self.subTest(signature=signature):
                    log.write_text(signature)
                    with self.assertRaises(DRIVER.DriverError):
                        DRIVER.assert_no_stability_failure_signatures(session)
            log.write_text("normal update")
            DRIVER.assert_no_stability_failure_signatures(session)


class ExternalCommandTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary_directory = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary_directory.cleanup)
        root = Path(self.temporary_directory.name)
        self.session = DRIVER.McCLIMGuiSession(
            window_id="4242",
            window_title="test",
            debug_log=root / "debug.log",
            artifact_dir=root / "artifacts",
        )

    def test_helper_timeout_is_reported_even_for_best_effort_commands(self) -> None:
        expired = subprocess.TimeoutExpired(
            ["import", "-window", "4242"], 0.25,
            output=b"partial stdout", stderr=b"partial stderr")
        with mock.patch.object(DRIVER.subprocess, "run",
                               side_effect=expired):
            with self.assertRaisesRegex(
                    DRIVER.DriverError,
                    r"(?s)command timed out after 0\.25s: import.*partial stdout"
                    r".*partial stderr"):
                self.session.run(
                    ["import", "-window", "4242"],
                    check=False, timeout=0.25)

        self.assertEqual(
            ["run", "run_timeout"],
            [step["action"] for step in self.session.steps])
        self.assertEqual(0.25, self.session.steps[-1]["timeout_seconds"])

    def test_screenshot_uses_the_shorter_capture_deadline(self) -> None:
        with mock.patch.object(DRIVER.shutil, "which",
                               side_effect=lambda name: (
                                   "/bin/import" if name == "import" else None)), \
             mock.patch.object(self.session, "run") as run, \
             mock.patch.object(self.session, "latest_event",
                               return_value=None):
            record = self.session.screenshot("failure")

        expected_path = self.session.screenshot_dir / "failure.png"
        run.assert_called_once_with(
            ["import", "-window", "4242", str(expected_path)],
            timeout=DRIVER.SCREENSHOT_TIMEOUT_SECONDS)
        self.assertEqual(str(expected_path), record["path"])


class FinalStateScreenshotTests(unittest.TestCase):
    def test_waits_for_newer_redisplay_then_barrier_then_capture(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, event_name: str, after_sequence: int,
                                 predicate: object,
                                 *, timeout: float) -> dict[str, object]:
                accepts = predicate  # Keep the assertions readable below.
                wrong_buffer = {"buffer_name": "other", "repeat": None}
                repeated = {"buffer_name": "chat", "repeat": True}
                completed = {"buffer_name": "chat", "repeat": None}
                calls.append(("wait", event_name, after_sequence, timeout,
                              accepts(wrong_buffer), accepts(repeated),
                              accepts(completed)))
                return {"event": event_name, "sequence": 43,
                        **completed}

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 17, "reason": "pane-rendered",
                            "buffer_name": "chat"},
            root=True,
            timeout=3.5)

        self.assertEqual(
            [("wait", "redisplay-handled", 17, 3.5,
              False, False, True),
             ("barrier",),
             ("screenshot", "final", True)],
            calls,
        )
        self.assertEqual(17, record["final_snapshot_sequence"])
        self.assertFalse(record["redisplay_already_handled"])
        self.assertEqual(43, record["redisplay_sequence"])

    def test_already_redisplayed_snapshot_skips_later_event_wait(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, *args: object,
                                 **kwargs: object) -> dict[str, object]:
                raise AssertionError("must not wait for another redisplay")

            def log_action(self, action: str, **fields: object) -> None:
                calls.append(("log", action, fields))

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 29,
                            "reason": "redisplay-handled",
                            "repeat": None})

        self.assertEqual(
            [("log", "final_snapshot_after_redisplay",
              {"snapshot_sequence": 29}),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertTrue(record["redisplay_already_handled"])
        self.assertIsNone(record["redisplay_sequence"])

    def test_repeating_redisplay_snapshot_waits_for_quiescent_cycle(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, event_name: str, after_sequence: int,
                                 predicate: object,
                                 *, timeout: float) -> dict[str, object]:
                repeated = {"buffer_name": "chat", "repeat": True}
                completed = {"buffer_name": "chat", "repeat": None}
                calls.append(("wait", event_name, after_sequence, timeout,
                              predicate(repeated), predicate(completed)))
                return {"event": event_name, "sequence": 37, **completed}

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 29,
                            "reason": "redisplay-handled",
                            "repeat": True,
                            "buffer_name": "chat"})

        self.assertEqual(
            [("wait", "redisplay-handled", 29, 10.0, False, True),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertFalse(record["redisplay_already_handled"])
        self.assertEqual(37, record["redisplay_sequence"])

    def test_legacy_redisplay_snapshot_without_repeat_waits(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def wait_event_after(self, event_name: str, after_sequence: int,
                                 predicate: object,
                                 *, timeout: float) -> dict[str, object]:
                completed = {"buffer_name": "chat", "repeat": None}
                calls.append(("wait", event_name, after_sequence, timeout,
                              predicate(completed)))
                return {"event": event_name, "sequence": 43, **completed}

            def x_request_reply_barrier(self) -> None:
                calls.append(("barrier",))

            def screenshot(self, name: str, *, root: bool) -> dict[str, object]:
                calls.append(("screenshot", name, root))
                return {"name": name}

        record = DRIVER.McCLIMGuiSession.final_state_screenshot(
            FakeSession(), "final",
            final_snapshot={"sequence": 31,
                            "reason": "redisplay-handled",
                            "buffer_name": "chat"})

        self.assertEqual(
            [("wait", "redisplay-handled", 31, 10.0, True),
             ("barrier",),
             ("screenshot", "final", False)],
            calls,
        )
        self.assertFalse(record["redisplay_already_handled"])
        self.assertEqual(43, record["redisplay_sequence"])

    def test_x_barrier_requires_an_x_server_reply(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            window_id = "4242"

            def run(self, argv: list[str]) -> None:
                calls.append(("run", tuple(argv)))

            def log_action(self, action: str, **fields: object) -> None:
                calls.append(("log", action, fields))

        DRIVER.McCLIMGuiSession.x_request_reply_barrier(FakeSession())

        self.assertEqual(
            ("run", ("xdotool", "getwindowgeometry", "--shell", "4242")),
            calls[0],
        )
        self.assertEqual(
            ("log", "x_request_reply_barrier", {"window_id": "4242"}),
            calls[1],
        )

    def test_mx_final_capture_uses_accepted_final_snapshot_sequence(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def screenshot(self, name: str) -> dict[str, object]:
                calls.append(("screenshot", name))
                return {"name": name}

            def type_text(self, text: str) -> None:
                calls.append(("type", text))

            def wait_snapshot(self, description: str, predicate: object,
                              *, timeout: float) -> dict[str, object]:
                calls.append(("wait_snapshot", description, timeout))
                if description == "M-x command executed":
                    return {"event": "ui-snapshot", "sequence": 97,
                            "reason": "pane-rendered"}
                return {"event": "ui-snapshot", "sequence": 41,
                        "reason": "pane-rendered"}

            def press(self, key: str) -> None:
                calls.append(("press", key))

            def final_state_screenshot(self, name: str, *,
                                       final_snapshot: dict[str, object]
                                       ) -> dict[str, object]:
                calls.append(("final", name,
                              final_snapshot.get("sequence")))
                return {"name": name}

        session = FakeSession()
        with mock.patch.object(DRIVER, "prepare_session", return_value=[]), \
             mock.patch.object(DRIVER, "open_mx", return_value=None):
            screenshots = DRIVER.run_mx(session)

        final_wait = ("wait_snapshot", "M-x command executed", 10.0)
        final_capture = ("final", "04-mx-result", 97)
        self.assertLess(calls.index(("press", "Return")),
                        calls.index(final_wait))
        self.assertLess(calls.index(final_wait), calls.index(final_capture))
        self.assertIn(final_capture, calls)
        self.assertEqual("04-mx-result", screenshots[-1]["name"])


class SwitchBufferAssertionTests(unittest.TestCase):
    class FakeSession:
        def __init__(self) -> None:
            self.actions: list[tuple[str, dict[str, object]]] = []

        def log_action(self, action: str, **fields: object) -> None:
            self.actions.append((action, fields))

    @staticmethod
    def expanded_snapshot() -> dict[str, object]:
        return {
            "minibuffer_active": True,
            "input_focus_pane": "compose",
            "minibuffer_filtered_count": 14,
            "minibuffer_visible_count": 11,
            "minibuffer_desired_rows": 12,
            "minibuffer_required_height": 315,
            "minibuffer_row_height": 25,
            "top_level_grafted": True,
            "pointer_documentation_grafted": True,
            "top_level_left": 0,
            "top_level_top": 0,
            "top_level_right": 900,
            "top_level_bottom": 846,
            "minibuffer_left": 0,
            "minibuffer_top": 480,
            "minibuffer_right": 900,
            "minibuffer_bottom": 795,
            "pointer_documentation_left": 1,
            "pointer_documentation_top": 796,
            "pointer_documentation_right": 899,
            "pointer_documentation_bottom": 845,
        }

    def test_expanded_and_collapsed_geometry_contracts(self) -> None:
        session = self.FakeSession()
        expanded_height = DRIVER.assert_switch_buffer_layout(
            session, self.expanded_snapshot())
        collapsed = {
            **self.expanded_snapshot(),
            "minibuffer_active": False,
            "top_level_bottom": 555,
            "minibuffer_top": 480,
            "minibuffer_bottom": 505,
            "pointer_documentation_top": 506,
            "pointer_documentation_bottom": 554,
        }
        DRIVER.assert_switch_buffer_collapsed(
            session, collapsed, expanded_height, "cancellation")

        self.assertEqual(846, expanded_height)
        self.assertEqual(
            ["assert_switch_buffer_layout", "assert_switch_buffer_collapsed"],
            [action for action, _fields in session.actions],
        )
        self.assertEqual("cancellation", session.actions[-1][1]["transition"])

    def test_expanded_geometry_rejects_pointer_help_overlap(self) -> None:
        snapshot = self.expanded_snapshot()
        snapshot["pointer_documentation_top"] = 793
        with self.assertRaisesRegex(DRIVER.DriverError, "overlaps pointer help"):
            DRIVER.assert_switch_buffer_layout(self.FakeSession(), snapshot)

    def test_large_draft_and_stress_burst_contract(self) -> None:
        self.assertEqual(32 * 1024,
                         len(DRIVER.SWITCH_BUFFER_LARGE_DRAFT))
        self.assertTrue(DRIVER.SWITCH_BUFFER_LARGE_DRAFT.startswith(
            DRIVER.SWITCH_BUFFER_LARGE_DRAFT_PREFIX))
        self.assertEqual(409,
                         DRIVER.SWITCH_BUFFER_LARGE_DRAFT.count("\n"))
        self.assertLessEqual(
            max(map(len, DRIVER.SWITCH_BUFFER_LARGE_DRAFT.splitlines())),
            DRIVER.SWITCH_BUFFER_LARGE_DRAFT_LINE_WIDTH - 1)
        self.assertEqual(
            "74D712A71BAED508",
            DRIVER.SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT)
        self.assertEqual(
            DRIVER.SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT,
            DRIVER.stable_text_fingerprint(
                DRIVER.SWITCH_BUFFER_LARGE_DRAFT))
        self.assertLess(DRIVER.SWITCH_BUFFER_LARGE_DRAFT_POINT,
                        DRIVER.SWITCH_BUFFER_LARGE_DRAFT_MARK)
        self.assertLess(DRIVER.SWITCH_BUFFER_LARGE_DRAFT_MARK,
                        len(DRIVER.SWITCH_BUFFER_LARGE_DRAFT))

        keys = DRIVER.switch_buffer_stress_keys()
        self.assertGreaterEqual(
            len(DRIVER.SWITCH_BUFFER_STRESS_QUERY) + len(keys), 50)
        self.assertNotIn("Return", keys)
        self.assertNotIn("ctrl+g", keys)
        self.assertIn("Down", keys)
        self.assertIn("Up", keys)
        self.assertIn("BackSpace", keys)

        snapshot = {
            # The debug logger intentionally sanitizes long strings. Compact
            # fields must still prove the exact unsanitized editor state.
            "compose_text": DRIVER.SWITCH_BUFFER_LARGE_DRAFT[:10000]
                            + "…[truncated]",
            "compose_length": DRIVER.SWITCH_BUFFER_LARGE_DRAFT_SIZE,
            "compose_fingerprint": (
                DRIVER.SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT),
            "compose_point": DRIVER.SWITCH_BUFFER_LARGE_DRAFT_POINT,
            "compose_mark": DRIVER.SWITCH_BUFFER_LARGE_DRAFT_MARK,
        }
        self.assertTrue(DRIVER.switch_buffer_large_draft_state_p(snapshot))
        snapshot["compose_fingerprint"] = "0000000000000000"
        self.assertFalse(DRIVER.switch_buffer_large_draft_state_p(snapshot))
        snapshot["compose_fingerprint"] = (
            DRIVER.SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT)
        snapshot["compose_mark"] = DRIVER.SWITCH_BUFFER_LARGE_DRAFT_MARK - 1
        self.assertFalse(DRIVER.switch_buffer_large_draft_state_p(snapshot))

    def test_selected_buffer_candidate_requires_an_exact_name(self) -> None:
        snapshot = {
            "minibuffer_text": (
                "Switch Buffer: switch-e2e-1\n"
                ">   switch-e2e-11  [agent] idle  msgs:0\n"
                "    switch-e2e-1  [agent] idle  msgs:0"),
        }
        self.assertEqual(
            "switch-e2e-11",
            DRIVER.selected_buffer_candidate_name(snapshot))
        self.assertNotEqual(
            "switch-e2e-1",
            DRIVER.selected_buffer_candidate_name(snapshot))

        snapshot["minibuffer_text"] = (
            "Switch Buffer: switch-e2e-1\n"
            "> * switch-e2e-1  [agent] idle  msgs:0\n"
            "    switch-e2e-11  [agent] idle  msgs:0")
        self.assertEqual(
            "switch-e2e-1",
            DRIVER.selected_buffer_candidate_name(snapshot))

    def test_press_keys_uses_one_xdotool_burst(self) -> None:
        calls: list[tuple[object, ...]] = []

        class FakeSession:
            def run(self, argv: list[str]) -> None:
                calls.append(("run", tuple(argv)))

            def log_action(self, action: str, **fields: object) -> None:
                calls.append(("log", action, fields))

        DRIVER.McCLIMGuiSession.press_keys(
            FakeSession(), ["Down", "Up", "BackSpace"], delay_ms=7)

        self.assertEqual(
            ("run", ("xdotool", "key", "--clearmodifiers", "--delay",
                     "7", "Down", "Up", "BackSpace")),
            calls[0],
        )
        self.assertEqual(
            ("log", "press_keys",
            {"keys": ["Down", "Up", "BackSpace"], "delay_ms": 7}),
            calls[1],
        )

    def test_stress_burst_uses_one_quiescence_wait_and_shared_deadline(self) -> None:
        calls: list[tuple[object, ...]] = []
        snapshot = {
            **self.expanded_snapshot(),
            "compose_text": DRIVER.SWITCH_BUFFER_LARGE_DRAFT[:10000]
                            + "…[truncated]",
            "compose_length": DRIVER.SWITCH_BUFFER_LARGE_DRAFT_SIZE,
            "compose_fingerprint": (
                DRIVER.SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT),
            "compose_point": DRIVER.SWITCH_BUFFER_LARGE_DRAFT_POINT,
            "compose_mark": DRIVER.SWITCH_BUFFER_LARGE_DRAFT_MARK,
            "minibuffer_text": (
                "Switch Buffer: switch-e2e\n> switch-e2e-0"),
        }

        class FakeSession:
            def latest_sequence(self) -> int:
                return 41

            def type_text(self, value: str) -> None:
                calls.append(("type", value))

            def press_keys(self, keys: list[str], *, delay_ms: int = 5
                           ) -> None:
                calls.append(("burst", tuple(keys), delay_ms))

            def log_action(self, action: str, **fields: object) -> None:
                calls.append(("log", action, fields))

        with mock.patch.object(
                DRIVER.time, "monotonic",
                side_effect=[100.0, 100.4, 101.0]), \
             mock.patch.object(
                 DRIVER, "wait_quiescent_snapshot_after",
                 return_value=snapshot) as wait_quiescent, \
             mock.patch.object(
                 DRIVER, "assert_switch_buffer_layout",
                 return_value=846.0):
            observed, height = DRIVER.run_switch_buffer_stress_burst(
                FakeSession())

        self.assertIs(snapshot, observed)
        self.assertEqual(846.0, height)
        self.assertEqual(("type", DRIVER.SWITCH_BUFFER_STRESS_QUERY), calls[0])
        self.assertEqual("burst", calls[1][0])
        self.assertEqual(50, len(calls[1][1]))
        self.assertAlmostEqual(
            4.6, wait_quiescent.call_args.kwargs["timeout"])
        self.assertEqual(
            "assert_switch_buffer_stress_burst", calls[-1][1])


if __name__ == "__main__":
    unittest.main()
