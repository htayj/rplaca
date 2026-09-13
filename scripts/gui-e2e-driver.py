#!/usr/bin/env python3
"""External xdotool/ImageMagick driver for the RPLACA GUI E2E suites."""

from __future__ import annotations

import argparse
import json
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any, Callable

EVENT_MARKER = "[e2e-event]"
HELLO_SENTINEL = "RPLACA_E2E_HELLO_SENTINEL"
DEBUG_LOG_ANCHOR_BYTES = 256
SWITCH_BUFFER_LARGE_DRAFT_PREFIX = "RPLACA_SWITCH_BUFFER_LARGE_DRAFT:"
SWITCH_BUFFER_LARGE_DRAFT_SIZE = 32 * 1024
SWITCH_BUFFER_LARGE_DRAFT_LINE_WIDTH = 80


def stable_text_fingerprint(text: str) -> str:
    """Return RPLACA' uppercase 64-bit FNV-1a fingerprint for TEXT."""
    value = 0xCBF29CE484222325
    for character in text:
        value ^= ord(character)
        value = (value * 0x100000001B3) & 0xFFFFFFFFFFFFFFFF
    return f"{value:016X}"


def make_switch_buffer_large_draft() -> str:
    """Return an exact-size multiline draft without adversarial long lines."""
    characters = list(
        SWITCH_BUFFER_LARGE_DRAFT_PREFIX
        + "x" * (SWITCH_BUFFER_LARGE_DRAFT_SIZE
                 - len(SWITCH_BUFFER_LARGE_DRAFT_PREFIX))
    )
    for offset in range(SWITCH_BUFFER_LARGE_DRAFT_LINE_WIDTH - 1,
                        len(characters),
                        SWITCH_BUFFER_LARGE_DRAFT_LINE_WIDTH):
        characters[offset] = "\n"
    return "".join(characters)


SWITCH_BUFFER_LARGE_DRAFT = make_switch_buffer_large_draft()
SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT = stable_text_fingerprint(
    SWITCH_BUFFER_LARGE_DRAFT)
SWITCH_BUFFER_LARGE_DRAFT_POINT = 8192
SWITCH_BUFFER_LARGE_DRAFT_MARK = 24576
SWITCH_BUFFER_STRESS_QUERY = "switch-e2e"
SWITCH_BUFFER_STRESS_BUDGET_SECONDS = 5.0
COMPOSE_MX_REGRESSION_LENGTHS = (100, 101, 112)
HELPER_TIMEOUT_SECONDS = 15.0
SCREENSHOT_TIMEOUT_SECONDS = 10.0


class DriverError(RuntimeError):
    pass


class McCLIMGuiSession:
    def __init__(self, *, window_id: str, window_title: str, debug_log: Path,
                 artifact_dir: Path) -> None:
        self.window_id = window_id
        self.window_title = window_title
        self.debug_log = debug_log
        self.artifact_dir = artifact_dir
        self.screenshot_dir = artifact_dir / "screenshots"
        self.screenshot_dir.mkdir(parents=True, exist_ok=True)
        self.action_log = artifact_dir / "actions.jsonl"
        self.steps: list[dict[str, Any]] = []
        self._event_cache: list[dict[str, Any]] = []
        self._debug_log_identity: tuple[int, int] | None = None
        self._debug_log_offset = 0
        self._debug_log_partial = b""
        self._debug_log_anchor = b""
        self._debug_log_mtime_ns: int | None = None

    def log_action(self, action: str, **fields: Any) -> None:
        entry = {
            "time": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
            "action": action,
            **fields,
        }
        self.steps.append(entry)
        with self.action_log.open("a", encoding="utf-8") as f:
            f.write(json.dumps(entry, sort_keys=True) + "\n")

    def run(self, argv: list[str], *, check: bool = True,
            timeout: float = HELPER_TIMEOUT_SECONDS
            ) -> subprocess.CompletedProcess[str]:
        self.log_action("run", argv=argv, timeout_seconds=timeout)
        try:
            result = subprocess.run(
                argv, text=True, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE, check=False, timeout=timeout)
        except subprocess.TimeoutExpired as exc:
            self.log_action("run_timeout", argv=argv,
                            timeout_seconds=timeout)
            stdout = exc.stdout or ""
            stderr = exc.stderr or ""
            if isinstance(stdout, bytes):
                stdout = stdout.decode("utf-8", errors="replace")
            if isinstance(stderr, bytes):
                stderr = stderr.decode("utf-8", errors="replace")
            raise DriverError(
                f"command timed out after {timeout:g}s: {' '.join(argv)}\n"
                f"stdout: {stdout[-1000:]}\n"
                f"stderr: {stderr[-1000:]}"
            ) from exc
        if check and result.returncode != 0:
            raise DriverError(
                f"command failed ({result.returncode}): {' '.join(argv)}\n"
                f"stdout: {result.stdout[-1000:]}\n"
                f"stderr: {result.stderr[-1000:]}"
            )
        return result

    def focus(self) -> None:
        # Xvfb runs without a window manager in the harness, so
        # _NET_ACTIVE_WINDOW activation can legitimately fail. Direct X focus is
        # sufficient for xdotool key/type delivery.
        self.run(["xdotool", "windowactivate", "--sync", self.window_id], check=False)
        self.run(["xdotool", "windowfocus", self.window_id])
        self.log_action("focus", window_id=self.window_id)

    def press(self, key: str) -> None:
        self.run(["xdotool", "key", "--clearmodifiers", key])
        self.log_action("press", key=key)

    def press_keys(self, keys: list[str], *, delay_ms: int = 5) -> None:
        """Deliver KEYS as one xdotool burst without per-key process waits."""
        if not keys:
            return
        self.run(["xdotool", "key", "--clearmodifiers", "--delay",
                  str(delay_ms), *keys])
        self.log_action("press_keys", keys=keys, delay_ms=delay_ms)

    def type_text(self, text: str) -> None:
        self.run(["xdotool", "type", "--clearmodifiers", "--delay", "25", text])
        self.log_action("type_text", text=text)

    def artifact_relative_path(self, path: Path) -> str:
        raw = str(path)
        workspace_prefix = "/workspace/"
        if raw.startswith(workspace_prefix):
            return raw[len(workspace_prefix):]
        try:
            return str(path.relative_to(Path.cwd()))
        except ValueError:
            return raw

    def screenshot(self, name: str, *, root: bool = False) -> dict[str, Any]:
        png_path = self.screenshot_dir / f"{name}.png"
        screenshot_format = "png"
        target = "root" if root else self.window_id
        if shutil.which("import"):
            argv = ["import", "-window", target, str(png_path)]
            output_path = png_path
        elif shutil.which("magick"):
            argv = ["magick", "import", "-window", target, str(png_path)]
            output_path = png_path
        elif shutil.which("xwd"):
            xwd_path = self.screenshot_dir / f"{name}.xwd"
            if root:
                argv = ["xwd", "-silent", "-root", "-out", str(xwd_path)]
            else:
                argv = ["xwd", "-silent", "-id", self.window_id,
                        "-out", str(xwd_path)]
            output_path = xwd_path
            screenshot_format = "xwd"
        else:
            raise DriverError("no screenshot command available")
        self.run(argv, timeout=SCREENSHOT_TIMEOUT_SECONDS)
        snapshot = self.latest_event("ui-snapshot") or {}
        record = {
            "name": name,
            "path": str(output_path),
            "relative_path": self.artifact_relative_path(output_path),
            "format": screenshot_format,
            "snapshot_sequence": snapshot.get("sequence"),
            "status": snapshot.get("status"),
            "reason": snapshot.get("reason"),
            "target": target,
        }
        self.log_action("screenshot", **record)
        return record

    def x_request_reply_barrier(self) -> None:
        """Require an X server round trip before image capture.

        The application emits ``redisplay-handled`` after its CLIM redisplay
        work. ``getwindowgeometry`` confirms that the server answers after the
        semantic gate and gives already-flushed drawing a settle point without
        reaching into McCLIM's repaint machinery. Because xdotool uses another
        X connection, this round trip is not itself a cross-client ordering
        fence; the explicit non-repeating redisplay event is the primary gate.
        """
        self.run(["xdotool", "getwindowgeometry", "--shell",
                  self.window_id])
        self.log_action("x_request_reply_barrier", window_id=self.window_id)

    def final_state_screenshot(self, name: str, *,
                               final_snapshot: dict[str, Any],
                               root: bool = False,
                               timeout: float = 10.0) -> dict[str, Any]:
        """Capture final state after a completed redisplay and X round trip."""
        snapshot_sequence = event_sequence(final_snapshot,
                                           "final screenshot snapshot")
        redisplay_already_handled = (
            final_snapshot.get("reason") == "redisplay-handled"
            and "repeat" in final_snapshot
            and not final_snapshot.get("repeat"))
        redisplay_sequence: int | None = None
        if redisplay_already_handled:
            self.log_action(
                "final_snapshot_after_redisplay",
                snapshot_sequence=snapshot_sequence)
        else:
            expected_buffer = final_snapshot.get("buffer_name")
            redisplay = self.wait_event_after(
                "redisplay-handled", snapshot_sequence,
                lambda event: (
                    (expected_buffer is None
                     or event.get("buffer_name") == expected_buffer)
                    and not event.get("repeat")
                ),
                timeout=timeout)
            redisplay_sequence = event_sequence(
                redisplay, "final screenshot redisplay")
        self.x_request_reply_barrier()
        record = self.screenshot(name, root=root)
        record["final_snapshot_sequence"] = snapshot_sequence
        record["redisplay_already_handled"] = redisplay_already_handled
        record["redisplay_sequence"] = redisplay_sequence
        return record

    def window_geometry(self) -> dict[str, int]:
        """Return xdotool's current geometry for the main application window."""
        result = self.run(["xdotool", "getwindowgeometry", "--shell",
                           self.window_id])
        geometry: dict[str, int] = {}
        for line in result.stdout.splitlines():
            key, separator, raw_value = line.partition("=")
            if separator and key in {"X", "Y", "WIDTH", "HEIGHT", "SCREEN"}:
                try:
                    geometry[key.lower()] = int(raw_value)
                except ValueError as exc:
                    raise DriverError(
                        f"invalid window geometry value {line!r}"
                    ) from exc
        if "width" not in geometry or "height" not in geometry:
            raise DriverError(f"incomplete window geometry: {result.stdout!r}")
        self.log_action("window_geometry", **geometry)
        return geometry

    def pointer_click(self, x: int, y: int) -> None:
        """Click a main-window-relative coordinate from the external driver."""
        # Do not use xdotool's --sync here: repeated menu stress intentionally
        # returns to the same coordinate, and --sync waits for a motion event
        # that cannot occur when the pointer is already there.
        self.run(["xdotool", "mousemove", "--window",
                  self.window_id, str(x), str(y)])
        self.run(["xdotool", "click", "--clearmodifiers", "1"])
        self.log_action("pointer_click", window_id=self.window_id, x=x, y=y)

    def pointer_drag(self, start_x: int, start_y: int,
                     end_x: int, end_y: int) -> None:
        """Send one ordered primary-button press, drag, and release gesture."""
        # Keep the whole gesture on one X connection.  Separate xdotool
        # clients, or a screenshot while the button is held, can interleave
        # with McCLIM's tracking-pointer loop and are not a user-like gesture.
        self.run([
            "xdotool",
            "mousemove", "--window", self.window_id,
            str(start_x), str(start_y),
            "mousedown", "1",
            "sleep", "0.35",
            "mousemove", "--window", self.window_id,
            str(end_x), str(end_y),
            "sleep", "0.15",
            "mouseup", "1",
        ])
        self.log_action("pointer_drag", window_id=self.window_id,
                        start_x=start_x, start_y=start_y,
                        end_x=end_x, end_y=end_y)

    def resize(self, width: int, height: int) -> None:
        """Resize the application window and verify the X server applied it."""
        self.run(["xdotool", "windowsize", "--sync", self.window_id,
                  str(width), str(height)])
        deadline = time.monotonic() + 5.0
        last_geometry: dict[str, int] = {}
        while time.monotonic() < deadline:
            last_geometry = self.window_geometry()
            if (abs(last_geometry["width"] - width) <= 2
                    and abs(last_geometry["height"] - height) <= 2):
                self.log_action("resize", requested_width=width,
                                requested_height=height,
                                actual_width=last_geometry["width"],
                                actual_height=last_geometry["height"])
                return
            time.sleep(0.1)
        raise DriverError(
            f"window did not resize to {width}x{height}; "
            f"last geometry: {last_geometry}"
        )

    def unmap_map(self) -> None:
        """Force a real unmap/map cycle, then restore focus to the frame."""
        self.run(["xdotool", "windowunmap", "--sync", self.window_id])
        time.sleep(0.05)
        self.run(["xdotool", "windowmap", "--sync", self.window_id])
        self.run(["xdotool", "windowraise", self.window_id])
        self.focus()
        self.log_action("unmap_map", window_id=self.window_id)

    def _reset_event_reader(self,
                            identity: tuple[int, int] | None = None) -> None:
        """Forget events and byte state after a debug-log lifecycle change."""
        self._event_cache.clear()
        self._debug_log_identity = identity
        self._debug_log_offset = 0
        self._debug_log_partial = b""
        self._debug_log_anchor = b""
        self._debug_log_mtime_ns = None

    def _consume_debug_log_lines(self, data: bytes) -> None:
        """Decode newly completed event lines from DATA exactly once."""
        buffered = self._debug_log_partial + data
        lines = buffered.split(b"\n")
        self._debug_log_partial = lines.pop()
        for raw_line in lines:
            line = raw_line.decode("utf-8", errors="replace")
            marker = line.find(EVENT_MARKER)
            if marker < 0:
                continue
            payload = line[marker + len(EVENT_MARKER):].strip()
            try:
                decoded = json.loads(payload)
            except json.JSONDecodeError:
                continue
            if isinstance(decoded, dict):
                self._event_cache.append(decoded)

    def _events(self) -> list[dict[str, Any]]:
        """Return cached events after consuming only newly appended log bytes."""
        try:
            stream = self.debug_log.open("rb")
        except FileNotFoundError:
            if self._debug_log_identity is not None:
                self._reset_event_reader()
            return list(self._event_cache)

        with stream:
            stat = os.fstat(stream.fileno())
            identity = (stat.st_dev, stat.st_ino)
            mtime_ns = getattr(stat, "st_mtime_ns",
                               int(stat.st_mtime * 1_000_000_000))
            if self._debug_log_identity is None:
                self._debug_log_identity = identity
            elif identity != self._debug_log_identity:
                self._reset_event_reader(identity)
            elif stat.st_size < self._debug_log_offset:
                self._reset_event_reader(identity)
            elif (self._debug_log_anchor
                  and (stat.st_size != self._debug_log_offset
                       or mtime_ns != self._debug_log_mtime_ns)):
                anchor_offset = (self._debug_log_offset
                                 - len(self._debug_log_anchor))
                stream.seek(anchor_offset)
                observed_anchor = stream.read(len(self._debug_log_anchor))
                if observed_anchor != self._debug_log_anchor:
                    # A same-inode truncate-and-rewrite can grow beyond the old
                    # offset before the next poll.  Validate a short consumed
                    # suffix so that case resets instead of reading mid-line.
                    self._reset_event_reader(identity)

            stream.seek(self._debug_log_offset)
            data = stream.read()
            self._debug_log_offset += len(data)
            if data:
                combined_anchor = self._debug_log_anchor + data
                self._debug_log_anchor = combined_anchor[-DEBUG_LOG_ANCHOR_BYTES:]
                self._consume_debug_log_lines(data)
            final_stat = os.fstat(stream.fileno())
            self._debug_log_mtime_ns = getattr(
                final_stat, "st_mtime_ns",
                int(final_stat.st_mtime * 1_000_000_000))

        return list(self._event_cache)

    def latest_event(self, event_name: str) -> dict[str, Any] | None:
        for event in reversed(self._events()):
            if event.get("event") == event_name:
                return event
        return None

    def wait_event(self, event_name: str,
                   predicate: Callable[[dict[str, Any]], bool] | None = None,
                   *, timeout: float = 10.0) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            event = self.latest_event(event_name)
            if event is not None and (predicate is None or predicate(event)):
                self.log_action("wait_event", event=event_name, sequence=event.get("sequence"))
                return event
            time.sleep(0.1)
        raise DriverError(f"timed out waiting for debug event {event_name}")

    def latest_sequence(self) -> int:
        sequence = 0
        for event in self._events():
            raw_sequence = event.get("sequence")
            if isinstance(raw_sequence, int):
                sequence = max(sequence, raw_sequence)
        return sequence

    def wait_event_after(self, event_name: str, after_sequence: int,
                         predicate: Callable[[dict[str, Any]], bool] | None = None,
                         *, timeout: float = 10.0) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        last_event: dict[str, Any] | None = None
        while time.monotonic() < deadline:
            for event in reversed(self._events()):
                if event.get("event") != event_name:
                    continue
                raw_sequence = event.get("sequence")
                if not isinstance(raw_sequence, int) or raw_sequence <= after_sequence:
                    continue
                last_event = event
                if predicate is None or predicate(event):
                    self.log_action("wait_event_after", event=event_name,
                                    sequence=event.get("sequence"),
                                    after_sequence=after_sequence)
                    return event
            time.sleep(0.1)
        raise DriverError(
            f"timed out waiting for debug event {event_name} after {after_sequence}; "
            f"last event: {json.dumps(last_event, sort_keys=True) if last_event else 'none'}"
        )

    def latest_snapshot(self) -> dict[str, Any]:
        snapshot = self.latest_event("ui-snapshot")
        if snapshot is None:
            raise DriverError("no ui-snapshot event found")
        return snapshot

    def text(self) -> str:
        snapshot = self.latest_snapshot()
        value = (snapshot.get("screen_text") or snapshot.get("screenText") or
                 snapshot.get("screen-text") or "")
        return str(value)

    def wait_text_contains(self, needle: str, *, timeout: float = 15.0) -> str:
        deadline = time.monotonic() + timeout
        last_text = ""
        while time.monotonic() < deadline:
            try:
                last_text = self.text()
            except DriverError:
                last_text = ""
            if needle in last_text:
                self.log_action("wait_text_contains", needle=needle)
                return last_text
            time.sleep(0.1)
        raise DriverError(f"timed out waiting for text {needle!r}; last text:\n{last_text}")

    def wait_snapshot(self, description: str,
                      predicate: Callable[[dict[str, Any]], bool],
                      *, timeout: float = 15.0) -> dict[str, Any]:
        deadline = time.monotonic() + timeout
        last_snapshot: dict[str, Any] | None = None
        while time.monotonic() < deadline:
            last_snapshot = self.latest_event("ui-snapshot")
            if last_snapshot is not None and predicate(last_snapshot):
                self.log_action("wait_snapshot", description=description,
                                sequence=last_snapshot.get("sequence"),
                                status=last_snapshot.get("status"))
                return last_snapshot
            time.sleep(0.1)
        raise DriverError(
            f"timed out waiting for snapshot {description}; "
            f"last snapshot: {json.dumps(last_snapshot, sort_keys=True) if last_snapshot else 'none'}"
        )

    def debug_tail(self, lines: int = 120) -> str:
        if not self.debug_log.exists():
            return ""
        data = self.debug_log.read_text(encoding="utf-8", errors="replace").splitlines()
        return "\n".join(data[-lines:])


def prepare_session(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Wait for the frame, focus it, and capture the initial state."""
    screenshots: list[dict[str, Any]] = []
    session.wait_event("frame-ready", timeout=20.0)
    session.wait_event("ui-snapshot", timeout=10.0)
    session.focus()
    screenshots.append(session.screenshot("01-initial"))
    return screenshots


def event_sequence(event: dict[str, Any], description: str) -> int:
    """Return EVENT's sequence or reject an unusable observability event."""
    sequence = event.get("sequence")
    if not isinstance(sequence, int):
        raise DriverError(f"{description} has no integer sequence: {event!r}")
    return sequence


def run_smoke(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    screenshots = prepare_session(session)

    session.type_text("hello")
    session.wait_snapshot("compose contains hello",
                          lambda snapshot: snapshot.get("compose_text") == "hello",
                          timeout=10.0)
    screenshots.append(session.screenshot("02-typed-hello"))

    session.press("Return")
    session.wait_text_contains(HELLO_SENTINEL, timeout=20.0)
    session.wait_event("e2e-provider-complete",
                       lambda event: event.get("sentinel") == HELLO_SENTINEL,
                       timeout=20.0)
    final_snapshot = session.wait_snapshot(
        "final idle response rendered",
        lambda snapshot: snapshot.get("status") == "idle"
        and HELLO_SENTINEL in str(snapshot.get("screen_text", "")),
        timeout=20.0)
    screenshots.append(session.final_state_screenshot(
        "03-agent-response",
        final_snapshot=final_snapshot,
        timeout=20.0))
    return screenshots


def positive_environment_integer(name: str, default: int) -> int:
    """Return positive integer environment variable NAME or DEFAULT."""
    raw_value = os.environ.get(name)
    if raw_value is None or not raw_value.strip():
        return default
    try:
        value = int(raw_value)
    except ValueError as exc:
        raise DriverError(f"{name} must be a positive integer") from exc
    if value <= 0:
        raise DriverError(f"{name} must be a positive integer")
    return value


def stability_effort_menu_coordinates(
        session: McCLIMGuiSession) -> tuple[int, int, int]:
    """Return robust leaf-menu Effort coordinates.

    McCLIM lays the menu bar from the upper-left using the fixed font supplied by
    the Guix E2E environment. Derive the vertical position from the actual
    window and keep the pointer safely inside the direct Effort leaf.
    Coordinates deliberately live here, outside application code.
    """
    geometry = session.window_geometry()
    width = geometry["width"]
    height = geometry["height"]
    menu_x = min(width - 80, max(280, round(width * 0.32)))
    menu_y = min(18, max(12, round(height * 0.027)))
    session.log_action("stability_menu_coordinates", menu_x=menu_x,
                       width=width, height=height)
    return menu_x, menu_y, menu_y + 34


def open_stability_effort_menu(session: McCLIMGuiSession,
                               menu_x: int, menu_y: int) -> None:
    """Open the frame-local Effort selector through its direct menu leaf."""
    session.pointer_click(menu_x, menu_y)
    # The click is delivered asynchronously to McCLIM. Leave enough time for
    # the frame command to activate and render the selector.
    time.sleep(0.35)


def stress_stability_menu_boundaries(session: McCLIMGuiSession,
                                     menu_y: int) -> None:
    """Cross direct top-level commands and boundaries with button 1 held."""
    geometry = session.window_geometry()
    width = geometry["width"]
    height = geometry["height"]
    # These centers are derived from the fixed E2E font and the real menu
    # labels. Coordinates remain in this external driver, never application
    # menu code.
    category_xs = (24, 105, 199, 288, 382, 487, 617, 710)
    session.run([
        "xdotool",
        "mousemove", "--window", session.window_id,
        str(category_xs[0]), str(menu_y),
        "mousedown", "1",
        "sleep", "0.2",
        *sum(([
            "mousemove", "--window", session.window_id, str(x), str(menu_y),
            "sleep", "0.05",
        ] for x in (*category_xs, *reversed(category_xs))), []),
        "mousemove", "--window", session.window_id,
        str(width + 24), str(menu_y),
        "sleep", "0.1",
        "mouseup", "1",
    ])
    session.log_action(
        "stability_menu_boundary_trajectory",
        category_xs=category_xs,
        menu_y=menu_y,
        width=width,
        height=height,
    )
    assert_compose_probe(session, "menu-boundary-probe")

    # Re-run the exact no-delay crossover shape that crashed the former nested
    # Appearance/System menu, now against two stable leaf buttons.
    repetitions = 40
    first_x, second_x = category_xs[-2:]
    argv = [
        "xdotool",
        "mousemove", "--window", session.window_id,
        str(first_x), str(menu_y),
        "mousedown", "1",
        "sleep", "0.2",
    ]
    for _ in range(repetitions):
        argv.extend([
            "mousemove", "--window", session.window_id,
            str(first_x), str(menu_y + 34),
            "mousemove", "--window", session.window_id,
            str(second_x), str(menu_y),
            "mousemove", "--window", session.window_id,
            str(second_x), str(menu_y + 34),
            "mousemove", "--window", session.window_id,
            str(first_x), str(menu_y),
        ])
    argv.extend([
        "mousemove", "--window", session.window_id,
        str(width + 24), str(menu_y),
        "sleep", "0.1",
        "mouseup", "1",
    ])
    session.run(argv)
    session.log_action(
        "stability_menu_rapid_crossover",
        repetitions=repetitions,
        first_x=first_x,
        second_x=second_x,
        menu_y=menu_y,
    )
    # The deliberately backlogged no-delay pointer burst may still be draining
    # after xdotool has sent its final release. Let the normal CLIM event loop
    # settle before using keyboard responsiveness as the next assertion.
    time.sleep(0.75)
    assert_compose_probe(session, "menu-rapid-probe")


def assert_compose_probe(session: McCLIMGuiSession, probe: str) -> None:
    """Prove the Drei compose pane accepts and clears PROBE."""
    session.focus()
    existing = str(session.latest_snapshot().get("compose_text", ""))
    if existing:
        session.press("ctrl+a")
        session.press("ctrl+k")
        wait_compose_text(session, "")
    session.type_text(probe)
    wait_compose_text(session, probe)
    session.press("ctrl+a")
    session.press("ctrl+k")
    wait_compose_text(session, "")


def assert_no_stability_failure_signatures(session: McCLIMGuiSession) -> None:
    """Fail on crash/recovery signatures that a responsive snapshot can miss."""
    paths = [session.debug_log, session.artifact_dir / "app.stderr"]
    text = "\n".join(
        path.read_text(encoding="utf-8", errors="replace")
        for path in paths if path.exists()
    ).lower()
    signatures = {
        "debugger invoked": "SBCL entered the debugger",
        " is not grafted": "McCLIM reported an ungrafted sheet",
        "menu-bar-error-recovered": "the historical menu recovery path ran",
        "redisplay-queue-failed": "a redisplay wakeup could not be queued",
        "redisplay-handler-error": "the redisplay handler contained an error",
        "ui-action-error": "a UI action contained an error",
        "graphical-debugger-entered": "a UI action entered the graphical debugger",
        "graphical-debugger-failed": "the graphical debugger failed",
        "frame-cleanup-error": "frame unwind cleanup contained an error",
        "runtime-stream-cleanup-error": "stream cleanup contained an error",
        "runtime-oauth-cleanup-error": "OAuth cleanup contained an error",
        "runtime-worker-settlement-timeout": "a runtime worker did not settle",
        "runtime-worker-reaper-error": "the runtime worker reaper failed",
        "runtime-tool-worker-start-error": "a runtime tool worker could not start",
        "runtime-tool-queue-cleanup-error": "tool-queue cleanup contained an error",
        "prompt-tool-result-cleanup-error": "prompt tool-result cleanup failed",
        "message-help-thread-start-error": "a message-help worker could not start",
        "message-help-frame-error": "message-help frame handling failed",
        "message-help-frame-construction-error": (
            "a message-help frame could not be constructed"),
        "chat-recurse-launch-error": "a recursive chat frame could not launch",
    }
    found = [description for signature, description in signatures.items()
             if signature in text]
    # SBCL emits these process diagnostics at column zero. Restrict the
    # unhandled-condition check to the runtime's "Unhandled ... in thread"
    # line so compiler notes or source snippets containing the word do not
    # become false positives.
    lowered_lines = [line.lower() for line in text.splitlines()]
    if any(line.startswith("fatal error encountered")
           for line in lowered_lines):
        found.append("SBCL reported a fatal runtime error")
    if any(line.startswith("unhandled ") and " in thread " in line
           for line in lowered_lines):
        found.append("an SBCL thread terminated with an unhandled condition")
    if any(line.startswith("heap exhausted") for line in lowered_lines):
        found.append("SBCL exhausted its dynamic heap")
    if found:
        raise DriverError("stability failure signatures: " + "; ".join(found))
    session.log_action("assert_no_stability_failure_signatures",
                       checked=[str(path) for path in paths])


def request_frame_exit(session: McCLIMGuiSession) -> None:
    """Exit through the ordinary frame command and prove unwind completed."""
    after_sequence = session.latest_sequence()
    press_chord(session, "ctrl+x", "ctrl+c")
    session.wait_event_after("frame-stopped", after_sequence, timeout=20.0)
    session.log_action("request_frame_exit", after_sequence=after_sequence)


def run_menu_boundaries(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Prove the leaf-only menu survives the former transient-sheet path."""
    screenshots = prepare_session(session)
    session.wait_snapshot(
        "effort-capable menu fixture selected",
        lambda snapshot: snapshot.get("provider") == "openai-codex"
        and snapshot.get("model") == "gpt-5.3-codex",
        timeout=10.0,
    )
    menu_x, menu_y, _selector_item_y = stability_effort_menu_coordinates(
        session)
    stress_stability_menu_boundaries(session, menu_y)
    screenshots.append(session.screenshot("02-after-menu-boundary-stress"))
    open_stability_effort_menu(session, menu_x, menu_y)
    wait_minibuffer_text(
        session, "direct Effort leaf opened the semantic selector",
        lambda text: "Select Think Level" in text,
        timeout=10.0)
    screenshots.append(session.screenshot("03-effort-selector-open", root=True))
    cancel_modal_input(session)
    assert_compose_probe(session, "menu-command-probe")
    screenshots.append(session.screenshot("04-menu-boundary-complete"))
    assert_no_stability_failure_signatures(session)
    return screenshots


def run_stability(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Stress stable menus, semantic selectors, X lifecycle, and redisplay."""
    screenshots = prepare_session(session)
    menu_iterations = positive_environment_integer(
        "RPLACA_GUI_E2E_STABILITY_MENU_ITERATIONS", 24)
    expose_iterations = positive_environment_integer(
        "RPLACA_GUI_E2E_STABILITY_EXPOSE_ITERATIONS", 6)

    session.wait_snapshot(
        "effort-capable stability fixture selected",
        lambda snapshot: snapshot.get("provider") == "openai-codex"
        and snapshot.get("model") == "gpt-5.3-codex",
        timeout=10.0,
    )
    menu_x, menu_y, _selector_item_y = stability_effort_menu_coordinates(session)
    stress_stability_menu_boundaries(session, menu_y)
    screenshots.append(session.screenshot("02-after-menu-boundary-stress"))
    selection_count = 0
    for iteration in range(menu_iterations):
        after_sequence = session.latest_sequence()
        if iteration % 4 == 3:
            # The stable menu dispatches into the frame-owned, presentation-
            # based minibuffer selector. Alternate values so every command has
            # a distinct semantic result for the driver to observe.
            select_low = selection_count % 2 == 0
            selected_level = "low" if select_low else "default"
            expected_message = (
                "[Think level set to low for openai-codex/gpt-5.3-codex]"
                if select_low else
                "[Think level reset to default for openai-codex/gpt-5.3-codex]"
            )
            # The visible bar is intentionally leaf-only: releasing on Effort
            # dispatches the stable frame command directly, without creating a
            # transient submenu frame in pinned McCLIM.
            session.pointer_click(menu_x, menu_y)
            wait_minibuffer_text(
                session, "effort selector opened from the menu",
                lambda text: "Select Think Level" in text,
                timeout=10.0)
            session.type_text(selected_level)
            session.wait_snapshot(
                f"effort candidate {selected_level!r} selected",
                lambda snapshot, selected_level=selected_level:
                    selected_candidate_contains(snapshot, selected_level),
                timeout=10.0)
            session.press("Return")
            selection_count += 1
            session.wait_event_after(
                "ui-snapshot", after_sequence,
                lambda snapshot: (
                    expected_message in str(snapshot.get("screen_text", ""))
                    and snapshot.get("status") == "idle"
                ),
                timeout=10.0,
            )
            session.log_action("stability_effort_selected",
                               level=selected_level,
                               selection_count=selection_count)
        else:
            open_stability_effort_menu(session, menu_x, menu_y)
            if iteration == 0:
                screenshots.append(
                    session.screenshot("02-effort-selector-open", root=True))
            cancel_modal_input(session)
            time.sleep(0.15)
        if (iteration + 1) % 8 == 0:
            assert_compose_probe(session, f"menu-probe-{iteration + 1}")
    if selection_count == 0:
        raise DriverError("stability suite did not use the effort menu selector")
    screenshots.append(session.screenshot("03-after-menu-stress"))

    original_geometry = session.window_geometry()
    original_width = original_geometry["width"]
    original_height = original_geometry["height"]
    resize_targets = [
        (min(1120, original_width + 120), min(760, original_height + 100)),
        (max(760, original_width - 120), max(500, original_height - 48)),
        (original_width, original_height),
    ]
    for index, (width, height) in enumerate(resize_targets, start=1):
        session.resize(width, height)
        assert_compose_probe(session, f"resize-probe-{index}")
    screenshots.append(session.screenshot("04-after-resize-stress"))

    for iteration in range(1, expose_iterations + 1):
        session.unmap_map()
        assert_compose_probe(session, f"expose-probe-{iteration}")
    screenshots.append(session.screenshot("05-after-expose-stress"))

    # Restore deterministic no-network routing and finish with a streamed
    # response. This proves menu/expose stress did not strand the redisplay
    # coalescer or the compose command path.
    run_mx_selection(session, "minibuffer-select-model-command")
    wait_minibuffer_text(session, "stability model selector opened",
                         lambda text: "Select Model" in text)
    session.type_text("e2e")
    session.wait_snapshot(
        "stability e2e model candidate selected",
        lambda snapshot: selected_candidate_contains(snapshot, "e2e/e2e-model"),
        timeout=10.0,
    )
    session.press("Return")
    session.wait_snapshot(
        "stability e2e model selected",
        lambda snapshot: snapshot.get("provider") == "e2e"
        and snapshot.get("model") == "e2e-model",
        timeout=10.0,
    )
    session.type_text("hello")
    wait_compose_text(session, "hello")
    session.press("Return")
    session.wait_event("e2e-provider-complete",
                       lambda event: event.get("sentinel") == HELLO_SENTINEL,
                       timeout=20.0)
    final_snapshot = session.wait_snapshot(
        "stability final idle response rendered",
        lambda snapshot: snapshot.get("status") == "idle"
        and HELLO_SENTINEL in str(snapshot.get("screen_text", "")),
        timeout=20.0,
    )
    screenshots.append(session.final_state_screenshot(
        "06-stability-complete",
        final_snapshot=final_snapshot,
        timeout=20.0))
    assert_no_stability_failure_signatures(session)
    return screenshots


def open_mx(session: McCLIMGuiSession) -> None:
    """Open the M-x fuzzy command selector."""
    # Use the Emacs-compatible ESC prefix because it is more robust across
    # Xvfb/window-manager modifier handling than synthesizing Alt+x.
    session.press("Escape")
    session.press("x")
    session.wait_snapshot("M-x minibuffer opened",
                          lambda snapshot: "M-x" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)


def selected_candidate_contains(snapshot: dict[str, Any], text: str) -> bool:
    minibuffer = str(snapshot.get("minibuffer_text", ""))
    return any(line.lstrip().startswith(">") and text in line
               for line in minibuffer.splitlines())


def selected_buffer_candidate_name(snapshot: dict[str, Any]) -> str | None:
    """Return the exact selected buffer name from its semantic display row."""
    minibuffer = str(snapshot.get("minibuffer_text", ""))
    for line in minibuffer.splitlines():
        selected = line.lstrip()
        if not selected.startswith(">"):
            continue
        candidate = selected[1:].lstrip()
        if candidate.startswith("*"):
            candidate = candidate[1:].lstrip()
        return candidate.split(maxsplit=1)[0] if candidate else None
    return None


def run_mx_selection(session: McCLIMGuiSession, query: str,
                     *, selected: str | None = None,
                     timeout: float = 10.0) -> None:
    """Open M-x, type QUERY, wait for SELECTED, and press Return."""
    open_mx(session)
    session.type_text(query)
    expected = selected or query
    session.wait_snapshot(f"M-x candidate {expected!r} selected",
                          lambda snapshot: selected_candidate_contains(snapshot, expected),
                          timeout=timeout)
    session.press("Return")


def wait_minibuffer_text(session: McCLIMGuiSession, description: str,
                         predicate: Callable[[str], bool],
                         *, timeout: float = 10.0) -> dict[str, Any]:
    return session.wait_snapshot(description,
                                 lambda snapshot: predicate(
                                     str(snapshot.get("minibuffer_text", ""))),
                                 timeout=timeout)


def wait_compose_text(session: McCLIMGuiSession, expected: str,
                      *, timeout: float = 10.0) -> dict[str, Any]:
    return session.wait_snapshot(f"compose text is {expected!r}",
                                 lambda snapshot: snapshot.get("compose_text") == expected,
                                 timeout=timeout)


def wait_compose_state(session: McCLIMGuiSession, text: str, point: int,
                       *, timeout: float = 10.0) -> dict[str, Any]:
    """Wait for the visible Drei draft and point to match one buffer state."""
    return session.wait_snapshot(
        f"compose state is {text!r} at {point}",
        lambda snapshot: (snapshot.get("compose_text") == text
                          and snapshot.get("compose_point") == point),
        timeout=timeout,
    )


def switch_buffer_large_draft_state_p(snapshot: dict[str, Any]) -> bool:
    """Return whether SNAPSHOT preserves the seeded large Drei state."""
    return (snapshot.get("compose_length") == SWITCH_BUFFER_LARGE_DRAFT_SIZE
            and snapshot.get("compose_fingerprint")
            == SWITCH_BUFFER_LARGE_DRAFT_FINGERPRINT
            and snapshot.get("compose_point")
            == SWITCH_BUFFER_LARGE_DRAFT_POINT
            and snapshot.get("compose_mark")
            == SWITCH_BUFFER_LARGE_DRAFT_MARK)


def assert_switch_buffer_large_draft(snapshot: dict[str, Any],
                                     action: str) -> None:
    """Require the exact E2E large draft, point, and mark after ACTION."""
    if not switch_buffer_large_draft_state_p(snapshot):
        text = snapshot.get("compose_text")
        logged_length = len(text) if isinstance(text, str) else None
        raise DriverError(
            f"large compose state changed after {action}: "
            f"length={snapshot.get('compose_length')!r}, "
            f"fingerprint={snapshot.get('compose_fingerprint')!r}, "
            f"logged_length={logged_length}, "
            f"point={snapshot.get('compose_point')!r}, "
            f"mark={snapshot.get('compose_mark')!r}"
        )


def switch_buffer_stress_keys() -> list[str]:
    """Return 50 non-confirming navigation/edit keys for one modal burst."""
    return (["Down"] * 15
            + ["Up"] * 15
            + ["Left"] * 5
            + ["Right"] * 5
            + ["BackSpace"] * 5
            + ["h", "minus", "e", "2", "e"])


def snapshot_number(snapshot: dict[str, Any], field: str) -> float:
    """Return a required numeric snapshot FIELD with a useful failure."""
    value = snapshot.get(field)
    if isinstance(value, bool) or not isinstance(value, (int, float)):
        raise DriverError(f"snapshot field {field!r} is not numeric: {value!r}")
    return float(value)


def snapshot_bounds(snapshot: dict[str, Any], prefix: str
                    ) -> tuple[float, float, float, float]:
    """Return PREFIX's CLIM bounds from one semantic snapshot."""
    return tuple(snapshot_number(snapshot, f"{prefix}_{edge}")
                 for edge in ("left", "top", "right", "bottom"))


def wait_quiescent_snapshot_after(
        session: McCLIMGuiSession, after_sequence: int, description: str,
        predicate: Callable[[dict[str, Any]], bool], *, timeout: float = 10.0
        ) -> dict[str, Any]:
    """Wait for a matching state after CLIM completed a non-repeating layout."""
    snapshot = session.wait_event_after(
        "ui-snapshot", after_sequence,
        lambda candidate: (
            candidate.get("reason") == "redisplay-handled"
            and not candidate.get("repeat")
            and predicate(candidate)
        ),
        timeout=timeout,
    )
    session.log_action("wait_quiescent_snapshot", description=description,
                       sequence=snapshot.get("sequence"))
    return snapshot


def assert_switch_buffer_layout(session: McCLIMGuiSession,
                                snapshot: dict[str, Any],
                                *, minimum_candidates: int = 12) -> float:
    """Prove a large selector fits above pointer help and inside the mirror."""
    filtered_count = int(snapshot_number(snapshot, "minibuffer_filtered_count"))
    visible_count = int(snapshot_number(snapshot, "minibuffer_visible_count"))
    desired_rows = int(snapshot_number(snapshot, "minibuffer_desired_rows"))
    required_height = snapshot_number(snapshot, "minibuffer_required_height")
    top_bounds = snapshot_bounds(snapshot, "top_level")
    minibuffer_bounds = snapshot_bounds(snapshot, "minibuffer")
    pointer_bounds = snapshot_bounds(snapshot, "pointer_documentation")
    tolerance = 1.0

    if not snapshot.get("minibuffer_active"):
        raise DriverError("large selector layout snapshot is not active")
    if snapshot.get("input_focus_pane") != "compose":
        raise DriverError(
            "large selector did not transfer CLIM input focus to compose: "
            f"{snapshot.get('input_focus_pane')!r}"
        )
    if filtered_count < minimum_candidates:
        raise DriverError(
            f"selector has {filtered_count} candidates; expected at least "
            f"{minimum_candidates}"
        )
    if visible_count <= 0 or desired_rows != visible_count + 1:
        raise DriverError(
            "selector prompt/candidate row contract is inconsistent: "
            f"visible={visible_count}, desired={desired_rows}"
        )
    actual_height = minibuffer_bounds[3] - minibuffer_bounds[1]
    if actual_height + tolerance < required_height:
        raise DriverError(
            "selector pane is shorter than its complete CLIM rows: "
            f"actual={actual_height}, required={required_height}"
        )
    if not snapshot.get("top_level_grafted"):
        raise DriverError("selector top-level sheet is not grafted")
    if not snapshot.get("pointer_documentation_grafted"):
        raise DriverError("pointer-documentation sheet is not grafted")

    top_left, top_top, top_right, top_bottom = top_bounds
    mini_left, mini_top, mini_right, mini_bottom = minibuffer_bounds
    pointer_left, pointer_top, pointer_right, pointer_bottom = pointer_bounds
    if (mini_left < top_left - tolerance
            or mini_top < top_top - tolerance
            or mini_right > top_right + tolerance
            or mini_bottom > top_bottom + tolerance):
        raise DriverError(
            f"selector bounds {minibuffer_bounds} escape top-level {top_bounds}"
        )
    if (pointer_left < top_left - tolerance
            or pointer_top < top_top - tolerance
            or pointer_right > top_right + tolerance
            or pointer_bottom > top_bottom + tolerance):
        raise DriverError(
            f"pointer-documentation bounds {pointer_bounds} escape "
            f"top-level {top_bounds}"
        )
    if pointer_bottom <= pointer_top:
        raise DriverError(
            f"pointer-documentation pane is not visibly allocated: {pointer_bounds}"
        )
    if mini_bottom > pointer_top + tolerance:
        raise DriverError(
            f"selector {minibuffer_bounds} overlaps pointer help {pointer_bounds}"
        )

    top_height = top_bottom - top_top
    session.log_action(
        "assert_switch_buffer_layout",
        filtered_count=filtered_count,
        visible_count=visible_count,
        desired_rows=desired_rows,
        actual_height=actual_height,
        required_height=required_height,
        top_height=top_height,
        minibuffer_bounds=minibuffer_bounds,
        pointer_documentation_bounds=pointer_bounds,
    )
    return top_height


def assert_switch_buffer_collapsed(session: McCLIMGuiSession,
                                   snapshot: dict[str, Any],
                                   previous_top_height: float,
                                   action: str) -> None:
    """Prove cancelling or confirming returns the frame to one compact row."""
    if snapshot.get("minibuffer_active"):
        raise DriverError(f"selector remained active after {action}")
    top_bounds = snapshot_bounds(snapshot, "top_level")
    minibuffer_bounds = snapshot_bounds(snapshot, "minibuffer")
    pointer_bounds = snapshot_bounds(snapshot, "pointer_documentation")
    row_height = snapshot_number(snapshot, "minibuffer_row_height")
    top_height = top_bounds[3] - top_bounds[1]
    minibuffer_height = minibuffer_bounds[3] - minibuffer_bounds[1]
    tolerance = 1.0
    if top_height >= previous_top_height - tolerance:
        raise DriverError(
            f"frame did not collapse after {action}: "
            f"before={previous_top_height}, after={top_height}"
        )
    if minibuffer_height > row_height + tolerance:
        raise DriverError(
            f"minibuffer did not collapse to one row after {action}: "
            f"height={minibuffer_height}, row={row_height}"
        )
    if minibuffer_bounds[3] > pointer_bounds[1] + tolerance:
        raise DriverError(
            f"collapsed minibuffer overlaps pointer help after {action}"
        )
    session.log_action(
        "assert_switch_buffer_collapsed", transition=action,
        previous_top_height=previous_top_height, top_height=top_height,
        minibuffer_height=minibuffer_height,
    )


def open_switch_buffer(session: McCLIMGuiSession) -> tuple[dict[str, Any], float]:
    """Open the real C-x b selector and wait for its complete CLIM layout."""
    command = expect_key_command(
        session, ("ctrl+x", "b"), "minibuffer-select-buffer-command")
    command_sequence = event_sequence(command, "switch-buffer command")
    snapshot = wait_quiescent_snapshot_after(
        session, command_sequence, "large switch-buffer selector",
        lambda candidate: (
            candidate.get("minibuffer_active")
            and "Switch Buffer" in str(candidate.get("minibuffer_text", ""))
            and candidate.get("input_focus_pane") == "compose"
        ),
        timeout=15.0,
    )
    return snapshot, assert_switch_buffer_layout(session, snapshot)


def run_switch_buffer_stress_burst(
        session: McCLIMGuiSession) -> tuple[dict[str, Any], float]:
    """Filter and navigate a large selector under one five-second deadline."""
    keys = switch_buffer_stress_keys()
    after_sequence = session.latest_sequence()
    started = time.monotonic()
    session.type_text(SWITCH_BUFFER_STRESS_QUERY)
    session.press_keys(keys)
    remaining = SWITCH_BUFFER_STRESS_BUDGET_SECONDS - (
        time.monotonic() - started)
    if remaining <= 0:
        raise DriverError(
            "switch-buffer stress key delivery exceeded the five-second "
            "responsiveness budget"
        )
    snapshot = wait_quiescent_snapshot_after(
        session, after_sequence, "switch-buffer large-draft stress burst",
        lambda candidate: (
            candidate.get("minibuffer_active")
            and candidate.get("input_focus_pane") == "compose"
            and f"Switch Buffer: {SWITCH_BUFFER_STRESS_QUERY}" in str(
                candidate.get("minibuffer_text", ""))
            and selected_candidate_contains(candidate, "switch-e2e")
            and switch_buffer_large_draft_state_p(candidate)
        ),
        timeout=remaining,
    )
    elapsed = time.monotonic() - started
    if elapsed > SWITCH_BUFFER_STRESS_BUDGET_SECONDS:
        raise DriverError(
            "switch-buffer stress burst did not quiesce within five seconds: "
            f"elapsed={elapsed:.3f}s"
        )
    assert_switch_buffer_large_draft(snapshot, "stress burst")
    top_height = assert_switch_buffer_layout(session, snapshot)
    session.log_action(
        "assert_switch_buffer_stress_burst",
        elapsed_seconds=elapsed,
        filter_key_count=len(SWITCH_BUFFER_STRESS_QUERY),
        burst_key_count=len(keys),
        total_key_count=len(SWITCH_BUFFER_STRESS_QUERY) + len(keys),
    )
    return snapshot, top_height


def set_compose_state(session: McCLIMGuiSession, text: str, point: int) -> None:
    """Set one draft and point entirely through physical editor gestures."""
    if session.latest_snapshot().get("compose_text"):
        press_chord(session, "ctrl+a", "ctrl+k")
        wait_compose_state(session, "", 0)
    session.type_text(text)
    wait_compose_text(session, text)
    session.press("ctrl+a")
    wait_compose_state(session, text, 0)
    for _ in range(point):
        session.press("Right")
    wait_compose_state(session, text, point)


def switch_buffer_with_keyboard(session: McCLIMGuiSession, query: str,
                                expected_name: str, expected_text: str,
                                expected_point: int, *,
                                minimum_matches: int = 1,
                                expected_state: Callable[
                                    [dict[str, Any]], bool] | None = None,
                                ) -> dict[str, Any]:
    """Switch buffers through C-x b and prove filtered/closed frame geometry."""
    open_switch_buffer(session)
    after_type = session.latest_sequence()
    session.type_text(query)
    filtered = wait_quiescent_snapshot_after(
        session, after_type, f"switch-buffer candidate {query!r}",
        lambda snapshot: (
            snapshot.get("minibuffer_active")
            and selected_buffer_candidate_name(snapshot) == expected_name
        ),
        timeout=15.0,
    )
    filtered_count = int(snapshot_number(
        filtered, "minibuffer_filtered_count"))
    if filtered_count < minimum_matches:
        raise DriverError(
            f"switch-buffer query {query!r} returned {filtered_count} match(es); "
            f"expected at least {minimum_matches}"
        )
    filtered_top = snapshot_bounds(filtered, "top_level")
    filtered_height = filtered_top[3] - filtered_top[1]
    after_confirm = session.latest_sequence()
    session.press("Return")
    confirmed = wait_quiescent_snapshot_after(
        session, after_confirm, f"switched to {expected_name!r}",
        lambda snapshot: (
            snapshot.get("buffer_name") == expected_name
            and not snapshot.get("minibuffer_active")
            and (expected_state(snapshot) if expected_state is not None
                 else (snapshot.get("compose_text") == expected_text
                       and snapshot.get("compose_point") == expected_point))
        ),
        timeout=15.0,
    )
    assert_switch_buffer_collapsed(
        session, confirmed, filtered_height, "confirmation")
    return confirmed


def run_switch_buffer_regression(session: McCLIMGuiSession) -> dict[str, Any]:
    """Regress selector focus/layout and per-buffer draft/point transitions."""
    session.wait_snapshot(
        "ESA standard input initially owns keyboard focus",
        lambda snapshot: (
            snapshot.get("buffer_name") == "rplaca:e2e"
            and snapshot.get("input_focus_pane") == "standard-input"
            and switch_buffer_large_draft_state_p(snapshot)
        ),
        timeout=10.0,
    )

    expanded, _ = open_switch_buffer(session)
    assert_switch_buffer_large_draft(expanded, "selector activation")
    screenshot = session.final_state_screenshot(
        "02-switch-buffer-expanded", final_snapshot=expanded)

    _stressed, stressed_height = run_switch_buffer_stress_burst(session)
    after_cancel = session.latest_sequence()
    session.press("ctrl+g")
    cancelled = wait_quiescent_snapshot_after(
        session, after_cancel, "switch-buffer accepts C-g",
        lambda snapshot: (
            not snapshot.get("minibuffer_active")
            and snapshot.get("buffer_name") == "rplaca:e2e"
            and snapshot.get("input_focus_pane") == "compose"
            and switch_buffer_large_draft_state_p(snapshot)
        ),
        timeout=15.0,
    )
    assert_switch_buffer_large_draft(cancelled, "cancellation")
    assert_switch_buffer_collapsed(session, cancelled, stressed_height,
                                   "cancellation")

    switch_buffer_with_keyboard(
        session, "switch-e2e-0", "switch-e2e-0", "", 0,
        minimum_matches=2)
    set_compose_state(session, "alpha-tail", 5)
    switch_buffer_with_keyboard(
        session, "switch-e2e-1", "switch-e2e-1", "", 0,
        minimum_matches=2)
    set_compose_state(session, "bravo-tail", 2)
    switch_buffer_with_keyboard(
        session, "switch-e2e-0", "switch-e2e-0", "alpha-tail", 5,
        minimum_matches=2)
    switch_buffer_with_keyboard(
        session, "switch-e2e-1", "switch-e2e-1", "bravo-tail", 2,
        minimum_matches=2)
    restored_large = switch_buffer_with_keyboard(
        session, "rplaca:e2e", "rplaca:e2e", "", 0,
        expected_state=switch_buffer_large_draft_state_p)
    assert_switch_buffer_large_draft(restored_large, "buffer round trip")
    switch_buffer_with_keyboard(
        session, "switch-e2e-0", "switch-e2e-0", "alpha-tail", 5,
        minimum_matches=2)
    press_chord(session, "ctrl+a", "ctrl+k")
    wait_compose_state(session, "", 0)
    return screenshot


def press_chord(session: McCLIMGuiSession, *keys: str) -> None:
    """Press KEYS in order, allowing McCLIM time to retain prefix state."""
    for key in keys:
        session.press(key)
        time.sleep(0.08)


def expect_key_command(session: McCLIMGuiSession, keys: tuple[str, ...],
                       command: str, *, timeout: float = 10.0) -> dict[str, Any]:
    """Press KEYS and assert the normalized RPLACA keymap command fired."""
    after_sequence = session.latest_sequence()
    press_chord(session, *keys)
    return session.wait_event_after(
        "key-command", after_sequence,
        lambda event: event.get("command") == command,
        timeout=timeout,
    )


def cancel_modal_input(session: McCLIMGuiSession) -> None:
    """Cancel minibuffer/selector state if one is active."""
    if (session.latest_snapshot().get("minibuffer_active")
            or session.latest_snapshot().get("model_selector_active")
            or session.latest_snapshot().get("think_selector_active")
            or session.latest_snapshot().get("session_tree_selector_active")
            or session.latest_snapshot().get("buffer_selector_active")):
        session.press("ctrl+g")
        session.wait_snapshot("modal input cancelled",
                              lambda snapshot: not snapshot.get("minibuffer_active")
                              and not snapshot.get("model_selector_active")
                              and not snapshot.get("think_selector_active")
                              and not snapshot.get("session_tree_selector_active")
                              and not snapshot.get("buffer_selector_active"),
                              timeout=10.0)


def close_current_buffer_with_key(session: McCLIMGuiSession,
                                  expected_name: str | None = None) -> None:
    """Close the current auxiliary buffer with C-x k during scenario cleanup."""
    initial_name = session.latest_snapshot().get("buffer_name")
    press_chord(session, "ctrl+x", "k")
    if expected_name is not None:
        session.wait_snapshot(f"buffer {expected_name!r} selected after kill",
                              lambda snapshot: snapshot.get("buffer_name") == expected_name,
                              timeout=10.0)
    else:
        session.wait_snapshot("current buffer changed after kill",
                              lambda snapshot, initial_name=initial_name:
                              snapshot.get("buffer_name") != initial_name,
                              timeout=10.0)


def kill_current_buffer_with_key(session: McCLIMGuiSession,
                                 expected_name: str | None = None) -> None:
    """Assert C-x k dispatches through the RPLACA keymap and kills a buffer."""
    expect_key_command(session, ("ctrl+x", "k"), "kill-buffer-command")
    if expected_name is not None:
        session.wait_snapshot(f"buffer {expected_name!r} selected after kill",
                              lambda snapshot: snapshot.get("buffer_name") == expected_name,
                              timeout=10.0)


def run_mx(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    screenshots = prepare_session(session)

    open_mx(session)
    screenshots.append(session.screenshot("02-mx-open"))

    abbreviation = "tdbg"
    command = "toggle-debug-mode-command"
    session.type_text(abbreviation)
    session.wait_snapshot("M-x fuzzy command candidate listed",
                          lambda snapshot: (
                              f"M-x: {abbreviation}" in str(snapshot.get("minibuffer_text", ""))
                              and selected_candidate_contains(snapshot, command)
                          ),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-mx-command"))

    session.press("Return")
    final_snapshot = session.wait_snapshot(
        "M-x command executed",
        lambda snapshot: "[Debug mode ON" in str(snapshot.get("screen_text", ""))
        and not snapshot.get("minibuffer_text"),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "04-mx-result",
        final_snapshot=final_snapshot))
    return screenshots


def write_organa_fixture(session: McCLIMGuiSession) -> Path:
    """Create a deterministic organa TODO file under the artifact directory."""
    path = session.artifact_dir / "organa-tasks.org"
    path.write_text(
        """#+TITLE: E2E Project

* NEXT Implement package UI
:PROPERTIES:
:ID: implement-package-ui
:END:
* TODO Test package UI
:PROPERTIES:
:ID: test-package-ui
:ORGANA_DEPENDS: implement-package-ui
:END:
* DONE Document package UI
""",
        encoding="utf-8",
    )
    session.log_action("write_organa_fixture", path=str(path))
    return path


def run_organa(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise Organa's package-owned McCLIM presentation views."""
    screenshots = prepare_session(session)
    org_path = write_organa_fixture(session)

    run_mx_selection(session, "organa-open-todo-file-command")
    wait_minibuffer_text(session, "Organa path prompt opened",
                         lambda text: "Org TODO file path" in text)
    session.type_text(str(org_path))
    session.press("Return")
    session.wait_snapshot("Organa dashboard shown",
                          lambda snapshot: snapshot.get("buffer_name") == "organa:organa-tasks.org"
                          and snapshot.get("major_mode") == "organa"
                          and "Organa dashboard" in str(snapshot.get("screen_text", ""))
                          and "Ready next" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("02-organa-dashboard"))

    run_mx_selection(session, "organa-cycle-view-command")
    session.wait_snapshot("Organa kanban shown",
                          lambda snapshot: "Organa kanban" in str(snapshot.get("screen_text", ""))
                          and "TODO" in str(snapshot.get("screen_text", ""))
                          and "NEXT" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-organa-kanban"))

    run_mx_selection(session, "organa-cycle-view-command")
    session.wait_snapshot("Organa dependency view shown",
                          lambda snapshot: "Organa dependency" in str(snapshot.get("screen_text", ""))
                          and "Test package UI" in str(snapshot.get("screen_text", ""))
                          and "Implement package UI" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("04-organa-dependency"))

    run_mx_selection(session, "organa-cycle-view-command")
    final_snapshot = session.wait_snapshot(
        "Organa outline view shown",
        lambda snapshot: "Organa outline" in str(snapshot.get("screen_text", ""))
        and "Implement package UI" in str(snapshot.get("screen_text", ""))
        and "Test package UI" in str(snapshot.get("screen_text", "")),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "05-organa-outline",
        final_snapshot=final_snapshot))
    return screenshots


def run_quaestor(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise Quaestor's package-owned active request panel."""
    screenshots = prepare_session(session)

    session.wait_snapshot("Quaestor request panel shown",
                          lambda snapshot: "Quaestor request 1/1: Scope" in str(snapshot.get("screen_text", ""))
                          and "[x] Alpha" in str(snapshot.get("screen_text", ""))
                          and "[ ] Beta" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("02-quaestor-panel"))

    session.press("Down")
    session.wait_snapshot("Quaestor option changed",
                          lambda snapshot: "[x] Beta" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    session.press("Tab")
    session.type_text("ship it")
    session.wait_snapshot("Quaestor notes updated",
                          lambda snapshot: "Notes: ship it" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-quaestor-answering"))

    session.press("Return")
    final_snapshot = session.wait_snapshot(
        "Quaestor request answered",
        lambda snapshot: "[request_user_input answered]" in str(snapshot.get("screen_text", ""))
        and "Scope: Beta; ship it" in str(snapshot.get("screen_text", ""))
        and "Quaestor request 1/1" not in str(snapshot.get("screen_text", "")),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "04-quaestor-answered",
        final_snapshot=final_snapshot))
    return screenshots


def exercise_native_listener(session: McCLIMGuiSession) -> dict[str, Any]:
    """Evaluate a form in the native Listener frame and quit through its command."""
    deadline = time.monotonic() + 15.0
    listener_id = None
    while time.monotonic() < deadline:
        result = session.run(
            ["xdotool", "search", "--onlyvisible", "--name", "^Listener$"],
            check=False)
        candidates = [value for value in result.stdout.split()
                      if value != session.window_id]
        if candidates:
            listener_id = candidates[-1]
            break
        time.sleep(0.1)
    if listener_id is None:
        raise DriverError("native McCLIM Listener window did not open")
    session.run(["xdotool", "windowfocus", listener_id])
    marker = session.artifact_dir / "native-listener-evaluated.txt"
    marker.unlink(missing_ok=True)
    pathname = str(marker).replace("\\", "\\\\").replace('"', '\\"')
    session.type_text(
        f'(with-open-file (s "{pathname}" :direction :output '
        ':if-exists :supersede) (write-line "NATIVE_LISTENER_OK" s))')
    session.press("Return")
    deadline = time.monotonic() + 15.0
    while time.monotonic() < deadline:
        if marker.exists() and marker.read_text().strip() == "NATIVE_LISTENER_OK":
            break
        time.sleep(0.1)
    else:
        raise DriverError("native Listener did not evaluate the typed Lisp form")
    screenshot = session.screenshot("native-listener-evaluation", root=True)
    session.type_text(",Quit")
    session.press("Return")
    deadline = time.monotonic() + 10.0
    while time.monotonic() < deadline:
        result = session.run(
            ["xdotool", "getwindowname", listener_id], check=False)
        if result.returncode != 0:
            break
        time.sleep(0.1)
    else:
        raise DriverError("native Listener did not close after Quit")
    session.focus()
    session.log_action("native_listener_evaluated_and_closed",
                       window_id=listener_id, marker=str(marker))
    return screenshot


def run_keybinds(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise default keybindings through the real McCLIM/ESA GUI path."""
    screenshots = prepare_session(session)
    screenshots.append(run_switch_buffer_regression(session))
    compose_test_buffer = str(session.latest_snapshot().get("buffer_name"))

    # Drei-owned compose editing keys should edit text instead of being stolen
    # by the frame command table.
    session.type_text("one two")
    wait_compose_text(session, "one two")
    press_chord(session, "ctrl+a", "Escape", "f")
    session.type_text("X")
    wait_compose_text(session, "oneX two")
    press_chord(session, "Escape", "b")
    session.type_text("Y")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+e", "ctrl+u")
    wait_compose_text(session, "")
    session.type_text("YoneX two")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+a", "ctrl+k")
    wait_compose_text(session, "")
    session.press("ctrl+y")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+a", "Escape", "d")
    wait_compose_text(session, " two")
    session.press("ctrl+y")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+e", "ctrl+w")
    wait_compose_text(session, "YoneX ")
    session.type_text("two")
    wait_compose_text(session, "YoneX two")
    press_chord(session, "ctrl+a", "ctrl+d")
    wait_compose_text(session, "oneX two")
    session.press("BackSpace")
    wait_compose_text(session, "oneX two")
    press_chord(session, "ctrl+a", "ctrl+k")
    wait_compose_text(session, "")
    screenshots.append(session.screenshot("02-compose-keybinds"))

    # M-x and redraw are global keybindings that should dispatch from compose.
    expect_key_command(session, ("Escape", "x"), "execute-extended-command")
    wait_minibuffer_text(session, "M-x opened by keybinding",
                         lambda text: "M-x" in text)
    cancel_modal_input(session)
    expect_key_command(session, ("ctrl+l",), "redraw-screen-command")
    screenshots.append(session.screenshot("03-global-keybinds"))

    # C-c mode-specific commands.
    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "t"), "toggle-tool-results-command")
    session.wait_snapshot("tool result visibility toggled",
                          lambda snapshot: snapshot.get("show_tool_results")
                          != initial_snapshot.get("show_tool_results"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "t"), "toggle-tool-results-command")
    session.wait_snapshot("tool result visibility restored",
                          lambda snapshot: snapshot.get("show_tool_results")
                          == initial_snapshot.get("show_tool_results"),
                          timeout=10.0)

    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "shift+v"), "toggle-reasoning-output-command")
    session.wait_snapshot("reasoning visibility toggled",
                          lambda snapshot: snapshot.get("show_reasoning")
                          != initial_snapshot.get("show_reasoning"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "shift+v"), "toggle-reasoning-output-command")
    session.wait_snapshot("reasoning visibility restored",
                          lambda snapshot: snapshot.get("show_reasoning")
                          == initial_snapshot.get("show_reasoning"),
                          timeout=10.0)

    initial_snapshot = session.latest_snapshot()
    expect_key_command(session, ("ctrl+c", "shift+i"), "toggle-metadata-output-command")
    session.wait_snapshot("metadata visibility toggled",
                          lambda snapshot: snapshot.get("show_metadata")
                          != initial_snapshot.get("show_metadata"),
                          timeout=10.0)
    expect_key_command(session, ("ctrl+c", "shift+i"), "toggle-metadata-output-command")
    session.wait_snapshot("metadata visibility restored",
                          lambda snapshot: snapshot.get("show_metadata")
                          == initial_snapshot.get("show_metadata"),
                          timeout=10.0)

    expect_key_command(session, ("ctrl+c", "c"), "compact-buffer-command")
    session.wait_snapshot("compact command feedback shown",
                          lambda snapshot: "Nothing compacted" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)

    expect_key_command(session, ("ctrl+c", "shift+a"), "minibuffer-select-agent-command")
    wait_minibuffer_text(session, "agent selector opened by keybinding",
                         lambda text: "Select Agent" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "s"), "minibuffer-insert-skill-command")
    session.wait_snapshot("insert-skill command handled",
                          lambda snapshot: "Insert Skill" in str(snapshot.get("minibuffer_text", ""))
                          or "No enabled skills" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+s"), "minibuffer-toggle-skill-command")
    session.wait_snapshot("toggle-skill command handled",
                          lambda snapshot: "Toggle Skill" in str(snapshot.get("minibuffer_text", ""))
                          or "No skills" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "ctrl+Return"), "minibuffer-select-model-command")
    wait_minibuffer_text(session, "model selector opened by keybinding",
                         lambda text: "Select Model" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+m"), "select-model-command")
    wait_minibuffer_text(session, "compatibility model selector opened by keybinding",
                         lambda text: "Select Model" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "ctrl+r"), "minibuffer-select-think-level-command")
    session.wait_snapshot("minibuffer think command handled",
                          lambda snapshot: "Think levels" in str(snapshot.get("screen_text", ""))
                          or "Select Think Level" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+c", "shift+r"), "select-think-level-command")
    session.wait_snapshot("compatibility think command handled",
                          lambda snapshot: "Think levels" in str(snapshot.get("screen_text", ""))
                          or "Select Think Level" in str(snapshot.get("minibuffer_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)
    screenshots.append(session.screenshot("04-ctrl-c-keybinds"))

    # C-h help prefix and C-c compatibility aliases.
    for keys, command, prompt in [
        (("ctrl+h", "f"), "describe-function-command", "Describe Function"),
        (("ctrl+h", "v"), "describe-variable-command", "Describe Variable"),
        (("ctrl+h", "shift+t"), "describe-type-command", "Describe Type"),
        (("ctrl+c", "f"), "describe-function-command", "Describe Function"),
        (("ctrl+c", "v"), "describe-variable-command", "Describe Variable"),
        (("ctrl+c", "shift+t"), "describe-type-command", "Describe Type"),
    ]:
        expect_key_command(session, keys, command)
        wait_minibuffer_text(session, f"{command} prompt opened", lambda text, prompt=prompt: prompt in text)
        cancel_modal_input(session)

    for keys, command in [
        (("ctrl+h", "b"), "describe-bindings-command"),
        (("ctrl+c", "b"), "describe-bindings-command"),
    ]:
        expect_key_command(session, keys, command)
        session.wait_snapshot("keybindings help shown",
                              lambda snapshot: snapshot.get("buffer_name") == "*help:keybindings*"
                              and "Key Bindings" in str(snapshot.get("screen_text", "")),
                              timeout=10.0)
        close_current_buffer_with_key(session, compose_test_buffer)

    for keys, command in [
        (("ctrl+h", "i"), "info-directory-command"),
        (("ctrl+h", "shift+i"), "rplaca-manual-command"),
    ]:
        expect_key_command(session, keys, command)
        session.wait_snapshot("info buffer shown",
                              lambda snapshot: snapshot.get("major_mode") == "info",
                              timeout=10.0)
        close_current_buffer_with_key(session, compose_test_buffer)
    screenshots.append(session.screenshot("05-help-keybinds"))

    # C-x global buffer/session/file commands.  C-x b is the regression path:
    # it must open the ESA minibuffer selector rather than crash the frame.
    expect_key_command(session, ("ctrl+x", "n"), "new-buffer-command")
    session.wait_snapshot("new buffer created by C-x n",
                          lambda snapshot: snapshot.get("buffer_name") == "session-1",
                          timeout=10.0)
    expect_key_command(session, ("ctrl+x", "b"), "minibuffer-select-buffer-command")
    wait_minibuffer_text(session, "C-x b buffer selector opened",
                         lambda text: "Switch Buffer" in text)
    session.type_text("rplaca")
    session.wait_snapshot("original buffer selected in C-x b selector",
                          lambda snapshot: selected_candidate_contains(snapshot, "rplaca:e2e"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("C-x b switched back to original buffer",
                          lambda snapshot: snapshot.get("buffer_name") == "rplaca:e2e",
                          timeout=10.0)

    expect_key_command(session, ("ctrl+x", "ctrl+b"), "minibuffer-select-buffer-command")
    wait_minibuffer_text(session, "C-x C-b buffer selector opened",
                         lambda text: "Switch Buffer" in text)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "p"), "minibuffer-select-project-command")
    session.wait_snapshot("project selector keybinding handled",
                          lambda snapshot: "Select Project" in str(snapshot.get("minibuffer_text", ""))
                          or "No projects" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "ctrl+f"), "open-project-file-command")
    session.wait_snapshot("open-project-file keybinding handled",
                          lambda snapshot: snapshot.get("minibuffer_active")
                          or "No projects" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "l"), "new-listener-buffer-command")
    screenshots.append(exercise_native_listener(session))

    expect_key_command(session, ("ctrl+x", "shift+f"), "font-editor-command")
    session.wait_snapshot("font editor opened",
                          lambda snapshot: snapshot.get("major_mode") == "font-editor",
                          timeout=10.0)
    close_current_buffer_with_key(session)

    expect_key_command(session, ("ctrl+x", "ctrl+s"), "save-session-command")
    session.wait_snapshot("save-session feedback shown",
                          lambda snapshot: "Session saved to" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)

    expect_key_command(session, ("ctrl+x", "ctrl+r"), "load-session-command")
    session.wait_snapshot("load-session keybinding handled",
                          lambda snapshot: "Load Session" in str(snapshot.get("minibuffer_text", ""))
                          or "No saved sessions" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "t"), "session-tree-command")
    session.wait_snapshot("session-tree keybinding handled",
                          lambda snapshot: snapshot.get("session_tree_selector_active")
                          or "Current session has no tree entries" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "shift+t"), "fork-session-command")
    session.wait_snapshot("fork-session keybinding handled",
                          lambda snapshot: snapshot.get("session_tree_selector_active")
                          or "Current session has no tree entries" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    cancel_modal_input(session)

    expect_key_command(session, ("ctrl+x", "n"), "new-buffer-command")
    temp_name = str(session.latest_snapshot().get("buffer_name"))
    kill_current_buffer_with_key(session)
    final_snapshot = session.wait_snapshot(
        "temporary buffer killed by C-x k",
        lambda snapshot, temp_name=temp_name:
        snapshot.get("buffer_name") != temp_name,
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "06-ctrl-x-keybinds",
        final_snapshot=final_snapshot))

    return screenshots


def run_compose_geometry_regression(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise M-x over the smallest failing one-line Drei drafts.

    100 characters is the adjacent safe boundary.  101 is the minimal crash
    trigger seen in the application and 112 is the user's reported input.  The
    selector remains open through a real bounded resize, then C-g must retain
    the exact draft, point, and compose focus.  Repeating each path catches a
    layout/repaint race without reaching into Drei's renderer.
    """
    screenshots = prepare_session(session)
    for iteration in range(2):
        for length in COMPOSE_MX_REGRESSION_LENGTHS:
            draft = "x" * length
            if session.latest_snapshot().get("compose_text"):
                press_chord(session, "ctrl+a", "ctrl+k")
                wait_compose_state(session, "", 0)
            session.type_text(draft)
            wait_compose_state(session, draft, length)

            command = expect_key_command(
                session, ("Escape", "x"), "execute-extended-command")
            opened = wait_quiescent_snapshot_after(
                session, event_sequence(command, "M-x command"),
                f"M-x open over {length}-character draft (iteration {iteration + 1})",
                lambda snapshot: (
                    snapshot.get("minibuffer_active")
                    and "M-x" in str(snapshot.get("minibuffer_text", ""))
                    and snapshot.get("compose_text") == draft
                    and snapshot.get("compose_point") == length
                    and snapshot.get("input_focus_pane") == "compose"
                ),
                timeout=15.0,
            )
            if length == 112 and iteration == 0:
                screenshots.append(session.final_state_screenshot(
                    "02-mx-112-open", final_snapshot=opened, timeout=15.0))

            # This reaches the actual CLX expose/repaint path while the
            # selector is active. Keep both dimensions comfortably inside the
            # private 1280x900 Xvfb screen.
            session.resize(960 - iteration * 80, 720 + iteration * 40)
            session.x_request_reply_barrier()
            time.sleep(0.35)
            cancel_modal_input(session)
            cancelled = session.wait_snapshot(
                f"M-x cancelled over {length}-character draft (iteration {iteration + 1})",
                lambda snapshot: (
                    not snapshot.get("minibuffer_active")
                    and snapshot.get("compose_text") == draft
                    and snapshot.get("compose_point") == length
                    and snapshot.get("input_focus_pane") == "compose"
                ),
                timeout=15.0,
            )
            if length == 112 and iteration == 1:
                screenshots.append(session.final_state_screenshot(
                    "03-mx-112-cancelled", final_snapshot=cancelled,
                    timeout=15.0))
    assert_no_stability_failure_signatures(session)
    return screenshots


def run_features(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise broad no-network GUI feature coverage in one RPLACA session."""
    screenshots = prepare_session(session)

    # Compose-pane editing: ordinary typing, C-a/C-e movement, C-b movement,
    # and C-k killing all text from the beginning of the line.
    session.type_text("abc")
    wait_compose_text(session, "abc")
    session.press("ctrl+a")
    session.type_text("X")
    wait_compose_text(session, "Xabc")
    session.press("ctrl+e")
    session.type_text("Y")
    wait_compose_text(session, "XabcY")
    session.press("ctrl+b")
    session.type_text("Z")
    wait_compose_text(session, "XabcZY")
    session.press("ctrl+a")
    session.press("ctrl+k")
    wait_compose_text(session, "")
    screenshots.append(session.screenshot("02-compose-editing"))

    # Minibuffer editing while M-x is active, including C-b and ESC-b (M-b).
    open_mx(session)
    session.type_text("foo bar")
    wait_minibuffer_text(session, "M-x text typed",
                         lambda text: "M-x: foo bar" in text)
    session.press("ctrl+b")
    session.press("ctrl+b")
    session.type_text("Z")
    wait_minibuffer_text(session, "C-b moved minibuffer point",
                         lambda text: "M-x: foo bZar" in text)
    session.press("Escape")
    session.press("b")
    session.type_text("X")
    wait_minibuffer_text(session, "M-b moved minibuffer point",
                         lambda text: "M-x: foo XbZar" in text)
    session.press("ctrl+a")
    session.type_text("S")
    session.press("ctrl+e")
    session.type_text("E")
    wait_minibuffer_text(session, "C-a/C-e moved minibuffer point",
                         lambda text: "M-x: Sfoo XbZarE" in text)
    session.press("ctrl+u")
    wait_minibuffer_text(session, "C-u cleared M-x input",
                         lambda text: "M-x: " in text and "No matches" not in text)
    session.type_text("tdbg")
    session.wait_snapshot("M-x fuzzy debug command selected",
                          lambda snapshot: selected_candidate_contains(
                              snapshot, "toggle-debug-mode-command"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("debug mode toggled by edited M-x",
                          lambda snapshot: "[Debug mode ON" in str(snapshot.get("screen_text", ""))
                          and not snapshot.get("minibuffer_text"),
                          timeout=10.0)
    screenshots.append(session.screenshot("03-minibuffer-editing"))

    # Help/introspection through the fuzzy command selector.
    run_mx_selection(session, "describe-bindings-command")
    session.wait_snapshot("keybindings help buffer shown",
                          lambda snapshot: snapshot.get("buffer_name") == "*help:keybindings*"
                          and "execute-extended-command" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("04-help-bindings"))

    # Buffer management and fuzzy buffer selector.
    run_mx_selection(session, "new-buffer-command")
    session.wait_snapshot("new buffer selected",
                          lambda snapshot: snapshot.get("buffer_name") == "session-1"
                          and snapshot.get("major_mode") == "chat",
                          timeout=10.0)
    run_mx_selection(session, "minibuffer-select-buffer-command")
    wait_minibuffer_text(session, "buffer selector opened",
                         lambda text: "Switch Buffer" in text)
    session.type_text("rplaca")
    session.wait_snapshot("original e2e buffer candidate selected",
                          lambda snapshot: selected_candidate_contains(snapshot, "rplaca:e2e"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("original e2e buffer restored",
                          lambda snapshot: snapshot.get("buffer_name") == "rplaca:e2e",
                          timeout=10.0)
    screenshots.append(session.screenshot("05-buffer-selector"))

    # Agent/model/think selectors without real provider credentials.
    run_mx_selection(session, "minibuffer-select-agent-command")
    wait_minibuffer_text(session, "agent selector opened",
                         lambda text: "Select Agent" in text)
    session.type_text("agent")
    session.wait_snapshot("agent candidate selected",
                          lambda snapshot: selected_candidate_contains(snapshot, "agent"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("agent selector closed",
                          lambda snapshot: not snapshot.get("minibuffer_text")
                          and "agent agent" in str(snapshot.get("info_text", "")),
                          timeout=10.0)

    run_mx_selection(session, "minibuffer-select-model-command")
    wait_minibuffer_text(session, "model selector opened",
                         lambda text: "Select Model" in text)
    session.type_text("e2e")
    session.wait_snapshot("e2e model candidate selected",
                          lambda snapshot: selected_candidate_contains(snapshot, "e2e/e2e-model"),
                          timeout=10.0)
    session.press("Return")
    session.wait_snapshot("e2e model selected",
                          lambda snapshot: "[Model changed to e2e/e2e-model" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)

    run_mx_selection(session, "minibuffer-select-think-level-command")
    session.wait_snapshot("think-level unavailable message shown",
                          lambda snapshot: "Think levels not available" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    screenshots.append(session.screenshot("06-agent-model-think"))

    # Prompted commands and session persistence feedback.
    run_mx_selection(session, "set-session-display-name-command")
    wait_minibuffer_text(session, "display-name prompt opened",
                         lambda text: "Session display name" in text)
    session.type_text("E2E Label")
    session.press("Return")
    session.wait_snapshot("display name set",
                          lambda snapshot: "[Session display name: E2E Label]" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    run_mx_selection(session, "save-session-command")
    expected_session_dir = session.artifact_dir / "home" / ".config" / "rplaca" / "sessions"
    session.wait_snapshot("session saved under isolated artifact home",
                          lambda snapshot: (
                              "[Session saved to" in str(snapshot.get("screen_text", ""))
                              and str(expected_session_dir)
                              in str(snapshot.get("screen_text", ""))
                          ),
                          timeout=10.0)
    screenshots.append(session.screenshot("07-session-commands"))

    # Offline help/dashboard features that do not require credentials.
    run_mx_selection(session, "list-skills-command")
    session.wait_snapshot("skills help buffer shown",
                          lambda snapshot: snapshot.get("buffer_name") == "*help:skills*"
                          and "Skills" in str(snapshot.get("screen_text", "")),
                          timeout=10.0)
    run_mx_selection(session, "package-dashboard-command")
    final_snapshot = session.wait_snapshot(
        "package dashboard shown",
        lambda snapshot: snapshot.get("buffer_name") == "*Packages*"
        and snapshot.get("major_mode") == "package-dashboard"
        and "Packages for" in str(snapshot.get("screen_text", "")),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "08-offline-help-dashboards",
        final_snapshot=final_snapshot))

    return screenshots


def run_reload(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Exercise safe in-place reload through the semantic GUI harness."""
    screenshots = prepare_session(session)

    session.type_text("reload draft remains visible")
    wait_compose_text(session, "reload draft remains visible")
    screenshots.append(session.screenshot("02-before-safe-reload"))

    after_sequence = session.latest_sequence()
    run_mx_selection(session, "safe-reload-rplaca-command", timeout=20.0)
    session.wait_event_after(
        "ui-snapshot", after_sequence,
        lambda snapshot: (
            snapshot.get("buffer_name") == "rplaca:e2e"
            and snapshot.get("status") == "reloading"
            and snapshot.get("compose_text") == "reload draft remains visible"
            and " reloading " in str(snapshot.get("info_text", ""))
            and "RPLACA safe reload started" in str(snapshot.get("screen_text", ""))
        ),
        timeout=15.0,
    )
    screenshots.append(session.screenshot("03-safe-reload-started"))
    session.wait_event_after(
        "safe-reload-result", after_sequence,
        lambda event: event.get("status") == "ok",
        timeout=90.0,
    )
    final_snapshot = session.wait_snapshot(
        "safe reload success notification visible",
        lambda snapshot: (
            snapshot.get("buffer_name") == "rplaca:e2e"
            and snapshot.get("status") == "idle"
            and snapshot.get("compose_text") == "reload draft remains visible"
            and "RPLACA safe reload succeeded" in str(snapshot.get("screen_text", ""))
        ),
        timeout=30.0,
    )
    screenshots.append(session.final_state_screenshot(
        "04-after-safe-reload",
        final_snapshot=final_snapshot,
        timeout=30.0))
    return screenshots


def run_appearance_stage(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Prove staging, restart-only application, editor semantics, and save.

    The surrounding shell harness relaunches the real application with this
    same artifact-private HOME for ``appearance-restart``.  This phase proves
    that a dark surface candidate remains a frame-local staged value after an
    Apply attempt: it must not be partly painted into the live classic frame.
    """
    screenshots = prepare_session(session)
    initial = session.latest_snapshot()
    if initial.get("appearance_active_theme") != "classic":
        raise DriverError(f"appearance cold start was not classic: {initial!r}")
    if initial.get("appearance_staged_theme") is not None:
        raise DriverError("appearance cold start unexpectedly retained staging")
    if initial.get("appearance_catalog_generation") is None:
        raise DriverError("appearance snapshot has no catalog generation")
    screenshots.append(session.screenshot("02-appearance-classic-cold-start"))

    # The frame event owns the real CLX inventory refresh.  The deterministic
    # unit fixture covers sorting/negative-cache semantics; this live check
    # proves the public port enumeration route advances only this frame's
    # generation without invalidating McCLIM's live shared font mappings.  The
    # separate pinned-font probe is intentionally stricter and requires native
    # TrueType listed sizes plus a complete mapping/metrics round trip.
    initial_font_generation = initial.get("appearance_font_inventory_generation")
    if not isinstance(initial_font_generation, int):
        raise DriverError("appearance snapshot has no font inventory generation")
    before_font_refresh = session.latest_sequence()
    run_mx_selection(session, "refresh-font-inventory-command")
    session.wait_event_after(
        "font-inventory-refresh", before_font_refresh,
        lambda event: (event.get("status") == "ready"
                       and event.get("generation") == initial_font_generation + 1
                       and isinstance(event.get("choice_count"), int)
                       and event.get("choice_count") >= 0),
        timeout=20.0)
    session.press("ctrl+l")
    refreshed = session.wait_snapshot(
        "real public CLX font inventory refreshed",
        lambda snapshot: (
            snapshot.get("appearance_font_inventory_generation")
            == initial_font_generation + 1
            and isinstance(snapshot.get("appearance_font_choice_count"), int)
            and snapshot.get("appearance_font_choice_count") >= 0),
        timeout=20.0)
    screenshots.append(session.final_state_screenshot(
        "02-appearance-font-inventory", final_snapshot=refreshed))

    # Both compatibility bindings must reach the ordinary CLIM command that
    # opens the presentation-backed editor.  Close it between invocations so
    # focus and the compose command table are exercised twice.
    original_buffer = str(initial.get("buffer_name"))
    for index, keys in enumerate((("ctrl+h", "shift+f"),
                                  ("ctrl+c", "shift+f")), start=1):
        # The command switches buffers before the compose-key instrumentation
        # can emit its optional key-command acknowledgement.  The semantic
        # appearance-editor snapshot is the authoritative proof that each
        # compatibility chord reached the command.
        for key in keys:
            session.press(key)
        session.wait_snapshot(
            f"appearance editor opened by compatibility binding {index}",
            lambda snapshot: (
                snapshot.get("major_mode") == "appearance-editor"
                and snapshot.get("input_focus_pane") == "compose"
                and "Appearance" in str(snapshot.get("screen_text", ""))
                and "Preview roles" in str(snapshot.get("screen_text", ""))
                and "Apply staged appearance" in str(snapshot.get("screen_text", ""))),
            timeout=10.0)
        screenshots.append(session.screenshot(
            f"02-appearance-editor-binding-{index}"))
        close_current_buffer_with_key(session, original_buffer)

    run_mx_selection(session, "switch-appearance-theme-command")
    wait_minibuffer_text(session, "appearance theme prompt opened",
                         lambda text: "Appearance theme" in text)
    session.type_text("dark")
    session.press("Return")
    staged = session.wait_snapshot(
        "dark appearance is staged without live mutation",
        lambda snapshot: (
            snapshot.get("appearance_active_theme") == "classic"
            and snapshot.get("appearance_staged_theme") == "dark"
            and snapshot.get("appearance_profile_revision") == 0),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "03-appearance-dark-staged", final_snapshot=staged))

    run_mx_selection(session, "customize-appearance-command")
    editor = session.wait_snapshot(
        "staged dark profile shown by appearance presentations",
        lambda snapshot: (
            snapshot.get("major_mode") == "appearance-editor"
            and "Active: classic" in str(snapshot.get("screen_text", ""))
            and "Staged: dark" in str(snapshot.get("screen_text", ""))
            and "Preview roles" in str(snapshot.get("screen_text", ""))),
        timeout=10.0)
    screenshots.append(session.final_state_screenshot(
        "04-appearance-editor-preview", final_snapshot=editor))

    run_mx_selection(session, "apply-staged-appearance-command")
    # The queued frame-process event need not produce visible text, so request
    # an ordinary CLIM redisplay and observe its snapshot rather than relying
    # on a timing-sensitive pixel transition.
    session.press("ctrl+l")
    after_apply = session.wait_snapshot(
        "dark surface apply remains restart required",
        lambda snapshot: (
            snapshot.get("appearance_active_theme") == "classic"
            and snapshot.get("appearance_staged_theme") == "dark"
            and snapshot.get("appearance_activation_status") == "restart-required"
            and snapshot.get("appearance_activation_classification")
            == "restart-required"
            and snapshot.get("appearance_profile_revision") == 0),
        timeout=15.0)
    screenshots.append(session.final_state_screenshot(
        "05-appearance-restart-required", final_snapshot=after_apply))

    run_mx_selection(session, "save-appearance-command")
    session.press("ctrl+l")
    saved = session.wait_snapshot(
        "staged dark profile persisted without live mutation",
        lambda snapshot: (
            snapshot.get("appearance_active_theme") == "classic"
            and snapshot.get("appearance_staged_theme") == "dark"
            and snapshot.get("appearance_persisted_theme") == "dark"),
        timeout=15.0)
    screenshots.append(session.final_state_screenshot(
        "06-appearance-dark-saved", final_snapshot=saved))
    return screenshots


def run_appearance_restart(session: McCLIMGuiSession) -> list[dict[str, Any]]:
    """Prove a separate real process reads the explicit saved dark profile."""
    screenshots = prepare_session(session)
    # PREPARE_SESSION may observe a complete first-paint snapshot.  Request one
    # ordinary CLIM redisplay so WAIT_SNAPSHOT proves the restarted process
    # remains coherent after expose rather than waiting for an event that need
    # not otherwise occur in an idle frame.
    session.press("ctrl+l")
    restarted = session.wait_snapshot(
        "saved dark profile activated at real application startup",
        lambda snapshot: (
            snapshot.get("appearance_active_theme") == "dark"
            and snapshot.get("appearance_staged_theme") is None
            and snapshot.get("appearance_bundle_theme") == "dark"
            and snapshot.get("appearance_bundle_catalog_generation")
            == snapshot.get("appearance_catalog_generation")
            and snapshot.get("appearance_bundle_profile_revision")
            == snapshot.get("appearance_profile_revision")
            and snapshot.get("appearance_bundle_font_inventory_generation")
            == snapshot.get("appearance_font_inventory_generation")
            and snapshot.get("appearance_transcript_surface") is not None
            and snapshot.get("appearance_compose_surface") is not None),
        timeout=15.0)
    screenshots.append(session.final_state_screenshot(
        "02-appearance-dark-restarted", final_snapshot=restarted))
    return screenshots


def write_summary(path: Path, *, ok: bool, suite: str, artifact_dir: Path,
                  steps: list[dict[str, Any]], screenshots: list[dict[str, Any]],
                  failure: str | None = None,
                  last_snapshot: dict[str, Any] | None = None) -> None:
    payload: dict[str, Any] = {
        "ok": ok,
        "suite": suite,
        "artifact_dir": str(artifact_dir),
        "artifact_dir_relative": str(artifact_dir).replace("/workspace/", "", 1),
        "steps": steps,
        "screenshots": screenshots,
    }
    if failure is not None:
        payload["failure"] = failure
    if last_snapshot is not None:
        payload["last_snapshot"] = last_snapshot
    with path.open("w", encoding="utf-8") as f:
        json.dump(payload, f, indent=2, sort_keys=True)
        f.write("\n")


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--suite", default="smoke")
    parser.add_argument("--artifact-dir", required=True)
    parser.add_argument("--debug-log", required=True)
    parser.add_argument("--window-title", default="RPLACA E2E")
    parser.add_argument("--window-id", default=None)
    args = parser.parse_args(argv)

    artifact_dir = Path(args.artifact_dir)
    artifact_dir.mkdir(parents=True, exist_ok=True)
    summary_path = artifact_dir / "summary.json"
    screenshots: list[dict[str, Any]] = []

    window_id = args.window_id
    if not window_id:
        result = subprocess.run(["xdotool", "search", "--name", args.window_title],
                                text=True, stdout=subprocess.PIPE,
                                stderr=subprocess.PIPE, check=False)
        if result.returncode != 0 or not result.stdout.strip():
            raise DriverError(f"could not find window titled {args.window_title!r}")
        window_id = result.stdout.split()[0]

    session = McCLIMGuiSession(window_id=window_id,
                               window_title=args.window_title,
                               debug_log=Path(args.debug_log),
                               artifact_dir=artifact_dir)
    try:
        if args.suite == "smoke":
            screenshots = run_smoke(session)
        elif args.suite == "mx":
            screenshots = run_mx(session)
        elif args.suite == "features":
            screenshots = run_features(session)
        elif args.suite == "keybinds":
            screenshots = run_keybinds(session)
        elif args.suite == "compose-geometry":
            screenshots = run_compose_geometry_regression(session)
        elif args.suite == "organa":
            screenshots = run_organa(session)
        elif args.suite == "quaestor":
            screenshots = run_quaestor(session)
        elif args.suite == "reload":
            screenshots = run_reload(session)
        elif args.suite == "appearance-stage":
            screenshots = run_appearance_stage(session)
        elif args.suite == "appearance-restart":
            screenshots = run_appearance_restart(session)
        elif args.suite == "stability":
            screenshots = run_stability(session)
        elif args.suite == "menu-boundaries":
            screenshots = run_menu_boundaries(session)
        else:
            raise DriverError(f"unsupported suite: {args.suite}")
        # Every successful scenario finishes through the same public CLIM
        # command.  FRAME-STOPPED is emitted after RUN-FRAME-TOP-LEVEL unwinds,
        # so the shell harness can then require a natural zero-status process
        # exit instead of relying on its emergency EXIT trap.
        request_frame_exit(session)
        write_summary(summary_path, ok=True, suite=args.suite,
                      artifact_dir=artifact_dir, steps=session.steps,
                      screenshots=screenshots,
                      last_snapshot=session.latest_snapshot())
        return 0
    except Exception as exc:  # noqa: BLE001 - script must preserve artifacts on any failure.
        try:
            screenshots.append(session.screenshot("failure"))
        except Exception:
            pass
        (artifact_dir / "debug.tail").write_text(session.debug_tail(), encoding="utf-8")
        last_snapshot = session.latest_event("ui-snapshot")
        write_summary(summary_path, ok=False, suite=args.suite,
                      artifact_dir=artifact_dir, steps=session.steps,
                      screenshots=screenshots, failure=str(exc),
                      last_snapshot=last_snapshot)
        print(f"GUI E2E failure: {exc}", file=sys.stderr)
        if last_snapshot:
            print(json.dumps(last_snapshot, indent=2, sort_keys=True), file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
