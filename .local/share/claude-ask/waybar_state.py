"""Waybar state file writer for claude-ask.

Writes a JSON state file that the Waybar module script reads.
Signals Waybar to refresh immediately after writing.
"""

import json
import logging
import subprocess
from pathlib import Path

log = logging.getLogger("claude-ask")

STATE_FILE = Path.home() / ".local" / "state" / "claude-ask" / "waybar.json"
WAYBAR_SIGNAL = 12  # SIGRTMIN+12


class WaybarState:
    """Manage the Waybar state file."""

    def __init__(self):
        self._state = {
            "status": "idle",
            "speak_enabled": False,
            "usage": {
                "session_cost": "$0.00",
                "last_query_cost": "$0.00",
                "total_tokens": 0,
            },
        }
        STATE_FILE.parent.mkdir(parents=True, exist_ok=True)
        self._write()

    @property
    def speak_enabled(self) -> bool:
        return self._state["speak_enabled"]

    @speak_enabled.setter
    def speak_enabled(self, val: bool):
        self._state["speak_enabled"] = val
        self._write()

    @property
    def status(self) -> str:
        return self._state["status"]

    def set_status(self, status: str):
        """Set status to idle, thinking, or speaking. Writes + signals."""
        self._state["status"] = status
        self._write()

    def update_usage(self, query_cost: float, total_tokens: int):
        """Update usage after an API call. Accumulates session cost."""
        current = float(self._state["usage"]["session_cost"].replace("$", ""))
        current += query_cost
        self._state["usage"]["session_cost"] = f"${current:.2f}"
        self._state["usage"]["last_query_cost"] = f"${query_cost:.4f}"
        self._state["usage"]["total_tokens"] = total_tokens
        self._write()

    def reload_speak_enabled(self):
        """Re-read the state file to pick up toggle changes from external scripts."""
        try:
            if STATE_FILE.exists():
                data = json.loads(STATE_FILE.read_text())
                self._state["speak_enabled"] = data.get("speak_enabled", False)
        except (json.JSONDecodeError, OSError):
            pass

    def _write(self):
        """Write state to disk and signal Waybar."""
        try:
            STATE_FILE.write_text(json.dumps(self._state, indent=2))
        except OSError:
            log.exception("Failed to write waybar state")
        self._signal_waybar()

    def _signal_waybar(self):
        """Send SIGRTMIN+12 to waybar for immediate refresh."""
        try:
            subprocess.Popen(
                ["pkill", f"-SIGRTMIN+{WAYBAR_SIGNAL}", "waybar"],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        except OSError:
            pass
