#!/usr/bin/env python3
"""Claude-hid daemon: evdev-based HID event dispatcher.

Grabs configured input devices exclusively, listens for key presses,
and dispatches actions (e.g. triggering claude-voice listen) based on
config-driven bindings. Handles hot-plug by re-scanning when devices
disconnect or aren't found.
"""

import json
import logging
import os
import re
import select
import socket
import time
import tomllib
from pathlib import Path

import evdev

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%H:%M:%S",
)
log = logging.getLogger("claude-hid")

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------

CONFIG_PATH = Path.home() / ".config" / "claude-hid" / "config.toml"
DEBOUNCE_MS = 500
SCAN_INTERVAL = 5


def load_config():
    """Load bindings from config.toml."""
    with open(CONFIG_PATH, "rb") as f:
        return tomllib.load(f)


# ---------------------------------------------------------------------------
# Action dispatch
# ---------------------------------------------------------------------------

def get_voice_socket_path():
    """Return path to the claude-voice control socket."""
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-voice.sock")


def dispatch_action(action):
    """Dispatch a bound action."""
    if action == "claude-voice-listen":
        sock_path = get_voice_socket_path()
        payload = json.dumps({"action": "listen"}).encode("utf-8")
        try:
            sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
            sock.connect(sock_path)
            sock.sendall(payload)
            sock.close()
            log.info("Sent listen to claude-voice")
        except Exception:
            log.exception("Failed to send to claude-voice at %s", sock_path)
    else:
        log.warning("Unknown action: %s", action)


# ---------------------------------------------------------------------------
# Device scanning
# ---------------------------------------------------------------------------

def scan_devices(bindings):
    """Scan /sys/class/input for devices matching binding regexes.

    Returns list of (evdev.InputDevice, binding_dict) tuples with devices
    already grabbed.
    """
    matched = []
    for binding in bindings:
        pattern = re.compile(binding["device_match"])
        for path in sorted(evdev.list_devices()):
            dev = evdev.InputDevice(path)
            if not pattern.search(dev.name):
                dev.close()
                continue
            try:
                dev.grab()
                matched.append((dev, binding))
                log.info("Grabbed %s (%s)", dev.name, dev.path)
            except OSError:
                log.warning("Failed to grab %s (%s)", dev.name, dev.path)
                dev.close()
    return matched


# ---------------------------------------------------------------------------
# Main loop
# ---------------------------------------------------------------------------

def run():
    config = load_config()
    bindings = config.get("binding", [])
    if not bindings:
        log.error("No bindings configured in %s", CONFIG_PATH)
        return

    log.info("Loaded %d binding(s)", len(bindings))

    last_trigger = 0.0
    devices = []  # list of (InputDevice, binding)
    was_waiting = False

    while True:
        # Scan for devices if we have none
        if not devices:
            devices = scan_devices(bindings)

        if not devices:
            if not was_waiting:
                log.info("No matching devices found, waiting...")
                was_waiting = True
            time.sleep(SCAN_INTERVAL)
            continue

        if was_waiting:
            log.info("Device(s) found, resuming")
            was_waiting = False

        # Build fd -> (device, binding) map
        fd_map = {dev.fd: (dev, binding) for dev, binding in devices}

        try:
            r, _, _ = select.select(fd_map.keys(), [], [], SCAN_INTERVAL)
        except (ValueError, OSError):
            # Bad file descriptor — a device disconnected
            _close_all(devices)
            devices = []
            continue

        for fd in r:
            dev, binding = fd_map[fd]
            try:
                for event in dev.read():
                    if event.type != evdev.ecodes.EV_KEY:
                        continue
                    if event.value != 1:  # press only
                        continue

                    key_name = evdev.ecodes.KEY.get(event.code)
                    if isinstance(key_name, list):
                        key_name = key_name[0]

                    if key_name != binding["key"]:
                        continue

                    # Debounce
                    now = time.monotonic()
                    if (now - last_trigger) < (DEBOUNCE_MS / 1000.0):
                        log.debug("Debounced %s", key_name)
                        continue
                    last_trigger = now

                    log.info("Key %s on %s -> %s", key_name, dev.name, binding["action"])
                    dispatch_action(binding["action"])

            except OSError:
                log.warning("Device disconnected: %s", dev.name)
                dev.close()
                devices = [(d, b) for d, b in devices if d is not dev]
                break  # Re-enter select with remaining devices


def _close_all(devices):
    """Close all grabbed devices."""
    for dev, _ in devices:
        try:
            dev.close()
        except Exception:
            pass


if __name__ == "__main__":
    run()
