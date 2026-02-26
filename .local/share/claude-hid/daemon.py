#!/usr/bin/env python3
"""claude-hid: listen for HID key events and trigger actions."""

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

CONFIG_PATH = Path.home() / ".config" / "claude-hid" / "config.toml"
SCAN_INTERVAL = 5
DEBOUNCE_SECS = 0.5


def load_config():
    """Load bindings from config.toml."""
    with open(CONFIG_PATH, "rb") as f:
        config = tomllib.load(f)

    bindings = []
    for b in config.get("binding", []):
        key_name = b["key"]
        key_code = evdev.ecodes.ecodes.get(key_name)
        if key_code is None:
            log.warning("Unknown key name %r, skipping binding", key_name)
            continue
        bindings.append({
            "device_re": re.compile(b["device_match"]),
            "key_code": key_code,
            "key_name": key_name,
            "action": b["action"],
        })

    log.info("Loaded %d binding(s)", len(bindings))
    return bindings


def find_devices(bindings):
    """Scan /sys/class/input for devices matching any binding."""
    matched = []
    for path in sorted(Path("/sys/class/input").iterdir()):
        if not path.name.startswith("event"):
            continue
        try:
            name = (path / "device" / "name").read_text().strip()
        except (FileNotFoundError, PermissionError):
            continue

        dev_bindings = [b for b in bindings if b["device_re"].search(name)]
        if dev_bindings:
            dev_path = f"/dev/input/{path.name}"
            try:
                dev = evdev.InputDevice(dev_path)
                matched.append((dev, dev_bindings))
                log.info("Matched device: %s (%s)", name, dev_path)
            except (PermissionError, OSError) as e:
                log.warning("Cannot open %s: %s", dev_path, e)

    return matched


def grab_devices(device_list):
    """Grab all devices exclusively."""
    grabbed = []
    for dev, bindings in device_list:
        try:
            dev.grab()
            grabbed.append((dev, bindings))
            log.info("Grabbed %s (%s)", dev.name, dev.path)
        except OSError as e:
            log.warning("Cannot grab %s: %s", dev.path, e)
            dev.close()
    return grabbed


def _get_voice_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-voice.sock")


def action_claude_voice_listen():
    """Send a listen request to the claude-voice daemon."""
    sock_path = _get_voice_socket_path()
    payload = json.dumps({"action": "listen"})
    try:
        with socket.socket(socket.AF_UNIX, socket.SOCK_STREAM) as sock:
            sock.connect(sock_path)
            sock.sendall(payload.encode("utf-8"))
        log.info("Sent listen request to claude-voice")
    except (ConnectionRefusedError, FileNotFoundError):
        log.warning("claude-voice not running, listen request dropped")
    except Exception:
        log.exception("Failed to send to claude-voice")


ACTIONS = {
    "claude-voice-listen": action_claude_voice_listen,
}


def run(bindings):
    """Main loop: find devices, listen for events, reconnect on disconnect."""
    last_press_time = 0.0
    had_devices = None

    while True:
        device_list = find_devices(bindings)
        grabbed = grab_devices(device_list)

        if not grabbed:
            if had_devices is not False:
                log.info("No matching devices found, waiting for hot-plug...")
                had_devices = False
            time.sleep(SCAN_INTERVAL)
            continue

        had_devices = True
        dev_map = {dev.fd: (dev, blist) for dev, blist in grabbed}

        try:
            while True:
                fds = list(dev_map.keys())
                r, _, _ = select.select(fds, [], [], SCAN_INTERVAL)

                if not r:
                    continue

                for fd in r:
                    dev, blist = dev_map[fd]
                    try:
                        for event in dev.read():
                            if event.type != evdev.ecodes.EV_KEY or event.value != 1:
                                continue

                            now = time.monotonic()
                            if now - last_press_time < DEBOUNCE_SECS:
                                continue

                            for b in blist:
                                if event.code == b["key_code"]:
                                    action_fn = ACTIONS.get(b["action"])
                                    if action_fn:
                                        log.info("Key %s → %s", b["key_name"], b["action"])
                                        last_press_time = now
                                        action_fn()
                                    else:
                                        log.warning("Unknown action: %s", b["action"])
                    except OSError:
                        log.info("Device disconnected: %s", dev.name)
                        dev.close()
                        del dev_map[fd]
                        if not dev_map:
                            break

                if not dev_map:
                    break

        except KeyboardInterrupt:
            raise
        except Exception:
            log.exception("Error in event loop")
            for dev, _ in dev_map.values():
                try:
                    dev.close()
                except Exception:
                    pass
            time.sleep(1)


def main():
    bindings = load_config()
    if not bindings:
        log.error("No bindings configured in %s", CONFIG_PATH)
        return

    log.info("claude-hid starting with %d binding(s)", len(bindings))
    try:
        run(bindings)
    except KeyboardInterrupt:
        log.info("Shutting down")


if __name__ == "__main__":
    main()
