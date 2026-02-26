# claude-hid Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Listen for HID key events from USB devices and trigger actions (first use: Hollyland Lark A1 record button → claude-voice listen).

**Architecture:** Standalone systemd user service running an evdev event loop. Config-driven device matching and key→action dispatch. Gracefully idles when no matching devices are plugged in; reconnects on hot-plug.

**Tech Stack:** Python 3.11, evdev, tomllib (stdlib), systemd user service

---

### Task 1: Create project skeleton and config

**Files:**
- Create: `.local/share/claude-hid/requirements.txt`
- Create: `.config/claude-hid/config.toml`

**Step 1: Create requirements.txt**

```
evdev==1.9.3
```

**Step 2: Create config.toml**

```toml
[[binding]]
device_match = "Hollyland.*Consumer Control"
key = "KEY_VOLUMEDOWN"
action = "claude-voice-listen"
```

**Step 3: Commit**

```bash
git add .local/share/claude-hid/requirements.txt .config/claude-hid/config.toml
git commit -m "claude-hid: project skeleton with config"
```

---

### Task 2: Write the daemon

**Files:**
- Create: `.local/share/claude-hid/daemon.py`

**Step 1: Write daemon.py**

The daemon does:
1. Load config from `~/.config/claude-hid/config.toml`
2. Scan `/sys/class/input/event*` device names, match against `device_match` regexes
3. For each match, open evdev device and `grab()` it
4. `select.select()` across all grabbed devices for events
5. On `EV_KEY` with `value == 1`, check key name against bindings, dispatch action
6. Actions: `claude-voice-listen` sends `{"action": "listen"}` to `claude-voice.sock`
7. On device error (disconnect): close it, re-enter scan loop
8. When no devices matched: sleep 5s, re-scan (no error log spam — one INFO on state change)
9. Debounce: ignore presses within 500ms of last triggered press

Key code reference:
- `evdev.ecodes.KEY_VOLUMEDOWN` = 114
- Event type `EV_KEY` = 1, value 1 = press, 0 = release

```python
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
SCAN_INTERVAL = 5  # seconds between device scans when nothing matched
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
    """Scan /sys/class/input for devices matching any binding.

    Returns list of (evdev.InputDevice, [matching_bindings]).
    """
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
    """Grab all devices exclusively. Returns list of successfully grabbed (dev, bindings)."""
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


# -- Actions ---------------------------------------------------------------

def _get_voice_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/run/user/{os.getuid()}")
    return os.path.join(runtime_dir, "claude-voice.sock")


def action_claude_voice_listen():
    """Send a listen request to the claude-voice daemon."""
    sock_path = _get_voice_socket_path()
    payload = json.dumps({"action": "listen"})
    try:
        sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        sock.connect(sock_path)
        sock.sendall(payload.encode("utf-8"))
        sock.close()
        log.info("Sent listen request to claude-voice")
    except (ConnectionRefusedError, FileNotFoundError):
        log.warning("claude-voice not running, listen request dropped")
    except Exception:
        log.exception("Failed to send to claude-voice")


ACTIONS = {
    "claude-voice-listen": action_claude_voice_listen,
}


# -- Main loop -------------------------------------------------------------

def run(bindings):
    """Main loop: find devices, listen for events, reconnect on disconnect."""
    last_press_time = 0.0
    had_devices = None  # Track state for logging

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
                    # Timeout — check for new devices
                    new_devices = find_devices(bindings)
                    new_grabbed = grab_devices(new_devices)
                    for dev, blist in new_grabbed:
                        if dev.path not in [d.path for d, _ in dev_map.values()]:
                            dev_map[dev.fd] = (dev, blist)
                        else:
                            dev.close()
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
```

**Step 2: Commit**

```bash
git add .local/share/claude-hid/daemon.py
git commit -m "claude-hid: daemon with evdev event loop and config-driven bindings"
```

---

### Task 3: Create systemd service

**Files:**
- Create: `.config/systemd/user/claude-hid.service`

**Step 1: Write service unit**

```ini
[Unit]
Description=Claude HID event listener (USB button → action)
After=claude-voice.service

[Service]
Type=simple
Environment=PYTHONUNBUFFERED=1
ExecStart=%h/.local/share/claude-hid/.venv/bin/python3 %h/.local/share/claude-hid/daemon.py
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
```

**Step 2: Commit**

```bash
git add .config/systemd/user/claude-hid.service
git commit -m "claude-hid: systemd user service"
```

---

### Task 4: Add to setup.sh

**Files:**
- Modify: `setup.sh` (the "Claude Ask / Claude Voice" section, ~line 477-511)

**Step 1: Add claude-hid venv setup after claude-voice block (after line 497)**

```bash
# claude-hid venv + deps
CH_DIR="$HOME/.local/share/claude-hid"
ensure_venv "$CH_DIR" python3.11 "claude-hid"
"$CH_DIR/.venv/bin/pip" install -q -r "$CH_DIR/requirements.txt"
ok "claude-hid dependencies installed"
```

**Step 2: Add claude-hid.service to the enable loop (line 504)**

Change:
```bash
for svc in claude-ask.service claude-voice.service; do
```
To:
```bash
for svc in claude-ask.service claude-voice.service claude-hid.service; do
```

**Step 3: Commit**

```bash
git add setup.sh
git commit -m "setup.sh: add claude-hid venv and service"
```

---

### Task 5: Stow, create venv, and test

**Step 1: Stow new files**

```bash
cd /home/ifit/dotfiles && stow .
```

**Step 2: Create the venv and install deps**

```bash
CH_DIR="$HOME/.local/share/claude-hid"
python3.11 -m venv "$CH_DIR/.venv"
"$CH_DIR/.venv/bin/pip" install -q -r "$CH_DIR/requirements.txt"
```

**Step 3: Test daemon starts and idles gracefully**

```bash
timeout 5 "$CH_DIR/.venv/bin/python3" "$CH_DIR/daemon.py" 2>&1 || true
```

Expected: logs showing "Loaded 1 binding(s)" and either device matched or "No matching devices found, waiting for hot-plug..."

**Step 4: Enable and start the service**

```bash
systemctl --user daemon-reload
systemctl --user enable claude-hid.service
systemctl --user start claude-hid.service
```

**Step 5: Verify it's running**

```bash
systemctl --user status claude-hid.service
journalctl --user -u claude-hid -f
```

Expected: active (running), appropriate log messages.

**Step 6: Live test (if mic plugged in)**

Press the record button on the Lark A1 while watching `journalctl`. Expected:
```
KEY_VOLUMEDOWN → claude-voice-listen
Sent listen request to claude-voice
```

And claude-voice should start listening (notification ack + listening indicator).
