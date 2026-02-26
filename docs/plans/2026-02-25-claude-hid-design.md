# claude-hid: HID event listener daemon

## Purpose

Listen for key events from specific USB input devices and trigger actions.
First use case: Hollyland Lark A1 "record" button → trigger claude-voice
listening (bypassing wake word).

## Architecture

```
/dev/input/eventX (evdev) → claude-hid daemon → claude-voice.sock
                                                  {"action": "listen"}
```

Separate systemd user service. Own venv. Single dependency (evdev).

## File layout

```
~/.local/share/claude-hid/
  daemon.py          # Main daemon
  requirements.txt   # evdev==1.9.3

~/.config/claude-hid/
  config.toml        # Device match rules + key→action mappings

~/.config/systemd/user/
  claude-hid.service  # After claude-voice.service
```

## Config format

```toml
[[binding]]
device_match = "Hollyland.*Consumer Control"  # regex against device name
key = "KEY_VOLUMEDOWN"
action = "claude-voice-listen"
```

Adding future bindings = adding another `[[binding]]` block. No code changes.

## Daemon behavior

1. **Startup:** Parse config, scan `/sys/class/input/event*` for devices
   matching each binding's `device_match` regex.
2. **No devices found:** Idle gracefully. Poll every 5s for new devices.
   The mic is often unplugged — this is not an error.
3. **Grab matched devices** exclusively (`evdev.InputDevice.grab()`) to
   prevent system side effects (e.g. volume changes).
4. **Event loop:** `select()` across all grabbed devices. On `EV_KEY`
   with `value == 1` (press only — ignore release/repeat), look up the
   key code in bindings and fire the action.
5. **Actions:** Dict dispatch. Initially just `claude-voice-listen` which
   sends `{"action": "listen"}` to the claude-voice Unix socket.
6. **Hot-plug:** If a device disappears (USB disconnect), close it and
   re-enter the polling state. Re-scan every 5s.
7. **Debounce:** Ignore duplicate presses within 500ms.

## Hardware details (Hollyland Lark A1)

- Two evdev devices: plain "Wireless Microphone" and "Consumer Control"
- The record button is on the "Consumer Control" device
- Button sends `EV_KEY KEY_VOLUMEDOWN value=1` then `value=0` within ~1ms
- Device name: `Shenzhen Hollyland Technology Co.,Ltd Wireless Microphone Consumer Control`

## Dependencies

- `evdev==1.9.3`

## Permissions

evdev needs read access to `/dev/input/eventX`. User must be in the
`input` group (standard on Arch).

## Setup.sh addition

```bash
CH_DIR="$HOME/.local/share/claude-hid"
ensure_venv "$CH_DIR" python3.11 "claude-hid"
"$CH_DIR/.venv/bin/pip" install -q -r "$CH_DIR/requirements.txt"
ok "claude-hid dependencies installed"
```

Plus add `claude-hid.service` to the service-enable loop.
