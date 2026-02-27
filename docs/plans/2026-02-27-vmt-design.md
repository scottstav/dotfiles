# vmt — Visual VM Testing for Wayland Apps

## Goal

A standalone CLI tool that spins up ephemeral QEMU/libvirt VMs with different distro+compositor combinations, enabling both automated visual verification (screenshot diffing) and interactive manual testing (SPICE) of Wayland applications. Designed so Claude can drive it as an interactive QA engineer — SSHing in, running commands, taking screenshots, and reasoning about what it sees.

## Architecture

```
vmt up arch-sway          # Boot VM from cloud image, provision via cloud-init
vmt ssh arch-sway -- cmd  # Run command inside VM (how Claude drives it)
vmt view arch-sway        # Open SPICE viewer (manual testing, includes audio)
vmt screenshot arch-sway /tmp/shot.png ./local.png   # SCP screenshot back
vmt test arch-sway --manifest tests/e2e/test.toml    # Batch: run scenarios, diff screenshots
vmt snapshot arch-sway clean-install                   # Save VM state
vmt restore arch-sway clean-install                    # Rollback
vmt update-references arch-sway                        # Update reference images
vmt destroy arch-sway                                  # Tear down
```

### Data flow

```
Host (your machine)                          VM (ephemeral)
─────────────────                            ──────────────
vmt up ──► libvirt create domain ──────────► cloud-init provisions:
           attach cloud-init ISO               - SSH key
           SPICE graphics                      - packages (sway, grim, pipewire...)
           virtio devices                      - compositor as systemd user unit
                                               - graphical-session.target
vmt ssh ──► paramiko / subprocess ssh ─────► run commands
                                               systemctl --user start aside-overlay
                                               echo '{"cmd":"text",...}' | socat ...
vmt screenshot ◄── SCP ───────────────────── grim /tmp/screenshot.png

vmt test ──► for each scenario:
               ssh: run commands
               ssh: grim screenshot
               scp: pull screenshot back
               SSIM diff against reference
             return pass/fail summary

vmt view ──► remote-viewer spice://...       (manual interaction, audio passthrough)
```

### Project structure

```
~/projects/vmt/
├── vmt/
│   ├── __init__.py
│   ├── cli.py              # argparse CLI entry point
│   ├── vm.py               # libvirt VM lifecycle (create, start, stop, destroy, snapshot)
│   ├── provision.py         # cloud-init YAML generation
│   ├── connect.py          # SSH client (paramiko for programmatic, passthrough for interactive)
│   ├── screenshot.py       # grim inside VM, SCP back, SSIM diffing, visual diff generation
│   └── manifest.py         # Parse TOML manifests (VM specs + test scenarios)
├── manifests/              # Built-in VM definitions
│   ├── arch-sway.toml
│   ├── fedora-gnome.toml
│   └── alpine-weston.toml
├── pyproject.toml
└── README.md
```

## Test Manifests

Two levels of TOML config: **VM manifests** (owned by vmt, define the environment) and **test manifests** (owned by the project under test, define what to install and verify).

### VM manifest (vmt repo)

```toml
# manifests/arch-sway.toml
[vm]
name = "arch-sway"
image = "https://geo.mirror.pkgbuild.com/images/latest/Arch-Linux-x86_64-cloudimg.qcow2"
memory = 2048
cpus = 2
disk = 10

[provision]
packages = ["sway", "foot", "grim", "slurp", "pipewire", "wireplumber", "socat"]
compositor = "sway"
compositor_cmd = "WLR_BACKENDS=drm sway"
display_server = "wayland"
screenshot_tool = "grim"

[provision.env]
XDG_RUNTIME_DIR = "/run/user/1000"
WLR_RENDERER = "pixman"

[ssh]
user = "arch"
port = 22
```

### Test manifest (project repo, e.g. aside)

```toml
# tests/e2e/aside-test.toml
[test]
name = "aside-e2e"
manifests = ["arch-sway", "fedora-gnome"]

[install]
commands = [
    "sudo pacman -S --noconfirm meson ninja cairo pango json-c wayland-protocols",
    "git clone https://github.com/scottstav/aside.git /tmp/aside",
    "cd /tmp/aside && make all && make install",
]

[install.fedora]
commands = [
    "sudo dnf install -y meson ninja-build cairo-devel pango-devel json-c-devel wayland-protocols-devel",
    "git clone https://github.com/scottstav/aside.git /tmp/aside",
    "cd /tmp/aside && make all && make install",
]

[[scenario]]
name = "overlay-renders-text"
commands = [
    "systemctl --user start aside-overlay",
    "sleep 1",
    """echo '{"cmd":"open"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/aside-overlay.sock""",
    """echo '{"cmd":"text","data":"Hello from E2E test"}' | socat - UNIX-CONNECT:$XDG_RUNTIME_DIR/aside-overlay.sock""",
    "sleep 0.5",
]
screenshot = "overlay-text.png"
reference = "references/arch-sway/overlay-text.png"
threshold = 0.95

[[scenario]]
name = "daemon-starts-cleanly"
commands = [
    "systemctl --user start aside-daemon",
    "sleep 2",
    "systemctl --user is-active aside-daemon",
]
expect_output = "active"

[[scenario]]
name = "gtk-input-opens"
commands = [
    "aside-input &",
    "sleep 1",
]
screenshot = "gtk-input.png"
reference = "references/arch-sway/gtk-input.png"
threshold = 0.93
```

## VM Lifecycle

All managed through libvirt Python bindings (`libvirt-python`).

### Boot sequence (`vmt up`)

1. Download cloud image to `~/.cache/vmt/images/` (skip if cached, verify by filename)
2. Create copy-on-write overlay: `qemu-img create -f qcow2 -b base.qcow2 -F qcow2 overlay.qcow2`
3. Generate cloud-init ISO (user-data + meta-data) with SSH pubkey, packages, compositor unit
4. Define libvirt domain: SPICE graphics, virtio disk/net, cloud-init ISO attached
5. Start domain
6. Poll SSH until ready (timeout 120s, 2s interval)
7. Print ready message with SSH and SPICE ports

### Cloud-init user-data (generated)

```yaml
#cloud-config
users:
  - name: ${user}
    ssh_authorized_keys:
      - ${ssh_pubkey}
    sudo: ALL=(ALL) NOPASSWD:ALL
    shell: /bin/bash
    groups: [video, audio]

package_update: true
packages: ${packages}

write_files:
  - path: /home/${user}/.config/systemd/user/test-compositor.service
    content: |
      [Unit]
      Description=Test compositor
      [Service]
      Type=simple
      Environment=${env_vars}
      ExecStart=${compositor_cmd}
      [Install]
      WantedBy=default.target

runcmd:
  - loginctl enable-linger ${user}
  - systemctl --user -M ${user}@ enable --now pipewire wireplumber
  - systemctl --user -M ${user}@ enable --now test-compositor
```

The compositor runs inside a real systemd user session with `graphical-session.target`, so service files (like aside's) start exactly as they would on a real user's machine.

### Snapshots

`vmt snapshot arch-sway after-install` saves a libvirt snapshot. `vmt restore arch-sway after-install` rolls back. Useful for running multiple test scenarios from a clean post-install state without re-provisioning.

## Screenshot Pipeline

### Capture

Inside the VM, the screenshot tool (per manifest) captures the Wayland display:
- **Sway/wlroots:** `grim /tmp/screenshot.png`
- **GNOME:** `gnome-screenshot --file=/tmp/screenshot.png`
- **Weston:** `grim /tmp/screenshot.png` (screencopy protocol)

### Transfer

`vmt screenshot <manifest> <remote-path> <local-path>` SCPs the file back via paramiko.

### Comparison

SSIM (structural similarity index) from scikit-image. More forgiving than pixel-exact — tolerates antialiasing and minor font rendering differences between distros while catching layout breaks, missing text, wrong colors.

```python
from skimage.metrics import structural_similarity
from skimage.io import imread

def compare_screenshots(actual, reference, threshold=0.95):
    img_a = imread(str(actual))
    img_b = imread(str(reference))
    if img_a.shape != img_b.shape:
        img_b = resize(img_b, img_a.shape)
    score = structural_similarity(img_a, img_b, channel_axis=2)
    return score >= threshold, score
```

When a diff fails, vmt generates a **visual diff image** highlighting changed regions (red overlay on the delta), saved alongside the actual screenshot for inspection.

### Reference management

Reference images stored per-manifest in the project's test directory:

```
tests/e2e/references/
├── arch-sway/
│   ├── overlay-text.png
│   └── gtk-input.png
└── fedora-gnome/
    ├── overlay-text.png
    └── gtk-input.png
```

`vmt update-references <manifest>` copies current screenshots to references after manual verification.

## Claude Integration

No special tooling. Claude uses its existing capabilities:

- **Bash tool** to run `vmt` CLI commands
- **Read tool** to view screenshots (multimodal image reading)
- **Reasoning** to judge whether rendered output looks correct

**Interactive mode** (Claude as QA engineer):
```
Claude runs: vmt up arch-sway
Claude runs: vmt ssh arch-sway -- "make install && systemctl --user start aside-overlay"
Claude runs: vmt ssh arch-sway -- "echo '...' | socat ..."
Claude runs: vmt ssh arch-sway -- "grim /tmp/s.png"
Claude runs: vmt screenshot arch-sway /tmp/s.png /tmp/vmt/s.png
Claude reads: /tmp/vmt/s.png  (sees the actual desktop with overlay)
Claude: "The overlay rendered correctly — text visible, positioned top-center, styled properly."
```

**Batch mode** (scripted):
```
Claude runs: vmt test arch-sway --manifest tests/e2e/aside-test.toml
Output:
  arch-sway: 3/3 scenarios passed
    ✓ overlay-renders-text (SSIM: 0.98)
    ✓ daemon-starts-cleanly
    ✓ gtk-input-opens (SSIM: 0.96)
```

If batch mode fails, Claude switches to interactive mode to investigate.

## SPICE Manual Access

`vmt view <manifest>` opens `remote-viewer spice://localhost:<port>`. Full desktop interaction with mouse and keyboard. SPICE supports audio passthrough — the VM's PipeWire output plays through your speakers, and mic input can be forwarded. This enables manual voice testing (wake word, speech, TTS playback) even though automated audio verification is out of scope for v1.

## Host Requirements

```bash
# Arch packages
sudo pacman -S qemu-full libvirt virt-viewer dnsmasq cloud-image-utils

# Enable libvirt
sudo systemctl enable --now libvirtd
sudo usermod -aG libvirt $USER
```

Python dependencies for vmt: `libvirt-python`, `paramiko`, `scikit-image`, `tomli` (or stdlib tomllib on 3.11+).

## Starting Test Matrix

| Manifest | Distro | Compositor | Cloud Image | Rationale |
|----------|--------|-----------|-------------|-----------|
| arch-sway | Arch Linux | Sway | Official cloud image | Closest to your setup, wlroots-based |
| fedora-gnome | Fedora 41 | GNOME (Mutter) | Fedora cloud base | Most popular Wayland desktop, non-wlroots |
| alpine-weston | Alpine 3.21 | Weston | Alpine cloud image | Minimal, musl libc, reference compositor |

Alpine+Weston with musl libc is a good stress test — if the C overlay compiles and renders there, it works almost anywhere.

## Out of Scope (v1)

- **GPU/hardware acceleration** — VMs use software rendering (`WLR_RENDERER=pixman`, `LIBGL_ALWAYS_SOFTWARE=1`)
- **Automated audio verification** — future enhancement (record with `pw-record`, STT to verify, inject audio via virtual PipeWire source)
- **CI pipeline integration** — future (GitHub Actions with nested virtualization)
- **Multi-monitor testing** — single virtual display only

## Future Enhancements

- Automated audio verification (record VM output, STT, verify)
- Audio input injection (pipe WAV into virtual PipeWire mic)
- GitHub Actions integration with nested KVM
- Parallel VM execution (test all manifests simultaneously)
- Auto-generate reference images on first run
