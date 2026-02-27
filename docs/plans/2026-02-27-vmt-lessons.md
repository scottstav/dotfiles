# vmt + aside: Lessons from First Real VM Test

## Issues Hit (in order)

### 1. VM disk too small
**Symptom:** `pacman -S` fails with "Partition / too full"
**Root cause:** `vmt up` created a qcow2 overlay but never resized it. The Arch cloud image is ~2GB; installing sway + build tools needs ~800MB.
**Fix (vmt):** Add `qemu-img resize overlay.qcow2 ${disk}G` after creating the overlay. Already fixed in vm.py.

### 2. Cloud-init package install fails on Arch
**Symptom:** `cloud-init status` reports error on `package_update_upgrade_install`
**Root cause:** Fresh Arch cloud images have an empty/stale pacman keyring. `pacman -Sy` fails signature checks.
**Fix (vmt):** Add `pacman-key --init && pacman-key --populate archlinux` to runcmd *before* package install. Or add it as a bootcmd.

### 3. Compositor ExecStart broken
**Symptom:** `test-compositor.service` fails to start
**Root cause:** cloud-init generates `ExecStart=WLR_BACKENDS=drm sway` — systemd treats the whole thing as one command. Environment vars need to go in `Environment=` directives.
**Fix (vmt):** provision.py should put each env var in a separate `Environment=` line and keep `ExecStart` as just the binary path.

### 4. Headless backend doesn't show on SPICE
**Symptom:** SPICE viewer shows TTY login, not sway desktop
**Root cause:** `WLR_BACKENDS=headless` renders to a virtual display that isn't connected to the SPICE virtual GPU. SPICE needs sway running on the DRM backend, which requires a real VT (TTY session).
**Fix (vmt):** For SPICE-visible VMs, set up TTY autologin + compositor launch from `.bash_profile` instead of a systemd user service. The headless backend is fine for screenshot-only testing (grim captures it), but not for interactive SPICE sessions. Could offer both modes.

### 5. No user password set
**Symptom:** SPICE console shows login prompt, can't log in
**Root cause:** Cloud-init creates the user with SSH key auth only, no password.
**Fix (vmt):** Set a default password in cloud-init user-data (e.g. `passwd: vmt` + `chpasswd: {expire: false}`). Document it.

### 6. Docker blocks VM networking
**Symptom:** VM can't reach internet despite NAT masquerade being present
**Root cause:** Docker's nftables FORWARD chain has `policy drop` and only allows Docker bridge traffic. VM traffic on virbr0 gets dropped.
**Fix (vmt/docs):** Add `iptables -I DOCKER-USER -i virbr0 -j ACCEPT && iptables -I DOCKER-USER -o virbr0 -m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT`. Document this in README as a known Docker interaction. Consider adding it to `vmt up` automatically if Docker is detected.

### 7. NAT masquerade doesn't persist across reboots
**Symptom:** VM has internet, reboot host, VM has no internet
**Root cause:** The nftables masquerade rule is added at runtime but not saved. libvirt *should* manage this via its network, but the iptables-nft interaction is fragile.
**Fix (docs):** Document that `virsh net-start default` should handle this, or persist rules with `nft list ruleset > /etc/nftables.conf`. Consider having `vmt up` check and fix NAT.

### 8. aside pyproject.toml broken on fresh setuptools
**Symptom:** `pip install .` fails with `Cannot import 'setuptools.backends._legacy'`
**Root cause:** `build-backend = "setuptools.backends._legacy:_Backend"` is a non-standard path that doesn't exist in upstream setuptools.
**Fix (aside):** Change to `build-backend = "setuptools.build_meta"`.

### 9. aside package discovery fails
**Symptom:** `pip install .` fails with "multiple top-level packages discovered"
**Root cause:** No `[tool.setuptools.packages.find]` section. setuptools auto-discovers the `overlay/` C directory as a Python package.
**Fix (aside):** Add `[tool.setuptools.packages.find]` with `include = ["aside*"]`.

### 10. No `aside daemon` CLI command
**Symptom:** `aside daemon` → "invalid choice"
**Root cause:** The daemon is started via `python3 -m aside.daemon`, not exposed as a CLI subcommand.
**Fix (aside):** Add `daemon` as a subcommand to the CLI, or at minimum document the correct invocation. Users shouldn't need to know about `python3 -m`.

### 11. ~/.local/bin not on PATH
**Symptom:** `aside: command not found` after pip install
**Root cause:** pip installs scripts to `~/.local/bin` which isn't on PATH in the Arch cloud image.
**Fix (vmt):** Add `export PATH=$HOME/.local/bin:$PATH` to the user's `.bashrc` or `.profile` in cloud-init.

---

## Summary of Changes Needed

### aside (the project)
- [ ] Fix `build-backend` in pyproject.toml → `setuptools.build_meta`
- [ ] Add `[tool.setuptools.packages.find]` with `include = ["aside*"]`
- [ ] Add `aside daemon` CLI subcommand (or wrapper script)

### vmt (the tool)
- [x] Resize overlay disk to manifest spec (already fixed)
- [ ] Fix compositor service: env vars as `Environment=` lines, not inline
- [ ] Arch manifest: add `pacman-key --init && --populate` to bootcmd
- [ ] Cloud-init: set user password for SPICE console login
- [ ] Cloud-init: add `~/.local/bin` to PATH
- [ ] Support two compositor modes: headless (screenshot-only) vs DRM (SPICE-interactive)
- [ ] DRM mode: TTY autologin + compositor launch from .bash_profile
- [ ] Detect Docker and add DOCKER-USER forwarding rules on `vmt up`
- [ ] Check/fix NAT masquerade on `vmt up`

### Documentation
- [ ] Docker + libvirt networking conflict and fix
- [ ] NAT persistence across reboots
- [ ] SPICE requires DRM backend, not headless
- [ ] API key setup for end-to-end testing of LLM apps
- [ ] Default VM credentials (user/password)
