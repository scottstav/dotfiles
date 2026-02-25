# Shared Dropbox Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Consolidate Dropbox into `/shared/` so both `ifit` and `scottstav` users access the same 78GB of synced files via symlinks.

**Architecture:** Move Dropbox data, metadata, and binary from `/home/ifit/` to `/shared/`. Create a shared `dropbox` group with ACLs for cross-user read/write. Both users get symlinks (`~/Dropbox`, `~/.dropbox`, `~/.dropbox-dist`) pointing to `/shared/`. The existing systemd user service follows the symlinks transparently.

**Tech Stack:** Linux groups, POSIX ACLs (setfacl), setgid, systemd user services, GNU Stow (not used for symlinks — these are system-level, not dotfiles).

---

### Task 1: Stop Dropbox and Verify It's Down

**Context:** The daemon holds locks on `.dropbox/` files (sqlite, sockets). Must be fully stopped before moving anything.

**Step 1: Stop the Dropbox systemd user service**

Run (as ifit):
```bash
systemctl --user stop dropbox.service
```

**Step 2: Verify the daemon is gone**

Run:
```bash
systemctl --user status dropbox.service
ps aux | grep dropbox | grep -v grep
```
Expected: service shows `inactive (dead)`, no dropbox processes listed.

**Step 3: Verify no locks remain**

Run:
```bash
ls /home/ifit/.dropbox/command_socket 2>/dev/null && echo "SOCKET STILL EXISTS" || echo "OK: socket gone"
ls /home/ifit/.dropbox/dropbox.pid 2>/dev/null && echo "PID FILE EXISTS (normal, not a lock)" || echo "pid gone"
```
Expected: Socket should be gone. PID file may remain (it's just a stale marker, not a lock).

---

### Task 2: Create the `dropbox` Group and Add Users

**Context:** Both users need a shared group for file ownership. This requires sudo.

**Step 1: Create the group**

Run:
```bash
sudo groupadd dropbox
```

**Step 2: Add both users**

Run:
```bash
sudo usermod -aG dropbox ifit
sudo usermod -aG dropbox scottstav
```

**Step 3: Verify membership**

Run:
```bash
getent group dropbox
```
Expected: `dropbox:x:NNNN:ifit,scottstav`

**Step 4: Activate the new group in the current session**

Run:
```bash
newgrp dropbox
```
Note: This is needed so subsequent commands in this session see the new group membership. Without it, `id` won't show `dropbox` until next login.

---

### Task 3: Create `/shared/` Directory Structure

**Context:** `/shared/` does not exist yet. Same partition as `/home` (both on `/dev/nvme0n1p3`), so `mv` will be instant renames.

**Step 1: Create the directory**

Run:
```bash
sudo mkdir -p /shared
```

**Step 2: Set ownership and setgid**

Run:
```bash
sudo chown ifit:dropbox /shared
sudo chmod 2775 /shared
```
The `2` sets the setgid bit so new files/dirs inherit the `dropbox` group.

**Step 3: Verify**

Run:
```bash
stat -c '%a %U:%G %n' /shared
ls -ld /shared
```
Expected: `2775 ifit:dropbox /shared`, and `ls` shows `drwxrwsr-x` (note the `s` for setgid).

---

### Task 4: Move Dropbox Data to `/shared/`

**Context:** Three directories to move. All on the same partition, so `mv` is a rename (instant, no copy). Order matters — move data last since it's largest and most important.

**Step 1: Move the binary distribution**

Run:
```bash
mv /home/ifit/.dropbox-dist /shared/.dropbox-dist
```

**Step 2: Move the metadata**

Run:
```bash
mv /home/ifit/.dropbox /shared/.dropbox
```

**Step 3: Move the synced files (78GB — instant rename)**

Run:
```bash
mv /home/ifit/Dropbox /shared/Dropbox
```

**Step 4: Verify all three moved**

Run:
```bash
ls -la /shared/
du -sh /shared/Dropbox /shared/.dropbox /shared/.dropbox-dist
```
Expected: All three dirs present. `Dropbox` ~78GB, `.dropbox` ~113M, `.dropbox-dist` ~114M.

**Step 5: Verify nothing left behind**

Run:
```bash
ls -la /home/ifit/Dropbox 2>/dev/null && echo "ERROR: still exists" || echo "OK: gone"
ls -la /home/ifit/.dropbox 2>/dev/null && echo "ERROR: still exists" || echo "OK: gone"
ls -la /home/ifit/.dropbox-dist 2>/dev/null && echo "ERROR: still exists" || echo "OK: gone"
```
Expected: All three report "OK: gone".

---

### Task 5: Set Group Ownership and ACLs

**Context:** After the move, files are still `ifit:ifit`. Need to change group to `dropbox` and set default ACLs so all future files are also group-writable.

**Step 1: Change group ownership recursively**

Run:
```bash
sudo chgrp -R dropbox /shared/
```
Note: This touches every file. With 78GB of small files this may take a minute or two, but it's metadata-only (no data copied).

**Step 2: Set the setgid bit on all directories**

Run:
```bash
sudo find /shared -type d -exec chmod g+s {} +
```
This ensures any new subdirectory also inherits the `dropbox` group.

**Step 3: Make all existing files/dirs group-writable**

Run:
```bash
sudo chmod -R g+rwX /shared/
```
The capital `X` sets execute only on directories and files that already have execute, preserving file executability.

**Step 4: Set default ACLs on all three top-level dirs**

Run:
```bash
sudo setfacl -R -m g:dropbox:rwX /shared/Dropbox
sudo setfacl -R -d -m g:dropbox:rwx /shared/Dropbox

sudo setfacl -R -m g:dropbox:rwX /shared/.dropbox
sudo setfacl -R -d -m g:dropbox:rwx /shared/.dropbox

sudo setfacl -R -m g:dropbox:rwX /shared/.dropbox-dist
sudo setfacl -R -d -m g:dropbox:rwx /shared/.dropbox-dist
```
The `-d` flag sets *default* ACLs, which are inherited by newly created files/dirs.

**Step 5: Verify ACLs**

Run:
```bash
getfacl /shared/Dropbox
getfacl /shared/.dropbox
```
Expected: Both show `group:dropbox:rwx` and `default:group:dropbox:rwx`.

---

### Task 6: Create Symlinks for `ifit`

**Context:** `ifit`'s original dirs have been moved. Create symlinks pointing to `/shared/`.

**Step 1: Create the symlinks**

Run (as ifit):
```bash
ln -s /shared/Dropbox /home/ifit/Dropbox
ln -s /shared/.dropbox /home/ifit/.dropbox
ln -s /shared/.dropbox-dist /home/ifit/.dropbox-dist
```

**Step 2: Verify**

Run:
```bash
ls -la /home/ifit/Dropbox /home/ifit/.dropbox /home/ifit/.dropbox-dist
```
Expected: All three show as symlinks to `/shared/...`.

**Step 3: Verify target is accessible**

Run:
```bash
ls /home/ifit/Dropbox/ | head -5
cat /home/ifit/.dropbox-dist/VERSION
```
Expected: Dropbox files listed, VERSION shows the Dropbox version number.

---

### Task 7: Create Symlinks for `scottstav`

**Context:** `scottstav` has no Dropbox dirs. Create symlinks to the shared location. Need sudo since we're writing to another user's home.

**Step 1: Check for any existing Dropbox artifacts**

Run:
```bash
sudo ls -la /home/scottstav/Dropbox /home/scottstav/.dropbox /home/scottstav/.dropbox-dist 2>&1
```
Expected: All should report "No such file or directory". If any exist, investigate before overwriting.

**Step 2: Create the symlinks**

Run:
```bash
sudo -u scottstav ln -s /shared/Dropbox /home/scottstav/Dropbox
sudo -u scottstav ln -s /shared/.dropbox /home/scottstav/.dropbox
sudo -u scottstav ln -s /shared/.dropbox-dist /home/scottstav/.dropbox-dist
```

**Step 3: Verify**

Run:
```bash
sudo ls -la /home/scottstav/Dropbox /home/scottstav/.dropbox /home/scottstav/.dropbox-dist
```
Expected: All three show as symlinks owned by `scottstav` pointing to `/shared/...`.

---

### Task 8: Start Dropbox and Verify Sync

**Context:** Everything is in place. Start the daemon and confirm it picks up seamlessly.

**Step 1: Start the service**

Run (as ifit):
```bash
systemctl --user start dropbox.service
```

**Step 2: Check service status**

Run:
```bash
systemctl --user status dropbox.service
```
Expected: `active (running)`.

**Step 3: Verify the daemon is using /shared/ via the symlink**

Run:
```bash
ls -la /proc/$(cat /home/ifit/.dropbox/dropbox.pid)/exe 2>/dev/null
readlink /home/ifit/.dropbox-dist/dropboxd
```
Expected: Process running, symlink resolves to `/shared/.dropbox-dist/...`.

**Step 4: Check Dropbox status**

Run:
```bash
dropbox status
```
Expected: "Up to date" (may show "Syncing..." briefly after restart, which is fine — wait a minute and re-check).

---

### Task 9: Verify Cross-User File Access

**Context:** Confirm scottstav can actually read and write files through the shared setup.

**Step 1: Test write access as scottstav**

Run:
```bash
sudo -u scottstav touch /shared/Dropbox/.shared-access-test
sudo -u scottstav ls -la /shared/Dropbox/.shared-access-test
```
Expected: File created successfully, owned by `scottstav:dropbox`.

**Step 2: Test that ifit can read/modify scottstav's file**

Run:
```bash
echo "cross-user test" > /shared/Dropbox/.shared-access-test
cat /shared/Dropbox/.shared-access-test
```
Expected: Write succeeds, reads back "cross-user test".

**Step 3: Clean up test file**

Run:
```bash
rm /shared/Dropbox/.shared-access-test
```

**Step 4: Test scottstav can read existing Dropbox files**

Run:
```bash
sudo -u scottstav ls /shared/Dropbox/ | head -5
```
Expected: Lists Dropbox contents without permission errors.

---

### Task 10: Commit and Clean Up

**Step 1: Verify no changes needed in dotfiles**

The `dropbox.service` file uses `%h/.dropbox-dist/dropboxd` which follows the symlink — no changes needed. Verify:

Run:
```bash
grep ExecStart /home/ifit/.config/systemd/user/dropbox.service
```
Expected: `ExecStart=%h/.dropbox-dist/dropboxd` — unchanged, works via symlink.

**Step 2: Final disk usage check**

Run:
```bash
df -h /
du -sh /shared/Dropbox
```
Expected: Disk usage should be roughly the same as before (data moved, not copied). No duplication.

**Step 3: Commit the plan**

```bash
cd /home/ifit/dotfiles
git add docs/plans/2026-02-24-shared-dropbox-plan.md
git commit -m "add implementation plan for shared Dropbox migration"
```

---

## Rollback Plan

If anything goes wrong mid-migration:

1. Stop Dropbox: `systemctl --user stop dropbox.service`
2. Remove symlinks: `rm ~/Dropbox ~/.dropbox ~/.dropbox-dist`
3. Move data back: `mv /shared/Dropbox /home/ifit/Dropbox && mv /shared/.dropbox /home/ifit/.dropbox && mv /shared/.dropbox-dist /home/ifit/.dropbox-dist`
4. Start Dropbox: `systemctl --user start dropbox.service`

All moves are on the same partition, so rollback is also instant.
