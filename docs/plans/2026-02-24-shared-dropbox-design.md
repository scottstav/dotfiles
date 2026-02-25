# Shared Dropbox Across Users

## Problem

Two users on the same machine (`ifit` and `scottstav`) both need access to the same Dropbox files. Currently only `ifit` has Dropbox set up (78GB). Duplicating it for `scottstav` wastes disk space on an already 86%-full drive. Both users share the same dotfiles repo via GNU Stow.

## Constraints

- Only one user logged in at a time (no concurrent sessions)
- Both users share identical dotfiles (stowed from same repo)
- Both need full read/write access to all Dropbox files
- Single Dropbox daemon, run by whichever user is active
- ext4 filesystem with ACL support

## Design

### Directory Structure

```
/shared/
├── Dropbox/          # 78GB synced files
├── .dropbox/         # metadata, account link, sync state
└── .dropbox-dist/    # Dropbox binary distribution

/home/{ifit,scottstav}/
├── Dropbox       → /shared/Dropbox
├── .dropbox      → /shared/.dropbox
└── .dropbox-dist → /shared/.dropbox-dist
```

### Permissions

- New `dropbox` group with `ifit` and `scottstav` as members
- `/shared/` owned by `ifit:dropbox`, mode `2775` (setgid)
- Default ACLs on `/shared/Dropbox`: `group:dropbox:rwx`
- Setgid ensures new files/dirs inherit `dropbox` group
- ACLs ensure group read/write regardless of umask

### Daemon

The existing systemd user service (`~/.config/systemd/user/dropbox.service`) runs the daemon. Since both users share the same dotfiles and only one is logged in at a time, the active user's systemd instance starts/stops Dropbox with their session. No `enable-linger` needed.

The `ExecStart` path (`%h/.dropbox-dist/dropboxd`) follows the symlink to `/shared/.dropbox-dist/dropboxd`.

### Migration

1. Stop Dropbox daemon
2. Create `dropbox` group, add both users
3. Create `/shared/` with proper ownership and permissions
4. Move `/home/ifit/Dropbox` to `/shared/Dropbox`
5. Move `/home/ifit/.dropbox` to `/shared/.dropbox`
6. Move `/home/ifit/.dropbox-dist` to `/shared/.dropbox-dist`
7. Set default ACLs recursively
8. Create symlinks for `ifit`
9. Create symlinks for `scottstav`
10. Start Dropbox, verify sync resumes
11. Verify scottstav can read/write files

### Disk Savings

~78GB saved by preventing duplication. `/shared/` is on the same partition (`/dev/nvme0n1p3`), so the move is a rename, not a copy.
