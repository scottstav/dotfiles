# YubiKey Setup

All cryptographic keys live on YubiKeys. No private key material on disk.

## Hardware

- **Primary:** YubiKey 5 NFC — Serial 26894911
- **Backup:** YubiKey 5C NFC — Serial 35816981

Both keys are interchangeable — either one works for all operations.

## GPG Keys

Identity: `Scott Stavinoha <scottstavinoha@gmail.com>`

| Slot           | Key ID             | Algorithm | Created    | Expires    |
|----------------|--------------------|-----------|------------|------------|
| Signing [SC]   | B22E59FC3E226CCE   | rsa4096   | 2024-02-15 | 2029-02-13 |
| Encryption [E] | 2AA730551D0C6422   | rsa4096   | 2024-02-15 | 2029-02-13 |
| Auth [A]       | 2F9C6CDA8C8CF17D   | rsa4096   | 2026-03-01 | 2029-02-13 |

Fingerprint: `ED13C159C6CAE08D7FA0CF5EB22E59FC3E226CCE`

The same GPG keys are loaded on both YubiKeys. `keytocard` was used to copy each subkey to both cards.

### Key backup

A backup of the secret subkeys (taken before moving to cards) is at `~/.gnupg/backup-20260301/`. This is needed if you ever need to load keys onto a replacement YubiKey.

## SSH via gpg-agent

SSH authentication uses the GPG **Authentication subkey** (2F9C6CDA8C8CF17D) via `gpg-agent` with `enable-ssh-support`.

- Config: `~/.gnupg/gpg-agent.conf` (stowed from dotfiles)
- Socket: `~/.profile` sets `SSH_AUTH_SOCK` to gpg-agent's SSH socket

This means `ssh-add -L` shows the GPG auth key as a standard SSH public key. Any SSH client (git, scp, ssh) uses it transparently. Either YubiKey works — same key, same public key.

### SSH public key

```
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC+m8SAZ1+8A+qGYDF/rqn+vBdkAOBR8+ujsOcqdSHhAXlxv1gnvQUwp//cjJ/oGUyr225pj2hPXaoKGAoPxgAMtOTg1RhzRPOwf9OsNwKtfVV/5yRLGv2kuMYk3+j2+5y8eiSic93O7L52PaJdUvOjyDnFwBGwCKpQ/k1XQkrI72UYJThGZgEM3r/fnZ400hc2xrYIkciNtGUvOmPPK9pQ8JIZiNNj4MwJi2Xajx0BDQCLWSJ1kNvJmOU84peBMN6ZHTNwnyP/nodEDosC8SuglyDJR6yNoKc2UponQ2OWv97OupBi5sbybvSc6GXyCMXy3LNRFE/f0dP7Hy9MmJZQkEdh1c2dm5PfPUsbQ339udOAKAiqMbceZEksdDk1EGDWLL3+ghge3XF6vjL7yfZtzjFfILmoq8m2UOa9v83zAnOMPfQ5UxLrv/TN9EMoFpgZGeePqkx/aK7GGhGdzOXHqL7bNvYm5AqLauwoL6a4nGfh8YAroqazUQviT9CwS2/Sl2I9209HyHHU/fOMs+qrbPd6nbkcnE42jWHm2EMITia6MJGXxCDNVArUeBli1DK9AjpwjwBc8lTQVr4LQjSWEbuAKzEjSwQywfLt8DBLwRUWp0GK8WhHSEVQA6dMAHJzss5hH8MQNU+UEPzVAZbZmNS4vpIU5dEUFvmhNZEQHw== openpgp:0x8C8CF17D
```

This key is registered at:
- GitHub (scottstav) — as authentication key
- AUR (scottstav)
- Add to `~/.ssh/authorized_keys` on any remote server

## Git Commit Signing

Both users on this machine sign commits with the GPG signing key:

- **scottstav** (`~/.gitconfig`): `signingkey = B22E59FC3E226CCE`, `gpg.format = openpgp`
- **ifit** (`/home/ifit/.gitconfig`): same key, same format

## Emacs authinfo

`~/.authinfo.gpg` is encrypted to the GPG encryption subkey (2AA730551D0C6422). Either YubiKey decrypts it.

## Setting Up a New Machine

Run the automated setup script:

```
yubikey-setup
```

This handles everything: installs dependencies, enables pcscd, fetches the GPG public key from the keyserver, configures gpg-agent for SSH, sets up git signing, and binds the YubiKey stubs.

If you've already stowed dotfiles, `gpg-agent.conf` and `.profile` are already in place — the script is safe to run either way.

### Manual steps (if not using the script)

1. Install: `gnupg`, `ccid`, `pcsclite`, `pinentry`
2. Enable pcscd: `sudo systemctl enable --now pcscd`
3. Fetch GPG public key: `gpg --keyserver keys.openpgp.org --recv-keys ED13C159C6CAE08D7FA0CF5EB22E59FC3E226CCE`
4. Set trust: `echo "ED13C159C6CAE08D7FA0CF5EB22E59FC3E226CCE:6:" | gpg --import-ownertrust`
5. Plug in YubiKey — `gpg --card-status` will bind the stubs to the card
6. Stow dotfiles — this sets up `gpg-agent.conf` (SSH support), `.profile` (SSH_AUTH_SOCK), `.gitconfig` (signing)
7. Add SSH public key to any new remote hosts

## Loading Keys onto a New YubiKey

If you get a new YubiKey and want it to work identically to the existing ones:

### Prerequisites

- The new YubiKey must be a YubiKey 5 series (supports OpenPGP 3.4)
- You need the secret subkey backup at `~/.gnupg/backup-20260301/`

### Steps

1. **Set a FIDO2 PIN** (for WebAuthn/passkeys):
   ```
   ykman fido access change-pin
   ```

2. **Delete the current GPG secret key stubs** (they point to whichever card was last used):
   ```
   gpg --batch --yes --delete-secret-keys ED13C159C6CAE08D7FA0CF5EB22E59FC3E226CCE
   ```

3. **Re-import the secret subkeys from backup**:
   ```
   gpg --batch --import ~/.gnupg/backup-20260301/secret-subkeys.asc
   ```

4. **Restore trust on the key** (import wipes it):
   ```
   gpg --edit-key ED13C159C6CAE08D7FA0CF5EB22E59FC3E226CCE
   # type: trust → 5 (ultimate) → y → quit
   ```

5. **Move each subkey to the new card** — plug in the new YubiKey, then:
   ```
   gpg --edit-key ED13C159C6CAE08D7FA0CF5EB22E59FC3E226CCE
   ```
   For each subkey (encryption, auth), select it and move to card:
   - `key 1` → `keytocard` → choose **(2) Encryption key** → `key 1` (deselect)
   - `key 2` → `keytocard` → choose **(3) Authentication key** → `key 2` (deselect)

   For the primary signing key (no `key N` selection needed):
   - `keytocard` → choose **(1) Signature key**

   Then `save`.

6. **Verify** — `gpg --card-status` should show all three slots populated with the correct key IDs.

### Important notes

- `keytocard` **deletes the local copy** of the subkey each time. That's why you must re-import from the backup before loading each new card.
- Keep the backup (`~/.gnupg/backup-20260301/`) safe — without it you cannot provision new YubiKeys. Consider storing a copy on an encrypted USB drive.
- After loading, `gpg --card-status` will bind the stubs to the most recently used card. Switching YubiKeys may require `gpg-connect-agent "scd serialno" "learn --force" /bye` to re-bind.

## FIDO2 PINs

Both YubiKeys have FIDO2 PINs set (used for WebAuthn, passkeys, etc). These are separate from the GPG card PIN.

## Old Keys

Retired keys are in `~/.ssh/old-keys/`:
- `id_ed25519` — old standalone SSH key (was used by both scottstav and ifit)
- `id_rsa` / `id_rsa.pub` / `id_rsa.bak` — older RSA key

These are kept temporarily for reference. Safe to delete once all remote `authorized_keys` are updated.
