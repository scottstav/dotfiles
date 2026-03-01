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

1. Install: `gnupg`, `libfido2`, `pcsclite` (if using ykman PIV/OATH)
2. Import GPG public key: `gpg --import <public-key>` or fetch from keyserver
3. Plug in YubiKey — `gpg --card-status` will bind the stubs to the card
4. Stow dotfiles — this sets up `gpg-agent.conf` (SSH support), `.profile` (SSH_AUTH_SOCK), `.gitconfig` (signing)
5. Add SSH public key to any new remote hosts

## FIDO2 PINs

Both YubiKeys have FIDO2 PINs set (used for WebAuthn, passkeys, etc). These are separate from the GPG card PIN.

## Old Keys

Retired keys are in `~/.ssh/old-keys/`:
- `id_ed25519` — old standalone SSH key (was used by both scottstav and ifit)
- `id_rsa` / `id_rsa.pub` / `id_rsa.bak` — older RSA key

These are kept temporarily for reference. Safe to delete once all remote `authorized_keys` are updated.
