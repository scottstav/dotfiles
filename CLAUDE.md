# Dotfiles Repository

This is a dotfiles tracking repo managed with GNU Stow.

## Important Rules

**When creating new files:** Any new file created in this repo must be stowed to its respective place in the home directory. After creating a file, run:

```bash
cd /home/ifit/dotfiles && stow .
```

This symlinks files from the dotfiles repo structure to their corresponding locations under `$HOME`. Do NOT run stow when editing existing files — they are already symlinked, so changes are reflected immediately.

**Emacs configuration:** All Emacs config changes must be made to `.emacs.d/init.org`, NOT `init.el` directly. This is a literate config in org-mode that tangles to `init.el` on save. After editing `init.org`, always:

1. Tangle it: `emacsclient --eval '(org-babel-tangle-file "~/.emacs.d/init.org")'`
2. Evaluate the changed code in the running Emacs daemon so it takes effect immediately (e.g. `emacsclient --eval '(...)'` with the relevant forms)

**Foot + fzf picker scripts:** Scripts launched via `foot --app-id=fzf-picker -e <script>` run inside a foot terminal that closes when the script exits. To launch external processes (apps, commands), use `hyprctl dispatch exec "..."` — NOT `nohup ... &` or bare backgrounding. Subprocesses of foot die when foot exits.

**Waybar:** After modifying waybar config or styles, always restart waybar so changes take effect:

```bash
killall waybar && hyprctl dispatch exec waybar
```

**Android scripts:** After modifying files under `android/`, push them to the phone by running `./android/push-to-phone.sh` (requires phone connected via USB with ADB enabled).
