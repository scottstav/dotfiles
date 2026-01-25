# Dotfiles Repository

This is a dotfiles tracking repo managed with GNU Stow.

## Important Rules

**When creating new files:** Any new file created in this repo must be stowed to its respective place in the home directory. After creating a file, run:

```bash
cd /home/ifit/dotfiles && stow .
```

This symlinks files from the dotfiles repo structure to their corresponding locations under `$HOME`.

**Emacs configuration:** All Emacs config changes must be made to `.emacs.d/init.org`, NOT `init.el` directly. This is a literate config in org-mode that tangles to `init.el` on save.
