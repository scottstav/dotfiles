# Editor

When asked to open a file, use `emacsclient -c +LINE FILE` to open it in a new Emacs frame. Replace `LINE` with the target line number and `FILE` with the file path.

# Denote Journal

When asked to add something to the journal, write to today's Denote journal file. Use `emacsclient --eval` to call Denote's elisp functions so file naming and creation stays in sync with the user's Emacs config.

## Getting today's journal file path

```sh
emacsclient --eval '(denote-journal-path-to-new-or-existing-entry)'
```

This returns the file path as a quoted string. It creates the entry if one doesn't exist for today, and doesn't switch buffers. Strip the surrounding quotes from the returned path before using it.

For the **work journal**, override the directory and title:

```sh
emacsclient --eval '(let ((denote-journal-directory (expand-file-name "~/Dropbox/org/denote/work/journal")) (denote-journal-file-name-title "work")) (denote-journal-path-to-new-or-existing-entry))'
```

Use the work journal when the context is clearly work-related.

## Writing content

Once you have the file path, read the file and append content using the normal Edit tool.

- Use org-mode headings (`* Heading`) for distinct topics
- Keep the tone consistent with existing entries (casual, concise)

# Agents and Commands

After creating or modifying a file in `~/.claude/agents/` or `~/.claude/commands/`, run `~/dotfiles/sync-claude.sh` to adopt it into the dotfiles repo and symlink it back. This ensures new agents and skills are version-controlled.
