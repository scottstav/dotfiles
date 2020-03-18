# Alias
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias ls='ls -GAF'
alias emacs='emacs -nw'    # no gui

# clean help
show_space() {
    du -xhS $1 | sort -h | tail -n15
}

# set default editor
export VISUAL=emacs
export EDITOR="$VISUAL"

. ~/.bash_prompt

# macOS Catalina switched to zsh but i dont wanna deal
export BASH_SILENCE_DEPRECATION_WARNING=1

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
