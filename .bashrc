# Amazon specific settings
. ~/.bashrc_amzn

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
