
# Alias
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias ls='ls -GAF'
alias emacs='emacs -nw'    # no gui

# set default editor
export VISUAL=emacs
export EDITOR="$VISUAL"

. ~/.bash_prompt

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
