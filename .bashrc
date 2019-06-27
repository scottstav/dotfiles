# Alias
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias ls='ls -GAF'

# Amazon specific settings
. ~/.bashrc_amzn

# set default editor
export VISUAL=emacs
export EDITOR="$VISUAL"

# Run twolfson/sexy-bash-prompt
. ~/.bash_prompt
