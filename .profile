#!/bin/bash

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias tf="terraform"
alias ls='ls --color=auto'
export PS1="\[\e[36m\]\u\[\e[m\]@\[\e[32m\]\W\[\e[m\] \[\e[33m\]\\$\[\e[m\] "

# local scripts and go path
PATH=$PATH:~/.local/bin:~/go/bin

export PATH

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob;

# Append to the Bash history file, rather than overwriting it
shopt -s histappend;

if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
elif [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
fi

# Autocorrect typos in path names when using `cd`
shopt -s cdspell;

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
source /usr/share/nvm/init-nvm.sh

if [ -z "${WAYLAND_DISPLAY}" ]; then
    if [ "$(tty)" = "/dev/tty1" ];then
       exec Hyprland
    fi
fi
