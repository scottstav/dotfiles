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

# macOS Catalina switched to zsh but i dont wanna deal
export BASH_SILENCE_DEPRECATION_WARNING=1

if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"
