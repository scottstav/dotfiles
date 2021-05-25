# Alias
alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'
alias ls='ls -GAF'
alias emacs='emacsclient -create-frame --alternate-editor="" -t'    # no gui

# clean help
show_space() {
    du -xhS $1 | sort -h | tail -n15
}

alias video_to_gif='function video_to_gif(){ ffmpeg -i $1 output.gif && gifsicle -O3 output.gif -o output.gif && say "Video is ready!"};video_to_gif'

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
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR opens in terminal
export VISUAL="emacsclient -t -a emacs"         # $VISUAL opens in GUI mode

# TODO: this clashes with ruby bundle...
export GOROOT=/usr/local/opt/go/libexec
export GOPATH=$HOME/go
export PATH=$GOPATH/bin:$GOROOT/bin:$PATH
export PATH="/usr/local/opt/mongodb-community@3.4/bin:$PATH"
export PATH="~/.bin:$HOME/bin:$PATH"
export PATH="~/.bin:/opt/homebrew/bin:$PATH"

function vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
export PS1="\[\e[34m\][\[\e[m\]\u\[\e[m\]\[\e[34m\]]\[\e[m\]\w\[\e[m\] \\$ "
PS1=$PS1'\[$(vterm_prompt_end)\]'

# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[ -f /Users/scott.stavinoha/projects/ifit/janus/node_modules/tabtab/.completions/serverless.bash ] && . /Users/scott.stavinoha/projects/ifit/janus/node_modules/tabtab/.completions/serverless.bash
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[ -f /Users/scott.stavinoha/projects/ifit/janus/node_modules/tabtab/.completions/sls.bash ] && . /Users/scott.stavinoha/projects/ifit/janus/node_modules/tabtab/.completions/sls.bash
# added by travis gem
[ ! -s /Users/scott.stavinoha/.travis/travis.sh ] || source /Users/scott.stavinoha/.travis/travis.sh

# tabtab source for slss package
# uninstall by removing these lines or running `tabtab uninstall slss`
[ -f /Users/scott.stavinoha/projects/ifit/workout-store/node_modules/tabtab/.completions/slss.bash ] && . /Users/scott.stavinoha/projects/ifit/workout-store/node_modules/tabtab/.completions/slss.bash
