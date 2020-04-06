# dot-files

## Prerequisites
1. install git
2. install emacs
3. setup ssh key for git

### setup
Run script:

``bash <(curl -s https://scotty.dance/dotfiles.sh)``

### tmux
`ln -s -f .tmux/.tmux.conf`

### iTerm2
import `scott-default-iterm-profile.json` into iTerm2 preferences

### emacs
* i'd reccomend emacs 24+ (MELPA/ELPA shipped by default)\
* if you can't upgrade, try installing package.el manually from [here](http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el)
