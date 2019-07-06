# /Users/sstavino/

### setup
```
# create bare repo in ~/.cfg
git clone --bare git@github.com:scottstav/dotfiles.git $HOME/.cfg
# config command will be used to track files in ~
function config {
   /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME $@
}
mkdir -p .config-backup
# pull all files into ~
config checkout
if [ $? = 0 ]; then
  echo "Checked out config.";
  else
	# merge conflicts due to pre-existing dot files, move them to a backup directory
    echo "Backing up pre-existing dot files.";
    config checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} .config-backup/{}
fi;
# rety checkout in case of failure
config checkout
# make 'config status' command not overwhelming
config config status.showUntrackedFiles no

```
### tmux
`ln -s -f .tmux/.tmux.conf`

### emacs
* i'd reccomend emacs 24+ (MELPA/ELPA shipped by default)\
* if you can't upgrade, try installing package.el manually from [here](http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el)
