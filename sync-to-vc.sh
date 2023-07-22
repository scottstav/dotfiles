#!/bin/bash

cd "$(dirname "${BASH_SOURCE}")";

ls -A > files-to-sync

function doIt() {
    rsync --files-from=files-to-sync \
          --exclude ".git/" \
          --exclude "bootstrap.sh" \
          --exclude "sync-to-vc.sh" \
          --exclude "README.md" \
          -ar ~ .;
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt;
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1;
    echo "";
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        doIt;
    fi;
fi;
unset doIt;

rm files-to-sync
