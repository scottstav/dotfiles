#!/bin/bash

cd "$(dirname "${BASH_SOURCE}")";

function doIt() {
    find . -path ./.git -prune -o -type f \( -not -name "bootstrap.sh" -and -not -name "README.md" -and -not -name "sync-to-vc.sh" \) -print > ./.git/files-to-sync
    rsync --files-from=./.git/files-to-sync \
          -ar ~ .;
}

doIt;
unset doIt;

rm ./.git/files-to-sync
