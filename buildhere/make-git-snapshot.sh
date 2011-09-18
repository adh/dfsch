#!/bin/sh

REV=+`date --iso=date`

if git rev-parse --verify HEAD >/dev/null 2>&1; then
    REV=_git-`git describe`
fi

if make distcheck; then
  mv dfsch-0.4.0-dev.tar.gz dfsch-0.4.0-dev$REV.tar.gz
  echo  dfsch-0.4.0-dev$REV.tar.gz packaged sucessfully
  ln -sf dfsch-0.4.0-dev$REV.tar.gz dfsch-current-snapshot.tar.gz
  exit 0
fi

exit 1

