#!/bin/sh

REV=+`date --iso=date`

if git rev-parse --verify HEAD >/dev/null 2>&1; then
    REV=+git-`git describe`
fi

if make distcheck; then
  mv dfsch-0.4.0-dev.tar.gz dfsch-0.4.0-dev$REV.tar.gz
  echo  dfsch-0.4.0-dev$REV.tar.gz packaged sucessfully
  exit 0
fi

exit 1

