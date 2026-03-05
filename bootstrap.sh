#!/bin/sh
VERSION=

if [ "x$1" != "x" ]; then
  VERSION=-$1
fi

if [ -d "./.git" ]; then 
  git log > ChangeLog
fi

autoheader \
&& aclocal$VERSION \
&& (command -v libtoolize >/dev/null 2>&1 && libtoolize || glibtoolize) \
&& automake$VERSION --add-missing \
&& autoconf
