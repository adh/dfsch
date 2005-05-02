#! /bin/sh
aclocal-1.9 \
&& automake-1.9 --add-missing \
&& autoconf
