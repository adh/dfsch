#! /bin/sh
autoheader \
&& aclocal-1.9 \
&& libtoolize \
&& automake-1.9 --add-missing \
&& autoconf
