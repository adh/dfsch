#!/bin/sh

BUILD_DIR="`pwd`"

cd $srcdir/tests


$BUILD_DIR/dfsch-repl -L $BUILD_DIR/.libs -L ../lib-scm -L . main.scm

