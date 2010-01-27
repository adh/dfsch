#!/bin/sh

BUILD_DIR="`pwd`"

cd $srcdir/tests


$BUILD_DIR/dfsch-repl -L $BUILD_DIR/.libs -L $srcdir/scm interp-test.scm

