#!/bin/sh

BUILD_DIR="`pwd`"

cd $srcdir/tests

$BUILD_DIR/dfsch-repl interp-test.scm

