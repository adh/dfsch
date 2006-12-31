#!/bin/sh

BUILD_DIR="`pwd`"

cd $srcdir/scm

$BUILD_DIR/dfsch-repl test.scm

