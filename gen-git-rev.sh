#!/bin/sh

if git rev-parse --verify HEAD >/dev/null 2>&1; then
    REV=`git rev-parse --verify HEAD`
    echo '#define DFSCH_GIT_REV "'$REV'"'
fi