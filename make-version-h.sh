#!/bin/bash

if [ -f "$1" ]; then
    REV=`cat "$1"`
else
    REV=snapshot
    if git rev-parse --verify HEAD >/dev/null 2>&1; then
        REV=`git describe`
    fi
fi

echo "#define BUILD_ID \"$REV\"" > version.h
