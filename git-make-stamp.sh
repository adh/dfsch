#!/bin/bash

if git rev-parse --verify HEAD >/dev/null 2>&1; then
    git describe > "$1"
fi
