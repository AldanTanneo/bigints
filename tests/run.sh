#!/bin/bash

set -e

function join() {
    local IFS=$1
    shift
    echo "$*"
}

ORIG="$(pwd)"
cd src
export TEST_FILES=$(join ':' *.adb)
cd "$ORIG"
alr build --development
./bin/runner < /dev/null
