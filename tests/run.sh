#!/bin/bash

set -e

trap cleanup EXIT

function cleanup() {
    printf "\e[?25h"
}

function join() {
    local IFS=$1
    shift
    echo "$*"
}

printf "\e[?25l"
ORIG="$(pwd)"
cd src
export TEST_FILES=$(join ':' *.adb)
cd "$ORIG"
alr build --development
./bin/runner < /dev/null
