#!/bin/bash

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
alr run < /dev/null
printf "\e[?25h"