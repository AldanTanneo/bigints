#!/bin/bash

function join() {
    local IFS=$1
    shift
    echo "$*"
}

cd src
export TEST_FILES=$(join ':' *.adb)
cd ..
alr run