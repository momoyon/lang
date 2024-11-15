#!/bin/sh

TESTS_DIR=./tests/

test_file_exists() {
    filename=$1
    testfilename=$filename.test

    # TODO: Maybe create the test file with the current output
    if [ ! -f "$testfilename" ]; then
        echo "ERROR: test file \"$testfilename\" doesn't exist!"
        exit 1
    fi
}

test_source_file() {
    filename=$1

    if [ ! -f "$filename" ]; then
        echo "ERROR: File \"$filename\" doesn't exist!"
        exit 1
    fi

    test_file_exists $filename

    printf "INFO: Testing file \"$filename\"... "

    output=$(python3 main.py $filename)
    expected_output=$(cat $filename.test)
    if [ "$output" != "$expected_output" ]; then
        echo "Failed!"
        echo "Expected: \`$expected_output\`"
        echo "Got:      \`$output\`"
    else
        echo "Success!"
    fi
}

# set -xe

test_source_file tests/01-unterminated-string.momo
