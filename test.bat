@echo off

set TESTS_DIR=./tests/
set BUILD_CMD=../lang.exe {test_name}.{src_suffix}
set SRC_SUFFIX=.momo

python .\test.py %*
