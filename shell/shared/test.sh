#!/bin/sh

# Testing for functions in .profile.

# shellcheck source=/dev/null
. "$PWD/.profile"

# setenv
setenv TEST_VAR "test_value"
[ "$TEST_VAR" = "test_value" ] || echo "Failed:" setenv TEST_VAR "test_value"

setenv TEST_VAR "test_value2"
[ "$TEST_VAR" = "test_value2" ] || echo "Failed:" setenv TEST_VAR "test_value2"
unset TEST_VAR


# addenv TEST_PATH
addenv TEST_PATH "$HOME"
[ "$TEST_PATH" = "$HOME:" ] || echo "Failed:" addenv TEST_PATH "$HOME"

addenv TEST_PATH "/usr"
[ "$TEST_PATH" = "/usr:$HOME:" ] || echo "Failed:" addenv TEST_PATH "/usr"
unset TEST_PATH

addenv TEST_PATH "/boot" "/dev" "/etc"
[ "$TEST_PATH" = "/boot:/dev:/etc:" ] || echo "Failed:" addenv TEST_PATH "/boot" "/dev" "/etc"
unset TEST_PATH

addenv TEST_PATH "$HOME" "/usr" "/etc"
[ "$TEST_PATH" = "$HOME:/usr:/etc:" ] || echo "Failed:" addenv TEST_PATH "$HOME" "/usr" "/etc"
addenv TEST_PATH "/usr"
[ "$TEST_PATH" = "$HOME:/usr:/etc:" ] || echo "Failed:" addenv TEST_PATH "/usr"
unset TEST_PATH


# addenv --froce TEST_PATH
addenv TEST_PATH "$HOME"
output=$(addenv TEST_PATH "/noexist" 2>&1)
[ "$output" = "warning: no such directory (addenv): /noexist" ] || echo "Failed:" "Output from" addenv TEST_PATH "/noexist"
[ "$TEST_PATH" = "$HOME:" ] || echo "Failed:" addenv TEST_PATH "/noexist"

addenv --force TEST_PATH "/noexist" 2>&1
output=$(addenv --force TEST_PATH "/noexist" 2>&1)
[ "$output" = "" ] || echo "Failed:" "Output from" addenv --force TEST_PATH "/noexist"
[ "$TEST_PATH" = "/noexist:$HOME:" ] || echo "Failed:" addenv --force TEST_PATH "/noexist"
unset TEST_PATH

addenv TEST_PATH "$HOME"
addenv --force TEST_PATH "/noexist1" "/noexist2" 2>&1
addenv --force TEST_PATH "/noexist1" 2>&1
[ "$TEST_PATH" = "/noexist1:/noexist2:$HOME:" ] || echo "Failed:" addenv --force TEST_PATH "/noexist1"
unset TEST_PATH


# addenv TEST_VAR
addenv TEST_VAR "value1"
[ "$TEST_VAR" = "value1" ] || echo "Failed:" addenv TEST_VAR "value1"

addenv TEST_VAR "value2"
[ "$TEST_VAR" = "value2value1" ] || echo "Failed:" addenv TEST_VAR "value2"
unset TEST_VAR

addenv TEST_VAR "value1" "value2" "value3"
[ "$TEST_VAR" = "value1value2value3" ] || echo "Failed:" addenv TEST_VAR "value1" "value2" "value3"
unset TEST_VAR

addenv TEST_VAR "value1" "value2" "value3"
[ "$TEST_VAR" = "value1value2value3" ] || echo "Failed:" addenv TEST_VAR "value1" "value2" "value3"
addenv TEST_VAR "value2"
[ "$TEST_VAR" = "value1value2value3" ] || echo "Failed:" addenv TEST_VAR "value2"
unset TEST_VAR
