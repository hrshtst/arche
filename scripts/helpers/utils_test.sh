#!/usr/bin/env bash

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include unit testing framework.
# shellcheck source=unittest.sh
source "${THIS_DIR}/unittest.sh"

# Include functions to test.
# shellcheck source=utils.sh
source "${THIS_DIR}/utils.sh"

# Create workspace.
workspace="/tmp/workspace-$$"
setup() {
  mkdir -p "$workspace/usr/bin"
  mkdir -p "$workspace/src"
}

teardown() {
  rm -rf "$workspace"
}

testcase_has() {
  has "pwd"
  ! has "hoge"
}

testcase_mark() {
  local _pwd

  _pwd="$(pwd)"
  mark
  cd ..
  cd ..
  back
  [ "$(pwd)" = "$_pwd" ]
}

testcase_markcd() {
  local _pwd

  _pwd="$(pwd)"

  # mark here and go to another directory.
  markcd "$workspace"
  [ "$(pwd)" = "$workspace" ]

  # get back to marked place.
  back
  [ "$(pwd)" = "$_pwd" ]
}

testcase_abspath() {
  markcd "$workspace"

  local path expected
  path="$(abspath usr/bin)"
  expected="$workspace/usr/bin"

  [ "$path" = "$expected" ]
  back
}

testcase_parentdir() {
  markcd "$workspace"

  local parent expected
  parent="$(parentdir "src")"
  expected="$workspace"

  [ "$parent" = "$expected" ]
  back
}

testcase_ask() {
  echo 'y' | ask "Are you OK?" >/dev/null
  echo 'Y' | ask "Are you OK?" >/dev/null
  ! echo 'n' | ask "Are you OK?" >/dev/null
  ! echo 'N' | ask "Are you OK?" >/dev/null
}

testcase_contains() {
  contains "banana" "apple" "banana" "orange"
  ! contains "grape" "apple" "banana" "orange"
  contains "banana" "apple banana orange"
  ! contains "grape" "apple banana orange"
}

testcase_contains_array() {
  local list
  list=("apple" "banana" "orange")
  contains "banana" "${list[@]}"
  ! contains "grape" "${list[@]}"
}

testcase_lower() {
  local string
  string="$(lower "Apple")"
  [ "$string" = "apple" ]
}

testcase_upper() {
  local string
  string="$(upper "Apple")"
  [ "$string" = "APPLE" ]
}

testcase_detect_os() {
  detect_os
  if [[ "$(lsb_release -si)" = "Ubuntu" ]]; then
    [ "$OS_NAME" = "ubuntu" ]
    local ver code
    ver="$(lsb_release -sr)"
    code="$(lsb_release -sc)"
    [ "$OS_VERSION" = "$ver" ]
    [ "$OS_CODENAME" = "$code" ]
  fi
}


setup
unittest_run
teardown
