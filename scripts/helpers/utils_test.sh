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

setup
unittest_run
teardown
