#!/usr/bin/env bash

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include unit testing framework.
# shellcheck source=unittest.sh
source "${THIS_DIR}/unittest.sh"

# Include functions to test.
# shellcheck source=utils.sh
source "${THIS_DIR}/utils.sh"

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
  local _pwd _dir

  _pwd="$(pwd)"
  _dir="/tmp/tmp-$$"

  # mark here and go to another directory.
  markcd "$_dir"
  [ "$(pwd)" = "$_dir" ]

  # get back to marked place.
  back
  [ "$(pwd)" = "$_pwd" ]

  # clean
  rmdir "$_dir"
}

unittest_run
