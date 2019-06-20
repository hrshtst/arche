#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eEu

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include functions to testify.
source "${THIS_DIR}/functions.sh"

test_has() {
  has "pwd"
  ! has "hoge"
}
test_has

test_mark() {
  local _pwd="$(pwd)"
  mark
  cd ..
  cd ..
  getback
  [ "$(pwd)" = "${_pwd}" ]
}
test_mark

test_compare_ver_string() {
  compare_ver_string 4.2 4.2 && [ $? -eq 0 ]
  compare_ver_string 4.5 4.2 || [ $? -eq 1 ]
  compare_ver_string 4.1 4.2 || [ $? -eq 2 ]
}
test_compare_ver_string

test_is_get_repository() {
  cd $HOME
  ! is_git_repository
  cd - 1>/dev/null
  is_git_repository
}
test_is_get_repository

test_git_update() {
  :
  #git_update
}
test_git_update

echo "All tests were passed."
