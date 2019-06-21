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

test_abspath() {
  mark && cd "${HOME}"
  local _path="$(abspath develop)"
  local _expected="${HOME}/develop"
  [ "${_path}" = "${_expected}" ]
  getback
}
test_abspath

test_parentdir() {
  local _parent="$(parentdir "${HOME}/develop")"
  local _expected="${HOME}"
  [ "${_parent}" = "${_expected}" ]
}
test_parentdir

test_ask() {
  # Skip this test
  return

  if ask "Are you OK?"; then
    echo "yes"
  else
    echo "no"
  fi
}
test_ask

test_compare_ver_string() {
  compare_ver_string 4.2 4.2 && [ $? -eq 0 ]
  compare_ver_string 4.5 4.2 || [ $? -eq 1 ]
  compare_ver_string 4.1 4.2 || [ $? -eq 2 ]
}
test_compare_ver_string

test_is_get_repository() {
  mark && cd $HOME
  ! is_git_repository
  ! is_git_repository "hoge"
  ! is_git_repository "src"
  getback
  is_git_repository
}
test_is_get_repository

test_checkout_with_confirm() {
  return
  git_checkout_with_confirm "master"
}
test_checkout_with_confirm

test_git_update() {
  # Skip this test
  return

  git_update
}
test_git_update

test_git_clone_or_update() {
  # Skip this test
  return

  local _url="https://github.com/octocat/Spoon-Knife"
  local _dir="$HOME/tmp"
  mkdir -p "${_dir}"

  e_header "Clone"
  git_clone_or_update "${_url}" "${_dir}"

  e_header "Update"
  git_clone_or_update "${_url}" "${_dir}"

  e_header "Checkout"
  git_clone_or_update "${_url}" "${_dir}" "test-branch"

  rm -rf "${_dir}/Spoon-Knife"
}
test_git_clone_or_update

echo "All tests were passed."
