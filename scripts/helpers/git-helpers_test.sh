#!/usr/bin/env bash

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
cd "$THIS_DIR" || exit 1

# Include unit testing framework.
# shellcheck source=unittest.sh
source "unittest.sh"

# Include functions to test.
# shellcheck source=git-helpers.sh
source "git-helpers.sh"

workspace="/tmp/workspace-$PPID"
repo1="$workspace/repo1"
repo2="$workspace/repo2"
dummy="$workspace/dummy"
setup() {
  is_git_available || exit 1

  mkdir -p "$repo1" "$repo2" "$dummy"
  if [[ ! -d "$repo1/.git" ]]; then
    cd "$repo1" || return 1
    git init >/dev/null
  fi
  if [[ ! -d "$repo2/.git" ]]; then
    cd "$repo2" || return 1
    git init >/dev/null
  fi
  cd "$THIS_DIR" || return 1
}

teardown() {
  for dir in /tmp/workspace-*; do
    if [[ ! "$dir" = "$workspace" ]]; then
      rm -rf "$dir"
    fi
  done
  unset -v workspace
  unset -v repo1
  unset -v repo2
  unset -v dummy
}

testcase_is_get_repo() {
  markcd "$workspace"
  ! is_git_repo
  ! is_git_repo "dummy"
  ! is_git_repo "noexist"
  is_git_repo "repo1"
  cd "repo2" || return 1
  is_git_repo
  back
}

setup
unittest_run
teardown
