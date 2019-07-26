#!/usr/bin/env bash
# shellcheck disable=SC2119,SC2120

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

  setup_git_repo() {
    local dir
    dir="$1"
    mkdir -p "$dir"
    if [[ ! -d "$dir/.git" ]]; then
      cd "$dir" || return 1
      git init >/dev/null
      git commit --allow-empty -m "Initial commit" >/dev/null
      cd - >/dev/null || return 1
    fi
  }

  mkdir -p "$dummy"
  setup_git_repo "$repo1"
  setup_git_repo "$repo2"

  cd "$repo1" && git branch -f "develop"
  cd "$repo2" && git branch -f "hotfix"
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
  cd "$workspace" || return 1
  ! is_git_repo
  ! is_git_repo "dummy"
  ! is_git_repo "noexist"
  is_git_repo "repo1"

  cd "repo2" || return 1
  is_git_repo
}

testcase_git_branch_exists() {
  cd "$repo1" || return 1
  git_branch_exists "develop"
  ! git_branch_exists "hoge"

  cd .. || return 1
  git_branch_exists "master" "repo2"
  ! git_branch_exists "develop" "repo2"
}

testcase_git_get_branch_name() {
  cd "$repo1" || return 1
  git checkout -q master
  test "$(git_get_branch_name)" = "master"

  git checkout -q develop
  test "$(git_get_branch_name)" = "develop"

  git checkout -q master^0
  test -z "$(git_get_branch_name)"

  cd "$repo2" || return 1
  git checkout -q hotfix
  cd .. || return 1
  test "$(git_get_branch_name "repo2")" = "hotfix"
}

setup
unittest_run
teardown
