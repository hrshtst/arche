#!/usr/bin/env bash

# Avoid double sourcing of this file.
set +u
if [[ -n "$__GIT_HELPERS_SH_INCLUDED__" ]]; then
  set -u
  return
fi
__GIT_HELPERS_SH_INCLUDED__=yes

# Treat unset variable as an error.
set -u

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include utils.sh
# shellcheck source=utils.sh
source "${THIS_DIR}/utils.sh"

# Check if a git command is available on the system.
#
# @return True (0) if git command is available.
#         False (>0) otherwise.
readonly __MSG_GIT_NOT_INSTALLED="Git is not installed on the system."
is_git_available() {
  if ! has "git"; then
    e_error "$__MSG_GIT_NOT_INSTALLED"
    return 1
  fi
  return 0
}

# Check if a specified path or the current directory is a Git
# repository or not.
#
# Example usage:
#
#   $ if is_git_repository; then
#   >   git pull origin master
#   > fi
#
# @param $1  Directory to check if a Git repo.
# @return True (0) if the directory is a Git repo.
#         False (>0) if the  directory is not a Git repo.
is_git_repo() {
  is_git_available || return 1

  local dir
  dir="${1:-.}"
  if [[ ! -d "$dir" ]]; then
    return 1
  fi

  local retval=0
  mark
  cd "$dir" || return 1
  if ! git rev-parse --git-dir >/dev/null 2>&1; then
    retval=1
  fi
  back
  return $retval
}

# Check if a specified branch name does exists in a repository.
#
# Example usage:
#
#   $ if git_branch_exists "develop"; then
#   >   git checkout "develop"
#   > fi
#
# @param $1  Branch name to check.
# @param $2  Optional. Path to repository.
# @return True (0) If the branch name exists in the repository.
#         False (>0) Otherwise.
git_branch_exists() {
  is_git_available || return 1

  local branch repo retval
  branch="$1"
  repo="${2:-.}"

  mark
  cd "$repo" || return 1
  git rev-parse --verify --quiet "$branch" >/dev/null
  retval=$?
  back
  return $retval
}
