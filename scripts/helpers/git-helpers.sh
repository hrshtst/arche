#!/usr/bin/env bash
# shellcheck disable=SC2119,SC2120

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
#   $ if is_git_repo; then
#   >   git pull origin master
#   > fi
#
# @param $1  Optional. Directory to check if a Git repo.
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

# Check if a specified path or the current directory is cloned from a
# remote repository or not.
#
# Example usage:
#
#   $ if is_git_cloned; then
#   >   git remote get-url origin
#   > fi
#
# @param $1  Optional. Directory to check if a Git repo.
# @return True (0) if the directory is cloned from remote.
#         False (>0) otherwise.
is_git_cloned() {
  is_git_available || return 1

  local dir
  dir="${1:-.}"

  mark
  cd "$dir" || return 1
  if ! is_git_repo; then
    e_error "$dir is not a git repository."
    back
    return 1
  fi

  local retval
  git remote show | grep -q origin
  retval=$?
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
  if [[ $retval != 0 ]] && is_git_cloned; then
    git ls-remote --heads --quiet origin "$branch" >/dev/null
    retval=$?
  fi
  back
  return $retval
}

# Get the current branch name. If the repository is detached at a
# certain commit, nothing returns.
#
# @param $1  Optional. Path to repository.
# @return  Return the current branch name to stdout.
git_get_branch_name() {
  is_git_available || return 1

  local repo
  repo="${1:-.}"

  if is_git_repo "$repo"; then
    mark
    cd "$repo" || return 1
  else
    e_error "$repo is not a repository"
    return 1
  fi

  if branch=$(git symbolic-ref --short -q HEAD); then
    echo "$branch"
  else
    echo ""
  fi
  back
}

# Checkout a specified branch. Check the existence of the specified
# branch beforehand. If an option '-c' is provided, confirm
# destination branch to checkout with user before execution.
#
# Example usage:
#   $ git_checkout -c "develop"
#
# @option -c Confirm destination branch before checkout.
# @param $1  Branch name to checkout.
# @param $2  Optional. Path to repository.
git_checkout() {
  # Option parsing.
  local confirm=false
  OPTIND=1
  while getopts ":c" opt; do
    case $opt in
      c)
        confirm=true
        ;;
      \?)
        e_warning "Invralid option: -$OPTARG"
        ;;
    esac
  done
  shift "$((OPTIND - 1))"

  local dst_branch repo
  dst_branch="${1:-}"
  repo="${2:-.}"

  # Change directory into the specified repository.
  if is_git_repo "$repo"; then
    mark
    cd "$repo" || return 1
  else
    e_error "$repo is not a repository"
    return 1
  fi

  # Check if the specified branch exists in the repository.
  if ! git_branch_exists "$dst_branch"; then
    e_error "$dst_branch does not exist in $repo"
    back
    return 1
  fi

  # Checkout the branch.
  local cur_branch msg
  cur_branch="$(git_get_branch_name)"
  msg="Are you sure to checkout '$dst_branch'"
  if [[ "$cur_branch" != "$dst_branch" ]]; then
    if [[ "$confirm" = false ]] || ask "$msg"; then
      git checkout -q "$dst_branch"
    fi
  fi
  back
}
