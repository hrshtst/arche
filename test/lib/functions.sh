#!/usr/bin/env bash

#
# Display and logging utilities
#

# Define faces and colors
bold=$(tput bold)
underline=$(tput sgr 0 1)
reset=$(tput sgr0)

purple=$(tput setaf 171)
red=$(tput setaf 1)
green=$(tput setaf 76)
tan=$(tput setaf 3)
blue=$(tput setaf 38)

# Headers and logging
e_header() {
  printf "\n${bold}${purple}==========  %s  ==========${reset}\n" "$@"
}

e_arrow() {
  printf "➜ $@\n"
}

e_success() {
  printf "${green}✔ %s${reset}\n" "$@"
}

e_error() {
  printf "${red}✖ %s${reset}\n" "$@" 1>&2
}

e_warning() {
  printf "${tan}➜ %s${reset}\n" "$@"
}

e_underline() {
  printf "${underline}${bold}%s${reset}\n" "$@"
}

e_bold() {
  printf "${bold}%s${reset}\n" "$@"
}

e_note() {
  printf "${underline}${bold}${blue}Note:${reset}  ${blue}%s${reset}\n" "$@"
}

e_purple() {
  printf "${purple}%s${reset}" "$@"
}

e_red() {
  printf "${red}%s${reset}" "$@"
}

e_green() {
  printf "${green}%s${reset}" "$@"
}

e_tan() {
  printf "${tan}%s${reset}" "$@"
}

e_blue() {
  printf "${blue}%s${reset}" "$@"
}

# Exits the script with an error code.
abort() {
  e_error "$@"
  exit -1
}

# Check if a command is installed or available on the $PATH
# environment variable.
#
# Example usage:
#
#   $ if ! has some_command; then
#   >   echo "some_command is required."
#   > fi
#
# @param $1 Command to check if it exists.
# @return True (0) if the command is installed,
#         False (>0) if the command is not available.
has() {
  type "$1" >/dev/null 2>&1
}

# Add a directory to the top of the directory stack. NOTE: This
# function does not change the current working directory.
#
# Example usage:
#
#   $ cd $HOME/.emacs.d
#   $ mark           # mark here
#   $ cd straight/
#   $ cd repos/
#   $ getback        # get back to $HOME/.emacs.d
#
# @param $1  Directory to make a mark. If omitted, the current
#            directory will be marked.
# @see getback()
mark() {
  local dir="${1:-$(pwd)}"
  pushd -n "$dir" 1>/dev/null
}

# Get back to the top of the directory stack, and remove it from the
# stack.
#
# @see mark()
getback() {
  popd 1>/dev/null
}

# Compares two version strings and returns the result of comparison as
# an exit-status. The implementation of this function is based on
# https://stackoverflow.com/a/4025065
#
# Example usage:
#
#   $ set +e; compare_ver_string ${BASH_VERSION} 4.2; result=$?; set -e
#   $ if [[ result == 2 ]]; then
#   >   echo "Minimum requirement for bash version is 4.2, abort."
#   >   exit 1
#   > fi
#
# @param $1 ver1  First version string to compare
# @param $2 ver2  Second version string to compare
# @return 0 if the two versions are equal,
#         1 if the first version is bigger,
#         2 if the first version is lower.
compare_ver_string() {
  if [[ $1 == $2 ]]; then
    return 0
  fi
  local IFS=.
  local i ver1=($1) ver2=($2)
  # fill empty fields in ver1 with zeros
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++)); do
    ver1[i]=0
  done
  for ((i=0; i<${#ver1[@]}; i++)); do
    if [[ -z ${ver2[i]} ]]; then
      # fill empty fields in ver2 with zeros
      ver2[i]=0
    fi
    if ((10#${ver1[i]%%[^0-9]*} > 10#${ver2[i]%%[^0-9]*})); then
      return 1
    fi
    if ((10#${ver1[i]%%[^0-9]*} < 10#${ver2[i]%%[^0-9]*})); then
      return 2
    fi
  done
  return 0
}

#
# Git operations
#

readonly __MSG_GIT_NOT_INSTALLED="Git is not installed on the system."

# Check if the current working directory is a Git repository or not.
#
# Example usage:
#
#   $ if is_git_repository; then
#   >   git pull origin master
#   > fi
#
# @return True (0) if the current working directory is a Git repo.
#         False (>0) if the current working directory is not a Git
#         repo.
is_git_repository() {
  if ! has "git"; then
    e_error "${__MSG_GIT_NOT_INSTALLED}"
    return 1
  fi

  if git rev-parse --git-dir >/dev/null 2>&1; then
    return 0
  else
    return 1
  fi
}

# Pull updates from remote and subsequently updates submodules within
# the repository.
#
# @param $1 branch  Branch name to fetch (default: master)
# @return True (0) if sucessful,
#         False (>0) otherwise.
git_update() {
  if ! has "git"; then
    e_error "${__MSG_GIT_NOT_INSTALLED}"
    return 1
  fi

  local branch="${1:-master}"
  git pull origin "${branch}"
  git submodule init
  git submodule update
}
