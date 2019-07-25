#!/usr/bin/env bash

# Avoid double sourcing of this file.
set +u
if [[ -n "$__UTILS_SH_INCLUDED__" ]]; then
  set -u
  return
fi
__UTILS_SH_INCLUDED__=yes

# Treat unset variable as an error.
set -u

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
  printf "➜ %s\n" "$@"
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
  exit 1
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
#   $ back        # get back to $HOME/.emacs.d
#
# @param $1  Directory to make a mark. If omitted, the current
#            directory will be marked.
# @see back()
# shellcheck disable=SC2120
mark() {
  local dir

  dir="${1:-$(pwd)}"
  pushd -n "$dir" 1>/dev/null
}

# Mark the current directory and change it to the destination. If the
# destination does not exist, make that directory.
#
# Example usage:
#
#   $ cd $HOME/.emacs.d
#   $ mark_cd straight
#   $ cd repos
#   $ back        # get back to $HOME/.emacs.d
# @param $1  Destination directory to change.
# @see mark()
# @see back()
markcd() {
  local dest

  dest="$1"
  mark "$(pwd)"
  mkdir -p "$dest"
  cd "$dest" || return 1
}

# Get back to the top of the directory stack, and remove it from the
# stack.
#
# @see mark()
back() {
  popd 1>/dev/null || return 1
}

# Convert a relative path to an absolute path.
#
# Example usage:
#
#   $ project_path="$(abspath develop/awesome_project)"
#
# @param $1  Relative path to a file or a directory.
# @return  Return its absolute path to stdout.
abspath() {
  # shellcheck disable=SC2164
  if [[ -d "$1" ]]; then
    # dir
    (cd "$1"; pwd)
  elif [[ -f "$1" ]]; then
    # file
    if [[ $1 = /* ]]; then
      echo "$1"
    elif [[ $1 == */* ]]; then
      echo "$(cd "${1%/*}"; pwd)/${1##*/}"
    else
      echo "$(pwd)/$1"
    fi
  else
    e_error "$1 does not exist."
  fi
}

# Return a path to the parent directory for a given file or directory.
#
# Example usage:
#
#   $ parent="$(parentdir develop/awesome_project)"
#
# @param $1 Path to a file or directory to find its parent directory.
# @return Return its parent directory path to stdout.
parentdir() {
  local path
  path="$(abspath "$1")"
  echo "${path%/*}"
}

# Make a prompt to ask user yes or no question.
#
# Example usage:
#
#   $ if ask "Are you sure?"; then
#   >   echo "Okay!"
#   > else
#   >   echo "Hmmm..."
#   > fi
#   Are you sure? [Y/n] y
#   Okay!
#
# @param $1 prompt  Prompt message.
# @return True (0) if the answer from user is yes,
#         False (>0) if the answer from user is no.
ask() {
  local prompt="${1:-Are you sure?}"
  prompt="$prompt [y/n] "
  local retval answer

  echo -n "$prompt"
  while read -r -n 1 -s answer; do
    if [[ $answer = [YyNn] ]]; then
      [[ $answer = [Yy] ]] && retval=0
      [[ $answer = [Nn] ]] && retval=1
      break
    fi
    echo
    echo "Please answer with y/n."
  done
  echo
  return $retval
}

# Check if a string is contained in a list separated with space.
#
# Example usage:
#
#   $ list="apple banana orange"
#   $ if contains "banana" "$list"; then
#   >   echo "banana is in list."
#   > fi
#
# @param $1 string  String to check.
# @param $2 list    List to be searched.
# @return True(0)  If the string is contained in the list.
#         False(>0) Otherwise.
contains() {
  local string="$1"
  shift
  local list=("$@")

  if [[ "${list[*]}" =~ (^|[[:space:]])"$string"($|[[:space:]]) ]]; then
    return 0
  else
    return 1
  fi
}

# Convert a string to a lower-case string.
#
# Example usage:
#
#   $ echo $(lower "Something.")
#   something.
#
# @see upper()
lower() {
  echo "${1,,}"
}

# Convert a string to a upper-case string.
#
# Example usage:
#
#   $ echo $(lower "Something.")
#   SOMETHING.
#
# @see lower()
upper() {
  echo "${1^^}"
}

# Detect the running system. Once this function executed, exported
# variables OS_NAME, OS_VERSION and OS_CODENAME are set. Note that the
# all the results are converted into lower-case strings.
#
# Example usage:
#
#   $ detect_os
#   $ echo $OS_NAME $OS_VERSION $OS_CODENAME
#   ubuntu 18.04 bionic
detect_os() {
  export OS_NAME
  export OS_VERSION
  export OS_CODENAME

  if [[ -f /etc/os-release ]]; then
    source /etc/os-release
    OS_NAME="$(lower $NAME)"
    OS_VERSION="$VERSION_ID"
    OS_CODENAME="$(lower $VERSION_CODENAME)"
  elif has lsb_release; then
    OS_NAME="$(lower "$(lsb_release -si)")"
    OS_VERSION="$(lsb_release -sr)"
    OS_CODENAME="$(lower "$(lsb_release -sc)")"
  elif [[ -f /etc/lsb-release ]]; then
    source /etc/lsb-release
    OS_NAME="$(lower $DISTRIB_ID)"
    OS_VERSION="$DISTRIB_RELEASE"
    OS_CODENAME="$(lower $DISTRIB_CODENAME)"
  else
    OS_NAME="$(lower "$(uname -s)")"
    OS_VERSION="$(uname -r)"
    OS_CODENAME=
  fi
}
