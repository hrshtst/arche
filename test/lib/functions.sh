#!/usr/bin/env bash

# Displays a string of text to the standard output.
warn() {
  echo "$(tput setaf 1)$@$(tput sgr0)"
}

# Displays a string of text to the standard error.
error() {
  echo "$(tput setaf 1)$@$(tput sgr0)" >&2
}

# Displays a string of text in green to the standard output.
info() {
  echo "$(tput setaf 2)$@$(tput sgr0)"
}

# Exits the script with an error code.
abort() {
  error "$@"
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
