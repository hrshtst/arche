#!/usr/bin/env bash

set -eu

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include utility functions.
source "${THIS_DIR}/lib/functions.sh"

# Check bash version.
BASH_VERSION_MINIMUM=4.2
set +e
compare_ver_string ${BASH_VERSION} ${BASH_VERSION_MINIMUM}; result=$?
if [[ $result == 2 ]]; then
  abort "Minimum requirement for bash version is \
${BASH_VERSION_MINIMUM}, but running on ${BASH_VERSION}."
fi
set -e

# Default values.
HOME_DIR=${HOME}      # This is treated as home directory for deployment.
OVERWRITE=false

# Usage.
usage() {
  name=${BASH_SOURCE[0]}
  cat <<EOF

Utility script for dotfile deployment and pacakge installation.

This script allows you to deploy dotfiles to user's home directory and install required packages. Symbolic links to dotfiles (and dot directory) are created on a directory, which is home directory by default or can be specified. Since the installation process differs among systems which the script is running, the way to install packages (i.e. package manager) and required packages must be defined depending on a system in advance.

Currently the following OSs are supported for installation:
  - Ubuntu 18.04 (bionic)

Usage:
  $name [options] [command]

Options:
  -d, --directory  Specify directory to deploy dotfiles.
                   (default: ${HOME_DIR})
  -f, --force      $(warn "** warning **") Overwrite exisiting dotfiles.
  -h, --help       Show this message and exit.

Commands:
  update           Pull updates for this repository from remote.
  deploy           Create symlinks to dotfiles in home directory.
  init             Install required packages.
  clean            Remove symlinks in home directory.
  help             Show this message and exit.
EOF
}

# Argument parsing.
POSITIONAL=()
while [[ $# > 0 ]]; do
  key="$1"
  case $key in
    -d|--directory)
      HOME_DIR="$2"
      shift 2
      ;;
    -f|--force)
      OVERWRITE=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      POSITIONAL+=("$1")
      shift
      ;;
  esac
done
set -- "${POSITIONAL[@]}"

# Subcommand: update
# This function pulls updates for this repository from remote.
_update() {
  git_update "master"
}

# Subcommand: deploy
# This function creates symbolic links to dotfiles into $HOME_DIR.
#
# @see $HOME_DIR
_deploy() {
  echo "deploy ${THIS_DIR} to ${HOME_DIR}"
}

# Subcommand: init
# This function installs required packages depending on OSs.
_init() {
  echo "init"
}

# Subcommand: clean
# This function cleans deployed symlinks from $HOME_DIR
#
# @see $HOME_DIR
_clean() {
  echo "clean from ${HOME_DIR}"
}

main () {
  local command=$1

  case $command in
    update)
      _update
      ;;
    deploy)
      _deploy
      ;;
    init*)
      _init
      ;;
    clean)
      _clean
      ;;
    *)
      usage
      ;;
  esac
}

if [[ $# > 0 ]]; then
  main $1
else
  usage
fi
