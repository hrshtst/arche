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

# Make links between files. The target file/directory will be
# converted to its absolute path. If global variable `OVERWRITE` is
# set to true, the link to be created will overwirte the existing
# file.
#
# Example usage:
#
#   $ _make_link "target_file" "$HOME/link_to_be_created"
#
# @global $OVERWRITE If set to true, overwrite the existing file.
# @param $1 target  Target file or directory.
# @param $2 link    Directory or link name to create a link to
#                   the target.
_make_link() {
  local target="$(abspath "$1")"
  local linkname="$2"

  if [[ "${OVERWRITE}" = true && -e "${linkname}" ]]; then
    rm -rf "${linkname}"
  fi
  if [[ ! -e "${linkname}" ]]; then
    ln -snfv "${target}" "${linkname}"
  else
    e_warning "'${linkname}' already exists. (${FUNCNAME[0]})"
  fi
}

# Create links to all files/directories found in a specified
# directory. Directory to search files to create links must be
# specified as the relative path to dotfiles directory to preserve the
# relative structure of the directory tree. Home directory means the
# location to create the links.
#
# Example usage:
#
#   $ _deploy_files_in_dir "${THIS_DIR}" "usr/bin" "${HOME_DIR}"
#
# @param $1 dotfiles_dir  Dotfiles directory.
# @param $2 target_dir    Relative path to directory to search files
#                         to create links from dotfiles directory.
# @param $3 home_dir      Directory to place created links.
_deploy_files_in_dir() {
  local dotfiles_dir="$(abspath "$1")"
  local target_dir="$2"
  local home_dir="${3:-${HOME}}"

  mark && cd "${dotfiles_dir}"

  # ${target_dir} must be a directory
  if [[ ! -d "${target_dir}" ]]; then
    e_error "${dotfiles_dir}/${target_dir} is not a directory. (${FUNCNAME[0]})"
    getback
    return 1
  fi

  # Create missing directories in $target_dir to $home_dir.
  mkdir -p "${home_dir}/${target_dir}"
  find "${target_dir}" -mindepth 1 -type d -print0 \
    | xargs -r0 -n 1 -I{} mkdir -p "${home_dir}/{}"

  # Find files in $target_dir recursively.
  unset list
  while IFS= read -r -d '' file; do
    list+=("$file")
  done < <(find "${target_dir}" -type f -print0)

  # Create links to found files to $home_dir.
  for file in "${list[@]}"; do
    _make_link "${file}" "${home_dir}/${file}"
  done
  getback
}

# Create links to all files which begins with 'dot' in dotfiles
# directory to a specified location.
#
# Example usage:
#
#   $ _deploy_dotfiles "${THIS_DIR}" "${HOME_DIR}"
#
# @global $EXCLUSIONS  Exclude files/directories listed in this.
# @param $1 dotfiles_dir  Directory to search dotfiles.
# @param $2 home_dir      Directory to place created links.
readonly EXCLUSIONS=".git .gitignore .config"
_deploy_dotfiles() {
  local dotfiles_dir="$(abspath "$1")"
  local home_dir="${2:-${HOME}}"

  mark && cd "${dotfiles_dir}"

  # Find files/directories in $home_dir whose name begins with '.'.
  for name in .??*; do
    contains "${name}" "${EXCLUSIONS}" && continue
    _make_link "${name}" "${home_dir}/${name}"
  done
  getback
}

# Subcommand: deploy
# This function creates symbolic links to dotfiles into $HOME_DIR.
#
# @see $HOME_DIR
_deploy() {
  _deploy_dotfiles "${THIS_DIR}" "${HOME_DIR}"
  _deploy_files_in_dir "${THIS_DIR}" ".config" "${HOME_DIR}"
  _deploy_files_in_dir "${THIS_DIR}" "usr/bin" "${HOME_DIR}"
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
