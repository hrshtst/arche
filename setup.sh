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
  init             Install required packages. If package names are provided,
                   install only specified ones. If an option '--list' is
                   provided, list all available packages and exit.
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
  unset -v list
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
  detect_os
  msg="Detected OS: ${OS_NAME} ${OS_VERSION} (${OS_CODENAME})"
  init_script="${THIS_DIR}/lib/init/${OS_NAME}-${OS_CODENAME:-${OS_VERSION}}.sh"
  if [[ ! -f "${init_script}" ]]; then
    e_error "$msg"
    e_error "Unable to find ${init_script}"
    abort "Abort."
  fi

  e_note "$msg"
  if ask "Are you sure to execute ${init_script}?"; then
    _keep_sudo
    bash "${init_script}" "${THIS_DIR}" "$@"
    _reset_sudo
  fi
}

# Check if a file is a symbolic link which links to a file in dotfile
# directory.
#
# Example usage:
#
#   $ if _check_if_link_to_dotfile "${HOME_DIR}" "${THIS_DIR}" ".tmux.conf"
#   > then
#   >   rm -f "${HOME_DIR}/.tmux.conf"
#   > fi
#
# @param $1 home_dir  Base directory to find if a file links to dotfile.
# @param $2 dotfiles_dir  Base directory which contains target files.
# @param $3 filename  Relative path to home directory or dotfiles
#                     directory.
# @return True(0)  If a file in home directory is a symbolic link to
#                  a file in dotfiles directory.
#         False(>0) Otherwise.
_check_if_link_to_dotfile() {
  local _link="${1}/${3}"       # Link file to be checked
  local _target="${2}/${3}"     # Target file

  if [[ -L "${_link}" ]]; then
    local parent1="$(parentdir $(readlink "${_link}"))"
    local parent2="$(parentdir ${_target})"
    if [[ "${parent1}" = "${parent2}" ]]; then
      return 0
    else
      return 1
    fi
  else
    return 1
  fi
}

# Remove a symbolic link if it links to a dotfile. Path to the
# symbolic link should be specified as a relative path from dotfile
# directory or home directory. Dotfile directory and home directory
# should be given as well.
#
# Example usage:
#
#   $ _remove_symlinks_to_dotfile "${HOME_DIR}" "${THIS_DIR}"
#
# @global DIRECTORIES_TO_INCLUDE  Directories to find files
readonly DIRECTORIES_TO_INCLUDE=".config usr/bin"
_remove_symlinks_to_dotfile() {
  local home_dir="${1}"
  local dotfiles_dir="${2}"

  mark && cd "${dotfiles_dir}"

  unset -v list
  # Find dotfiles in ${dotfiles_dir}
  while IFS= read -r -d '' file; do
    list+=("$(echo "${file}" | sed "s|^\./||" )")
  done < <(find . -maxdepth 1 -name ".??*" -print0)
  # Find files in ${dotfiles_dir}/$DIRECTORIES_TO_INCLUDE
  while IFS= read -r -d '' file; do
    list+=("$file")
  done < <(find $DIRECTORIES_TO_INCLUDE -type f -print0)

  # Remove if a file in home directory has a link to dotfiles
  # directory
  for file in "${list[@]}"; do
    if _check_if_link_to_dotfile "${home_dir}" "${dotfiles_dir}" "${file}"
    then
      rm -f "${home_dir}/${file}"
    fi
  done
  getback
}

# Remove broken symbolic links in home directory. Global variable
# DIRECTORIES_TO_INCLUDE specifies relative paths to look for broken
# links additionally.
#
# Example usage:
#
#   $ _remove_broken_symlinks "${HOME_DIR}"
#
# @global DIRECTORIES_TO_INCLUDE  Directories to look for broken links
_remove_broken_symlinks() {
  local home_dir="$(abspath ${1})"

  __remove_broken_symlinks() {
    find "${1}" -maxdepth 1 -type l ! -exec test -e {} \; -exec rm -f {} \;
  }
  export -f __remove_broken_symlinks

  mark && cd "${home_dir}"

  # Remove broken symbolic links in home directory
  __remove_broken_symlinks "${home_dir}"
  # Remove broken symbolic links in specified directory
  find $DIRECTORIES_TO_INCLUDE -type d -print0 \
    | xargs -r0 -n 1 -I{} bash -c '__remove_broken_symlinks "$@"' _ "${home_dir}/{}"
  getback
}

# Subcommand: clean
# This function cleans deployed symlinks from $HOME_DIR
#
# @see $HOME_DIR
_clean() {
  _remove_symlinks_to_dotfile "${HOME_DIR}" "${THIS_DIR}"
  _remove_broken_symlinks "${HOME_DIR}"
}

main () {
  local command=$1
  shift

  case $command in
    update)
      _update
      ;;
    deploy)
      _deploy
      ;;
    init*)
      _init $@
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
  main $@
else
  usage
fi
