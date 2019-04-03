#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"
source "${THIS_DIR}/scripts/init/ubuntu-bionic/apt-get-common.sh"

declare -a requested_packages=(
  'build-essential'
  'curl'
  'git'
  'libbz2-dev'
  'libffi-dev'
  'liblzma-dev'
  'libncurses5-dev'
  'libncursesw5-dev'
  'libreadline-dev'
  'libsqlite3-dev'
  'libssl-dev'
  'llvm'
  'make'
  'python-openssl'
  'tk-dev'
  'wget'
  'xz-utils'
  'zlib1g-dev'
)

## to get sorted list, uncomment below and execute
# IFS=$'\n'
# sorted=($(sort <<<"${requested_packages[*]}"))
# printf "  '%s'\n" "${sorted[@]}"
# echo "num: ${#sorted[@]}"
# exit

find_missing_packages
if [[ "${#missing_packages[@]}" > 0 ]]; then
  info "Installing missing packages..."
  echo sudo apt install -y "${missing_packages[@]}"
  [[ $? ]] && ok
fi

git_clone_or_update https://github.com/pyenv/pyenv.git ${HOME}/.pyenv

