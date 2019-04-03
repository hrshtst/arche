#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"
source "${THIS_DIR}/scripts/init/ubuntu-bionic/apt-get-common.sh"

add_apt_repository ppa:gophers/archive

declare -a requested_packages=(
  'golang-1.11-go'
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
  go get github.com/motemen/ghq
fi

set +e
# install peco
ver=v0.5.3
name=peco_linux_amd64
if ! has peco; then
  mkdir -p "${HOME}/usr/bin"
  cd "${HOME}/usr/bin"
  wget https://github.com/peco/peco/releases/download/${ver}/${name}.tar.gz
  tar xvfz ${name}
  mv ${name}/peco .
  rm -rf ${name} ${name}.tar.gz
  back_to_oldwd
fi
set -e
