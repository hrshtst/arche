#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"
source "${THIS_DIR}/scripts/init/ubuntu-bionic/apt-get-common.sh"

add_apt_repository ppa:fish-shell/release-2

declare -a requested_packages=(
  'fish'
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
  sudo apt install -y "${missing_packages[@]}"
  [[ $? ]] && ok

  info "Installing fisherman..."
  here="$(pwd)"
  mkdir -p "${HOME}/src"
  cd  "${HOME}/src"
  curl -Lo ${HOME}/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
  git clone https://github.com/powerline/fonts.git --depth=1
  cd fonts
  ./install.sh
  mkdir -p ${HOME}/.local/share/fonts
  cd ${HOME}/.local/share/fonts
  ver=v2.0.0
  wget https://github.com/ryanoasis/nerd-fonts/releases/download/${ver}/FiraCode.zip
  unzip FiraCode.zip
  wget https://github.com/ryanoasis/nerd-fonts/releases/download/${ver}/FiraMono.zip
  unzip FiraMono.zip
  fc-cache -fv
  ok
  cd "${here}"
fi

