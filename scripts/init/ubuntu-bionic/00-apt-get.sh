#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"
source "${THIS_DIR}/scripts/init/ubuntu-bionic/apt-get-common.sh"

# add repositories
sudo apt install -y software-properties-common
add_apt_repository ppa:kelleyk/emacs
sudo apt upgrade -y
[[ $? ]] && info "Package update done."

declare -a requested_packages=(
  'automake'
  'autotools-dev'
  'build-essential'
  'clang'
  'clang-format'
  'clang-tidy'
  'cmake'
  'cmigemo'
  'curl'
  'emacs-mozc-bin'
  'emacs26'
  'ffmpeg'
  'flake8'
  'fontforge'
  'fonts-ipafont'
  'freeglut3-dev'
  'gfortran'
  'git'
  'imagemagick'
  'libclang-dev'
  'libgl1-mesa-dev'
  'libglew-dev'
  'libglu1-mesa-dev'
  'libjpeg-dev'
  'libncurses5-dev'
  'libncursesw5-dev'
  'libpng-dev'
  'libx11-dev'
  'libxext-dev'
  'libxml2'
  'libxpm-dev'
  'lv'
  'mercurial'
  'pandoc'
  'powerline'
  'pstoedit'
  'python3-dev'
  'python-dev'
  'python-virtualenv'
  'silversearcher-ag'
  'vim'
  'xclip'
  'xsel'
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
  sudo apt install -y "${missing_packages[@]}"
  [[ $? ]] && ok
fi
