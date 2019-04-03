#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"
source "${THIS_DIR}/scripts/init/ubuntu-bionic/apt-get-common.sh"

add_apt_repository ppa:inkscape.dev/stable

declare -a requested_packages=(
  'inkscape'
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
  info "Installing eqtexsvg..."
  mkdir -p "${HOME}/src"
  cd  "${HOME}/src"
  wget https://github.com/julienvitard/eqtexsvg/archive/master.tar.gz
  [[ $? ]] || abort "Failed to download"
  tar xvf master.tar.gz
  chmod +x eqtexsvg-master/eqtexsvg.py
  sudo cp -p eqtexsvg-master/{eqtexsvg.py,eqtexsvg.inx} /usr/share/inkscape/extensions
  [[ $? ]] && ok
  back_to_oldwd
fi

