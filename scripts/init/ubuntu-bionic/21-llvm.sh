#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"
source "${THIS_DIR}/scripts/init/ubuntu-bionic/apt-get-common.sh"

# See: https://apt.llvm.org/
curl https://apt.llvm.org/llvm-snapshot.gpg.key | sudo apt-key add -
sudo sh -c "cat << EOF > /etc/apt/sources.list.d/llvm.list
# i386 not available
deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic main
deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic main
# 7
deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-7 main
deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-7 main
# 8
deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-8 main
deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-8 main
EOF"
sudo apt update

declare -a requested_packages=(
  'clang-9'
  'clang-format-9'
  'clang-tidy-9'
  'clang-tools-9'
  'libclang-9-dev'
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
  sudo update-alternatives --install /usr/bin/clang clang /usr/bin/clang-9 100
  sudo update-alternatives --install /usr/bin/clang++ clang++ /usr/bin/clang++-9 100
  sudo update-alternatives --install /usr/bin/clang-format clang-format /usr/bin/clang-format-9 100
  sudo update-alternatives --install /usr/bin/clang-tidy clang-tidy /usr/bin/clang-tidy-9 100
  sudo update-alternatives --install /usr/bin/clangd clangd /usr/bin/clangd-9 100
fi
