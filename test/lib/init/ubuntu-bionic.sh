#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/lib/functions.sh"
source "${THIS_DIR}/lib/init_packages.sh"

## Emacs
__init_packages_emacs__init() {
  init_packages_add_repository "ppa:kelleyk/emacs"
}

__init_packages_emacs__install() {
  init_packages_depends "emacs-mozc-bin emacs26"
}
__init_packages_emacs__config() {
  echo "This is 'config' for 'emacs'"
}

## Inkscape
__init_packages_inkscape__init() {
  init_packages_add_repository "ppa:inkscape.dev/stable"
}

__init_packages_inkscape__install() {
  init_packages_depends 'inkscape' 'pstoedit'
}

__init_packages_inkscape__config() {
  echo "This is 'config' for 'inkscape'"
}

## tmux
__init_packages_tmux() {
  init_packages_depends "tmux xclip xsel"
  init_packages_always_config
}

__init_packages_tmux__config() {
  echo git clone https://github.com/tmux-plugins/tpm ${HOME}/.tmux/plugins/tpm
}

## LLVM
__init_packages_clang_9__config() {
  echo "This is 'config' for 'clang-9'"
}

## TeX
__init_packages_tex() {
  init_packages_depends \
    'ghostscript' \
    'texlive-full' \
    'xzdec'
}

## Debug
__init_packages_debug__init() {
  init_packages_add_repository "ppa:debug"
}

__init_packages_debug__install() {
  init_packages_depends "debug-package debug-package-dev"
}

init_packages
