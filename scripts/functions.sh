#!/usr/bin/env bash

warn() {
  echo "$(tput setaf 1)$@$(tput sgr0)"
}

error() {
  echo "$(tput setaf 1)$@$(tput sgr0)" >&2
}

info() {
  echo "$(tput setaf 2)$@$(tput sgr0)"
}

abort() {
  error "$@" 1>&2
  exit -1
}

ok() {
  info "done! ✔"
}

has() {
  type "$1" >/dev/null 2>&1
}

contains() {
  local list="$1"
  local item="$2"
  if [[ $list =~ (^|[[:space:]])"$item"($|[[:space:]]) ]]; then
    return 0
  else
    return 1
  fi
}

abspath() {
  if [[ -d "$1" ]]; then
    # dir
    (cd "$1"; pwd)
  elif [[ -f "$1" ]]; then
    # file
    if [[ $1 = /* ]]; then
      echo "$1"
    elif [[ $1 == */* ]]; then
      echo "$(cd "${1%/*}"; pwd)/${1##*/}"
    else
      echo "$(pwd)/$1"
    fi
  fi
}

parentdir() {
  local path="$(abspath "$1")"
  echo "${path%/*}"
}

lower() {
  echo "${1,,}"
}

upper() {
  echo "${1^^}"
}

detect_os() {
  export OS_NAME
  export OS_VERSION
  export OS_CODENAME
  set +e
  if [[ -f /etc/os-release ]]; then
    source /etc/os-release
    OS_NAME="$(lower $NAME)"
    OS_VERSION="$VERSION_ID"
    OS_CODENAME="$(lower $VERSION_CODENAME)"
  elif has lsb_release; then
    OS_NAME="$(lower $(lsb_release -si))"
    OS_VERSION="$(lsb_release -sr)"
    OS_CODENAME="$(lower $(lsb_release -sc))"
  elif [[ -f /etc/lsb-release ]]; then
    source /etc/lsb-release
    OS_NAME="$(lower $DISTRIB_ID)"
    OS_VERSION="$DISTRIB_RELEASE"
    OS_CODENAME="$(lower $DISTRIB_CODENAME)"
  else
    OS_NAME="$(lower $(uname -s))"
    OS_VERSION="$(uname -r)"
    OS_CODENAME=
  fi
  set -e
}

vercomp() {
  if [[ $1 == $2 ]]; then
    return 0
  fi
  local IFS=.
  local i ver1=($1) ver2=($2)
  # fill empty fields in ver1 with zeros
  for ((i=${#ver1[@]}; i<${#ver2[@]}; i++)); do
    ver1[i]=0
  done
  for ((i=0; i<${#ver1[@]}; i++)); do
    if [[ -z ${ver2[i]} ]]; then
      # fill empty fields in ver2 with zeros
      ver2[i]=0
    fi
    if ((10#${ver1[i]%%[^0-9]*} > 10#${ver2[i]%%[^0-9]*})); then
      return 1
    fi
    if ((10#${ver1[i]%%[^0-9]*} < 10#${ver2[i]%%[^0-9]*})); then
      return 2
    fi
  done
  return 0
}

back_to_oldwd() {
  cd - &>/dev/null
}

is_git_repo() {
  if git rev-parse --git-dir > /dev/null 2>&1; then
    return 0
  else
    return 1
  fi
}

git_update() {
  git pull origin master
  git submodule init
  git submodule update
  git submodule foreach git pull origin master
}

_git_clone_or_update() {
  set +e
  if ! is_git_repo; then
    git clone --recursive "${uri}"
  else
    git_update
  fi
  set -e
}

git_clone_or_update() {
  local uri="$1"
  local dest="${2:-}"
  local clone=false
  local update=false

  set +e
  if ! has git; then
    error "'git' command is unavailable."
    return 1
  fi
  set -e

  if [[ -z "${dest}" ]]; then
    _git_clone_or_update
  else
    mkdir -p "${dest}"
    cd "${dest}"
    _git_clone_or_update
    back_to_oldwd
  fi
}
