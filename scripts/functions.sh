#!/usr/bin/env bash

warn() {
  echo "$(tput setaf 1)$@$(tput sgr0)"
}

error() {
  echo "$(tput setaf 1)$@$(tput sgr0)" >&2
}

abort() {
  error "$1" 1>&2
  exit "${2:-1}"
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

