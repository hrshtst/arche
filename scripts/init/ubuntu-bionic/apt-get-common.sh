#!/usr/bin/env bash

installed_packages=($(apt list --installed 2>/dev/null >&1 | grep -v deinstall | awk -F/ '{print $1}'))
if [[ "${installed_packages[0]}" = "Listing..." ]]; then
  # remove the first item since it is not a package name
  unset installed_packages[0]
  installed_packages=("${installed_packages[@]}")
fi

declare -a requested_packages=()
declare -a missing_packages=()
find_missing_packages() {
  for package in "${requested_packages[@]}"; do
    found=false
    for e in "${installed_packages[@]}"; do
      if [[ "${e}" = "${package}" ]]; then
        echo "'${package}' is already installed"
        found=true
        break
      fi
    done
    if [[ "${found}" = false ]]; then
      missing_packages+=("${package}")
    fi
  done
}

add_apt_repository() {
  local repo="$1"

  # sudo add-apt-repository -uy "${repo}"
  echo sudo add-apt-repository -uy "${repo}"
}
