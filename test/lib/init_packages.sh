#!/usr/bin/env bash

# Extract package name by removing prefix and suffix from function
# name. For example, __init_packages_awesome__init is given, return
# awesome. If function name does not match the prefix, return nothing.
#
# @param $1 func  Function name to extract package name.
# @return Output package name to stdout.
_extract_package_name() {
  local func="${1}"
  local package=
  local regex_prefix="^__init_packages_([a-zA-Z0-9_]+)"
  local regex_suffix="([a-zA-Z0-9_]+)__.*$"

  # Remove prefix
  if [[ $func =~ $regex_prefix ]]; then
    package="${BASH_REMATCH[1]}"
  fi
  # Remove suffix
  if [[ $package =~ $regex_suffix ]]; then
    package="${BASH_REMATCH[1]}"
  fi

  # If $func does not start with __init_packages_, return null string.
  echo "${package}"
}

# Find all packages to be initialized, installed or configured. This
# function will look for functions which named
# '__init_packages_<name>' or '__init_packages_<name>__*' and add the
# package name to a global variable '__package_names'.
#
# @global __package_names  List of all package names.
declare -a __package_names=()
init_packages_find() {
  declare -a __package_names_tmp=()
  while read -r func; do
    package="$(_extract_package_name $func)"
    if [[ -n "${package}" ]]; then
      __package_names_tmp+=("${package}")
    fi
  done < <(declare -F | cut -d ' ' -f 3)

  # Remove duplicates.
  __package_names=($(printf "%s\n" "${__package_names_tmp[@]}" | sort -u))
}

# Normalize PPA name for checking the repository exists or not. This
# modifies the given arguments like as follows.
#   - Remove the prefix 'ppa:'
#
# Example usage:
#   $ _normalize_repository_name "ppa:kelleyk/emacs"
#   kelleyk/emacs
#
# @see init_packages_repository_exists()
_normalize_repository_name() {
  local _given="${1}"
  local _ppa="${_given#ppa:}"

  echo "${_ppa}"
}

# Check if a repository is registered in the system.
#
# @param $1 ppa  Repository name.
# @return True(0)  If a repository exists.
#         False(>0) Otherwise.
init_packages_repository_exists() {
  local ppa="$(_normalize_repository_name "$1")"
  local sources="/etc/apt/sources.list /etc/apt/sources.list.d/*"

  if grep -q "^deb .*$ppa" $sources; then
    return 0
  else
    return 1
  fi
}

# Add a repository if it is not registered in the system. If the
# requested repository does not exist on the system, use
# 'add-apt-repository' command to add it.
#
# @param $1 ppa  Repository name.
init_packages_add_repository() {
  local ppa="${1}"

  if ! init_packages_repository_exists "${ppa}"; then
    echo sudo add-apt-repository -y "${ppa}"
  fi
}

# Execute all functions defined with names as
# '__init_packages_<name>__init'. Basically in initializing functions
# it is assumed that an additional repository is registered.
init_packages_initialize() {
  for package in "${__package_names[@]}"; do
    if declare -F __init_packages_${package}__init >/dev/null; then
      __init_packages_${package}__init
    fi
  done
}

# Execute update function.
init_packages_update() {
  echo sudo apt-get update -y
}

# Add dependencies for a package. Specified dependencies will be added
# to a global variable '__requested_packages'. Actually not only
# dependencies but also the package itself should be specified with
# this functions. Also, this function declares an array named
# '__packages_<name>', which contains the specified dependencies.
#
# Example usage:
#   $ init_packages_depends "emacs-mozc-bin emacs26"
#
# @global __requested_packages  List of all packages to be installed.
# @global __packages_<name>  Creates an array containing dependencies.
declare -a __requested_packages=()
init_packages_depends() {
  local dependency=()
  for arg in "$@"; do
    read -r -a _dependency <<< "${arg}"
    dependency+=("${_dependency[@]}")
  done
  __requested_packages+=("${dependency[@]}")

  # Creates an array named '__packages_<name>'. This variable has the
  # specified dependencies. Let's say the user defined the following.
  # __init_packages_emacs__install() {
  #   init_packages_depends "emacs26 emacs-mozc-bin"
  # }
  # After __init_packages_emacs__install executed, the following array
  # is automatically defined.
  # __packages_emacs=(emacs26 emacs-mozc-bin)
  package="$(_extract_package_name "${FUNCNAME[1]}")"
  eval "__packages_${package}=(${dependency[@]})"
}

# Get all packages installed on the system. Results are stored in a
# global variable '__installed_packages'. This is used to find missing
# packages on the system.
#
# @global __installed_packages
# @see init_packages_find_missing_packages()
declare -a __installed_packages=()
init_packages_get_installed_packages() {
  __installed_packages=($(apt list --installed 2>/dev/null >&1 \
                            | grep -v deinstall \
                            | awk -F/ '{print $1}'))

  if [[ "${__installed_packages[0]}" = "Listing..." ]]; then
    # Remove the first item since it is not a package name
    unset -v __installed_packages[0]
    __installed_packages=("${__installed_packages[@]}")
  fi
}

# Find packages to be newly installed on the system. Search through
# names within a global variable '__requested_packages', and missing
# packages are added to a global variable '__missing_packages'.
#
# @global __requested_packages
# @global __missing_packages
declare -a __missing_packages=()
init_packages_find_missing_packages() {
  # Get packages installed on the system.
  init_packages_get_installed_packages

  # Check if a requested package is found in installed packages. If
  # not found, it is added to missing packages.
  # FIXME: more efficient algorithm.
  for req in "${__requested_packages[@]}"; do
    _found=false
    for inst in "${__installed_packages[@]}"; do
      if [[ "${req}" = "${inst}" ]]; then
        _found=true
        break
      fi
    done
    if [[ $_found = false ]]; then
      __missing_packages+=("${req}")
    fi
  done
}

# Execute all functions defined with names as '__init_packages_<name>'
# or '__init_packages_<name>__install'. It is assumed to define
# specific packages to install with system package manager like
# 'apt-get' in installing functions.
init_packages_install() {
  # Register requested packages.
  for package in "${__package_names[@]}"; do
    if declare -F __init_packages_${package} >/dev/null; then
      __init_packages_${package}
    fi
    if declare -F __init_packages_${package}__install >/dev/null; then
      __init_packages_${package}__install
    fi
  done

  # Find missing packages.
  init_packages_find_missing_packages

  # Install missing packages.
  if [[ "${#__missing_packages[@]}" > 0 ]]; then
    echo sudo apt install -y "${__missing_packages[@]}"
  fi
}

# Make a configuration for a specific package always run at
# configuration step even if the package is not installed. This
# function must be executed at least once in
# __init_packages_<name>__init or __init_packages_<name>__install.
# Execution in __init_packages_<name>__config has no effect. In fact
# this function defines a variable named __always_config_<name> as
# true. If this variable set before configuration step, the config
# function for the package is guaranteed to be executed.
init_packages_always_config() {
  local package="$(_extract_package_name "${FUNCNAME[1]}")"
  eval "__always_config_${package}=true"
}

# Check if a configuration for a package is always executed or not.
#
# @param $1 package  Package name.
# @return True(0)  If configuration for the package is always run.
#         False(>0) Otherwise.
_is_always_config() {
  local package="${1}"
  local flag=$(eval "echo \${__always_config_${package}:-false}")

  if [[ $flag = true ]]; then
    return 0
  else
    return 1
  fi
}

# Get dependencies for a package as stdout.
#
# @param $1 package  Package name.
# @return  List of dependencies of the package.
_get_package_depends() {
  local package="${1}"
  local deps="\${__packages_${package}[@]}"
  eval "echo ${deps}"
}

# Check if a configuration for a packaged should be executed. If at
# least one dependency for the package is installed or the flag for
# always running configuration is set, return true.
#
# @global __missing_packages
# @param $1 package  Package name.
# @return True(0)  If the package should be configured.
#         False(>0) Otherwise.
#
# @see init_packages_always_config()
_should_be_configured() {
  local package="${1}"

  # Return 0 if the-always-running flag is set.
  if _is_always_config "${package}"; then
    return 0
  fi

  # Check if a dependency is newly installed.
  for dep in $(_get_package_depends "${package}"); do
    if contains "${dep}" "${__missing_packages[@]}"; then
      return 0
    fi
  done
  return 1
}

# Execute configuration functions if the dependency for each package
# is newly installed or the flag for always running configuraion is
# set.
init_packages_configure() {
  for package in "${__package_names[@]}"; do
    if _should_be_configured "${package}"; then
      if has "__init_packages_${package}__config"; then
        __init_packages_${package}__config
      fi
    fi
  done
}

init_packages() {
  e_header "Find packages"
  init_packages_find
  e_header "Initialize packages"
  init_packages_initialize
  e_header "Update repositories"
  init_packages_update
  e_header "Install packages"
  init_packages_install
  e_header "Configure packages"
  init_packages_configure
}
