#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eEu

# Get the path which this script exists.
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"

# Include functions to testify.
source "${THIS_DIR}/functions.sh"
source "${THIS_DIR}/init_packages.sh"

test_extract_package_name() {
  [ -z "$(_extract_package_name init_packages_incorrect)"  ]
  [ "$(_extract_package_name __init_packages_awesome)" = "awesome" ]
  [ "$(_extract_package_name __init_packages_Awesome__init)" = "Awesome" ]
  [ "$(_extract_package_name __init_packages_awesome2__install)" = "awesome2" ]
  [ "$(_extract_package_name __init_packages_awesome__config)" = "awesome" ]
}
test_extract_package_name

test_get_func_type() {
  [ "$(_get_func_type init_packages_incorrect)" = "none" ]
  [ "$(_get_func_type __init_packages_awesome__init)" = "init" ]
  [ "$(_get_func_type __init_packages_awesome__install)" = "install" ]
  [ "$(_get_func_type __init_packages_awesome__config)" = "config" ]
  [ "$(_get_func_type __init_packages_awesome)" = "install" ]
}
test_get_func_type

test_is_called_from() {
  _is_called_from "init" "init_packages_find" "__init_packages_awesome__init"
  ! _is_called_from "install" "init_packages_something" "__init_packages_awesome__config"
  _is_called_from "install" "init_packages_something" "__init_packages_awesome"
}
test_is_called_from

test_normalize_repository_name() {
  [ "$(_normalize_repository_name "ppa:super-repo/stable")" = "super-repo/stable" ]
}
test_normalize_repository_name

test_init_packages_repository_exists() {
  ! init_packages_repository_exists "ppa:awesome-repo/stable"
  init_packages_repository_exists "ppa:kelleyk/emacs"
}
test_init_packages_repository_exists

echo "All tests were passed."
