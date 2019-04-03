#!/usr/bin/env bash
set -eu
THIS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
source "${THIS_DIR}/scripts/functions.sh"

# check bash version
set +e
BASH_VERSION_MINIMUM_REQUIRED=4.2
vercomp ${BASH_VERSION} ${BASH_VERSION_MINIMUM_REQUIRED}; result=$?
set -e
if [[ $result == 2 ]]; then
  abort "Running bash version is ${BASH_VERSION}, but minimum supported is ${BASH_VERSION_MINIMUM_REQUIRED}. Sorry."
fi

# default values
HOME_DIR=${HOME}
OVERWRITE=false

usage() {
  name=${BASH_SOURCE[0]}
  cat <<EOF
Usage:
  $name [options] [command]

Commands:
  update
  deploy
  init
  clean

Options:
  -d, --home-directory  Specify directory to deploy dotfiles (default: ${HOME_DIR})
  -f, --force           $(warn "** warning **") Overwrite exsiting dotfiles.
  -h, --help            Show this message
EOF
}

POSITIONAL=()
while [[ $# > 0 ]]; do
  key="$1"
  case $key in
    -d|--home-directory)
      HOME_DIR="$2"
      shift 2
      ;;
    -f|--force)
      OVERWRITE=true
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      POSITIONAL+=("$1")
      shift
      ;;
  esac
done
set -- "${POSITIONAL[@]}"

source "${THIS_DIR}/scripts/deploy.sh"
# source "${THIS_DIR}/scripts/init.sh"
source "${THIS_DIR}/scripts/clean.sh"

update() {
  set +e
  if has "git"; then
    cd "${THIS_DIR}"
    git pull origin master
    git submodule init
    git submodule update
    git submodule foreach git pull origin master
    cd - &>/dev/null
  else
    abort "command 'git' is unavailable"
  fi
  set -e
}

main() {
  local command=$1

  case $command in
    update)
      update
      ;;
    deploy)
      info "Deploying dotfiles..."
      deploy "${THIS_DIR}" "${HOME_DIR}"
      ok
      ;;
    init*)
      info "Initializing..."
      bash "${THIS_DIR}/scripts/init.sh" "${THIS_DIR}"
      ok
      ;;
    clean)
      info "Cleaning dotfiles..."
      clean "${THIS_DIR}" "${HOME_DIR}"
      ok
      ;;
    *)
      usage
      ;;
  esac
}

if [[ $# > 0 ]]; then main $1; else usage; fi
exit 0
