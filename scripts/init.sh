#!/usr/bin/env bash

trap 'echo Error: $0:$LINENO; exit 1' ERR INT QUIT TERM
set -eu

THIS_DIR="${1}"
source "${THIS_DIR}/scripts/functions.sh"

# Ask for sudo password once, then keep it alive until
# the script has finished
# https://gist.github.com/cowboy/3118588
sudo -v
while true; do
    sudo -n true
    sleep 60;
    kill -0 "$$" || exit
done 2>/dev/null &

detect_os
init_dir="${THIS_DIR}/scripts/init/${OS_NAME}-${OS_CODENAME:-${OS_VERSION}}"

if [[ ! -d "${init_dir}" ]]; then
  msg=$(cat <<EOF
Not found: ${init_dir}
Initialization scripts are unavailable.
Detected OS is:
    name: ${OS_NAME}
 version: ${OS_VERSION}
codename: ${OS_CODENAME}
EOF
)
  error "$msg"
  abort "Abort."
fi

for file in "${init_dir}"/[0-9][0-9]-*.sh; do
  info "Running" "$(basename "${file}")..."
  bash "${file}" "${THIS_DIR}"
done
