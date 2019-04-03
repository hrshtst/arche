#!/usr/bin/env bash
set -eu

OVERWRITE=${OVERWRITE:-false}
make_link() {
  local target="$(abspath "$1")"
  local linkname="$2"

  if [[ "${OVERWRITE}" = true && -e "${linkname}" ]]; then
    rm -f "${linkname}"
  fi
  if [[ ! -e "${linkname}" ]]; then
    ln -snfv "${target}" "${linkname}"
  else
    warn "'${linkname}' already exists"
  fi
}

EXCLUSIONS=".git .gitignore .config"
deploy_dotfiles() {
  local dotfiles_dir="$1"
  local home_dir="$2"

  cd "${dotfiles_dir}"
  set +e
  for name in .??*; do
    contains "${EXCLUSIONS}" "${name}" && continue
    make_link "${name}" "${home_dir}/${name}"
  done
  set -e
  back_to_oldwd
}

_deploy_files() {
  local dotfiles_dir="$1"
  local home_dir="$2"
  local target_dir="$3"

  cd "${dotfiles_dir}"
  mkdir -p "${home_dir}/${target_dir}"
  find "${target_dir}" -mindepth 1 -type d -print0 | xargs -r0 -n 1 -I{} mkdir -p "${home_dir}/{}"
  unset list
  while IFS= read -r -d '' file; do
    list+=("$file")
  done < <(find  "${target_dir}" -type f -print0)
  for file in "${list[@]}"; do
    make_link "${file}" "${home_dir}/${file}"
  done
  back_to_oldwd
}

deploy_config() {
  _deploy_files "$1" "$2" ".config"
}

deploy_bin() {
  _deploy_files "$1" "$2" "usr/bin"
}

deploy() {
  local dotfiles_dir="$1"
  local home_dir="${2:-${HOME}}"

  deploy_dotfiles "${dotfiles_dir}" "${home_dir}"
  deploy_config "${dotfiles_dir}" "${home_dir}"
  deploy_bin "${dotfiles_dir}" "${home_dir}"
}
