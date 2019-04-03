#!/usr/bin/env bash
set -eu

remove_if_symlink_to_dotfile() {
  local home_dir="$1"
  local dotfiles_dir="$2"
  local filename="$3"

  if [[ -L "${home_dir}/${filename}" ]]; then
    local parent1="$(parentdir $(readlink "${home_dir}/${filename}"))"
    local parent2="$(parentdir ${dotfiles_dir}/${filename})"
    if [[ "${parent1}" = "${parent2}" ]]; then
      rm -f "${home_dir}/${filename}"
    fi
  fi
}

remove_broken_symlinks() {
  local dir="$1"

  find "${dir}" -maxdepth 1 -type l ! -exec test -e {} \; -exec rm -f {} \;
}
export -f remove_broken_symlinks

clean() {
  local dotfiles_dir="$1"
  local home_dir="${2:-${HOME}}"

  cd "${dotfiles_dir}"
  unset list
  # find dotfiles in ${dotfiles_dir}
  while IFS= read -r -d '' file; do
    list+=("$(echo "${file}" | sed "s|^\./||" )")
  done < <(find . -maxdepth 1 -name ".??*" -print0)
  # find dotfiles in ${dotfiles_dir}/.config
  while IFS= read -r -d '' file; do
    list+=("$file")
  done < <(find .config -type f -print0)

  for file in "${list[@]}"; do
    remove_if_symlink_to_dotfile "${home_dir}" "${dotfiles_dir}" "${file}"
  done

  remove_broken_symlinks "${home_dir}"
  find .config -type d -print0 | xargs -r0 -n 1 -I{} bash -c 'remove_broken_symlinks "$@"' _ "${home_dir}/{}"
  cd - &>/dev/null
}
