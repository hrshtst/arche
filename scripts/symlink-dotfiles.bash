#!/usr/bin/env bash

set -e
set -o pipefail

script="$(realpath "$0")"
scripts="$(dirname "$script")"
dotfiles="${1:-$(dirname "$scripts")}"

safe_link() {
  if [[ -e "$2" && ! -L "$2" ]]; then
    echo "already exists and not a symlink: $2" >&2
    exit 1
  fi

  ln -sf "$1" "$2"
}

force_link() {
  if [[ -e "$2" && ! -L "$2" ]];then
    echo "warning: forcibly replaced with symlink: $2" >&2
    mv "$2"{,.bk}
  fi

  ln -sf "$1" "$2"
}

# Shell
force_link "$dotfiles/shell/shared/.profile" "$HOME/.profile"

# Bash
force_link "$dotfiles/shell/bash/.bashrc" "$HOME/.bashrc"

# fish - friendly interactive shell
mkdir -p "$HOME/.config/fish"
safe_link "$dotfiles/shell/fish/config.fish" \
          "$HOME/.config/fish/config.fish"
safe_link "$dotfiles/shell/fish/fishfile" "$HOME/.config/fish/fishfile"
mkdir -p "$HOME/.config/fish/functions"
for i in "$dotfiles/shell/fish/functions/"*.fish; do
  filename="$(basename "$i")"
  safe_link "$dotfiles/shell/fish/functions/$filename" \
            "$HOME/.config/fish/functions/$filename"
done

# tmux
mkdir -p "$HOME/.config/tmux"
safe_link "$dotfiles/tmux/tmux.conf" "$HOME/.config/tmux/tmux.conf"

# Emacs
mkdir -p "$HOME/.emacs.d/straight/versions"
safe_link "$dotfiles/emacs/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "$dotfiles/emacs/init.el" "$HOME/.emacs.d/init.el"
safe_link "$dotfiles/emacs/versions.el" \
          "$HOME/.emacs.d/straight/versions/arche.el"

# Git
safe_link "$dotfiles/git/.gitconfig" "$HOME/.gitconfig"
safe_link "$dotfiles/git/.gitexclude" "$HOME/.gitexclude"

# pandoc
mkdir -p "$HOME/.local/share/pandoc"
safe_link "$dotfiles/pandoc/github-markdown.css" \
          "$HOME/.local/share/pandoc/github-markdown.css"
