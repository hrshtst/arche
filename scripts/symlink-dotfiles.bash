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

safe_link_directory() {
  if [[ ! -d "$1" ]]; then
    echo "source is not a directory, use safe_link: $1" >&2
    exit 1
  fi

  if [[ -f "$2" ]]; then
    echo "already exists and not a directory: $2" >&2
    exit 1
  elif [[ -L "$2" ]]; then
    return
  elif [[ -d "$2" ]]; then
    echo "already exists, and not empty: $2" >&2
    exit 1
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
safe_link "$dotfiles/shell/fish/fish_plugins" "$HOME/.config/fish/fish_plugins"
mkdir -p "$HOME/.config/fish/functions"
for i in "$dotfiles/shell/fish/functions/"*.fish; do
  filename="$(basename "$i")"
  safe_link "$dotfiles/shell/fish/functions/$filename" \
            "$HOME/.config/fish/functions/$filename"
done

# Zsh
safe_link "$dotfiles/shell/zsh/.zshrc" "$HOME/.zshrc"
safe_link "$dotfiles/shell/zsh/.zshenv" "$HOME/.zshenv"
safe_link "$dotfiles/shell/zsh/.zprofile" "$HOME/.zprofile"
safe_link "$dotfiles/shell/zsh/.p10k.zsh" "$HOME/.p10k.zsh"
mkdir -p "$HOME/.config/sheldon"
safe_link "$dotfiles/shell/zsh/sheldon/plugins.toml" "$HOME/.config/sheldon"
mkdir -p "$HOME/.zsh"
safe_link_directory "$dotfiles/shell/zsh/plugins" "$HOME/.zsh"
safe_link_directory "$dotfiles/shell/zsh/functions" "$HOME/.zsh"

# tmux
mkdir -p "$HOME/.config/tmux"
safe_link "$dotfiles/tmux/tmux.conf" "$HOME/.config/tmux/tmux.conf"

# Emacs
mkdir -p "$HOME/.emacs.d/straight/versions"
safe_link "$dotfiles/emacs/early-init.el" "$HOME/.emacs.d/early-init.el"
safe_link "$dotfiles/emacs/init.el" "$HOME/.emacs.d/init.el"
safe_link "$dotfiles/emacs/versions.el" \
          "$HOME/.emacs.d/straight/versions/arche.el"
safe_link_directory "$dotfiles/emacs/snippets" "$HOME/.emacs.d/snippets"

# Vim
safe_link "$dotfiles/vim/.vimrc" "$HOME/.vimrc"

# Git
safe_link "$dotfiles/git/.gitconfig" "$HOME/.gitconfig"
safe_link "$dotfiles/git/.gitexclude" "$HOME/.gitexclude"

# pandoc
mkdir -p "$HOME/.local/share/pandoc"
safe_link "$dotfiles/pandoc/github-markdown.css" \
          "$HOME/.local/share/pandoc/github-markdown.css"

# python
mkdir -p "$HOME/.ipython/profile_default"
safe_link "$dotfiles/python/ipython_config.py" \
          "$HOME/.ipython/profile_default/ipython_config.py"
