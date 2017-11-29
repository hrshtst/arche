#!/bin/bash -eu
#
# Create symbolic links to proper locations

SCRIPT_DIR="$(cd "$(dirname "$0")" ; pwd -P)"

INSTALL_TYPE=${1:-full}

warn() {
  echo "warning: $@" >&2
}

error() {
  echo "error: $@" >&2
  exit 1
}


##################################################
# Returns 0 if specified file or directory exists.
# Returns 1 otherwise.
# Globals:
#   None
# Arguments:
#   1: file or directory to check existence
# Returns:
#   0: if the argument exists as a file or a directory
#   1: otherwise
##################################################
exists() {
  local arg=$1
  if [[ -d $arg || -f $arg ]]; then
    return 0
  else
    return 1
  fi
}


##################################################
# Returns 0 if specified two files are equivalent
# Returns 1 otherwise.
# Globals:
#   None
# Arguments:
#   1: file
#   2: file
# Returns:
#   0: if the arguments are equivalent
#   1: otherwise
##################################################
equivalent() {
  # local realpath1=$(realpath $1)
  # local realpath2=$(realpath $2)
  local realpath1=$(readlink -f $1)
  local realpath2=$(readlink -f $2)

  if [[ $realpath1 = $realpath2 ]]; then
    return 0
  else
    return 1
  fi
}


##################################################
# Makes directories if they does not exist
# Otherwise, it shows warning
# Globals:
#   None
# Arguments:
#   1: target directory path
# Returns:
#   None
##################################################
timid_mkdir() {
  local dir=$1
  if ! exists $dir; then
    mkdir -p $dir
  else
    warn "'$dir' already exists (skipped making directory)"
  fi
}


##################################################
# Creates a symbolic link to a target after validating
# if the target exists and where to create the link
# is free
# Globals:
#   None
# Arguments:
#   1. target path
#   2. link path
# Returns:
#   None
##################################################
safe_ln() {
  local target=$1
  local link=$2
  if ! exists $target; then
    error "'$target' doesn't exist!"
  elif equivalent $target $link; then
    # $target and $link are equivalent
    :
  elif exists $link; then
    warn "'$link' already exists (skipped creating link)"
  else
    ln -sf $target $link
  fi
}


##################################################
# Create symbolic link to a target with omiting
# directories (instead specify by global variables)
# Globals:
#   DST_DIR: directory where link will be created
#   SRC_DIR: directory where target exists
# Arguments:
#   1: target filename to create a link
#   2: link name (optional)
# Returns:
#   None
##################################################
DST_DIR="."
SRC_DIR="."
make_link() {
  local target=$1
  local link=${2:-$target}
  safe_ln $SRC_DIR/$target $DST_DIR/$link
}

# Check if the current directory is where this script exists
if [[ "$PWD" != "$SCRIPT_DIR" ]]; then
  error "please execute this script on a directory where it exists"
fi


# Make symbolic links for Emacs
DST_DIR="$HOME/.emacs.d"
SRC_DIR="$PWD/emacs"
timid_mkdir $DST_DIR
make_link el-get-recipes
make_link init
make_link init.el
if [[ $INSTALL_TYPE = simple ]]; then
  make_link init-el-get-simple.el init-el-get.el
  make_link init-loader-simple init-loader
else
  make_link init-el-get.el
  make_link init-loader
fi
make_link snippets
make_link themes


# Make symbolic links for Xmodmap
DST_DIR="$HOME"
SRC_DIR="$PWD/keyboard"
make_link Xmodmap_hhk .Xmodmap


# Make symbolic links for bash
DST_DIR="$HOME"
SRC_DIR="$PWD/shell"
make_link bashrc .bashrc
make_link bash_aliases .bash_aliases
make_link bash_completion_make.sh .bash_completion_make.sh


# Make symbolic links for tmux
DST_DIR="$HOME"
SRC_DIR="$PWD/shell"
timid_mkdir $HOME/.tmux/plugins
make_link tmux.conf .tmux.conf

timid_mkdir $HOME/usr/bin
DST_DIR="$HOME/usr/bin"
SRC_DIR="$PWD/bin"
make_link git-echo-branch-tmux-current-pane
make_link git-echo-username-and-email

if [[ ! -d $HOME/.tmux/plugins/tpm ]]; then
  echo ""
  echo "You should run the following command to install tmux plugin manager:"
  echo "    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm"
  echo "After installing it, you should type the following on tmux:"
  echo "    <prefix> Shift+I"
  echo ""
fi


# Make symbolic links for Git
DST_DIR="$HOME"
SRC_DIR="$PWD/git"
make_link gitconfig .gitconfig
make_link git-completion.bash .git-completion.bash
make_link git-prompt.sh .git-prompt.sh
make_link gitignore .gitignore


# Make symbolic links for Vim
DST_DIR="$HOME/.vim"
SRC_DIR="$PWD/vim"
timid_mkdir $DST_DIR
make_link _vimrc
make_link _gvimrc
make_link vimfiles
