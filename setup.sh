#!/bin/bash -eu
#
# Create symbolic links for useful applications


SCRIPT_DIR="$(cd "$(dirname "$0")" ; pwd -P)"
PACKAGE_LIST="emacs emacs-simple emacs-windows bash tmux git vim keyboard gtags pandoc"
GROUP_LIST="full simple windows"
GIVEN_GROUP=
GIVEN_PACKAGE_LIST=
FLAG_DRY_RUN=FALSE
FLAG_CLEAN=FALSE


##################################################
# Returns package names that are contained in a group
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
##################################################
group_full() {
  echo "emacs bash tmux git vim keyboard gtags pandoc"
}
group_simple() {
  echo "emacs-simple bash tmux git vim keyboard gtags pandoc"
}
group_windows() {
  echo "emacs-windows bash tmux git vim keyboard gtags pandoc"
}


##################################################
# Utility functions
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
##################################################
warn() {
  echo "warning: $@" >&2
}

error() {
  echo "error: $@" >&2
  exit 1
}


##################################################
# Find if a string is contained in a list.
# Globals:
#   None
# Arguments:
#   1: string to find
#   2: list of strings which are separated by space
# Returns:
#   0: string is found within the list
#   1: otherwise
##################################################
contains() {
  str="$1"
  shift
  for i in $@; do
    if [[ $str = $i ]]; then
      return 0
    fi
  done
  return 1
}


##################################################
# Show help message
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
##################################################
usage() {
  cat <<END;
Usage:
  ${0} [OPTION]... PACKAGE...
Configure settings for specified PACKAGEs.

Options:
      --dry-run             Do not operate on files (Default: $FLAG_DRY_RUN)
  -h, --help                Show this message.

Available packages:
  * emacs           full installation of settings for Emacs
  * emacs-simple    installation of reduced settings for Emacs
  * emacs-windows   installation of settings for Emacs on Windows
  * bash            create symlink to .bashrc
  * tmux            create symlink to .tmux.conf
  * git             create symlink to .gitconfig
  * vim             installation of settings for Vim
  * keyboard        create symlink to .Xmodmap
  * gtags           create symlink to .gtagsrc

Available package groups:
  * full            full installation
  * simple          minimum installation
  * windows         cusomized installation for Windows

Examples:
  ${0}
  ${0} full
      install full packages
  ${0} simple
      install minimum packages
  ${0} windows
      install packages available on Windows
  ${0} emacs-simple bash git
      install selected packages
  ${0} clean
      clean installed settings
END
}


##################################################
# Check arguments
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
##################################################
check_args() {
  while [[ $# -gt 0 ]]; do
    case $1 in
      --dry-run)
        FLAG_DRY_RUN=TRUE
        shift
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      --)
        shift
        break
        ;;
      --*|-*)
        echo "unrecognized option: $1" >&2
        usage
        exit 1
        ;;
      *)
        if contains "$1" "$GROUP_LIST"; then
          GIVEN_GROUP="$1"
        elif contains "$1" "$PACKAGE_LIST"; then
          GIVEN_PACKAGE_LIST="${GIVEN_PACKAGE_LIST}$1 "
        elif [[ "$1" == "clean" ]]; then
          FLAG_CLEAN=TRUE
        else
          echo "unrecognized package: $1" >&2
          usage
          exit 1
        fi
        shift
        ;;
    esac
  done

  if [[ -z $GIVEN_PACKAGE_LIST ]] && [[ -z $GIVEN_GROUP ]]; then
    GIVEN_GROUP=full
  fi
  if [[ -n $GIVEN_GROUP ]]; then
    GIVEN_PACKAGE_LIST=$(group_${GIVEN_GROUP})
  fi
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
    if [[ $FLAG_DRY_RUN == FALSE ]]; then
      mkdir -p $dir
    else
      echo mkdir -p $dir
    fi
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
    if [[ $FLAG_DRY_RUN == FALSE ]]; then
      ln -sf $target $link
    else
      echo ln -sf $target $link
    fi
  fi
}


##################################################
# Copy files after validating whether the source
# exists and the destination is free
# Globals:
#   None
# Arguments:
#   1. source path
#   2. destination path
# Returns:
#   None
##################################################
safe_copy() {
  local src=$1
  local dst=$2
  if ! exists $src; then
    error "'$src' doesn't exist!"
  elif exists $dst; then
    warn "'$dst' already exists (skipped copying file)"
  else
    if [[ $FLAG_DRY_RUN == FALSE ]]; then
      cp -rf $src $dst
    else
      echo cp -rf $src $dst
    fi
  fi
}


##################################################
# Replace fake symlink file with actual target file.
# Available only on Windows.
# Globals:
#   None
# Arguments:
#   1. symlink path
# Returns:
#   None
##################################################
expand_symlink() {
  local link=$1
  local dst_dir="$(cd "$(dirname "$link")" ; pwd)"
  local src="$(readlink -e "$dst_dir"/$(cat $link))"
  if [[ $FLAG_DRY_RUN == FALSE ]]; then
    cp $src $link
  else
    echo cp $src $link
  fi
}


##################################################
# Replace all the fake symlink files with actual files
# in a directory.
# Available only on Windows.
# Globals:
#   None
# Arguments:
#   1. directory
# Returns:
#   None
##################################################
expand_all_symlinks_in_dir() {
  for i in $1/*; do
    expand_symlink $i
  done
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
  if [[ $(uname) == "Linux" ]]; then
    safe_ln $SRC_DIR/$target $DST_DIR/$link
  else
    safe_copy $SRC_DIR/$target $DST_DIR/$link
  fi
}


##################################################
# Check if the current working directory is the specified one
# Globals:
#   None
# Arguments:
#   1: directory path
# Returns:
#   Exit with 1 if the CWD is not the specified path.
##################################################
check_cwd() {
  # Check if the current directory is where this script exists
  if [[ "$PWD" != "$1" ]]; then
    error "please execute this script on a directory where it exists"
  fi
}


##################################################
# Functions to install each package
##################################################
install_emacs() {
  DST_DIR="$HOME/.emacs.d"
  SRC_DIR="$PWD/emacs"
  timid_mkdir $DST_DIR
  make_link el-get-recipes
  make_link init
  make_link init.el
  make_link init-el-get.el
  make_link init-loader
  make_link snippets
  make_link themes
}

install_emacs-simple() {
  DST_DIR="$HOME/.emacs.d"
  SRC_DIR="$PWD/emacs"
  timid_mkdir $DST_DIR
  make_link el-get-recipes
  make_link init
  make_link init.el
  make_link init-el-get-simple.el init-el-get.el
  make_link init-loader-simple init-loader
  make_link snippets
  make_link themes
}

install_emacs-windows() {
  DST_DIR="$HOME/.emacs.d"
  SRC_DIR="$PWD/emacs"
  timid_mkdir $DST_DIR
  make_link el-get-recipes
  make_link init
  make_link init.el
  make_link init-el-get-simple.el init-el-get.el
  make_link init-loader-simple init-loader
  expand_all_symlinks_in_dir $DST_DIR/init-loader
  make_link snippets
  make_link themes
}

install_bash() {
  DST_DIR="$HOME"
  SRC_DIR="$PWD/shell"
  make_link bashrc .bashrc
  make_link bash_aliases .bash_aliases
  make_link bash_completion_make.sh .bash_completion_make.sh
}

install_tmux() {
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
    cat <<END;
You should run the following command to install tmux plugin manager:
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
After installing it, you should type the following on tmux:
    <prefix> Shift+I

END
  fi
}

install_git() {
  DST_DIR="$HOME"
  SRC_DIR="$PWD/git"
  make_link gitconfig .gitconfig
  make_link git-completion.bash .git-completion.bash
  make_link git-prompt.sh .git-prompt.sh
  make_link gitignore .gitignore
}

install_vim() {
  DST_DIR="$HOME/.vim"
  SRC_DIR="$PWD/vim"
  timid_mkdir $DST_DIR
  make_link _vimrc
  make_link _gvimrc
  make_link vimfiles
}

install_keyboard() {
  DST_DIR="$HOME"
  SRC_DIR="$PWD/keyboard"
  make_link Xmodmap_hhk .Xmodmap
}

install_gtags() {
  DST_DIR="$HOME"
  SRC_DIR="$PWD/tag"
  make_link gtags.conf .globalrc
}

install_pandoc() {
  DST_DIR="$HOME"
  SRC_DIR="$PWD"
  make_link pandoc .pandoc
}


##################################################
# Functions to install each package
##################################################
clean_emacs() {
  rm -rf "$HOME/.emacs.d"
}

clean_emacs-simple() {
  clean_emacs
}

clean_emacs-windows() {
  clean_emacs
}

clean_bash() {
  rm -f "$HOME/.bashrc"
  rm -f "$HOME/.bash_aliases"
  rm -f "$HOME/.bash_completion_make.sh"
}

clean_tmux() {
  rm -f "$HOME/.tmux.conf"
  rm -rf "$HOME/.tmux"
  rm -f "$HOME/usr/bin/git-echo-branch-tmux-current-pane"
  rm -f "$HOME/usr/bin/git-echo-username-and-email"
}

clean_git() {
  rm -f "$HOME/.gitconfig"
  rm -f "$HOME/.git-completion.bash"
  rm -f "$HOME/.git-prompt.sh"
  rm -f "$HOME/.gitignore"
}

clean_vim() {
  rm -rf "$HOME/.vim"
}

clean_keyboard() {
  rm -f "$HOME/.Xmodmap"
}

clean_gtags() {
  rm -f "$HOME/.globalrc"
}

clean_pandoc() {
  rm -rf "$HOME/.pandoc"
}


##################################################
# Main functions
# Globals:
#   None
# Arguments:
#   None
# Returns:
#   None
##################################################
main() {
  check_cwd "$SCRIPT_DIR"
  check_args "$@"
  for package in $GIVEN_PACKAGE_LIST; do
    if [[ $FLAG_CLEAN == TRUE ]]; then
      clean_${package}
    else
      install_${package}
    fi
  done
}

main "$@"
