#!/usr/bin/env zsh

#### cp, mv, ln

# You can "copy" any number of files, then "paste", "move" or
# "pasteln" them to pass them as arguments to cp, mv, or ln
# respectively. Just like a graphical filesystem manager. Each of the
# latter three functions defaults to the current directory as the
# destination.

# Usage: copy <path>...
#
# Copy all of the paths provided to the clipboard, stored in the array
# $ARCHE_CLIPBOARD.
function copy() {
  emulate -LR zsh
  ARCHE_CLIPBOARD=()
  for target; do
    ARCHE_CLIPBOARD+=("${target:a}")
  done
}

# Usage: paste [<path>]
#
# Invoke 'cp -R' on all paths in $ARCHE_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function paste() {
  emulate -LR zsh
  cp -R $ARCHE_CLIPBOARD ${1:-.}
}

# Usage: move [<path>]
#
# Invoke 'mv' on all paths in $ARCHE_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function move() {
  emulate -LR zsh
  mv $ARCHE_CLIPBOARD ${1:-.}
}

# Usage: pasteln [<path>]
#
# Invoke 'ln -s' on all paths in $ARCHE_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function pasteln() {
  emulate -LR zsh
  ln -s $ARCHE_CLIPBOARD ${1:-.}
}

# Usage: delink <path>
#
# Resolve the symlink at the given path and replace it with a copy of
# the file it points to.
function delink() {
  emulate -LR zsh
  if [[ -z $1 ]]; then
    echo >&2 "usage: delink <symlinks>"
    return 1
  fi
  for link; do
    if [[ -L $link ]]; then
      if [[ -e $link ]]; then
        target=${link:A}
        if rm "$link"; then
          if cp -R "$target" "$link"; then
            echo >&2 "Copied $target to $link"
          else
            ln -s "$target" "$link"
          fi
        fi
      else
        echo >&2 "Broken symlink: $link"
      fi
    else
      echo >&2 "Not a symlink: $link"
    fi
  done
}

# Usage: transpose <path1> <path2>
#
# Swap the files or directories at the two provided paths. Not atomic.
# Both paths must exist.
function transpose {
  emulate -LR zsh
  if (( $# != 2 )); then
    echo >&2 "usage: transpose <path1> <path2>"
    return 1
  fi
  for arg in $1 $2; do
    if [[ ! -e $arg && ! -L $arg ]]; then
      echo >&2 "no such file or directory: $arg"
      return 1
    fi
    if [[ -e $path.tmp || -L $path.tmp ]]; then
      echo >&2 "already exists: $path.tmp"
      return 1
    fi
  done
  mv $1 $1.tmp
  mv $2 $2.tmp
  mv $1.tmp $2
  mv $2.tmp $1
}
