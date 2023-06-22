# shellcheck shell=sh
# This is my personal configuration for login shells.

## Utility functions

# Set a value to an environment variable and export it.
# Usage:
#    setenv        # print all exported variables
#    setenv NAME   # unset $NAME
#    setenv DISPLAY 127.0.0.1:0.0  # set $DISPLAY to 127.0.0.1:0.0
setenv () {
  case $# in
    0) export -p ;;
    1) unset "$1" ;;
    2) export "$1"="$2" ;;
    *) printf "error: ignored the last %d arguments (setenv)\n" $(($#-2)) >&2 ;;
  esac
}

# Set PATH so that it includes a path to PATH variable if it exists.
addpath() {
  if [ $# -ne 1 ]; then
    echo "addpath: More than one or zero arguments were given"
    return 1
  fi

  if [ -d "$1" ]; then
    setenv PATH "$1:$PATH"
  fi
}

# Return True if a command is hashed on the system.
has() {
  command -v "$1" >/dev/null 2>&1
}

## PATH setup

if [ -n "$HOME" ]; then

  # Set PATH so it includes user's private bin.
  addpath "$HOME/.local/bin"
  addpath "$HOME/usr/bin"
  setenv LD_LIBRARY_PATH "$HOME/usr/lib:$LD_LIBRARY_PATH"

  # Set PATH for pkg-config.
  # https://www.freedesktop.org/wiki/Software/pkg-config/
  setenv PKG_CONFIG_PATH "$HOME/usr/lib/pkgconfig:$PKG_CONFIG_PATH"

  # Configure paths for Go Programming Language.
  # https://golang.org/
  # https://github.com/golang/go/wiki/SettingGOPATH
  # $GOROOT is assumed to be /usr/local/go
  if [ -d /usr/local/go ]; then
    setenv GOPATH "$HOME/usr/go"
    addpath "/usr/local/go/bin"
    addpath "$GOPATH/bin"
  fi

  # Configure paths for packages installed by yarn.
  # https://classic.yarnpkg.com/en/docs/cli/global
  if [ -d "$HOME/.yarn" ] && has yarn; then
    addpath "$(yarn global bin)"
  fi

  # pyenv is a simple python version management.
  # https://github.com/pyenv/pyenv
  if [ -d "$HOME/.pyenv" ]; then
    setenv PYENV_ROOT "$HOME/.pyenv"
    has pyenv || addpath "$PYENV_ROOT/bin"
    eval "$(pyenv init -)"
  fi

  # LaTeX is a markup language to write a document.
  if [ -d "/usr/local/texlive" ]; then
    year=$(find "/usr/local/texlive" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | \
             grep "[0-9]\+" | sort -r | head -1)
    if [ -n "$year" ]; then
      os=$(uname -s | tr '[:upper:]' '[:lower:]')
      if [ "$os" = linux ] || [ "$os" = darwin ]; then
        arch=$(uname -m)
        addpath "/usr/local/texlive/${year}/bin/${arch}-${os}"
      fi
    fi
  fi

  # Lots of applications are installed in $ZPFX by zinit.
  if [ -d "$HOME/.config/zinit" ]; then
    ZPFX=$HOME/.config/zinit/polaris
    addpath "$ZPFX/bin"
  fi
fi

## External configuration
### ~/.profile.local

if [ -n "$HOME" ] && [ -f "$HOME/.profile.local" ]; then
  # shellcheck source=/dev/null
  . "$HOME/.profile.local"
fi

## DISPLAY setup

# Set DISPLAY When running on WSL.
# https://github.com/canonical/ubuntu-wsl-integration/blob/master/wsl-integration.sh
if grep -qEi "(microsoft|wsl)" /proc/version >/dev/null 2>&1; then
  WSL_HOST="$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null)"
  setenv DISPLAY "${WSL_HOST}:0"
  setenv PULSE_SERVER "tcp:${WSL_HOST}"
  setenv LIBGL_ALWAYS_INDIRECT 1
  unset WSL_HOST
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
  # include .bashrc if it exists
  if [ -f "$HOME/.bashrc" ] && [ -z "$ARCHE_SKIP_PROFILE" ]; then
    # shellcheck source=/dev/null
    . "$HOME/.bashrc"
  fi
fi
