# This is my personal configuration for login shells.

## Utility functions

# Set a value to an environment variable and export it.
# Usage:
#   setenv DISPLAY 127.0.0.1:0.0
setenv () {
  case $# in
    0) export -p ;;
    1) unset "$1" ;;
    2) export "$1"="$2" ;;
    *) printf "error: ignored the last %d arguments (setenv)\n" $(($#-2)) >&2 ;;
  esac
}

# Return True (0) if the path exists in the system. Otherwise, return
# False (1).
_is_valid () {
  if [ ! -d "$2" ]; then
    echo "warning: no such directory: $2" >&2
    return 1
  fi
  return 0
}

# Add a value to the head of a variable. If the variable name ends
# with PATH the value is added with a delimiter ':'.
# Usage:
#   addenv PATH $HOME/usr/bin
addenv () {
  force=false
  case $1 in
    --force|-f)
      force=true
      shift
      ;;
    *) ;;
  esac

  if [ $# -lt 2 ]; then
    printf "error: too few arguments (addenv)\n" >&2
    return 1
  fi

  var=$1; shift
  value="$(eval 'echo $'"$var")"

  case $var in
    *PATH)
      i=$#
      while [ "$i" -gt 0 ]; do
        eval "v=\${$i}"
        if [ $force = false ] && _is_valid "$var" "$v"; then
          value="${v}:$value"
        fi
        i=$((i-1))
      done
      ;;
    *)
      for v in "$@"; do
        value="${v}$value"
      done
      ;;
  esac
  setenv "$var" "$value"
}

# Return True if a command is hashed on the system.
has() {
  command -v "$1" >/dev/null 2>&1
}

## PATH setup

if [ -n "$HOME" ]; then

  # Set PATH so it includes user's private bin if it exists.
  addenv PATH "$HOME/.local/bin" "$HOME/usr/bin"
  addenv LD_LIBRARY_PATH "$HOME/usr/lib"

  # Set PATH for pkg-config.
  # https://www.freedesktop.org/wiki/Software/pkg-config/
  addenv --force PKG_CONFIG_PATH "$HOME/usr/lib/pkgconfig"

  # Configure paths for Go Programming Language.
  # https://golang.org/
  # https://github.com/golang/go/wiki/SettingGOPATH
  # $GOROOT is assumed to be /usr/local/go
  setenv GOPATH "$HOME/usr/go"
  addenv PATH "$GOPATH/bin" "/usr/local/go/bin"

  # Configure paths for packages installed by yarn.
  # https://classic.yarnpkg.com/en/docs/cli/global
  if has yarn; then
    addenv PATH "$(yarn global bin)"
  fi

  # pyenv is a simple python version management.
  # https://github.com/pyenv/pyenv
  setenv PYENV_ROOT "$HOME/.pyenv"
  addenv PATH "$PYENV_ROOT/bin"
  if has pyenv; then
    addenv PATH "$(pyenv root)/shims"
  fi

  # LaTeX is a markup language to write a document.
  if [ -d "/usr/local/texlive" ]; then
    year=$(find "/usr/local/texlive" -mindepth 1 -maxdepth 1 -type d -printf "%f\n" | \
             grep "[0-9]\+" | sort -r | head -1)
    if [ -n "$year" ]; then
      os=$(uname -s | tr '[:upper:]' '[:lower:]')
      if [ "$os" = linux ] || [ "$os" = darwin ]; then
        arch=$(uname -m)
        addenv PATH "/usr/local/texlive/${year}/bin/${arch}-${os}"
      fi
    fi
  fi
fi

## External configuration
### ~/.profile.local

if [ -n "$HOME" ] && [ -f "$HOME/.profile.local" ]; then
  # shellcheck source=/dev/null
  . "$HOME/.profile.local"
fi

## gpg-agent

if has gpg-connect-agent; then

  gpg_restart() {
    gpg-connect-agent reloadagent /bye
  }

  gpg_forget() {
    gpg-connect-agent reloadagent /bye
  }

fi

## ssh-agent

if has ssh-agent; then

  ssh_connect() {
    if [ -n "$HOME" ] && [ -f "$HOME/.ssh/agent-info" ]; then
      eval "$(cat "$HOME/.ssh/agent-info")" >/dev/null
    fi
  }

  ssh_connected() {
    # shellcheck disable=SC2009
    ps -p "$SSH_AGENT_PID" 2>&1 | grep -qF ssh-agent
  }

  ssh_forget() {
    ssh-add -D
  }

  ssh_restart() {
    if [ -n "$HOME" ]; then
      pkill -U "$USER" ssh-agent
      mkdir -p "$HOME/.ssh"
      ssh-agent "${SSH_AGENT_ARGS:--t 86400}" > "$HOME/.ssh/agent-info"
      ssh_connect
    fi
  }

  ssh_connect
  if ! ssh_connected; then
    ssh_restart
  fi

fi

# Prevent the system from loading this file more than once.
export ARCHE_SKIP_PROFILE=1

# When running login shell on WSL load $HOME/.bashrc automatically.
# https://github.com/canonical/ubuntu-wsl-integration/blob/master/wsl-integration.sh
if grep -qEi "(microsoft|wsl)" /proc/version >/dev/null 2>&1; then
  WSL_HOST="$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null)"
  export DISPLAY="${WSL_HOST}:0"
  export PULSE_SERVER="tcp:${WSL_HOST}"
  export LIBGL_ALWAYS_INDIRECT=1
  unset WSL_HOST
  case $0 in
    -zsh) ;;
    -*)
      # shellcheck source=/dev/null
      . "$HOME/.bashrc"
      ;;
  esac
fi
