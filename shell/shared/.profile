# shellcheck shell=sh
# This is my personal configuration for login shells.

# Check if the shell that sources this is a login shell.
case $0 in
  -*) login_shell=true;;
   *) login_shell=false;;
esac

# Show error messages to stderr.
error () {
  echo >&2 "error: $*"
}

# Show warning messages when the current shell is not a login shell.
warn () {
  if [ $login_shell = false ]; then
    echo >&2 "warning: $*"
  fi
}

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
  if [ ! -d "$1" ]; then
    warn "no such directory (addenv): $1"
    return 1
  fi
  return 0
}

# Add a value to the head of a variable. If the variable name ends
# with PATH the value is added with a delimiter ':'.
# Usage:
#   addenv PATH $HOME/usr/bin  # => PATH=$HOME/usr/bin:
#   addenv PATH /bin /usr/bin  # => PATH=/bin:/usr/bin:
#   addenv --force PATH /noexist  # => PATH=/noexist:
addenv () {
  force=false
  case $1 in
    --force|-f)
      force=true
      shift
      ;;
    --*|-*)
      warn "unrecognized option (addenv): $1"
      shift
      ;;
  esac

  if [ $# -lt 2 ]; then
    error "too few arguments (addenv)"
    return 1
  fi

  var=$1; shift
  curvals="$(eval 'echo $'"$var")"
  case $var in
    *PATH) del=":" ;;
    *) del= ;;
  esac
  newvals=
  i=$#
  v=

  while [ "$i" -gt 0 ]; do
    eval "v=\${$i}"
    if [ -z "$del" ] || [ "$force" = true ] || _is_valid "$v"; then
      case ${del}${curvals}${del} in
        *${del}${v}${del}*) ;;  # this means the entry already exists
        *) newvals="${v}${del}${newvals}";;
      esac
    fi
    i=$((i-1))
  done
  setenv "$var" "${newvals}${curvals}"
}

# Return True if a command is hashed on the system.
has() {
  command -v "$1" >/dev/null 2>&1
}

## PATH setup

if [ -n "$HOME" ]; then

  # Set PATH so it includes user's private bin.
  addenv --force PATH "$HOME/.local/bin" "$HOME/usr/bin"
  addenv --force LD_LIBRARY_PATH "$HOME/usr/lib"

  # Set PATH for pkg-config.
  # https://www.freedesktop.org/wiki/Software/pkg-config/
  addenv --force PKG_CONFIG_PATH "$HOME/usr/lib/pkgconfig"

  # Configure paths for Go Programming Language.
  # https://golang.org/
  # https://github.com/golang/go/wiki/SettingGOPATH
  # $GOROOT is assumed to be /usr/local/go
  if [ -d /usr/local/go ] || [ -d "$HOME/usr/local/go" ]; then
    setenv GOPATH "$HOME/usr/go"
    addenv PATH "$GOPATH/bin" "/usr/local/go/bin"
  fi

  # Configure paths for packages installed by yarn.
  # https://classic.yarnpkg.com/en/docs/cli/global
  if [ -d "$HOME/.yarn" ] && has yarn; then
    addenv PATH "$(yarn global bin)"
  fi

  # pyenv is a simple python version management.
  # https://github.com/pyenv/pyenv
  if [ -d "$HOME/.pyenv" ]; then
    setenv PYENV_ROOT "$HOME/.pyenv"
    has pyenv || addenv PATH "$PYENV_ROOT/bin"
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
        addenv PATH "/usr/local/texlive/${year}/bin/${arch}-${os}"
      fi
    fi
  fi

  # Lots of applications are installed in $ZPFX by zinit.
  if [ -d "$HOME/.config/zinit" ]; then
    ZPFX=$HOME/.config/zinit/polaris
    addenv PATH "$ZPFX/bin"
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

# Set DISPLAY When running on WSL.
# https://github.com/canonical/ubuntu-wsl-integration/blob/master/wsl-integration.sh
if grep -qEi "(microsoft|wsl)" /proc/version >/dev/null 2>&1; then
  WSL_HOST="$(awk '/nameserver / {print $2; exit}' /etc/resolv.conf 2>/dev/null)"
  export DISPLAY="${WSL_HOST}:0"
  export PULSE_SERVER="tcp:${WSL_HOST}"
  export LIBGL_ALWAYS_INDIRECT=1
  unset WSL_HOST
fi
