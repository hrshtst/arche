# This is my personal bash resource file for non-login shells executed
# by bash(1). Some snippets of code in the former part of this file
# are borrowed from the default .bashrc in Ubuntu which lives in
# /etc/skel/.bashrc. Examples in
# /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# are used as reference as well.

# If not running interactively, don't do anything.
case $- in
  *i*) ;;
    *) return;;
esac

### Preferences on command history

# Don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

### Shell options

# Check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

### Miscellaneous

# Make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Deactivate stty stop command to use <C-s> for incremental forward
# command search.
stty stop undef

### Prompt setting

# Set variable identifying the chroot you work in (used in the prompt
# below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
  debian_chroot=$(cat /etc/debian_chroot)
fi

# Set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
  xterm-color|*-256color) color_prompt=yes;;
esac

if [ -n "$force_color_prompt" ]; then
  if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
  else
    color_prompt=
  fi
fi

if [ "$color_prompt" = yes ]; then
  PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
  PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
  PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
  ;;
*)
  ;;
esac

# Append number of executed commands.
PS1="${PS1/%\\$ / #\\#\\n\\$ }"

# Append git repository status if available.
if [[ -f /usr/lib/git-core/git-sh-prompt ]]; then
  source /usr/lib/git-core/git-sh-prompt
  GIT_PS1_SHOWDIRTYSTATE=1
  GIT_PS1_SHOWUPSTREAM="auto"
  PS1="${PS1/ #\\#/\$(__git_ps1 \" (\%s)\") #\\#}"
fi

### Commands with colors

# Enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# Colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

### Useful aliases

# Check if a command is hashed on the system
has() {
  type "$1" >/dev/null 2>&1
}

# Open files with associated applications
if has xdg-open; then
  alias open='xdg-open &>/dev/null'
fi

# Manipulate X selection (aka clipboard)
if has xsel; then
  alias pbcopy="tr -d '\n' | xsel --clipboard --input"
  alias pbpaste='xsel --clipboard --output'
fi

# Run pylab
if has ipython; then
  alias pylab='ipython --pylab'
fi

# Go to parent directory
alias ..='cd ..'

# Prompt before removing more than three files
alias rm='rm -I --preserve-root'

# Get current IP address
alias myip='curl -sS http://ipecho.net/plain; echo'

# Some more useful aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias dot='ls .[a-zA-Z0-9_]*'
alias j='jobs -l'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# This function creates a new session of tmux if no arguments
# provided. The new session starts with an interactive bash shell
# and the deafult shell will be set to bash as well. If some
# arguments or options are provided, pass them to tmux command.
tmux() {
  TMUX_SHELL=${TMUX_SHELL:-$(which bash)}
  if test $# = 0; then
    command tmux new-session $TMUX_SHELL \; set-option default-shell $TMUX_SHELL
  else
    command tmux $@
  fi
}

### Bash completion

# Enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

### Set environment variables

# Set a value to an environment variable
setenv() {
  if [ $# -ne 2 ]; then
    echo "setenv: Too few arguments"
    return 1
  fi

  export $1="$2"
}

# Add a value to the head of a variable with a delimiter
addenv() {
  if [ $# -ne 2 ]; then
    echo "addenv: Too few arguments"
    return 1
  fi

  local var=$1
  local value="$2"
  local current="$(eval echo "\$$var")"

  if [[ -z $current ]]; then
    setenv $var "$value"
  else
    if [[ ":$current:" != *":$value:"* ]]; then
      setenv $var "${value}:${current}"
    fi
  fi
}

# Add a path for my own library
addenv PATH "$HOME/usr/bin"
addenv LD_LIBRARY_PATH "$HOME/usr/lib"

# Add a path for pkg-config
# https://www.freedesktop.org/wiki/Software/pkg-config/
addenv PKG_CONFIG_PATH "$HOME/usr/lib/pkgconfig"

# pipx provides a way to execute binaries from Python packages in
# isolated environments.
# https://github.com/pipxproject/pipx
addenv PATH "$HOME/.local/bin"
# Autocompletion function for pipx is provided along with argcomplete,
# which is a dependency of pipx.
if command -v register-python-argcomplete 1>/dev/null 2>&1; then
    eval "$(register-python-argcomplete pipx)"
fi

# Configure paths for Go Programming Language.
# https://golang.org/
# https://github.com/golang/go/wiki/SettingGOPATH
setenv GOPATH "$HOME/.go"
setenv GOROOT "$HOME/usr/lib/go"
addenv PATH "$GOROOT/bin"
addenv PATH "$GOPATH/bin"

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
setenv PYENV_ROOT "$HOME/.pyenv"
addenv PATH "$PYENV_ROOT/bin"
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi
