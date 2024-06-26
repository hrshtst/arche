# This is my personal bash resource file for non-login shells executed
# by bash(1). Some snippets of code in the former part of this file
# are borrowed from the default .bashrc in Ubuntu which lives in
# /etc/skel. Examples in /usr/share/doc/bash/examples/startup-files
# (in the package bash-doc) are used as reference as well.

# If not running interactively, don't do anything.
case $- in
  # *i*) [ $SHLVL -eq 1 ] && command -v zsh &> /dev/null && exec zsh;;
  *i*) ;;
  *) return;;
esac

## External configuration

if [ -f "$HOME/.bashrc.local" ]; then
  # shellcheck source=/dev/null
  . "$HOME/.bashrc.local"
fi

## Shell configuration

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

# shellcheck disable=SC2154
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
  if [ -r "$HOME/.dircolors" ]; then
    eval "$(dircolors -b "$HOME/.dircolors")"
  else
    eval "$(dircolors -b)"
  fi

  alias ls='ls --color=auto'
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'

  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# Colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

## Useful aliases

# Check if a command is hashed on the system.
has() {
  type "$1" >/dev/null 2>&1
}

# Return True (0) if the shell running on WSL.
is_wsl() {
  if grep -qEi "(microsoft|wsl)" /proc/version 2>&1 /dev/null; then
    return 0
  else
    return 1
  fi
}

# Return True (0) if the shell running on Mac OS.
is_mac() {
  if [[ "$OSTYPE" == darwin* ]]; then
    return 0
  else
    return 1
  fi
}

# Open files with associated applications.
if is_wsl; then
  alias open='wslview &>/dev/null'
else
  if has xdg-open; then
    alias open='xdg-open &>/dev/null'
  fi
fi

# Manipulate X selection (aka clipboard).
if ! is_mac && has xsel; then
  alias pbcopy="tr -d '\n' | xsel --clipboard --input"
  alias pbpaste='xsel --clipboard --output'
fi

# Run pylab.
if has ipython; then
  alias pylab='ipython --pylab'
fi

# Go to parent directory.
alias ..='cd ..'

# Prompt before removing more than three files.
if ! is_mac; then
  alias rm='rm -I --preserve-root'
fi

# Get current IP address.
alias myip='curl -sS http://ipecho.net/plain; echo'

# Some more useful aliases.
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias dot='ls .[a-zA-Z0-9_]*'
alias j='jobs -l'

# Add an "alert" alias for long running commands. Use like this:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# This function creates a new session of tmux if no arguments
# provided. The new session starts with an interactive bash shell
# and the deafult shell will be set to bash as well. If some
# arguments or options are provided, pass them to tmux command.
tmux() {
  TMUX_DEFAULT_SHELL="$(which bash)"
  if test $# = 0; then
    command tmux new-session "$TMUX_DEFAULT_SHELL" \; set-option default-shell "$TMUX_DEFAULT_SHELL"
  else
    command tmux "$@"
  fi
}

# Invoke tmux so that the default shell is set to zsh.
tmux-zsh() {
  local default_shell
  default_shell="$(which zsh)"
  command tmux new-session "$default_shell" \; set-option default-shell "$default_shell"
}
alias zsh-tmux='tmux-zsh'
alias tz='tmux-zsh'
alias zt='tmux-zsh'


## Bash completion

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

# Autocompletion function for pipx is provided along with argcomplete,
# which is a dependency of pipx.
if command -v pipx 1>/dev/null 2>&1; then
  eval "$(register-python-argcomplete pipx)"
fi

# Autocompletion function for pdm.
if command -v rye 1>/dev/null 2>&1; then
  mkdir -p "$HOME/.local/share/bash-completion/completions"
  pdm completion bash > "$HOME/.local/share/bash-completion/completions/pdm.bash"
fi

# Autocompletion function for rye.
if command -v rye 1>/dev/null 2>&1; then
  mkdir -p "$HOME/.local/share/bash-completion/completions"
  rye self completion > "$HOME/.local/share/bash-completion/completions/rye.bash"
fi

# Autocompletion function for alacritty.
if command -v alacritty 1>/dev/null 2>&1; then
  if [ -f  "$HOME/.bash_completion/alacritty" ]; then
    # shellcheck source=/dev/null
    source "$HOME/.bash_completion/alacritty"
  fi
fi

## Applications

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
if command -v pyenv 1>/dev/null 2>&1; then
  eval "$(pyenv init -)"
fi

# PDM is a project management tool.
# https://github.com/pdm-project/pdm
if command -v pdm 1>/dev/null 2>&1; then
  eval "$(pdm --pep582 bash)"
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

## External configuration hook

if type -t arche_after_init_hook > /dev/null; then
  arche_after_init_hook
fi
