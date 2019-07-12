# My personal aliases.

# Check if a command is hashed on the system.
has() {
  type "$1" >/dev/null 2>&1
}

# Mimic open command on Mac OS.
if has xdg-open; then
  alias open='xdg-open'
fi

# Mimic pbcopy command on Mac OS.
if has xsel; then
  alias pbcopy="tr -d '\n' | xsel --clipboard --input"
fi

if has ipython; then
  alias pylab='ipython --pylab'
fi

# Colorize dir command.
alias dir='dir --color=auto'

# Go to parent directory
alias ..='cd ..'.

# Prompt before removing more than tree files.
alias rm='rm -I --preserve-root'

# Get current IP adddress
alias myip='curl -sS http://ipecho.net/plain; echo'

# This function creates a new session of tmux if no arguments are
# passed. The new session starts with an interactive fish shell and
# the deafult shell will be set to fish as well. If one or more
# arguments or options are provided, pass them to tmux command.
tmux() {
  TMUX_SHELL=${TMUX_SHELL:-$(which bash)}
  if test $# = 0; then
    command tmux new-session $TMUX_SHELL \; set-option default-shell $TMUX_SHELL
  else
    command tmux $@
  fi
}
