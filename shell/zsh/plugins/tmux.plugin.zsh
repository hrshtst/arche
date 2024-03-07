#!/usr/bin/env zsh

#### Tmux
# if (( ! $+commands[tmux] )); then
#   return
# fi

# function tmux() {
#   TMUX_DEFAULT_SHELL=${TMUX_DEFAULT_SHELL:-$(which zsh)}
#   if [[ $# = 0 ]]; then
#     command tmux new-session "$TMUX_DEFAULT_SHELL" \; set-option default-shell "$TMUX_DEFAULT_SHELL"
#   else
#     command tmux "$@"
#   fi
# }

# alias ta='tmux attach'
# function ts() {
#   tmux new-session -s ${1:-tmux}
# }
# alias tl='tmux list-sessions'

if (( ! $+functions[_zsh_tmux_plugin_run] )); then
  print "Load tmux plugin from Oh My Zsh beforehand."
  return 1
fi

# CONFIGURATION VARIABLES
# Automatically start tmux
: ${ARCHE_ZSH_TMUX_AUTOSTART:=false}
# Only autostart once. If set to false, tmux will attempt to
# autostart every time your zsh configs are reloaded.
: ${ARCHE_ZSH_TMUX_AUTOSTART_ONCE:=true}

# Customized wrapper function for tmux.
function _arche_zsh_tmux_plugin_run() {
  if [[ -n "$@" ]]; then
    command tmux "$@"
    return $?
  fi

  local -a tmux_cmd
  tmux_cmd=(command tmux)
  [[ "$ZSH_TMUX_ITERM2" == "true" ]] && tmux_cmd+=(-CC)
  [[ "$ZSH_TMUX_UNICODE" == "true" ]] && tmux_cmd+=(-u)
  TMUX_DEFAULT_SHELL="$(which zsh)"

  local _detached=""
  [[ "$ZSH_TMUX_DETACHED" == "true" ]] && _detached="-d"
  # Try to connect to an existing session.
  if [[ -n "$ZSH_TMUX_DEFAULT_SESSION_NAME" ]]; then
    [[ "$ZSH_TMUX_AUTOCONNECT" == "true" ]] && $tmux_cmd attach $_detached -t $ZSH_TMUX_DEFAULT_SESSION_NAME
  else
    [[ "$ZSH_TMUX_AUTOCONNECT" == "true" ]] && $tmux_cmd attach $_detached
  fi

  # If failed, just run tmux, fixing the TERM variable if requested.
  if [[ $? -ne 0 ]]; then
    if [[ "$ZSH_TMUX_FIXTERM" == "true" ]]; then
      tmux_cmd+=(-f "$_ZSH_TMUX_FIXED_CONFIG")
    elif [[ -e "$ZSH_TMUX_CONFIG" ]]; then
      tmux_cmd+=(-f "$ZSH_TMUX_CONFIG")
    fi
    if [[ -n "$ZSH_TMUX_DEFAULT_SESSION_NAME" ]]; then
      $tmux_cmd new-session -s $ZSH_TMUX_DEFAULT_SESSION_NAME $TMUX_DEFAULT_SHELL \; set-option default-shell $TMUX_DEFAULT_SHELL
    else
      $tmux_cmd new-session $TMUX_DEFAULT_SHELL \; set-option default-shell $TMUX_DEFAULT_SHELL
    fi
  fi

  if [[ "$ZSH_TMUX_AUTOQUIT" == "true" ]]; then
    exit
  fi
}

# Use the completions for tmux for our function
compdef _tmux _arche_zsh_tmux_plugin_run
# Alias tmux to our wrapper function.
alias tmux=_arche_zsh_tmux_plugin_run

# Autostart if not already in tmux and enabled.
if [[ -z "$TMUX" && "$ARCHE_ZSH_TMUX_AUTOSTART" == "true" && -z "$INSIDE_EMACS" && -z "$EMACS" && -z "$VIM" && -z "$INTELLIJ_ENVIRONMENT_READER" ]]; then
  # Actually don't autostart if we already did and multiple autostarts are disabled.
  if [[ "$ARCHE_ZSH_TMUX_AUTOSTART_ONCE" == "false" || "$ARCHE_ZSH_TMUX_AUTOSTARTED" != "true" ]]; then
    export ARCHE_ZSH_TMUX_AUTOSTARTED=true
    TMUX_DEFAULT_SHELL="$(which zsh)"
    if [[ -n "$ZSH_TMUX_DEFAULT_SESSION_NAME" ]]; then
      command tmux new-session -s $ZSH_TMUX_DEFAULT_SESSION_NAME $TMUX_DEFAULT_SHELL \; set-option default-shell $TMUX_DEFAULT_SHELL
    else
      command tmux new-session $TMUX_DEFAULT_SHELL \; set-option default-shell $TMUX_DEFAULT_SHELL
    fi
  fi
fi
