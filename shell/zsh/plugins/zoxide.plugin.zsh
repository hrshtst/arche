#!/usr/bin/env zsh

### zoxide

if (( ! $+commands[zoxide] )); then
  return
fi

function zoxide-cd-widget() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local dir=$(zoxide query -l | fzf --query="$LBUFFER")
  if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  zle push-line
  BUFFER="cd ${(q)dir}"
  zle accept-line
  local ret=$?
  unset dir
  zle reset-prompt
  return $ret
}
zle -N zoxide-cd-widget
bindkey '^x^r' zoxide-cd-widget
