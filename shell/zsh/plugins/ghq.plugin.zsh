#!/usr/bin/env zsh

### ghq

# Lists repositories cloned by ghq in order so that the most
# recently accessed is first.
function __ghq_list_recent_first() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local root_dirs
  root_dirs=$(cat <(ghq root) <(git config --path --get-all ghq.root))
  ghq list --full-path | \
    xargs -I{} ls -dl --time-style=+%s {}/.git | \
    sed -E 's/.*([0-9]{10})/\1/' | \
    sort -nr | \
    sed -E "s@.*${root_dirs//$'\n'/|}/@@" | \
    sed -E 's/\/.git$//'
}

# Select a repository from ones managed by ghq and change to it.
function ghq-cd-widget() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local selected dir
  selected=$(__ghq_list_recent_first | fzf --query="$LBUFFER")
  if [[ -z "$selected" ]]; then
    zle redisplay
    return 0
  fi
  dir=$(ghq list --full-path | grep -E "${selected}$")
  zle push-line
  BUFFER="cd ${(q)dir}"
  zle accept-line
  local ret=$?
  unset selected dir
  zle reset-prompt
  return $ret
}
zle -N ghq-cd-widget
bindkey '\eg' ghq-cd-widget
