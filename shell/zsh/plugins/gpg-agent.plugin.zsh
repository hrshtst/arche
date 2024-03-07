#!/usr/bin/env zsh

#### gpg-agent

if (( ! $+commands[gpg-connect-agent] )); then
  return
fi

function gpg_restart() {
  gpg-connect-agent reloadagent /bye
}

function gpg_forget() {
  gpg-connect-agent reloadagent /bye
}
