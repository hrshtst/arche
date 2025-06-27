#!/usr/bin/env zsh

#### ssh-agent

if (( ! $+commands[ssh-agent] )); then
  return
fi

function ssh_connect() {
  if [ -n "$HOME" ] && [ -f "$HOME/.ssh/agent-info" ]; then
    eval "$(cat "$HOME/.ssh/agent-info")" >/dev/null
  fi
}

function ssh_connected() {
  ps -p "$SSH_AGENT_PID" 2>&1 | grep -qF ssh-agent
}

function ssh_forget() {
  ssh-add -D
}

function ssh_restart() {
  if [ -n "$HOME" ]; then
    pkill -U "$USER" ssh-agent
    mkdir -p "$HOME/.ssh"
    ssh-agent -t 86400 > "$HOME/.ssh/agent-info"
    ssh_connect
  fi
}

ssh_connect
if ! ssh_connected; then
  ssh_restart
fi
