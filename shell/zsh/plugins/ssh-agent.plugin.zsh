#!/usr/bin/env zsh

#### ssh-agent

if (( ! $+commands[ssh-agent] )); then
  return
fi

# Prefer the GNOME keyring agent (gcr-ssh-agent): it holds keys unlocked at
# login, so no passphrase prompts. Only override a socket that is dead or one of
# our own private agents, so a forwarded agent (ssh -A) is left alone.
_gcr_sock="${XDG_RUNTIME_DIR:-/run/user/$UID}/gcr/ssh"
if [[ -S "$_gcr_sock" ]] && { [[ ! -S "$SSH_AUTH_SOCK" ]] || [[ "$SSH_AUTH_SOCK" == "$HOME/.ssh/agent/"* ]]; }; then
  export SSH_AUTH_SOCK="$_gcr_sock"
fi
unset _gcr_sock

function ssh_connect() {
  if [ -n "$HOME" ] && [ -f "$HOME/.ssh/agent-info" ]; then
    eval "$(cat "$HOME/.ssh/agent-info")" >/dev/null
  fi
}

function ssh_connected() {
  [[ -S "$SSH_AUTH_SOCK" ]]
}

function ssh_forget() {
  ssh-add -D
}

function ssh_restart() {
  if [ -n "$HOME" ]; then
    pkill -U "$USER" -f '^ssh-agent( |$)'
    mkdir -p "$HOME/.ssh"
    ssh-agent -t 86400 > "$HOME/.ssh/agent-info"
    ssh_connect
  fi
}

if ! ssh_connected; then
  ssh_connect
  if ! ssh_connected; then
    ssh_restart
  fi
fi
