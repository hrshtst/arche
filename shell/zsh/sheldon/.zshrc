#!/usr/bin/env zsh

## Autostart tmux

# Start tmux session automatically when the current shell is
# interactive.
if (( $+commands[tmux] )) && [[ -o interactive ]] && (( ! $+TMUX )); then
  default_shell="$(which zsh)"
  exec tmux new-session $default_shell \; set-option default-shell $default_shell
fi

# Load plugins managed by sheldon
eval "$(sheldon source)"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
