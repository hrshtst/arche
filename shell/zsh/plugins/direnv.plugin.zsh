#!/usr/bin/env zsh

### direnv

# Per-directory environments (.envrc). Guarded so shells on machines
# without direnv start silently.
if (( ! $+commands[direnv] )); then
  return
fi

eval "$(direnv hook zsh)"
