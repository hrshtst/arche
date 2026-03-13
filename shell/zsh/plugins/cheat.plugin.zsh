#!/usr/bin/env zsh

### cheat

if (( ! $+commands[cheat] )); then
  return
fi

# Load cached integration script for zsh
cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}
cheat_cache="$cache_dir/_cheat"
cheat_bin="$GOPATH/bin/cheat"
if [[ ! -r "$cheat_cache" || "$cheat_bin" -nt "$cheat_cache" ]]; then
  mkdir -p $cache_dir
  cheat --completion zsh > $cheat_cache
fi
source "$cheat_cache"
unset cache_dir cheat_cache cheat_bin
