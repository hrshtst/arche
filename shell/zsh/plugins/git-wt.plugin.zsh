#!/usr/bin/env zsh

### git-wt

# Load cached script for git-wt
cache_dir=${XDG_CACHE_HOME:-$HOME/.cache}
git_wt_cache="$cache_dir/git-wt-init.zsh"
git_wt_bin="$GOPATH/bin/git-wt"
if [[ ! -r "$git_wt_cache" || "$git_wt_bin" -nt "$git_wt_cache" ]]; then
  mkdir -p $cache_dir
  git wt --init zsh > $git_wt_cache
fi
source "$git_wt_cache"
unset cache_dir git_wt_cache git_wt_bin

# Select a worktree created in the current repository
wt() {
  local worktree=$(git wt | tail -n +2 | fzf | awk '{print $(NF-1)}')
  if [[ -n $worktree ]]; then
    git wt $worktree
  fi
}
