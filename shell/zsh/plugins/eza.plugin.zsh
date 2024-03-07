#!/usr/bin/env zsh

#### ls, eza

if (( $+commands[eza] )); then

  alias ls='eza'

  function l() {
    emulate -LR zsh
    eza --all --header --long --classify --binary --color-scale $@
  }

  function lg() {
    emulate -LR zsh
    l --grid $@
  }

  function lt() {
    emulate -LR zsh
    l --tree --ignore-glob ".git|.svn|node_modules" $@
  }

  function lti() {
    emulate -LR zsh
    l --tree --ignore-glob ".git|.svn|node_modules|$1" ${@:2}
  }

  function ltl() {
    emulate -LR zsh
    lt --level $@
  }

  function ltli() {
    l --tree --level $1 --ignore-glob ".git|.svn|node_modules|$2" ${@:3}
  }

else
  # We alias gls to a git command elsewhere, so we use "command"
  # here to prevent it from being interpreted as said git command.
  if (( $+commands[gls] )); then
    alias l='command gls -AlhF --color=auto'
  else
    alias l='ls -AlhF --color=auto'
  fi
  if (( $+commands[tree] )); then
    alias lt='tree -a'
    alias ltl='tree -aL'
  fi
fi
