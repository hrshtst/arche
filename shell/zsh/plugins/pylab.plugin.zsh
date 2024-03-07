#!/usr/bin/env zsh

#### Python

function pylab() {
  emulate -LR zsh
  if ! (( $+commands[ipython] )); then
    echo >&2 "unable to find ipython in PATH"
    return 1
  fi
  ipython --pylab $@
}
