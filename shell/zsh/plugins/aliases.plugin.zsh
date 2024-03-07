#!/usr/bin/env zsh

## Aliases
### Filesystem navigation
#### cd

alias -- -='cd -'
alias -- -1='cd -1'
alias -- -2='cd -2'
alias -- -3='cd -3'
alias -- -4='cd -4'
alias -- -5='cd -5'
alias -- -6='cd -6'
alias -- -7='cd -7'
alias -- -8='cd -8'
alias -- -9='cd -9'

#### dirs

# This alias is a convenient way to list the last few directories
# visited, with their numbers. You can then use the 'cd -n' aliases to
# jump to those directories.
alias ds='dirs -v | head -10'

### Filesystem management

#### mkdir

alias md='mkdir -p'

#### rmdir

alias rmd='rmdir'

#### rm

# Prompt before removing more than three files and do not remove the
# root ('/').
if [[ "$OSTYPE" != darwin* ]]; then
  alias rm='rm -I --preserve-root'
fi

### Help system

alias help=run-help

### Utilities
#### Clipboard utilities

if (( $+commands[xsel] )) && [[ "$OSTYPE" != darwin* ]]; then
  alias pbcopy="tr -d '\n' | xsel --clipboard --input"
  alias pbpaste='xsel --clipboard --output'
fi

#### Docker

if (( $+commands[docker] )); then
  alias dr='docker run -it --rm'
fi

#### Emacs

if (( $+commands[emacs] )); then
  alias e='emacs -nw'
  alias eq='emacs -nw -Q'
  alias ew='emacs'
  alias eqw='emacs -Q'
  alias ue='USER_EMACS_DIRECTORY=$PWD e'
  alias uew='USER_EMACS_DIRECTORY=$PWD ew'
fi

if (( $+commands[emacsclient] )); then
  alias ec='emacsclient --alternate-editor= -nw'
  alias ecw='emacsclient --alternate-editor='
fi

#### fd

if (( $+commands[fdfind] )); then
  alias fd='fdfind'
fi

#### Grep

if (( $+commands[grep] )); then
  alias grep='grep --color=auto'
fi

#### Hub

if (( $+commands[hub] )); then
  alias hcl='hub clone --recursive'
  alias hc='hub create --copy'
  alias hcp='hub create -p --copy'
  alias hf='hub fork'
  alias hp='hub pull-request --copy'
  alias hb='hub browse'
  alias hh='hub help'
  alias hi='hub issue'
fi

#### Trash

if (( $+commands[trash] )); then
  alias t='trash'
fi

#### Vi, Vim, Neovim

if (( $+commands[nvim] )); then
  alias v='nvim'
elif (( $+commands[vim] )); then
  alias v='vim'
elif (( $+commands[vi] )); then
  alias v='vi'
fi

# Local Variables:
# outline-regexp: "##+"
# End:
