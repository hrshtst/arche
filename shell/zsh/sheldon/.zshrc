#!/usr/bin/env zsh

# This is my personal resource file for non-login shells executed by
# zsh(1).
#

## Autostart tmux

# Start tmux session automatically when the current shell is
# interactive.
if (( $+commands[tmux] )) && [[ -o interactive ]] && (( ! $+TMUX )); then
  default_shell="$(which zsh)"
  exec tmux new-session $default_shell \; set-option default-shell $default_shell
fi

## External configuration
### ~/.zshrc.local

if [[ -f "${HOME}/.zshrc.local" ]]; then
  . "${HOME}/.zshrc.local"
fi

## Sheldon
# Sheldon is a fast, configurable, command-line tool to manage your shell plugins.

# Load plugins managed by sheldon
eval "$(sheldon source)"

## Shell configuration
### Command line

# Force the use of Emacs keybindings.
bindkey -e

# Allow a very fast way (just typing ".") to reload the shell
# configuration. Based on [1].
#
# [1]: https://unix.stackexchange.com/a/326948/176805
function _accept-line() {
  emulate -LR zsh
  if [[ $BUFFER == "." ]]; then
    BUFFER="exec zsh"
  fi
  zle .accept-line
}
zle -N accept-line _accept-line

# Allow comments even in the interactive shell (start with #).
setopt interactive_comments

# Allow escaping a single quote within a singly-quoted string by
# prefixing it with an additional single quote: echo 'It''s me!'
setopt rc_quotes

# Turn off flow control (which makes it so that ctrl+s and ctrl+q
# freeze and unfreeze command output, respectively).
unsetopt flow_control

# Kill the whole, potentially multiline, command and yank it on the
# next fresh prompt.
bindkey '^q' push-line-or-edit

#### Completion

# Form of zstyle:
#   zstyle [--|-e] pattern style strings ...
# Pattern for the completion:
#   :completion:<function>:<completer>:<command>:<argument>:<tag>

# Using a cache for the completion can speed up some commands such as
# apt and dpkg.
zstyle ':completion:*' use-cache true

# Enable fuzzy completions. Sometimes it doesn't behave as I expected,
# but works more or less. Based on <https://superuser.com/a/815317>.
zstyle ':completion:*' matcher-list 'm:{a-z\-}={A-Z\_}' 'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' 'r:|?=** m:{a-z\-}={A-Z\_}'

# Use an interactive menu for ambiguous completions instead of
# overwriting the current command.
zstyle ':completion:*' menu select

# Allow the use of shift-tab to go backward in the completion menu.
bindkey '^[[Z' reverse-menu-complete

# Let the completion menu more descriptive.
zstyle ':completion:*' group-name ''
zstyle ':completion:*:*:*:*:options' auto-description 'specify: %d'
if (( $+_ftb_orig_widget )); then
  zstyle ':completion:*:*:*:*:descriptions' format '[%d]'
else
  zstyle ':completion:*:*:*:*:descriptions' format '%F{yellow}-- %d --%f'
fi
zstyle ':completion:*:*:*:*:corrections' format '%F{green}-- %d (errors: %e) --%f'
zstyle ':completion:*:*:*:*:messages' format '%F{purple}-- %d --%f'
zstyle ':completion:*:*:*:*:warnings' format '%F{red}-- no matches found --%f'

# Disable sort when completing 'git checkout'.
zstyle ':completion:*:*:*:*:git-checkout:*' sort false

# Configure completion of 'kill' command.
[[ -z $USER ]] && USER=$(whoami)
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
zstyle ':completion:*:*:kill:*' menu true select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Configure completion of 'man' command.
zstyle ':completion:*:*:man:*' menu true select
zstyle ':completion:*:*:*:*:manuals' separate-sections true
zstyle ':completion:*:*:*:*:manuals.*' insert-sections true

#### Globbing

# This makes globs case-insensitive.
unsetopt case_glob

# This makes globbing regexes case-insensitive.
unsetopt case_match

# Allow globs to match dotfiles.
setopt glob_dots

# Sort numeric filenames numerically, instead of lexicographically.
setopt numeric_glob_sort

# Disable history expansion, so we can use ! in our commands.
setopt no_bang_hist

### Filesystem navigation

# This makes it so that when you cd to a new directory, the old
# directory is saved in the directory stack.
setopt auto_pushd

# This makes it so that "cd -n" gives you the directory you were in n
# cd's ago, instead of the nth directory you have visited in the shell
# session. (You can use "cd +n" to recover the latter behavior.)
setopt pushd_minus

# This makes it so that the working directory path is automatically
# fully resolved. This means that symlink components will be followed,
# and capitalization will be corrected if you are on a
# case-insensitive filesystem.
setopt chase_links

# If you just type in a directory name, cd to it (unless it's also a
# valid executable name).
setopt auto_cd

### Help system

# By default, run-help is an alias to man. We want to turn that off so
# that we can access the function definition of run-help (by default,
# aliases take precedence over functions). But if you re-source this
# file, then the alias might already be removed, so we suppress any
# error that this might throw.
unalias run-help 2>/dev/null || true

# Now we autoload run-help and several extensions to it which provide
# more precise help for certain external commands.
autoload -Uz run-help
autoload -Uz run-help-git
autoload -Uz run-help-ip
autoload -Uz run-help-openssl
autoload -Uz run-help-p4
autoload -Uz run-help-sudo
autoload -Uz run-help-svk
autoload -Uz run-help-svn

## External configuration hook

if typeset -f arche_after_init_hook > /dev/null; then
  arche_after_init_hook
fi

## Closing remarks

# Local Variables:
# outline-regexp: "##+"
# End:
