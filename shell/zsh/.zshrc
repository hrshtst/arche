# This is my personal resource file for non-login shells executed by
# zsh(1).
#
# The configuration is basically stolen from raxod502/radian:
# https://github.com/raxod502/radian

## Autostart tmux

# Start tmux session automatically when the current shell is
# interactive.
if (( $+commands[tmux] )) && [[ -o interactive ]] && (( ! $+TMUX )); then
  export TMUX_DEFAULT_SHELL=$(which zsh)
  exec tmux new-session $TMUX_DEFAULT_SHELL \; set-option default-shell $TMUX_DEFAULT_SHELL
fi

## External configuration
### ~/.zshrc.local

if [[ -f "${HOME}/.zshrc.local" ]]; then
  . "${HOME}/.zshrc.local"
fi

## Zinit
# Zinit is a fast and featureful plugin manager for Zsh.

# Configure paths for Zinit.
ZSH_CACHE_DIR="${XDG_CACHE_HOME:-${HOME}/.cache}/zsh"
[[ -n $ZSH_CACHE_DIR ]] && command mkdir -p "$ZSH_CACHE_DIR"
typeset -A ZINIT
ZINIT[HOME_DIR]="${XDG_CONFIG_HOME:-${HOME}/.config}/zinit"
ZINIT[ZCOMPDUMP_PATH]="${ZSH_CACHE_DIR}/zcompdump"

# Install Zinit if missing.
if [[ ! -f "${ZINIT[HOME_DIR]}/bin/zinit.zsh" ]]; then
  print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
  command mkdir -p "$ZINIT[HOME_DIR]" && command chmod g-rwX "$ZINIT[HOME_DIR]"
  command git clone https://github.com/zdharma-continuum/zinit "${ZINIT[HOME_DIR]}/bin" && \
    print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
      print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

# Load Zinit.
source "${ZINIT[HOME_DIR]}/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo (this is currently
# required for annexes)
zinit light-mode for \
      zdharma-continuum/z-a-rust \
      zdharma-continuum/z-a-as-monitor \
      zdharma-continuum/z-a-patch-dl \
      zdharma-continuum/z-a-bin-gem-node

### Prompt and color theme

# Define colors so that to work across a range of terminal emulators.
[[ $COLORTERM == *(24bit|truecolor)* ]] || zmodload zsh/nearcolor 2>/dev/null

# Theme the prompt with Powerlevel10k.
if [[ -f "${HOME}/.p10k.zsh" ]]; then
  zinit ice wait'!' lucid depth'1' atload'source ${HOME}/.p10k.zsh; _p9k_precmd' nocd
else
  zinit ice wait'!' lucid depth'1' atload'true; _p9k_precmd' nocd
fi
zinit light romkatv/powerlevel10k
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ -f "${HOME}/.p10k.zsh" ]] && source "${HOME}/.p10k.zsh"

# Set an arctic, north-bluish clean and elegant color theme.
zinit ice atclone"dircolors -b src/dir_colors > c.zsh" \
      atpull'%atclone' pick'c.zsh' nocompile'!' \
      atload'zstyle ":completion:*" list-colors ${(s.:.)LS_COLORS}'
zinit light arcticicestudio/nord-dircolors

### Command history utility

# By typing in any part of any command from history and pressing <up>
# and <down>, you can choose completed commands in history.
zinit ice wait lucid atload"bindkey -e '^P' history-substring-search-up; bindkey -e '^N' history-substring-search-down"
zinit light zsh-users/zsh-history-substring-search

### Modern CLI utility

# A replacement for cat with syntax highlighting.
zinit ice wait lucid from"gh-r" cp"bat-*/bat.1 -> $ZPFX/man/man1" mv"bat-*/autocomplete/bat.zsh -> $ZINIT[COMPLETIONS_DIR]/_bat" sbin"bat-*/bat"
zinit light sharkdp/bat

# View cheatsheets maintained by comunity on the command-line.
zinit ice wait lucid from"gh-r" mv"cheat* -> cheat" sbin"cheat"
zinit light cheat/cheat

# A syntax-highlighting pager for git and diff output
zinit ice wait lucid from"gh-r" sbin"delta-*/delta"
zinit light dandavison/delta

# A replacement for ls with more color support.
zinit ice wait lucid from"gh-r" cp"man/exa.1 -> $ZPFX/man/man1" sbin"bin/exa"
zinit light ogham/exa

# Load completion for exa.
zinit ice wait lucid id-as"exa-completion" depth"1" as"completion"
zinit light ogham/exa

# A simple, fast and user-friendly alternative to find.
zinit ice wait lucid from"gh-r" cp"fd-*/fd.1 -> $ZPFX/man/man1" sbin"fd-*/fd"
zinit light sharkdp/fd

# Install a o called command-line fuzzy finder, which enables us to
# filter and choose a selection from any list.
zinit ice wait lucid id-as"fzf-bin" from"gh-r" sbin"fzf"
zinit light junegunn/fzf

# Install miscellaneous files related to fzf with Zinit.
zinit ice wait lucid depth"1" as"program" pick"bin/fzf-tmux" atload"[[ $- == *i* ]] && source shell/completion.zsh 2> /dev/null" src"shell/key-bindings.zsh"
zinit light junegunn/fzf

# Management utility for remote repository clones.
zinit ice wait lucid from"gh-r" cp"ghq*/README.adoc -> $ZPFX/man/man1/ghq.1" sbin"ghq*/ghq"
zinit light x-motemen/ghq

# A fast alternative to grep that respects your gitignore.
zinit ice wait lucid from"gh-r" cp"ripgrep-*/doc/rg.1 -> $ZPFX/man/man1" sbin"ripgrep-*/rg"
zinit light BurntSushi/ripgrep

# A smarter cd command inspired by z and autojump.
# zinit ice wait lucid from"gh-r" cp"man/*.1 -> $ZPFX/man/man1" mv"completions/_zoxide -> $ZINIT[COMPLETIONS_DIR]" sbin"zoxide"
zinit ice wait lucid from"gh-r" atclone"cp man/* $ZPFX/man/man1" atpull"%atclone" mv"completions/_zoxide -> $ZINIT[COMPLETIONS_DIR]" atload"!eval $(zoxide init zsh)" sbin"zoxide"
zinit light ajeetdsouza/zoxide

### Completions and syntax highlighting

# These plugins should be loaded last.

# Replace the default completion menu with fzf. Zsh's compinit should
# be executed before loading fzf-tab, and plugins that wrap widegets
# such as fast-syntax-highlighting and zsh-autosuggestions should be
# loaded after this. Note that lazy-loading is desabled to set
# the description style for group names in completion menu.
zinit ice lucid atinit'ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay'
zinit light Aloxaf/fzf-tab

# Create an impressive look on the command line with a rich syntax
# highlighting for Zsh.
zinit ice wait lucid
zinit light zdharma-continuum/fast-syntax-highlighting

# Install additional completion definitions for Zsh.
zinit ice wait lucid blockf atpull'zinit creinstall -q .'
zinit light zsh-users/zsh-completions

# When a previous command starts with what you are typing, show the
# completed command in dimmed color, and you can accept it by pressing
# <right> key (forward-char) or <End> (end-of-line).
zinit ice wait lucid atload'!_zsh_autosuggest_start; ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=244"'
zinit light zsh-users/zsh-autosuggestions

## Shell configuration
### Command line

# Force the use of Emacs keybindings.
bindkey -e

# Allow just typing "." to reload the shell configuration. Based on
# below.
# https://unix.stackexchange.com/a/326948/176805
function _accept-line() {
  emulate -LR zsh
  if [[ $BUFFER == "." ]]; then
    BUFFER="exec zsh"
  fi
  zle .accept-line
}
zle -N accept-line _accept-line

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
zstyle ':completion:*' cache-path "${ZSH_CACHE_DIR}/zcompcache"

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

# This makes globbing (filename generation) insensive to case.
unsetopt case_glob

# This makes regular expressions using the regex module (including
# matches with =~) insensitive to case.
unsetopt case_match

# Do not require a leading '.' in a filename to be matched explicitly.
setopt glob_dots

# If numeric filenames are matched by a filename generation pattern,
# sort the filenames numerically rather than lexicographically.
setopt numeric_glob_sort

#### Command history

# Never discard history within a session, or at least not before any
# reasonable amount of time.
HISTSIZE=1000000

# Save history to disk.
HISTFILE=${ZSH_CACHE_DIR}/zsh_history

# Never discard history in the file on disk, either.
SAVEHIST=1000000

# Don't save commands to the history if they start with a leading
# space. This is useful if you have to pass a password as a parameter
# to a command.
setopt hist_ignore_space

# Do not enter command lines into the history list if they are
# duplicates of the previous event.
setopt hist_ignore_dups

# All zsh sessions share the same history file. Timestamps are also
# recorded for each command.
setopt share_history

# Use OS-provided locking mechanisms for the history file, if
# available. The manual says this might improve performance and
# decrease the chance of corruption.
setopt hist_fcntl_lock

# Remove superfluous whitespace when saving commands to the history.
setopt hist_reduce_blanks

# Whenever the user enters a line with history expansion (e.g. sudo
# !!), don't execute the line directly; instead, perform history
# expansion and reload the line into the editing buffer.
setopt hist_verify

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

#### ls, exa

if (( $+commands[exa] )); then

  function l() {
    emulate -LR zsh
    exa --all --header --long --classify --binary --color-scale $@
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

#### open

# We define a function to open a file with OS's native handler. See
# [1].
#
# [1]: https://github.com/ohmyzsh/ohmyzsh/blob/7eeb1e193d4a55ab706931fb80ef556a939be8fd/lib/functions.zsh#L16-L34

function open() {
  local open_cmd

  case "$OSTYPE" in
    darwin*)
      open_cmd='open'
      ;;
    cygwin*)
      open_cmd='cygstart'
      ;;
    linux*)
      [[ "$(uname -r)" != *icrosoft* ]] && open_cmd='nohup xdg-open' || {
          open_cmd='cmd.exe /c start ""'
          [[ -e "$1" ]] && { 1="$(wslpath -w "${1:a}")" || return 1 }
        }
      ;;
    msys*)
      open_cmd='start ""'
      ;;
    *)
      echo >&2 "Platform $OSTYPE not supported"
      return 1
      ;;
  esac

  ${=open_cmd} "$@" &>/dev/null
}

### Filesystem management

#### cp, mv, ln

# You can "copy" any number of files, then "paste", "move" or
# "pasteln" them to pass them as arguments to cp, mv, or ln
# respectively. Just like a graphical filesystem manager. Each of the
# latter three functions defaults to the current directory as the
# destination.

# Usage: copy <path>...
#
# Copy all of the paths provided to the clipboard, stored in the array
# $ARCHE_CLIPBOARD.
function copy() {
  emulate -LR zsh
  ARCHE_CLIPBOARD=()
  for target; do
    ARCHE_CLIPBOARD+=("${target:a}")
  done
}

# Usage: paste [<path>]
#
# Invoke 'cp -R' on all paths in $ARCHE_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function paste() {
  emulate -LR zsh
  cp -R $ARCHE_CLIPBOARD ${1:-.}
}

# Usage: move [<path>]
#
# Invoke 'mv' on all paths in $ARCHE_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function move() {
  emulate -LR zsh
  mv $ARCHE_CLIPBOARD ${1:-.}
}

# Usage: pasteln [<path>]
#
# Invoke 'ln -s' on all paths in $ARCHE_CLIPBOARD as well as the
# argument provided, which defaults to the current directory.
function pasteln() {
  emulate -LR zsh
  ln -s $ARCHE_CLIPBOARD ${1:-.}
}

# Usage: delink <path>
#
# Resolve the symlink at the given path and replace it with a copy of
# the file it points to.
function delink() {
  emulate -LR zsh
  if [[ -z $1 ]]; then
    echo >&2 "usage: delink <symlinks>"
    return 1
  fi
  for link; do
    if [[ -L $link ]]; then
      if [[ -e $link ]]; then
        target=${link:A}
        if rm "$link"; then
          if cp -R "$target" "$link"; then
            echo >&2 "Copied $target to $link"
          else
            ln -s "$target" "$link"
          fi
        fi
      else
        echo >&2 "Broken symlink: $link"
      fi
    else
      echo >&2 "Not a symlink: $link"
    fi
  done
}

#### mkdir

alias md='mkdir -p'

function mkcd() {
  emulate -LR zsh
  mkdir -p "$@" && cd "${@:$#}"
}

#### take

# Define a function to create a new directory and change to it. The
# argument can take URL or remote Git repository, then create the
# directory and change to it. See [1].
#
# [1]: https://github.com/ohmyzsh/ohmyzsh/blob/7eeb1e193d4a55ab706931fb80ef556a939be8fd/lib/functions.zsh#L58-L66
function takeurl() {
  local data thedir
  if ! (( $+commands[curl] )); then
    echo >&2 "unable to find 'curl' on the system"
    return 1
  fi
  data=$(mktemp)
  curl -L "$1" > "$data"
  tar xf "$data"
  thedir=$(tar tf "$data" | head -1)
  rm -rf "$data"
  cd "$thedir"
}

function takegit() {
  if ! (( $+commands[git] )); then
    echo >&2 "unable to find 'git' on the system"
    return 1
  fi
  git clone "$1"
  cd "$(basename ${1%%.git})"
}

function take() {
  emulate -LR zsh
  if [[ $1 =~ ^(https?|ftp).*\.tar\.(gz|bz2|xz)$ ]]; then
    takeurl "$1"
  elif [[ $1 =~ ^([A-Za-z0-9]\+@|https?|git|ssh|ftps?|rsync).*\.git/?$ ]]; then
    takegit "$1"
  else
    mkcd "$@"
  fi
}

#### rmdir

alias rmd='rmdir'

#### rm

# Prompt before removing more than three files and do not remove the
# root ('/').
alias rm='rm -I --preserve-root'

### Help system

alias help=run-help

### Utilities
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

#### Git

if (( $+commands[git] )); then
  alias g=git

  alias gh='git help'
  alias ghi='git help init'
  alias ghst='git help status'
  alias ghsh='git help show'
  alias ghl='git help log'
  alias gha='git help add'
  alias ghrm='git help rm'
  alias ghmv='git help mv'
  alias ghr='git help reset'
  alias ghcm='git help commit'
  alias ghcp='git help cherry-pick'
  alias ghrv='git help revert'
  alias ght='git help tag'
  alias ghn='git help notes'
  alias ghsta='git help stash'
  alias ghd='git help diff'
  alias ghbl='git help blame'
  alias ghb='git help branch'
  alias ghco='git help checkout'
  alias ghlsf='git help ls-files'
  alias ghx='git help clean'
  alias ghbs='git help bisect'
  alias ghm='git help merge'
  alias ghrb='git help rebase'
  alias ghsm='git help submodule'
  alias ghcl='git help clone'
  alias ghre='git help remote'
  alias ghf='git help fetch'
  alias ghu='git help pull'
  alias ghp='git help push'

  alias gi='git init'

  alias gst='git status'

  alias gsh='git show'
  alias gshs='git show --stat'

  for nograph in "" n; do
    local graph_flags=
    if [[ -z $nograph ]]; then
      graph_flags=" --graph"
    fi
    for all in "" a; do
      local all_flags=
      if [[ -n $all ]]; then
        all_flags=" --all"
      fi
      for oneline in "" o; do
        local oneline_flags=
        if [[ -n $oneline ]]; then
          oneline_flags=" --oneline"
        fi
        for diff in "" s p ps sp; do
          local diff_flags=
          case $diff in
            s) diff_flags=" --stat";;
            p) diff_flags=" --patch";;
            ps|sp) diff_flags=" --patch --stat";;
          esac
          for search in "" g G S; do
            local search_flags=
            case $search in
              g) search_flags=" --grep";;
              G) search_flags=" -G";;
              S) search_flags=" -S";;
            esac
            alias="gl${nograph}${all}${oneline}${diff}${search}="
            alias+="git log --decorate"
            alias+="${graph_flags}${all_flags}"
            alias+="${oneline_flags}${diff_flags}${search_flags}"
            alias $alias
          done
        done
      done
    done
  done

  alias ga='git add'
  alias gap='git add --patch'
  alias gaa='git add --all'

  alias grm='git rm'

  alias gmv='git mv'

  alias gr='git reset'
  alias grs='git reset --soft'
  alias grh='git reset --hard'
  alias grp='git reset --patch'

  alias gc='git commit --verbose'
  alias gca='git commit --verbose --amend'
  alias gcaa='git commit --verbose --amend --all'
  alias gcf='git commit -C HEAD --amend'
  alias gcfa='git commit -C HEAD --amend --all'
  alias gce='git commit --verbose --allow-empty'
  alias gcm='git commit -m'
  alias gcma='git commit --all -m'
  alias gcam='git commit --amend -m'
  alias gcama='git commit --amend --all -m'
  alias gcem='git commit --allow-empty -m'

  alias gcp='git cherry-pick'
  alias gcpc='git cherry-pick --continue'
  alias gcpa='git cherry-pick --abort'

  alias grv='git revert'
  alias grva='git revert --abort'
  alias grvm='git revert -m'

  alias gt='git tag'
  alias gtd='git tag --delete'

  alias gn='git notes'
  alias gna='git notes add'
  alias gne='git notes edit'
  alias gnr='git notes remove'

  alias gsta='git stash save'
  alias gstau='git stash save --include-untracked'
  alias gstap='git stash save --patch'
  alias gstl='git stash list'
  alias gsts='git stash show --text'
  alias gstss='git stash show --stat'
  alias gstaa='git stash apply'
  alias gstp='git stash pop'
  alias gstd='git stash drop'

  alias gd='git diff'
  alias gds='git diff --stat'
  alias gdc='git diff --cached'
  alias gdcs='git diff --cached --stat'
  alias gdn='git diff --no-index'

  alias gbl='git blame'

  alias gb='git branch'
  alias gbsu='git branch --set-upstream-to'
  alias gbusu='git branch --unset-upstream'
  alias gbd='git branch --delete'
  alias gbdd='git branch --delete --force'

  alias gco='git checkout'
  alias gcot='git checkout --track'
  alias gcop='git checkout --patch'
  alias gcob='git checkout -B'

  alias glsf='git ls-files'

  alias gx='git clean'
  alias gxu='git clean -ffd'
  alias gxi='git clean -ffdX'
  alias gxa='git clean -ffdx'

  alias gbs='git bisect'
  alias gbss='git bisect start'
  alias gbsg='git bisect good'
  alias gbsb='git bisect bad'
  alias gbsr='git bisect reset'

  alias gm='git merge'
  alias gma='git merge --abort'

  alias grb='git rebase'
  alias grbi='git rebase --interactive'
  alias grbc='git rebase --continue'
  alias grbs='git rebase --skip'
  alias grba='git rebase --abort'

  alias gsm='git submodule'
  alias gsma='git submodule add'
  alias gsms='git submodule status'
  alias gsmi='git submodule init'
  alias gsmd='git submodule deinit'
  alias gsmu='git submodule update'
  alias gsmui='git submodule update --init --recursive'
  alias gsmf='git submodule foreach'
  alias gsmy='git submodule sync'

  alias gcl='git clone --recursive'
  alias gcls='git clone --depth=1 --single-branch --no-tags'

  alias gre='git remote'
  alias grel='git remote list'
  alias gres='git remote show'

  alias gf='git fetch --prune --prune-tags'
  alias gfa='git fetch --all --prune --prune-tags'
  alias gfu='git fetch --unshallow'

  alias gu='git pull'
  alias gur='git pull --rebase --autostash'
  alias gum='git pull --no-rebase'

  alias gp='git push'
  alias gpa='git push --all'
  alias gpf='git push --force-with-lease'
  alias gpff='git push --force'
  alias gpu='git push --set-upstream'
  alias gpd='git push --delete'
  alias gpt='git push --tags'
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

#### Tmux

if (( $+commands[tmux] )); then
  alias ta='tmux attach'
  function ts() {
    tmux new-session -s ${1:-tmux}
  }
  alias tl='tmux list-sessions'
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

#### Grep

if (( $+commands[grep] )); then
  alias grep='grep --color=auto'
fi

### Python

function pylab() {
  emulate -LR zsh
  if ! (( $+commands[ipython] )); then
    echo >&2 "unable to find ipython in PATH"
    return 1
  fi
  ipython --pylab $@
}

## External command configuration
### man

# We define a function that wraps man to provide some basic
# highlighting for man pages. This makes them a little easier on the
# eyes. (This is done by binding some environment variables that less
# looks at.) See [1].
#
# [1]: https://github.com/ohmyzsh/ohmyzsh/blob/3ebbb40b31fa1ce9f10040742cdb06ea04fa7c41/plugins/colored-man-pages/colored-man-pages.plugin.zsh
function man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man $@
}

### fzf

# Configure the popping-up window and assign additional keybindings.
export FZF_DEFAULT_OPTS="--height 40% --reverse --border --inline-info --bind='ctrl-j:preview-down,ctrl-k:preview-up,?:toggle-preview,ctrl-space:toggle+down'"

# When no input is given to fzf, list files in the current directory
# using a modern file searcher such as 'fd' and 'rg'.
export FZF_DEFAULT_COMMAND="(fd --type f --hidden --follow --exclude .git || git ls-tree -r --name-only HEAD || rg --files --hidden --follow --glob '!.git' || find .) 2> /dev/null"

# FZF_CTRL_T_COMMAND is used to make a list of files in the current
# directory. This is called `fzf-file-widget`.
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# When we call `fzf-file-widget` show content of the selected
# candidate.
export FZF_CTRL_T_OPTS="--preview '(bat --style=numbers --color=always {} || cat {} || tree -NC {}) 2> /dev/null | head -200'"

# Bind `fzf-file-widget` to `C-x C-f`.
bindkey -s '^x^f' '^t'

# When a previous command is too long, it is truncated in
# `fzf-history-wiedget`. The preview window (typeiing ? key) shows the
# whole command.
export FZF_CTRL_R_OPTS="--preview 'echo {}' --preview-window down:3:hidden:wrap --exact"

# When we call `fzf-cd-widget` the tree struction of the current
# candidate is shown in preview.
export FZF_ALT_C_OPTS="--preview '(tree -NC {}) 2> /dev/null | head -200'"

### fzf-tab

# Zstyle syntax for fzf-tab:
#   zstyle ':fzf-tab:<context>' tag value

# Preview directory's content when completing cd, ls, etc.
zstyle ':fzf-tab:complete:(cd|ls|exa|bat|cat|emacs|nano|vi|vim):*' fzf-preview \
       '(exa -1 --color=always $realpath || ls -1AF --color=always $realpath) 2> /dev/null'
# Give a preview of commandline arguments when completing kill.
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
       '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

# Switch group using `,` and `.`.
zstyle ':fzf-tab:*' switch-group ',' '.'

### ghq

# Lists repositories cloned by ghq in order so that the most
# recently accessed is first.
function __ghq_list_recent_first() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local root_dirs
  root_dirs=$(cat <(ghq root) <(git config --path --get-all ghq.root))
  ghq list --full-path | \
    xargs -I{} ls -dl --time-style=+%s {}/.git | \
    sed -E 's/.*([0-9]{10})/\1/' | \
    sort -nr | \
    sed -E "s@.*${root_dirs//$'\n'/|}/@@" | \
    sed -E 's/\/.git$//'
}

# Select a repository from ones managed by ghq and change to it.
function ghq-cd-widget() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local selected dir
  selected=$(__ghq_list_recent_first | fzf --query="$LBUFFER")
  if [[ -z "$selected" ]]; then
    zle redisplay
    return 0
  fi
  dir=$(ghq list --full-path | grep -E "${selected}$")
  zle push-line
  BUFFER="cd ${(q)dir}"
  zle accept-line
  local ret=$?
  unset selected dir
  zle reset-prompt
  return $ret
}
zle -N ghq-cd-widget
bindkey '\eg' ghq-cd-widget

### zoxide

function zoxide-cd-widget() {
  setopt localoptions pipefail no_aliases 2> /dev/null
  local dir=$(zoxide query -l | fzf --query="$LBUFFER")
  if [[ -z "$dir" ]]; then
    zle redisplay
    return 0
  fi
  zle push-line
  BUFFER="cd ${(q)dir}"
  zle accept-line
  local ret=$?
  unset dir
  zle reset-prompt
  return $ret
}
zle -N zoxide-cd-widget
bindkey '^x^r' zoxide-cd-widget

## External configuration hook

if typeset -f arche_after_init_hook > /dev/null; then
  arche_after_init_hook
fi

## Closing remarks

# Local Variables:
# outline-regexp: "##+"
# End:
