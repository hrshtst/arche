#
# Configuration file for fish shell
# https://fishshell.com/
#

####################################################################
## Autostart tmux

set -gx TMUX_SHELL (which fish)
if status is-interactive; and not set -q TMUX
    exec tmux new-session $TMUX_SHELL \; set-option default-shell $TMUX_SHELL
end

####################################################################
## Environment variables

# Helper function used in `setenv` and `addenv`
# The first argument is the caller function name, i.e. `setenv` or
# `addenv`. The second is the number of arguments provided for the
# caller function. The third is the variable name to create if it is not
# exist.
function __helper_setenv --description 'Helper function for setenv.'

    set -l func $argv[1]
    set -l n_arg $argv[2]

    # No arguments should raise an error.
    if test $n_arg -eq 0
        printf (_ '%s: No arguments provided\n') $func >&2
        return
    end

    # `setenv` and `addenv` accepts two arguments: the var name and the
    # value. If there are more than two arguments it is an error.
    if test $n_arg -ge 3
        printf (_ '%s: Too many arguments\n') $func >&2
        return 1
    end

    if not set -q argv[3]
        return
    end
    set -l var $argv[3]

    # Validate the variable name.
    if not string match -qr '^\w+$' -- $var
        printf (_ '%s: Variable name must contain alphanumeric characters\n') $func >&2
        return 1
    end
end

# Set an environment variable to a provided value.
function setenv --description 'Set an env var to a provided value.'

    __helper_setenv setenv (count $argv) $argv[1]
    if test $status -ne 0
        return 1
    end

    # A single argument should create the named var with an empty
    # string, the second provided set the var to it.
    set -l var $argv[1]
    set -l val ''
    set -q argv[2]; and set -l val $argv[2]

    # All variables that end in `PATH` are treated as a special kind to
    # support colon-delimited path lists. All other variables are
    # treated literally. See details in the doc:
    # https://fishshell.com/docs/current/index.html#path-variables
    if string match -r -q -- 'PATH$' $var
        set -gx $var (string split -- ':' $val)
    else
        set -gx $var $val
    end
end

# Add a value to the head of an environment variable.
function addenv --description 'Add a value to the head of an env var.'

    __helper_setenv addenv (count $argv) $argv[1]
    if test $status -ne 0
        return 1
    end

    # A single argument should create the named var with an empty
    # string, the second provided set the var to it.
    set -l var $argv[1]
    set -l val ''
    set -q argv[2]; and set -l val $argv[2]

    # If the named variabled does not exist create it.
    set -q $var; or set -gx $var

    # All variables that end in `PATH` are treated as a special kind to
    # support colon-delimited path lists. All other variables are
    # treated literally. See details in the doc:
    # https://fishshell.com/docs/current/index.html#path-variables
    if string match -r -q -- 'PATH$' $var
        for i in (string split -- ':' $val)[-1..1]
            if not contains -- "$i" $$var
                set --prepend -gx $var "$i"
            end
        end
    else
        set -gx $var "$val$$var"
    end
end

# Set paths for my own library
addenv PATH "$HOME/usr/bin"
addenv LD_LIBRARY_PATH "$HOME/usr/lib"

# Add directories to search for .pc files for pkg-config.
# https://www.freedesktop.org/wiki/Software/pkg-config/
addenv PKG_CONFIG_PATH "$HOME/usr/lib/pkgconfig"

# pipx provides a way to execute binaries from Python packages in
# isolated environments.
# https://github.com/pipxproject/pipx
addenv PATH "$HOME/.local/bin"

# Configure paths for Go Programming Language.
# https://golang.org/
# https://github.com/golang/go/wiki/SettingGOPATH
setenv GOPATH "$HOME/.go"
setenv GOROOT "$HOME/usr/lib/go"
addenv PATH "$GOROOT/bin"
addenv PATH "$GOPATH/bin"

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
setenv PYENV_ROOT "$HOME/.pyenv"
addenv PATH "$PYENV_ROOT/bin"
if command -v pyenv 1>/dev/null 2>&1;
    status is-interactive; and source (pyenv init -|psub)
end

####################################################################
## Appearance

# Enable starship.
# https://github.com/starship/starship
if type -q starship
    starship init fish | source
end

# Use the fish prompt for virtualenv instead of the original one.
set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

####################################################################
## Colors

# Enable color support of ls and also add handy aliases.
if test -x /usr/bin/dircolors
    if test -r $HOME/.dircolors
        eval "(dircolors -b $HOME/.dircolors)"
        or eval "(dircolors -b)"
    end
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
end

# Colored GCC warnings and errors
setenv GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

####################################################################
## Aliases

# Run pylab.
function pylab --wraps ipython --description 'alias pylab=ipython --pylab'
    if not type -q ipython
        printf (_ '%s: unable to find ipython\n') pylab >&2
        return 1
    end
    ipython --pylab $argv
end

# Prompt before removing more than tree files.
function rm --wraps /bin/rm
    /bin/rm -I --preserve-root $argv
end

# Get current IP address.
function myip
    curl -sS http://ipecho.net/plain
    echo
end

# Some more useful aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias dot='ls .[a-zA-Z0-9_]*'
alias j='jobs -l'

####################################################################
## Package manager

# Fisher is a package manager for the fish shell.
# https://github.com/jorgebucaran/fisher
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

####################################################################
## fzf -- a command-line fuzzy finder
## https://github.com/junegunn/fzf

# Add path.
addenv PATH ~/.fzf/bin

# Use ripgrep as default command if available.
if type -q rg
    set -gx FZF_DEFAULT_COMMAND 'rg --files --hidden --glob "!.git"'
end

# Use bat for fzf preview if available.
if type -q bat
    set -gx FZF_PREVIEW_PAGER 'bat --color=always --style=plain'
end

# Show list in top-down, and make line at border, etc.
set -gx FZF_DEFAULT_OPTS '--height 40% --reverse --border --inline-info --bind=ctrl-j:preview-down,ctrl-k:preview-up,\?:toggle-preview,ctrl-space:toggle+down'

####################################################################
## Keybindings

# Remove all the keybindings defined by fzf
bind -e \ct
bind -e \cr
bind -e \ec
bind -e \eC
bind -e \cg
bind -e \co
bind -e -M insert \ct
bind -e -M insert \cr
bind -e -M insert \ec
bind -e -M insert \eC
bind -e -M insert \cg
bind -e -M insert \co

# Complete command history.
bind \cr '__fzf_reverse_isearch'
bind -M insert \cr '__fzf_reverse_isearch'

# Complete file name.
bind \et '__fzf_find_file'
bind -M insert \et '__fzf_find_file'

# Go to directory.
bind \ec '__fzf_cd'
bind -M insert \ec '__fzf_cd'
bind \eC '__fzf_cd --hidden'
bind -M insert \eC '__fzf_cd --hidden'

# Go to most frequent directory.
bind \cx\cr '__fzf_z'
bind -M insert \cx\cr '__fzf_z'

# Open file with xdg-open.
bind \co '__fzf_open'
bind -M insert \co '__fzf_open'

# Open file with editor.
bind \cx\cf '__fzf_open --editor'
bind -M insert \cx\cf '__fzf_open --editor'

# Select modified files in Git repository.
bind \eg\ef '__fzf_git_file'
bind -M insert \eg\ef '__fzf_git_file'

# Select branches existing in Git repository.
bind \eg\eb '__fzf_git_branch'
bind -M insert \eg\eb '__fzf_git_branch'

# Select tags existing in Git repository.
bind \eg\et '__fzf_git_tag'
bind -M insert \eg\et '__fzf_git_tag'

# Select commit hashes in commit logs.
bind \eg\eh '__fzf_git_hash'
bind -M insert \eg\eh '__fzf_git_hash'

# Select remote repository we track.
bind \eg\er '__fzf_git_remote'
bind -M insert \eg\er '__fzf_git_remote'

# Select repository cloned with ghq and go to it.
bind \eg\eq '__fzf_ghq'
bind -M insert \eg\eq '__fzf_ghq'

# Select docker container ID.
bind \ed\ec '__fzf_docker_ps'
bind -M insert \ed\ec '__fzf_docker_ps'

# Select docker image ID.
bind \ed\ei '__fzf_docker_images'
bind -M insert \ed\ei '__fzf_docker_images'
