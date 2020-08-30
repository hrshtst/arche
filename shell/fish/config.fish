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
## Configuration of environment variables

# Helper function to put one or more paths to the head of a variable
# that holds a list of paths. The specified paths will be added only
# if it is not contained in the variable and it is an existing
# directory. The variable will be set globally and exported to persist
# for child processes.
#
# Example usage:
#
#   prepend_path_to_env LD_LIBRARY_PATH ~/usr/lib
#   prepend_path_to_env PATH ~/usr/bin ~/.local/bin
#
# @param $1 var  The name of environment variable.
# @param $2..N paths  The list of paths to be added.
function prepend_path_to_env

    # Check the number of passed arguments.
    if test (count $argv) -lt 2
        printf (_ "%s: At least two arguments are required.\n") (status filename)
        return 1
    end

    set -l var $argv[1]
    set -l paths $argv[2..-1]

    # Warn if the first argument is a path.
    if string match -q -r '/' $var
        printf (_ "%s: %s should be put after variable name.\n") (status filename) $var
        return 1
    end

    # If the specified variable does not exist, create it.
    set -q $var
    or set -gx $var

    # Iterate over the specified paths in reversed order to keep the
    # passed order.
    for path in $paths[-1..1]
        # Skip if the path is not a directory.
        not test -d "$path"; and continue

        # Add path to the head of the variable if it is not contained.
        # NOTE: set with '-p' option is available since fish version 3.0
        if not contains "$path" $$var
            set -pgx $var "$path"
        end
    end
end

# Motor Intelligence Lab. library.
# https://www.mi.ams.eng.osaka-u.ac.jp/open-e.html
prepend_path_to_env LD_LIBRARY_PATH ~/usr/lib
prepend_path_to_env PATH ~/usr/bin

# Add directories to search for .pc files for pkg-config.
# https://www.freedesktop.org/wiki/Software/pkg-config/
prepend_path_to_env PKG_CONFIG_PATH ~/usr/lib/pkgconfig

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
set -gx PYENV_ROOT ~/.pyenv
prepend_path_to_env PATH $PYENV_ROOT/bin
if command -v pyenv 1>/dev/null 2>&1;
    status is-interactive; and source (pyenv init -|psub)
end

# pipx provides a way to execute binaries from Python packages in
# isolated environments.
# https://github.com/pipxproject/pipx
prepend_path_to_env PATH ~/.local/bin

# Configure paths for Go Programming Language.
# https://golang.org/
# https://github.com/golang/go/wiki/SettingGOPATH
set -gx GOPATH ~/.go
set -gx GOROOT ~/usr/lib/go
prepend_path_to_env PATH $GOPATH/bin $GOROOT/bin

####################################################################
## Aliases

# Define function or use 'abbr' command instead of using builtin
# 'alias' command.
# cf. https://github.com/jorgebucaran/fish-cookbook#aliases

# Prompt before removing more than tree files.
function rm --wraps /bin/rm
    /bin/rm -I --preserve-root $argv
end

# Get current IP address.
function myip
    curl -sS http://ipecho.net/plain
    echo
end

# Colorize dir command.
abbr -a dir 'dir --color=auto'

# Alias to ipython --pylab
abbr -a pylab 'ipython --pylab'

####################################################################
## Appearance

# Use the fish prompt for virtualenv instead of the original one.
set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

# Show symbol (rocket) when Python virtual envinronment is set.
set -g SPACEFISH_VENV_SYMBOL "🚀"

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
prepend_path_to_env PATH ~/.fzf/bin

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
