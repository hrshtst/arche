#
# Configuration file for fish shell
# https://fishshell.com/
#

####################################################################
## Autostart tmux

set -gx TMUX_DEFAULT_SHELL (which fish)
if status is-interactive; and not set -q TMUX
    exec tmux new-session $TMUX_DEFAULT_SHELL \; set-option default-shell $TMUX_DEFAULT_SHELL
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

# Create a new directory and change to it.
function take
    if test (count $argv) -eq 0
        return 1
    end
    mkdir -p $argv
    cd $argv[1]
    pwd
end

# Open file with the preferred application.
if type -q wslview
    alias open='wslview'
else
    alias open='xdg-open &>/dev/null'
end

# Some more useful aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias dot='ls .[a-zA-Z0-9_]*'
alias j='jobs -l'

####################################################################
## Applications

# pipx provides a way to execute binaries from Python packages in
# isolated environments.
# https://github.com/pipxproject/pipx
# Autocompletion function for pipx is provided along with argcomplete,
# which is a dependency of pipx.
if command -v register-python-argcomplete 1>/dev/null 2>&1;
    register-python-argcomplete --shell fish pipx | source
end

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
if command -v pyenv 1>/dev/null 2>&1;
    status is-interactive; and source (pyenv init -|psub)
end

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

# Show list in top-down, and make line at border, etc.
set -gx FZF_DEFAULT_OPTS '--height 40% --reverse --border --inline-info --bind=ctrl-j:preview-down,ctrl-k:preview-up,\?:toggle-preview,ctrl-space:toggle+down'

####################################################################
## Keybindings

# Remove key bindings defined by fzf.fish
_fzf_uninstall_bindings

# Add my key bindings for fzf.fish
fzf_configure_bindings \
    --directory=\cx\cf \
    --git_log=\cxgl \
    --git_status=\cxgs \
    --history=\cr \
    --variables=\chv

bind \cxdc  '_fzf_search_docker_container'
bind \cxdi  '_fzf_search_docker_image'
bind \cx\[  '_fzf_search_ghq'
bind \cxgb  '_fzf_search_git_branch'
bind \cxgt  '_fzf_search_git_tag'
bind \cxp   '_fzf_search_process'
bind \cx\cr '_fzf_search_z'

# Bind the function push-line to Ctrl-Q.
bind \cq 'push-line'
