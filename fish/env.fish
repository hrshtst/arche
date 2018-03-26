# set language
set -x LC_ALL en_US.UTF-8
set -x LC_CTYPE en_US.UTF-8

# set environment values
prepend-to-path $HOME/.local/bin
prepend-to-path $HOME/usr/bin
set -x LD_LIBRARY_PATH $HOME/usr/lib:$LD_LIBRARY_PATH

# python
set -x VIRTUAL_ENV_DISABLE_PROMPT 1

# pyenv - python version management
set -x PYENV_ROOT $HOME/.pyenv
prepend-to-path $PYENV_ROOT/bin $PATH
if command -v pyenv 1>/dev/null 2>&1;
    status --is-interactive; and . (pyenv init -|psub)
end

# pipsi - python scripting manager
switch (hostname)
case 'rmc-lx*'
    set -x PIPSI_HOME /home_local/atsu_hi/pipsi
    set -x WORKON_HOME /home_local/atsu_hi/virtualenvs
end

# Go
set -x GOPATH $HOME/.go:$HOME/develop/go
prepend-to-path $HOME/.go/bin
prepend-to-path /usr/lib/go-1.10/bin

# GNU global
set -x GTAGSLIBPATH $HOME/usr/src

