# set language
set -gx LC_ALL en_US.UTF-8
set -gx LC_CTYPE en_US.UTF-8

# set environment values
prepend-to-path $HOME/.local/bin
prepend-to-path $HOME/usr/bin
set -q LD_LIBRARY_PATH; or set -gx LD_LIBRARY_PATH /usr/local/lib
set -gx LD_LIBRARY_PATH $HOME/usr/lib:$LD_LIBRARY_PATH

# python
set -gx VIRTUAL_ENV_DISABLE_PROMPT 1

# pyenv - python version management
set -gx PYENV_ROOT $HOME/.pyenv
prepend-to-path $PYENV_ROOT/bin $PATH
if command -v pyenv 1>/dev/null 2>&1;
    status --is-interactive; and . (pyenv init -|psub)
end

# pipsi - python scripting manager
switch (hostname)
case 'rmc-lx*'
    set -gx PIPSI_HOME /home_local/atsu_hi/pipsi
    set -gx WORKON_HOME /home_local/atsu_hi/virtualenvs
end

# Go
set -gx GOPATH $HOME/.go:$HOME/develop/go
prepend-to-path $HOME/.go/bin
prepend-to-path /usr/lib/go-1.10/bin
prepend-to-path /usr/lib/go-1.11/bin

# GNU global
set -gx GTAGSLIBPATH $HOME/usr/src

# pkg-config
set -gx PKG_CONFIG_PATH $HOME/usr/lib/pkgconfig
