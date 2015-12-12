#!/bin/sh

set -eux

mkdir_if_not_exist () {
    local dir=$1
    if [ ! -d $dir ]; then
        mkdir -p $dir
    fi
}

# emacs
mkdir_if_not_exist ~/.emacs.d
ln -sf $PWD/emacs/el-get-recipes ~/.emacs.d/
ln -sf $PWD/emacs/init ~/.emacs.d/
ln -sf $PWD/emacs/init.el ~/.emacs.d/
ln -sf $PWD/emacs/init-el-get.el ~/.emacs.d/
ln -sf $PWD/emacs/init-loader ~/.emacs.d/
ln -sf $PWD/emacs/snippets ~/.emacs.d/
ln -sf $PWD/emacs/themes ~/.emacs.d/

# keyboard
ln -sf $PWD/keyboard/Xmodmap_hhk ~/.Xmodmap

# tmux
ln -sf $PWD/shell/tmux.conf ~/.tmux.conf
