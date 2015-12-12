#!/bin/sh

set -eux

mkdir_if_not_exist () {
    local dir=$1
    if [ ! -d $dir ]; then
        mkdir -p $dir
    fi
}

ln_if_target_exist () {
    local target=$1
    local link=$2
    if [ -f $target -o -d $target ]; then
        ln -sf $target $link
    else
        echo "$target doesn't exist!"
        return 1
    fi
}

# emacs
mkdir_if_not_exist ~/.emacs.d
ln_if_target_exist $PWD/emacs/el-get-recipes ~/.emacs.d/
ln_if_target_exist $PWD/emacs/init ~/.emacs.d/
ln_if_target_exist $PWD/emacs/init.el ~/.emacs.d/
ln_if_target_exist $PWD/emacs/init-el-get.el ~/.emacs.d/
ln_if_target_exist $PWD/emacs/init-loader ~/.emacs.d/
ln_if_target_exist $PWD/emacs/snippets ~/.emacs.d/
ln_if_target_exist $PWD/emacs/themes ~/.emacs.d/

# keyboard
ln_if_target_exist $PWD/keyboard/Xmodmap_hhk ~/.Xmodmap

# bash
ln_if_target_exist $PWD/shell/bashrc ~/.bashrc
ln_if_target_exist $PWD/shell/bash_aliases ~/.bash_aliases
ln_if_target_exist $PWD/shell/bash_completion_make.sh ~/.bash_completion_make.sh

# tmux
ln_if_target_exist $PWD/shell/tmux.conf ~/.tmux.conf
ln_if_target_exist ~/src/tmux/examples/bash_completion_tmux.sh ~/.bash_completion_tmux.sh

# git
ln_if_target_exist $PWD/git/gitconfig ~/.gitconfig
ln_if_target_exist ~/src/git/contrib/completion/git-completion.bash ~/.git-completion.bash
ln_if_target_exist ~/src/git/contrib/completion/git-prompt.sh ~/.git-prompt.sh

# ssh
mkdir_if_not_exist ~/.ssh
ln_if_target_exist $PWD/ssh/config ~/.ssh/

# vim
mkdir_if_not_exist ~/.vim
ln_if_target_exist $PWD/vim/_vimrc ~/.vim
ln_if_target_exist $PWD/vim/_gvimrc ~/.vim
ln_if_target_exist $PWD/vim/vimfiles ~/.vim


echo ""
echo "Successfully executed."
