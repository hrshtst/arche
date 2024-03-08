#!/usr/bin/env zsh

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
