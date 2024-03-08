#!/usr/bin/env zsh

### fzf-tab

# Zstyle syntax for fzf-tab:
#   zstyle ':fzf-tab:<context>' tag value

# Preview directory's content when completing cd, ls, etc.
zstyle ':fzf-tab:complete:(cd|ls|exa|eza|bat|cat|emacs|nano|vi|vim):*' fzf-preview \
       '(eza -1 --color=always $realpath || ls -1AF --color=always $realpath) 2> /dev/null'
# Give a preview of commandline arguments when completing kill.
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
       '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

# Switch group using `,` and `.`.
zstyle ':fzf-tab:*' switch-group ',' '.'

# Prevent fzf-tab from modifying user's input query.
zstyle ':fzf-tab:*' query-string input first

# Somehow this is required when showing small complemention menu.
zstyle ':fzf-tab:*' fzf-pad 3
