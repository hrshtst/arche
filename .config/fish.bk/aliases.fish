#
# Simple aliases for fish shell
#
# Note that too many definitions with alias will slow down the shell start
# and cannot take advantage of lazy-loading mecanism. Thus creating a function
# and saving it to $HOME/.config/fish/functions is preferred.
# Please see the following link:
# https://github.com/jorgebucaran/fish-shell-cookbook#whats-wrong-with-aliases

# basic commands
alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -CF'
alias dir='dir --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias df='df -h'
alias g='git'
alias pylab='ipython --pylab'

# useful aliases
# alias open='nautilus 1>/dev/null 2>&1'
alias today='date +%Y%m%d'
alias pbcopy="tr -d '\n' | xsel --clipboard --input"
alias pbpaste="xsel --clipboard --output"
alias myip="curl -s checkip.dyndns.org | command grep -Eo '[0-9\.]+'"
