#
# Configuration file for fish shell
#

# laod aliases
load-script $HOME/.config/fish/aliases.fish

# load envirionemt variables
load-script $HOME/.config/fish/env.fish

# theme
set -g theme_color_scheme dark
set -g theme_display_virtualenv yes
set -g theme_nerd_fonts yes
set -g theme_newline_cursor yes

# install fisherman
if not test -f ~/.config/fish/functions/fisher.fish
  echo "Installing fisherman for the first time"
  curl -sLo ~/.config/fish/functions/fisher.fish --create-dirs git.io/fisher
  fisher
end

# http://news.ycombinator.com/item?id=4492682
function fish_user_key_bindings
        bind \cx\cl peco_select_ghq_repository
        bind \cr peco_select_history
        bind \cx\ck peco_kill
        bind \cx\cr peco_recentd
end

# autostart tmux
if command -v tmux >/dev/null
    test $TERM != "screen"; and test -z "$TMUX"; and exec tmux
end
