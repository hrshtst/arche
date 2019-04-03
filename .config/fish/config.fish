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
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
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
