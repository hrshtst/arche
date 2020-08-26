# This function creates a new session of tmux if no arguments are
# passed. The new session starts with an interactive fish shell and
# the deafult shell will be set to fish as well. If one or more
# arguments or options are provided, pass them to tmux command.
function tmux -d "Starts tmux new session or pass arguments to tmux"
    set -q TMUX_SHELL
    or set -l TMUX_SHELL (which fish)
    if test (count $argv) = 0
        command tmux new-session $TMUX_SHELL \; set-option default-shell $TMUX_SHELL
    else
        command tmux $argv
    end
end
