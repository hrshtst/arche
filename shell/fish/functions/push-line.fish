# Push the current commandline content onto the local variable (buffer
# stack) and clear the buffer. Next time the editor starts up, the
# buffer will be popped off the top of the buffer stack and loaded
# into the editing buffer.
#
# This function is recommended to be bound to a keybinding, for
# example, Ctrl-Q.
# function fish_user_keybindings
#   bind \cq push-line
# end
#
# The implementaion of the function is adopted from the following:
# https://gist.github.com/dedeibel/471526594cdb7f53cbfb354ba22d3ab5
# https://github.com/mattgreen/park.fish/blob/main/conf.d/park.fish
function push-line --description 'Push the current commandline onto the buffer stack and clear the prompt. Next time the prompt starts, the buffer be popped off.'

    # Push the current commandline content and the cursor position
    # onto the buffer stack and clear the buffer.
    set --local cmd (commandline)
    set --local cursor (commandline --cursor)
    commandline --replace ""

    # Define a function to restore the commandline content and the
    # cursor position and remove the function itself immediately after
    # the prompt starts off.
    function __restore --inherit-variable cmd --inherit-variable cursor --on-event fish_prompt
        commandline --replace $cmd
        commandline --cursor $cursor
        functions --erase __restore
    end
end
