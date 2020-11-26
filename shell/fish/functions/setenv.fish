# Set a value to an environment variable and export it.
#
# Example:
#   $ setenv DISPLAY 127.0.0.1:0
function setenv --description 'Set a value to an environment variable and export it'
    switch (count $argv)
        case '0'
            # Show the list of environment variables.
            set -gx
        case '1'
            # Remove the variable.
            set -e $argv[1]
        case '2'
            # All variables that end in `PATH` are treated as a
            # special kind to support colon-delimited path lists. All
            # other variables are treated literally. See details in
            # the doc:
            # https://fishshell.com/docs/current/index.html#path-variables
            if string match -r -q -- 'PATH$' $argv[1]
                set -gx $argv[1] (string split -- ':' $argv[2])
            else
                set -gx $argv[1] $argv[2]
            end
        case '*'
            # Print warning.
            printf 'error: ignored the last %d arguments (setenv)\n' (math (count $argv) - 2) >&2
    end
end
