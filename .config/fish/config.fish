#
# Initialization file for fish shell.
#

####################################################################
## Configuration of environment variables

# Helper function to put one or more paths to the head of a variable
# that holds a list of paths. The specified paths will be added only
# if it is not contained in the variable and it is an existing
# directory. The variable will be set globally and exported to persist
# for child processes.
#
# Example usage:
#
#   prepend_path_to_env LD_LIBRARY_PATH ~/usr/lib
#   prepend_path_to_env PATH ~/usr/bin ~/.local/bin
#
# @param $1 var  The name of environment variable.
# @param $2..N paths  The list of paths to be added.
function prepend_path_to_env

    # Check the number of passed arguments.
    if test (count $argv) -lt 2
        printf (_ "%s: At least two arguments are required.\n") (status filename)
        return 1
    end

    set -l var $argv[1]
    set -l paths $argv[2..-1]

    # Warn if the first argument is a path.
    if string match -q -r '/' $var
        printf (_ "%s: %s should be put after variable name.\n") (status filename) $var
        return 1
    end

    # If the specified variable does not exist, create it.
    set -q $var
    or set -gx $var

    # Iterate over the specified paths in reversed order to keep the
    # passed order.
    for path in $paths[-1..1]
        # Skip if the path is not a directory.
        not test -d "$path"; and continue

        # Add path to the head of the variable if it is not contained.
        # NOTE: set with '-p' option is available since fish version 3.0
        if not contains "$path" $$var
            set -pgx $var "$path"
        end
    end
end

# Motor Intelligence Lab. library.
# https://www.mi.ams.eng.osaka-u.ac.jp/open-e.html
prepend_path_to_env LD_LIBRARY_PATH ~/usr/lib
prepend_path_to_env PATH ~/usr/bin

# Add directories to search for .pc files for pkg-config.
# https://www.freedesktop.org/wiki/Software/pkg-config/
prepend_path_to_env PKG_CONFIG_PATH ~/usr/lib/pkgconfig

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
set -gx PYENV_ROOT ~/.pyenv
prepend_path_to_env PATH $PYENV_ROOT/bin
if command -v pyenv 1>/dev/null 2>&1;
    status is-interactive; and source (pyenv init -|psub)
end

# pipx provides a way to execute binaries from Python packages in
# isolated environments.
# https://github.com/pipxproject/pipx
prepend_path_to_env PATH ~/.local/bin

# Configure paths for Go Programming Language.
# https://golang.org/
# https://github.com/golang/go/wiki/SettingGOPATH
set -gx GOPATH ~/.go
set -gx GOROOT ~/usr/lib/go
prepend_path_to_env PATH $GOPATH/bin $GOROOT/bin
