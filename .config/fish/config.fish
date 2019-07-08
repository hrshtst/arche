#
# Initialization file for fish shell.
#

####################################################################
## Configuration of environment variables

# Helper function to prepend a path to a list of paths. The path is
# not a directory, nothing is added to the list. The list of paths
# will be set globally and exported.
#
# Usage:
#   __prepend_path ~/usr/lib LD_LIBRARY_PATH
function __prepend_path

    # Check the number of passed arguments.
    if test (count $argv) -ne 2
        echo "Usage: __prepend_path path PATH" 1>&2
        return 1
    end

    set -l path "$argv[1]"
    set -l list $argv[2]

    # If the specified path is not a directory, return without an
    # error.
    if not test -d "$path"
        return 0
    end

    # If a variable named as the list does not exist, create that
    # variable.
    set -q $list
    or set -gx $list

    # If the specified path is not contained in the list, prepend it
    # to the list.
    # NOTE: set with '-p' option is available since fish version 3.0
    if not contains "$path" $$list
        set -pgx $list "$path"
    end
end

# Motor Intelligence Lab. library.
# https://www.mi.ams.eng.osaka-u.ac.jp/open-e.html
__prepend_path ~/usr/lib LD_LIBRARY_PATH
__prepend_path ~/usr/bin PATH

# Add directories to search for .pc files for pkg-config.
# https://www.freedesktop.org/wiki/Software/pkg-config/
__prepend_path ~/usr/lib/pkgconfig PKG_CONFIG_PATH

# pyenv is a simple python version management.
# https://github.com/pyenv/pyenv
set -gx PYENV_ROOT ~/.pyenv
__prepend_path $PYENV_ROOT/bin PATH
if command -v pyenv 1>/dev/null 2>&1;
    status is-interactive; and source (pyenv init -|psub)
end
