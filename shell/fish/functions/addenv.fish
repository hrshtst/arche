# Add a value to the head of a variable.
# Usage:
#   addenv PATH $HOME/usr/bin
function addenv --description 'Add a value to the head of a varialbe'
    set -l options (fish_opt -s f -l force)
    argparse $options -- $argv
    or return 1

    if test (count $argv) -lt 2
        echo "error: too few arguments (addenv)\n" >&2
        return 1
    end

    set -l var $argv[1]
    set -l val $argv[2..-1]

    # When the given variable name does not exist create it.
    set -q $var; or set -gx $var

    # All variables that end in `PATH` are treated as a special kind
    # to support colon-delimited path lists. All other variables are
    # treated literally. See details in the doc:
    # https://fishshell.com/docs/current/index.html#path-variables
    if string match -r -q -- 'PATH$' $var
        for i in $val[-1..1]
            for j in (string split -- ':' $i)[-1..1]
                if not contains -- "$j" $$var
                    set --prepend -gx $var "$j"
                end
            end
        end
    else
        for i in $val[-1..1]
            set -gx $var "$i$$var"
        end
    end

    # for i in (string split -- ':' $val)[-1..1]
    #         if not contains -- "$i" $$var
    #             set --prepend -gx $var "$i"
    #         end
    #     end
    # else
    #     set -gx $var "$val$$var"
    # end

end
