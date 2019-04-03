function load-script -d "Load fish script if it exists"
    if test (count $argv) -eq 0
        return
    end

    set -l script $argv[1]
    if test -f $script
        . $script
    end
end
