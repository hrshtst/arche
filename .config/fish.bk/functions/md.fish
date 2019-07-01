function md --description "make directory and change cwd to it"
    mkdir -p $argv; and cd "$argv[-1]"
end
