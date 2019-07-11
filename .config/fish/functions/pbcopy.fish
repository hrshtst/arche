function pbcopy -d "Copy strings to system clipboard"
    set -l line
    if count $argv >/dev/null
        set line $argv
    else
        read -al arr
        set line $arr
    end
    echo $line | tr -d '\n' | xsel --input --clipboard
end
