function abspath -d "Returns absolute path to the given file or directory"
    set -l cwd (pwd)
    set -l filename ''
    set -l dir $argv[1]
    if test -f $argv[1]
        set filename (echo '/'(basename $argv[1]))
        set dir (dirname $argv[1])
    end
    cd $dir; and set dir (pwd); and cd $cwd
    echo $dir$filename
end
