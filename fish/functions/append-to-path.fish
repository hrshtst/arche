function append-to-path --description 'Append the given directory to PATH variable'
    if test (count $argv) -eq 0
	return
    end

    set -l dir $argv[1]
    if test -d $dir
	set dir (abspath $dir)
	if not contains $dir $PATH
	    set -x PATH $PATH $dir
	end
    end
end
