function prepend-to-path --description 'Prepend the given directory to PATH variable'
    if test (count $argv) -eq 0
	return
    end

    set -l dir $argv[1]
    if test -d $dir
	set dir (abspath $dir)
	for i in (seq (count $PATH) -1 1)
	    if test $PATH[$i] = $dir
		set -e PATH[$i]
	    end
	end
	set -x PATH $dir $PATH
    end
end
