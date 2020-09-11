# Paste data from the clipboard to STDOUT. It acts like the command
# pbpaste in OS X.
#
# Example:
#   $ pbpaste > ~/foo.txt
#   $ pbpaste >> ~/foo.txt
function pbpaste --description 'Paste data from the clipboard to STDOUT'

    if not type -q xsel
        printf (_ '%s: xsel is required to work\n') pbpaste >&2
        return 1
    end

    xsel --clipboard --input
end
