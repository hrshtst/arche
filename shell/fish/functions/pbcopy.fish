# Copy data from STDIN to the clipboard. It acts like the command pbcopy
# in OS X, but behaves in a slightly different way.
#
# The main functionality is to copy text provided from STDIN to the
# system clipboard.
#   - If the provided text is only one line and it ends with a newline,
#     remove the newline.
#   - If the provided argument is a filename, copy the contents of the
#     file.
#   - If no arguments are provided, wait until any text is input.
#
# Example:
#   $ ls ~ | pbcopy
#   $ cat file.txt | pbcopy
#   $ pbcopy < file.txt
#   $ pbcopy file.txt
function pbcopy --description 'Copy data from STDIN to the clipboard'

    if not type -q xsel
        printf (_ '%s: xsel is required to work\n') pbcopy >&2
        return 1
    end

    cat $argv 2>/dev/null | while read -l line
        set --append lines $line
    end

    test -z '$lines'; and return

    if test 1 -eq (count $lines)
        printf '%s' $lines | tr -d '\n'
    else
        printf '%s\n' $lines
    end | xsel --clipboard --input
end
