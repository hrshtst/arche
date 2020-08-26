function __fzf_z -d "Lists most used directories and goes into one"
    type -q z; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_Z_COMMAND
    or set -l FZF_Z_COMMAND "z --list | cut -c 12-"

    set -q FZF_Z_OPTS
    or set -l FZF_Z_OPTS "--ansi"

    set -q FZF_Z_PREVIEW
    or set -l FZF_Z_PREVIEW "ls -AFlh --color=always {}"

    eval "$FZF_Z_COMMAND | "(__fzfcmd) "+m $FZF_DEFAULT_OPTS $FZF_Z_OPTS --query \"$fzf_query\" --preview-window right:60% --preview '$FZF_Z_PREVIEW'" | read -l select

    if not test -z "$select"
        cd "$select"
        commandline -t ""
    end

    commandline -f repaint
end
