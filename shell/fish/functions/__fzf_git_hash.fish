function __fzf_git_hash -d "Lists commit logs and obtains their commit hashes"
    git rev-parse --git-dir >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_GIT_HASH_COMMAND
    or set -l FZF_GIT_HASH_COMMAND "git log --date=short --format='%C(green)%C(bold)%cd %C(auto)%h%d %s (%an)' --graph --color=always"

    set -q FZF_GIT_HASH_OPTS
    or set -l FZF_GIT_HASH_OPTS "--ansi --no-sort --reverse"

    set -q FZF_GIT_HASH_PREVIEW_SHELL
    or set -l FZF_GIT_HASH_PREVIEW_SHELL (which bash)

    set -q FZF_GIT_HASH_PREVIEW
    or set -l FZF_GIT_HASH_PREVIEW "grep -o \"[a-f0-9]\{7,\}\" <<< {} | xargs git show --color=always | head -n $LINES"

    set -l cur_shell $SHELL
    set -x SHELL $FZF_GIT_HASH_PREVIEW_SHELL
    begin
        eval "$FZF_GIT_HASH_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_GIT_HASH_OPTS --query \"$fzf_query\" --bind 'ctrl-s:toggle-sort' --header 'Press CTRL-S to toggle sort' --preview '$FZF_GIT_HASH_PREVIEW' | grep -o \"[a-f0-9]\{7,\}\"" | while read -l s; set results $results $s; end
    end
    set -x SHELL $cur_shell

    if test -z "$results"
        commandline -f repaint
        return
    else
        commandline -t ""
    end

    for result in $results
        commandline -it -- (string escape $result)
        commandline -it -- " "
    end
    commandline -f repaint
end
