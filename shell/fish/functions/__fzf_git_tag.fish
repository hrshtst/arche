function __fzf_git_tag -d "Lists tags existing in Git repository"
    git rev-parse --git-dir >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_GIT_TAG_COMMAND
    or set -l FZF_GIT_TAG_COMMAND "git tag --sort -version:refname"

    set -q FZF_GIT_TAG_OPTS
    or set -l FZF_GIT_TAG_OPTS "--ansi"

    set -q FZF_GIT_TAG_PREVIEW_SHELL
    or set -l FZF_GIT_TAG_PREVIEW_SHELL (which bash)

    set -q FZF_GIT_TAG_PREVIEW
    or set -l FZF_GIT_TAG_PREVIEW "git show --color=always {} | head -n $LINES"

    set -l cur_shell $SHELL
    set -x SHELL $FZF_GIT_TAG_PREVIEW_SHELL
    begin
        eval "$FZF_GIT_TAG_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_GIT_TAG_OPTS --query \"$fzf_query\" --preview-window right:70% --preview '$FZF_GIT_TAG_PREVIEW'" | while read -l s; set results $results $s; end
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
