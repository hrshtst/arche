function __fzf_git_branch -d "Lists local and remote branches existing in Git repository"
    git rev-parse --git-dir >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_GIT_BRANCH_COMMAND
    or set -l FZF_GIT_BRANCH_COMMAND "git branch -a --color=always | grep -v '/HEAD\s' | sort"

    set -q FZF_GIT_BRANCH_OPTS
    or set -l FZF_GIT_BRANCH_OPTS "--ansi --tac"

    set -q FZF_GIT_BRANCH_PREVIEW_SHELL
    or set -l FZF_GIT_BRANCH_PREVIEW_SHELL (which bash)

    set -q FZF_GIT_BRANCH_PREVIEW
    or set -l FZF_GIT_BRANCH_PREVIEW "git log --oneline --graph --date=short --color=always --pretty=\'format:%C(auto)%cd %h%d %s\' \$(echo {} | sed s/^..// | cut -d\' \' -f1) -- | head -n $LINES"

    set -l cur_shell $SHELL
    set -x SHELL $FZF_GIT_BRANCH_PREVIEW_SHELL
    begin
        eval "$FZF_GIT_BRANCH_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_GIT_BRANCH_OPTS --query \"$fzf_query\" --preview-window right:70%  --preview '$FZF_GIT_BRANCH_PREVIEW' | sed 's/^..//' | cut -d' ' -f1 | sed 's#^remotes/##'" | while read -l s; set results $results $s; end
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
