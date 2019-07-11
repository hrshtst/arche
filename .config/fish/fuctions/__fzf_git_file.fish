function __fzf_git_file -d "Lists modified or untracked files in Git repository"
    git rev-parse --git-dir >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_GIT_FILE_COMMAND
    or set -l FZF_GIT_FILE_COMMAND "git -c color.status=always status --short -- \$dir"

    set -q FZF_GIT_FILE_OPTS
    or set -l FZF_GIT_FILE_OPTS "--ansi --nth 2..,.."

    set -q FZF_PREVIEW_PAGER
    or set -l FZF_PREVIEW_PAGER "cat"

    set -q FZF_PREVIEW_DIFF
    or set -l FZF_PREVIEW_DIFF "git diff --color=always"

    set -q FZF_GIT_FILE_PREVIEW_SHELL
    or set -l FZF_GIT_FILE_PREVIEW_SHELL (which fish)

    set -q FZF_GIT_FILE_PREVIEW
    or set -l FZF_GIT_FILE_PREVIEW "\
begin
    if string match -rq \'^[MAD]\' {}
        printf \'\\e[1m\\e[44m==== Staged diffs ====\\e[0m\\n\'
        $FZF_PREVIEW_DIFF --cached -- {-1} | sed 1,4d
    else if string match -rq \'^[RC]\' {}
        printf \'\\e[1m\\e[44m==== Staged file ====\\e[0m\\n\'
        $FZF_PREVIEW_PAGER {-1}
    end
    if string match -rq \'^.[MAD]\' {}
        printf \'\\e[1m\\e[41m==== Unstaged diffs ====\\e[0m\\n\'
        $FZF_PREVIEW_DIFF -- {-1} | sed 1,4d
    else if string match -rq \'^.[RC]\' {}
        printf \'\\e[1m\\e[41m==== Untaged file ====\\e[0m\\n\'
        $FZF_PREVIEW_PAGER {-1}
    end
    if string match -rq \'^\?\?\' {}
        if test -d {-1}
            printf \'\\e[1m\\e[46m==== Untracked directory ====\\e[0m\\n\'
            ls -la --color=always {-1}
        else
            printf \'\\e[1m\\e[46m==== Untracked file ====\\e[0m\\n\'
            $FZF_PREVIEW_PAGER {-1}
        end
    end
end | head -n 500"

    set -l cur_shell $SHELL
    set -x SHELL $FZF_GIT_FILE_PREVIEW_SHELL
    begin
        eval "$FZF_GIT_FILE_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_GIT_FILE_OPTS --query \"$fzf_query\" --preview '$FZF_GIT_FILE_PREVIEW' | cut -c4- | sed 's/.* -> //'" | while read -l s; set results $results $s; end
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
