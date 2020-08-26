function __fzf_git_remote -d "Lists existing remote repositories whose branches you track"
    git rev-parse --git-dir >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_GIT_REMOTE_COMMAND
    or set -l FZF_GIT_REMOTE_COMMAND "git remote -v | awk '{print \$1 \"\t\" \$2}' | uniq"

    set -q FZF_GIT_REMOTE_OPTS
    or set -l FZF_GIT_REMOTE_OPTS "--tac"

    set -q FZF_GIT_REMOTE_PREVIEW_SHELL
    or set -l FZF_GIT_REMOTE_PREVIEW_SHELL (which bash)

    set -q FZF_GIT_REMOTE_PREVIEW
    or set -l FZF_GIT_REMOTE_PREVIEW "git log --oneline --graph --date=short --pretty=\'format:%C(auto)%cd %h%d %s\' {1} -- | head -n 200"

    set -l cur_shell $SHELL
    set -x SHELL $FZF_GIT_REMOTE_PREVIEW_SHELL
    eval "$FZF_GIT_REMOTE_COMMAND | "(__fzfcmd) "+m $FZF_DEFAULT_OPTS $FZF_GIT_REMOTE_OPTS --query \"$fzf_query\" --preview '$FZF_GIT_REMOTE_PREVIEW' | cut -f1" | read -l select
    set -x SHELL $cur_shell

    if not test -z "$select"
        commandline -it -- (string escape $select)
    end

    commandline -f repaint
end
