function __fzf_docker_container_ls -d "Lists all containers and outputs selected container ID"
    type -q docker >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    # Fish shell version >= 2.7 is required to use argparse.
    set -l options "o/stdout"
    argparse $options -- $argv

    set -q FZF_DOCKER_CONTAINER_LS_COMMAND
    or set -l FZF_DOCKER_CONTAINER_LS_COMMAND "docker container ls --all"

    set -q FZF_DOCKER_CONTAINER_LS_OPTS
    or set -l FZF_DOCKER_CONTAINER_LS_OPTS "--ansi --header-lines=1 --nth=1..3,7,-2,-1"

    set -q FZF_DOCKER_CONTAINER_LS_PREVIEW
    or set -l FZF_DOCKER_CONTAINER_LS_PREVIEW "docker container logs --tail 50 {1}"

    begin
        eval "$FZF_DOCKER_CONTAINER_LS_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_DOCKER_CONTAINER_LS_OPTS --query=\"$fzf_query\" --preview='$FZF_DOCKER_CONTAINER_LS_PREVIEW' --preview-window=hidden | awk '{ print \$1 }'" | while read -l s; set results $results $s; end
    end

    if set -q _flag_stdout
        echo (string join ' ' $results)
        return
    end

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
