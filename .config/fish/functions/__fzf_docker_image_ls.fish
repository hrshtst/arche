function __fzf_docker_image_ls -d "Lists all images and outputs selected image ID"
    type -q docker >/dev/null 2>&1; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_DOCKER_IMAGE_LS_COMMAND
    or set -l FZF_DOCKER_IMAGE_LS_COMMAND "docker image ls"

    set -q FZF_DOCKER_IMAGE_LS_OPTS
    or set -l FZF_DOCKER_IMAGE_LS_OPTS "--ansi --header-lines=1 --nth=1..3"

    set -q FZF_DOCKER_IMAGE_LS_PREVIEW
    or set -l FZF_DOCKER_IMAGE_LS_PREVIEW "docker image history {3} | tail -n 50"

    begin
        eval "$FZF_DOCKER_IMAGE_LS_COMMAND | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS $FZF_DOCKER_IMAGE_LS_OPTS --query=\"$fzf_query\" --preview='$FZF_DOCKER_IMAGE_LS_PREVIEW' --preview-window=hidden | awk '{ print \$3 }'" | while read -l s; set results $results $s; end
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
