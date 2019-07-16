function __fzf_kill -d "Kill processes"
    set -l uid (id -u (whoami))

    set -l signal
    if test -z $argv[1]
        set signal "9"
    else
        set signal $argv[1]
    end

    set -q PS_USER_COMMAND
    or set -l PS_USER_COMMAND "command ps -f -u $uid"

    set -q PS_ROOT_COMMAND
    or set -l PS_ROOT_COMMAND "command ps -ef"

    set -l PS_COMMAND
    if test $uid != 0
        set PS_COMMAND $PS_USER_COMMAND
    else
        set PS_COMMAND $PS_ROOT_COMMAND
    end

    begin
        eval "$PS_COMMAND | sed 1d | "(__fzfcmd) "-m $FZF_DEFAULT_OPTS | awk '{ print \$2 }'" | while read -l s; set results $results $s; end
    end

    if test -z "$results"
        commandline -f repaint
        return
    else
        command kill -$signal $results
    end

    commandline -f repaint
end
