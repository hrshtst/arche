function __fzf_ghq -d "Lists repositories downloaded with ghq and goes into selected one."
    type -q ghq; or return

    set -l commandline (__fzf_parse_commandline)
    set -l dir $commandline[1]
    set -l fzf_query $commandline[2]

    set -q FZF_GHQ_COMMAND
    or set -l FZF_GHQ_COMMAND "ghq list"

    set -q FZF_GHQ_OPTS
    or set -l FZF_GHQ_OPTS ""

    set -q FZF_PREVIEW_PAGER
    or set -l FZF_PREVIEW_PAGER "cat"

    set -q FZF_GIT_FILE_PREVIEW_SHELL
    or set -l FZF_GIT_FILE_PREVIEW_SHELL (which bash)

    set -q FZF_GHQ_PREVIEW
    or set -l FZF_GHQ_PREVIEW "\
command -v git &>/dev/null && \
  readarray -t root_dirs < <(git config --get-all ghq.root)
[ \${#root_dirs[@]} -gt 0 ] || \
  readarray -t root_dirs < <(ghq root)

for (( i=\${#root_dirs[@]}-1; i>=0; i-- )); do
  root=\"\${root_dirs[\$i]/\'~\'/$HOME}\"
  dir=\$root/{}
  [ -d  \$dir ] && break
done

readme=\$(find \$dir -maxdepth 1 -name README* | \
  awk \'{ print length(\$0) \" \" \$0; }\' | \
  sort -n | cut -d \' \' -f 2-)
if [[ -z \$readme ]]; then
  echo \"Found no README files.\"
else
  $FZF_PREVIEW_PAGER \$readme | head -n 500
fi
"

    set -l cur_shell $SHELL
    set -x SHELL $FZF_GIT_FILE_PREVIEW_SHELL
    eval "$FZF_GHQ_COMMAND | "(__fzfcmd) "+m $FZF_DEFAULT_OPTS $FZF_GHQ_OPTS --query \"$fzf_query\" --preview-window right:70%:wrap --preview '$FZF_GHQ_PREVIEW'" | read -l select
    set -x SHELL $cur_shell

    # Find root directories
    type -q git; and set -l root_dirs (git config --get-all ghq.root)
    test (count $root_dirs) -gt 0; or set -l root_dirs (ghq root)

    if not test -z "$select"
        for root in $root_dirs[-1..0]
            set -l dir (string replace "~" $HOME "$root/$select")
            if test -d "$dir"
                cd "$dir"
                break
            end
        end

        # Remove last token from commandline.
        commandline -t ""
    end

    commandline -f repaint
end
