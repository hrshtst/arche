#!/usr/bin/env zsh

if (( ! $+commands[git] )); then
  return
fi

alias g=git

alias ge='git help'
alias ghi='git help init'
alias ghst='git help status'
alias ghsh='git help show'
alias ghl='git help log'
alias gha='git help add'
alias ghrm='git help rm'
alias ghmv='git help mv'
alias ghr='git help reset'
alias ghcm='git help commit'
alias ghcp='git help cherry-pick'
alias ghrv='git help revert'
alias ght='git help tag'
alias ghn='git help notes'
alias ghsta='git help stash'
alias ghd='git help diff'
alias ghbl='git help blame'
alias ghb='git help branch'
alias ghco='git help checkout'
alias ghlsf='git help ls-files'
alias ghx='git help clean'
alias ghbs='git help bisect'
alias ghm='git help merge'
alias ghrb='git help rebase'
alias ghsm='git help submodule'
alias ghcl='git help clone'
alias ghre='git help remote'
alias ghf='git help fetch'
alias ghu='git help pull'
alias ghp='git help push'

alias gi='git init'

alias gst='git status'

alias gsh='git show'
alias gshs='git show --stat'

for nograph in "" n; do
  local graph_flags=
  if [[ -z $nograph ]]; then
    graph_flags=" --graph"
  fi
  for all in "" a; do
    local all_flags=
    if [[ -n $all ]]; then
      all_flags=" --all"
    fi
    for oneline in "" o; do
      local oneline_flags=
      if [[ -n $oneline ]]; then
        oneline_flags=" --oneline"
      fi
      for diff in "" s p ps sp; do
        local diff_flags=
        case $diff in
          s) diff_flags=" --stat";;
          p) diff_flags=" --patch";;
          ps|sp) diff_flags=" --patch --stat";;
        esac
        for search in "" g G S; do
          local search_flags=
          case $search in
            g) search_flags=" --grep";;
            G) search_flags=" -G";;
            S) search_flags=" -S";;
          esac
          alias="gl${nograph}${all}${oneline}${diff}${search}="
          alias+="git log --decorate"
          alias+="${graph_flags}${all_flags}"
          alias+="${oneline_flags}${diff_flags}${search_flags}"
          alias $alias
        done
      done
    done
  done
done

alias ga='git add'
alias gap='git add --patch'
alias gaa='git add --all'

alias grm='git rm'

alias gmv='git mv'

alias gr='git reset'
alias grs='git reset --soft'
alias grh='git reset --hard'
alias grp='git reset --patch'

alias gc='git commit --verbose'
alias gca='git commit --verbose --amend'
alias gcaa='git commit --verbose --amend --all'
alias gcf='git commit -C HEAD --amend'
alias gcfa='git commit -C HEAD --amend --all'
alias gce='git commit --verbose --allow-empty'
alias gcm='git commit -m'
alias gcma='git commit --all -m'
alias gcam='git commit --amend -m'
alias gcama='git commit --amend --all -m'
alias gcem='git commit --allow-empty -m'

alias gcn='git commit --no-verify --verbose'
alias gcna='git commit --no-verify --verbose --amend'
alias gcnaa='git commit --no-verify --verbose --amend --all'
alias gcnf='git commit --no-verify -C HEAD --amend'
alias gcnfa='git commit --no-verify -C HEAD --amend --all'
alias gcne='git commit --no-verify --verbose --allow-empty'
alias gcnm='git commit --no-verify -m'
alias gcnma='git commit --no-verify --all -m'
alias gcnam='git commit --no-verify --amend -m'
alias gcnama='git commit --no-verify --amend --all -m'
alias gcnem='git commit --no-verify --allow-empty -m'

alias gcp='git cherry-pick'
alias gcpc='git cherry-pick --continue'
alias gcpa='git cherry-pick --abort'

alias grv='git revert'
alias grva='git revert --abort'
alias grvm='git revert -m'

alias gt='git tag'
alias gtd='git tag --delete'

alias gn='git notes'
alias gna='git notes add'
alias gne='git notes edit'
alias gnr='git notes remove'

alias gsta='git stash save'
alias gstau='git stash save --include-untracked'
alias gstap='git stash save --patch'
alias gstl='git stash list'
alias gsts='git stash show --text'
alias gstss='git stash show --stat'
alias gstaa='git stash apply'
alias gstp='git stash pop'
alias gstd='git stash drop'

alias gd='git diff --minimal'
alias gds='git diff --minimal --stat'
alias gdc='git diff --minimal --cached'
alias gdcs='git diff --minimal --cached --stat'
alias gdn='git diff --minimal --no-index'

alias gbl='git blame'

alias gb='git branch'
alias gba='git branch --all'
alias gbsu='git branch --set-upstream-to'
alias gbusu='git branch --unset-upstream'
alias gbd='git branch --delete'
alias gbdd='git branch --delete --force'

alias gco='git checkout'
alias gcot='git checkout --track'
alias gcop='git checkout --patch'
alias gcob='git checkout -B'

alias glsf='git ls-files'

alias gx='git clean'
alias gxu='git clean -ffd'
alias gxi='git clean -ffdX'
alias gxa='git clean -ffdx'

alias gbs='git bisect'
alias gbss='git bisect start'
alias gbsg='git bisect good'
alias gbsb='git bisect bad'
alias gbsr='git bisect reset'

alias gm='git merge'
alias gma='git merge --abort'

alias grb='git rebase'
alias grbi='git rebase --interactive'
alias grbc='git rebase --continue'
alias grbs='git rebase --skip'
alias grba='git rebase --abort'

alias gsm='git submodule'
alias gsma='git submodule add'
alias gsms='git submodule status'
alias gsmi='git submodule init'
alias gsmd='git submodule deinit'
alias gsmu='git submodule update --recursive'
alias gsmui='git submodule update --init --recursive'
alias gsmf='git submodule foreach'
alias gsmy='git submodule sync'

alias gcl='git clone --recursive'
alias gcls='git clone --depth=1 --single-branch --no-tags'

alias gre='git remote'
alias grel='git remote list'
alias gres='git remote show'

alias gf='git fetch'
alias gfa='git fetch --all'
alias gfu='git fetch --unshallow'

alias gu='git pull'
alias gur='git pull --rebase --autostash'
alias gum='git pull --no-rebase'

alias gp='git push'
alias gpa='git push --all'
alias gpf='git push --force-with-lease'
alias gpff='git push --force'
alias gpu='git push --set-upstream'
alias gpd='git push --delete'
alias gpt='git push --tags'
