# alias git='noglob git'

# g = git status
# g ... = git $@
g() { [[ $# = 0 ]] && git status --short . || git $*; }

alias gbr='git browse'
alias gi='git init'
alias gf='git fetch'
alias gs='git status'
alias gcl='git clone'
alias gsu='git submodule'
alias gcp='git cherry-pick -S'
alias gbo='git orphan'
alias gbl='git blame'
alias grp='git rev-parse'
alias gpl='git pull --rebase -S --autostash'
# alias gpl='git pull'
alias gg='git grep'

# stash
alias gss='git stash save'
alias gsa='git stash apply'
alias gsl='git stash list'
alias gsp='git stash pop'
alias gsd='git stash drop'

# reset
alias gr='git reset HEAD'
alias grs='git reset --soft HEAD^'
alias grh='git reset --hard'
alias gundo='git reset --hard HEAD~1'

# rebase
alias grb='git rebase -S --autostash -i'
alias grc='git rebase --continue'
alias grk='git rebase --skip'

# checkout
alias gcoo='git checkout --'

gco() {
  if ! _is_callable fzf || [ $# -gt 0 ]; then
    git checkout "$@"
    return $?
  fi

  # checkout git branch (including remote branches), sorted by most recent commit, limit 30 last branches
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
    branch=$(echo "$branches" |
      fzf-tmux -d $((2 + $(wc -l <<<"$branches"))) +m) &&
    git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}

# Create branch and checkout.
# If the argument is a url, extract everything after the last slash to use it as
# branch name. Very useful for JIRA tickets :)
gcob() {
  local url_regex='^(https?|ftp|file)://[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]\.[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]$'
  if [[ "$1" =~ $url_regex ]]; then
    local branch=${1##*/}
    git checkout -b "$branch"
  else
    git checkout -b "$1"
  fi
}

# commit
# alias gca='noglob git commit -S -am'
alias gca='git commit -S -am'
alias gc='git commit -S'
alias gC='git commit'
# alias gcm='noglob git commit -S -m'
alias gcm='git commit -S -m'
alias gCm='git commit -m'
# alias gcma='noglob git commit --amend -S -m'
alias gcma='git commit --amend -S -m'
alias gCma='git commit --amend -m'
# alias gcf='noglob git commit -S --fixup'
alias gcf='git commit -S --fixup'
alias gCf='git commit --fixup'

# diff
alias gd='git diff'
alias gdc='git diff --cached'
alias gdt='git difftool'

# push
alias gp='git push'
alias gpb='git push origin'
alias gpt='git push --follow-tags'

# add
alias ga='git add'
alias gau='git add -u'
alias gap='git add --patch'

# branch
alias gb='git branch'
alias gbb='git branch -v'
alias gbd='git branch -D'

# log
alias gll='git log --graph --color=always --format="%C(auto)%h%d %s %C(8)%cr %C(0)by %C(italic 8)%aN%C(reset)"'
alias gL='gl --stat'

if _is_callable fzf; then
  gl() {
    git log --color=always \
        --format="%C(auto)%h%d %s %C(8)%cr %C(0)by %C(italic 8)%aN%C(reset)" "$@" |
    fzf --ansi --no-sort --reverse --tiebreak=index --bind=ctrl-s:toggle-sort \
        --bind "ctrl-m:execute:
                  (grep -o '[a-f0-9]\{7\}' | head -1 |
                  xargs -I % sh -c 'git show --color=always % | less -R') << 'FZF-EOF'
                  {}
  FZF-EOF"
  }
else
  alias gl='git log --color=always --format="%C(auto)%h%d %s %C(8)%cr %C(0)by %C(italic 8)%aN%C(reset)"'
fi

# tag
alias gt='git tag --sort=v:refname'
alias gtd='gt -d'
alias gta='gt -a'

# do not get VCS status (much faster)
alias k='k -Ah --no-vcs'
