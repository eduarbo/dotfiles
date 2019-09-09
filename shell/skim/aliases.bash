alias skgcf='git commit -S --fixup `sk-git-sha`'
alias skgl='sk-git-log'

alias bm='sk-browser-bookmarks'
alias hist='sk-browser-history'

# TODO --select-1 and --exit-0 are not yet implemented, so it won't work as FZF does
j() {
  local dir
  dir="$(fasd -Rdl "$1" | sk --select-1 --exit-0 --no-sort)" && cd "${dir}" || return 1
}

# change directory
jj() {
  local dir
  dir="$(fd --type directory --follow --hidden "$1" | fzf --select-1 --exit-0 --no-sort)" && cd "${dir}" || return 1
}
