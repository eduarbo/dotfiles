alias skgcf='git commit -S --fixup `sk-git-sha`'
alias skgl='sk-git-log'

alias bm='sk-browser-bookmarks'
alias hist='sk-browser-history'

j() {
  local dir
  dir="$(fasd -Rdl "$1" | sk -1 -0 --no-sort)" && cd "${dir}" || return 1
}

# change directory
jj() {
  local dir
  dir="$(fd --type directory --follow --hidden "$1" | fzf -1 -0 --no-sort)" && cd "${dir}" || return 1
}
