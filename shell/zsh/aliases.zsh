alias cd='pushd -q'
alias ag="noglob ag"
alias rg='noglob rg'

autoload -U zmv

zman() { PAGER="less -g -s '+/^       "$1"'" man zshall; }


## FZF

# fasd & fzf change directory
j() {
  local dir
  dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

# fd & fzf change directory
cdd() {
  local dir
  dir="$(fd --type directory --follow --hidden "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

# cdf - cd into the directory of the selected file
cdf() {
   local file
   local dir
   file=$(fzf +m -q "$1") && dir=$(dirname "$file") && cd "$dir"
}

# fkill - kill processes - list only the ones you can kill. Modified the earlier script.
fkill() {
    local pid
    if [ "$UID" != "0" ]; then
        pid=$(ps -f -u $UID | sed 1d | fzf -m | awk '{print $2}')
    else
        pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
    fi

    if [ "x$pid" != "x" ]
    then
        echo $pid | xargs kill -${1:-9}
    fi
}

# Replacing GNU grep shell script with my own version to suppress the annoying
# obsolescence warning that started to appear in version 3.8 in MacOS
grep() {
  if [[ $(_os) == macos ]]; then
    ggrep "$@"
  else
    command grep "$@"
  fi
}
