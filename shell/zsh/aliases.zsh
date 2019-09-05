_load shell/bash/aliases.bash

alias cd="pushd -q"
alias ag="noglob ag -p $XDG_CONFIG_HOME/ag/agignore"
alias rg='noglob rg'

autoload -U zmv

zman() { PAGER="less -g -s '+/^       "$1"'" man zshall; }

r() {
  local time=$1; shift
  sched "$time" "notify-send --urgency=critical 'Reminder' '$@'; ding";
}; compdef r=sched
