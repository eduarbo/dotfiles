compdef g=hub
if _is_callable hub; then
  alias git='noglob hub'
else
  alias git='noglob git'
fi

alias gca='noglob git commit -S -am'
alias gcm='noglob git commit -S -m'
alias gcma='noglob git commit --amend -S -m'
alias gcf='noglob git commit -S --fixup'
