if ! _is_callable fzf; then
  return
fi

# Dedicated completion key
# https://github.com/junegunn/fzf/wiki/Configuring-fuzzy-completion#dedicated-completion-key
export FZF_COMPLETION_TRIGGER="#"

# automatically selects the item if there's only one
export FZF_DEFAULT_OPTS='
  --select-1
  --exit-0
  --height 50%
  --layout=reverse'

export FZF_CTRL_T_OPTS='
  --preview "[[ $(file --mime {}) =~ binary ]] &&
    echo {} is a binary file ||
    (bat --color=always --style=header,grid --line-range :300 {} ||
     highlight -O ansi -l {} ||
     coderay {} ||
     rougify {} ||
     cat {}) 2> /dev/null | head -500"'

# Use fd (https://github.com/sharkdp/fd) instead of the default find
# command for listing path candidates.
# - The first argument to the function ($1) is the base path to start traversal
# - See the source code (completion.{bash,zsh}) for the details.
export FZF_DEFAULT_COMMAND='
  (fd --type f --type d --hidden --follow --exclude .git ||
   git ls-tree -r --name-only HEAD ||
   rg --files ||
   find . -path "*/\.*" -prune -o -type f -print -o -type l -print |
      sed s/^..//) 2> /dev/null'

export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

_fzf_compgen_path() {
  fd --hidden --follow --exclude ".git" . "$1"
}

_fzf_compgen_dir() {
  fd --type d --hidden --follow --exclude ".git" . "$1"
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

_fzf_complete_gco() {
  local branches branch
  branches=$(git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format="%(refname:short)") &&
    branch=$(echo "$branches" |
               fzf-tmux -d $(( 2 + $(wc -l <<< "$branches") )) +m) &&
    git checkout $(echo "$branch" | sed "s/.* //" | sed "s#remotes/[^/]*/##")
}
