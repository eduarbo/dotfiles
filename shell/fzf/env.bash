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
