export SKIM_DIR="$XDG_CONFIG_HOME/skim"
export SKIM_DEFAULT_OPTIONS='--reverse --color=16,fg+:2,bg+:0,hl:4,hl+:4,prompt:4,pointer:8'

[[ $(_os) == debian ]] && path=( "$SKIM_DIR/bin" "${path[@]}" )
