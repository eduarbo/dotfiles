# ┏━┓╻  ╻ ╻┏━╸╻┏┓╻┏━┓
# ┣━┛┃  ┃ ┃┃╺┓┃┃┗┫┗━┓
# ╹  ┗━╸┗━┛┗━┛╹╹ ╹┗━┛
# Plugins


# Load zplugin and install it when missing
_load_repo zdharma/zplugin $ZPL_DIR zplugin.zsh

zplugin light zsh-users/zsh-history-substring-search
zplugin light zdharma/history-search-multi-word
zplugin light supercrabtree/k
zplugin light djui/alias-tips
zplugin ice from"gh-r" as"program"; zplugin light junegunn/fzf-bin
zplugin ice multisrc"shell/{key-bindings,completion}.zsh" pick""; zplugin light junegunn/fzf

# NOTE this async lib and the one used by zsh-autosuggestions spawns a new zsh process
zplugin light mafredri/zsh-async # Required by simpl

export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
zplugin light zsh-users/zsh-autosuggestions
zplugin ice blockf; zplugin light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
if [[ -z $SSH_CONNECTION ]]; then
  zplugin light zdharma/fast-syntax-highlighting
fi

typeset -gA SIMPL_HOST_SYMBOL_MAP
SIMPL_HOST_SYMBOL_MAP=(
  lavos "⑀"
  htpc "Ħ"
  GLaDOS "ᛟ"
)

SIMPL_HOST_SYMBOL_COLOR="%B%F{3}"
# SIMPL_USER_COLOR="%F{10}"
SIMPL_USER_COLOR="%F{11}"
SIMPL_ENABLE_RPROMPT=0
SIMPL_ALWAYS_SHOW_USER_AND_HOST=0

zplugin light eduarbo/simpl
