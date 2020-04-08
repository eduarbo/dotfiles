# ┏━┓╻  ╻ ╻┏━╸╻┏┓╻┏━┓
# ┣━┛┃  ┃ ┃┃╺┓┃┃┗┫┗━┓
# ╹  ┗━╸┗━┛┗━┛╹╹ ╹┗━┛
# Plugins


# Load zinit and install it when missing
_load_repo zdharma/zinit $ZINIT_DIR zinit.zsh

zinit light zsh-users/zsh-history-substring-search
zinit light zdharma/history-search-multi-word
zinit light supercrabtree/k
zinit light djui/alias-tips
zinit ice from"gh-r" as"program"; zinit light junegunn/fzf-bin
zinit ice multisrc"shell/{key-bindings,completion}.zsh" pick""; zinit light junegunn/fzf

# NOTE this async lib and the one used by zsh-autosuggestions spawns a new zsh process
zinit light mafredri/zsh-async # Required by simpl

export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
zinit light zsh-users/zsh-autosuggestions
zinit ice blockf; zinit light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
if [[ -z $SSH_CONNECTION ]]; then
  zinit light zdharma/fast-syntax-highlighting
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

zinit light eduarbo/simpl
