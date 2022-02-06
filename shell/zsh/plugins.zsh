# ┏━┓╻  ╻ ╻┏━╸╻┏┓╻┏━┓
# ┣━┛┃  ┃ ┃┃╺┓┃┃┗┫┗━┓
# ╹  ┗━╸┗━┛┗━┛╹╹ ╹┗━┛
# Plugins


# Load zinit and install it when missing
_load_repo zdharma-continuum/zinit $ZINIT_DIR zinit.zsh

zinit lucid wait for zsh-users/zsh-history-substring-search

zinit light zdharma-continuum/history-search-multi-word

zinit light djui/alias-tips
zinit ice multisrc"shell/{key-bindings,completion}.zsh" pick""; zinit light junegunn/fzf

zinit ice mv=":cht.sh -> cht.sh" atclone="chmod +x cht.sh" as="program"
zinit snippet https://cht.sh/:cht.sh
zinit ice mv=":zsh -> _cht" as="completion"
zinit snippet https://cheat.sh/:zsh

# # NOTE this async lib and the one used by zsh-autosuggestions spawns a new zsh process
zinit light mafredri/zsh-async # Required by simpl

export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
zinit light zsh-users/zsh-autosuggestions
zinit wait lucid for \
 atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions

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
