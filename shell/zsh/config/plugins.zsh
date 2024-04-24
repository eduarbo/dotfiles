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

# https://github.com/mustaqimM/dotfiles/blob/8dbe45b1dbe29fc4686dc025d824a84024916d4e/.zsh/.zshrc
zinit ice wait'[[ -n ${ZLAST_COMMANDS[(r)ch*]} ]]' lucid as"program" mv"*cht.sh -> cht.sh"
zinit snippet "https://cht.sh/:cht.sh"

zinit ice mv=":zsh -> _cht" as="completion"
zinit snippet https://cheat.sh/:zsh

zinit ice lucid as"program" pick"bin/git-dsf"; zinit load so-fancy/diff-so-fancy
zinit ice lucid as"program" pick"bin/git-*"; zinit light tj/git-extras

# To customize prompt edit this file or run `p10k configure`
_load shell/zsh/config/p10k.zsh
# Load powerlevel10k theme
zinit ice depth"1" # git clone depth
zinit light romkatv/powerlevel10k

# This should be loaded after plugins that are issuing compdefs
zinit wait lucid light-mode for \
  atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
      zdharma-continuum/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
      zsh-users/zsh-completions

export _FASD_DATA="$XDG_DATA_HOME/fasd"
export _FASD_VIMINFO="$XDG_DATA_HOME/viminfo"
_cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}
