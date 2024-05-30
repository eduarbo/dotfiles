# ┏━┓╻  ╻ ╻┏━╸╻┏┓╻┏━┓
# ┣━┛┃  ┃ ┃┃╺┓┃┃┗┫┗━┓
# ╹  ┗━╸┗━┛┗━┛╹╹ ╹┗━┛
# Plugins


# Load zinit and install it when missing
_load_repo zdharma-continuum/zinit $ZINIT_DIR zinit.zsh

zinit ice wait lucid multisrc"shell/*.zsh"
zinit light junegunn/fzf

# Binds Ctrl-R to a widget that performs a search for multiple keywords using AND logic
zinit lucid light-mode for zsh-users/zsh-history-substring-search

zinit ice wait lucid
zinit light zdharma-continuum/history-search-multi-word

zinit ice wait lucid
zinit light djui/alias-tips

# https://github.com/mustaqimM/dotfiles/blob/248e03018096e5913b82940dca626fa78d9cf46c/.zsh/.zshrc
zinit ice lucid as"program" mv"*cht.sh -> cht" pick"cht" id-as"cht.sh"
zinit snippet "https://cht.sh/:cht.sh"

# zinit ice mv=":zsh -> _cht" as="completion"
# zinit snippet https://cht.sh/:zsh

zinit ice lucid as"program" pick"bin/git-dsf"
zinit light so-fancy/diff-so-fancy

zinit ice lucid as"program" pick"bin/git-*"
zinit light tj/git-extras

# fasd is archived and unavailable for brew
# https://github.com/Homebrew/homebrew-core/pull/112791
zinit ice wait lucid
zinit light whjvenyl/fasd

# To customize prompt edit this file or run `p10k configure`
_load shell/zsh/config/p10k.zsh
# Load powerlevel10k theme
zinit ice depth"1" # git clone depth
zinit light romkatv/powerlevel10k

_load_all plugins.zsh

# This should be loaded after plugins that are issuing compdefs
zinit wait lucid for \
 atinit"zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions

_cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}
