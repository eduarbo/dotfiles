# ┏━┓╻  ╻ ╻┏━╸╻┏┓╻┏━┓
# ┣━┛┃  ┃ ┃┃╺┓┃┃┗┫┗━┓
# ╹  ┗━╸┗━┛┗━┛╹╹ ╹┗━┛
# Plugins


# Load zinit and install it when missing
_load_repo zdharma-continuum/zinit $ZINIT_DIR zinit.zsh

# ─── FZF and related ───────────────────────────────────────────────────────────

zinit ice wait"0" lucid multisrc"shell/*.zsh"
zinit load junegunn/fzf

zinit ice wait"1" lucid
zinit load Aloxaf/fzf-tab

# Binds Ctrl-R to a widget that performs a search for multiple keywords using AND logic
zinit ice wait"0" lucid light-mode
zinit load zsh-users/zsh-history-substring-search

zinit ice wait"0" lucid
zinit load zdharma-continuum/history-search-multi-word

# ─── Additional Tools ──────────────────────────────────────────────────────────

zinit ice wait"1" lucid
zinit load djui/alias-tips

# https://github.com/mustaqimM/dotfiles/blob/248e03018096e5913b82940dca626fa78d9cf46c/.zsh/.zshrc
zinit ice wait"1" lucid as"program" mv"*cht.sh -> cht" pick"cht" id-as"cht.sh"
zinit snippet "https://cht.sh/:cht.sh"

zinit ice wait"0" lucid as"program" pick"bin/git-dsf"
zinit load so-fancy/diff-so-fancy

zinit ice wait"0" lucid as"program" pick"bin/git-*"
zinit load tj/git-extras

# fasd is archived and unavailable for brew; we still can load from GitHub
# https://github.com/Homebrew/homebrew-core/pull/112791
zinit ice pick"fasd" atload'eval "$(fasd --init env)"'
zinit load whjvenyl/fasd
# eval "$(fasd --init env)"

# ─── powerlevel10k theme ───────────────────────────────────────────────────────

# To customize prompt edit this file or run `p10k configure`
_load shell/zsh/config/p10k.zsh

zinit ice depth"1"
zinit load romkatv/powerlevel10k

_load_all plugins.zsh

# ─── Now that plugins are loaded, run compinit or zicompinit ───────────────────

# This should be loaded after plugins that are issuing compdefs
zinit wait lucid for \
 atinit"zicompinit; zicdreplay" \
    zdharma-continuum/fast-syntax-highlighting \
 blockf \
    zsh-users/zsh-completions \
 atload"!_zsh_autosuggest_start" \
    zsh-users/zsh-autosuggestions
