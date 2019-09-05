# zshrc
# By Eduardo Ruiz <eduarbo@gmail.com>

# Set vi style bindings before sourcing fzf to prevent reset for TAB key binding
bindkey -v

#
# Plugins
#

# Load zplugin and install it when missing
_load_repo zdharma/zplugin $ZPL_DIR zplugin.zsh

# load plugins from enabled topics
_load_all plugins.zsh

zplugin ice multisrc"shell/{key-bindings,completion}.zsh" pick""; zplugin light junegunn/fzf
zplugin light zsh-users/zsh-history-substring-search
zplugin light zdharma/history-search-multi-word
zplugin light supercrabtree/k
zplugin light djui/alias-tips

# NOTE this async lib and the one used by zsh-autosuggestions spawns a new zsh process
zplugin light mafredri/zsh-async # Required by simpl

export ZSH_AUTOSUGGEST_USE_ASYNC=1
export ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE=20
zplugin light zsh-users/zsh-autosuggestions
zplugin ice blockf; zplugin light zsh-users/zsh-completions # Disallow zsh-completions to modify fpath
if [[ -z $SSH_CONNECTION ]]; then
  zplugin light zdharma/fast-syntax-highlighting
fi

typeset -gA PROMPT_SIMPL_HOSTNAME_SYMBOL_MAP
SIMPL_HOST_SYMBOL_MAP=(
  lavos "ᚱ"
  htpc "Ħ"
  chrono "ᛟ"
)

typeset -A SIMPL
SIMPL[ENABLE_RPROMPT]=1
zplugin light ~/dev/simpl
# zplugin light eduarbo/simpl

#
# configs
#

# ensure EXTENDED_GLOB is set before looking for expired zcompdump with glob
# qualifiers
_load shell/zsh/config.zsh
_load shell/zsh/completion.zsh
_load shell/zsh/keybinds.zsh
_load shell/zsh/fzf.zsh
_source ~/.secrets

#
# Speed up zsh load
#

# Compinit should be called after loading of all plugins and before possibly calling cdreply
autoload -Uz compinit

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
# The globbing is a little complicated here:
#
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+20' matches files (or directories or whatever) that are older than 20 hours.
if [[ -n $ZPLGM[ZCOMPDUMP_PATH](#qN.mh+20) ]]; then
	compinit -d $ZPLGM[ZCOMPDUMP_PATH];
  # update the timestamp on compdump file
  compdump
else
	compinit -C -d $ZPLGM[ZCOMPDUMP_PATH];
fi;

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed
  if [[ "${ZPLGM[ZCOMPDUMP_PATH]}" -nt "${ZPLGM[ZCOMPDUMP_PATH]}.zwc" || ! -s "${ZPLGM[ZCOMPDUMP_PATH]}.zwc" ]]; then
    zcompile "${ZPLGM[ZCOMPDUMP_PATH]}"
  fi
} &!

# execute compdefs provided by rest of plugins
zplugin cdreplay -q # -q is for quiet

# load aliases from enabled topics
# source them after compinit to be able to use compdef
_load_all aliases.bash
_load_all aliases.zsh

export _FASD_DATA="$XDG_DATA_HOME/fasd"
export _FASD_VIMINFO="$XDG_DATA_HOME/viminfo"
_cache fasd --init posix-alias zsh-{hook,{c,w}comp{,-install}}

# vim:set ft=sh:
