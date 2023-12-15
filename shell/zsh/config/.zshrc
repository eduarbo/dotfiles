#                             ░▀▀█░█▀▀░█░█░█▀▄░█▀▀
#                             ░▄▀░░▀▀█░█▀█░█▀▄░█░░
#                             ░▀▀▀░▀▀▀░▀░▀░▀░▀░▀▀▀


# let's make Tramp behave appropriately
if [[ "$TERM" == "dumb" ]]; then
    unsetopt zle
    PS1='$ '
    HISTFILE=~/.tramp-histfile
    return
fi


# Set vi style bindings before sourcing fzf to prevent reset for TAB key binding
bindkey -v


# ┏━╸┏━┓┏┓╻┏━╸╻┏━╸┏━┓
# ┃  ┃ ┃┃┗┫┣╸ ┃┃╺┓┗━┓
# ┗━╸┗━┛╹ ╹╹  ╹┗━┛┗━┛
# Configs

_load shell/zsh/config/plugins.zsh

# ensure EXTENDED_GLOB is set before looking for expired zcompdump with glob
# qualifiers
_load shell/zsh/config/config.zsh

_load shell/zsh/config/completion.zsh
_load shell/zsh/config/keybinds.zsh

# load configs from enabled topics
_load_all config.zsh

# TODO Revisit, maybe I don't need this anymore
# _load shell/zsh/config/speedup.zsh

function _set_terminal_title() {
    local title="$(basename "$PWD")"
    if [[ -n $SSH_CONNECTION ]]; then
        title="$title \xE2\x80\x94 $HOSTNAME"
    fi
    echo -ne "\033]0;$title\007"
}
add-zsh-hook precmd _set_terminal_title


# ┏━┓╻  ╻┏━┓┏━┓┏━╸┏━┓
# ┣━┫┃  ┃┣━┫┗━┓┣╸ ┗━┓
# ╹ ╹┗━╸╹╹ ╹┗━┛┗━╸┗━┛
# Load aliases from enabled topics

# source them after compinit to be able to use compdef
_load_all aliases.bash
_load_all aliases.zsh


# ┏━┓╻ ╻┏━╸┏━┓┏━┓╻╺┳┓┏━╸┏━┓
# ┃ ┃┃┏┛┣╸ ┣┳┛┣┳┛┃ ┃┃┣╸ ┗━┓
# ┗━┛┗┛ ┗━╸╹┗╸╹┗╸╹╺┻┛┗━╸┗━┛
_source $ZSH_DATA_HOME/secrets.zsh
_source $ZSH_DATA_HOME/local.zsh
