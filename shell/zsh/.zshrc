#                             ░▀▀█░█▀▀░█░█░█▀▄░█▀▀
#                             ░▄▀░░▀▀█░█▀█░█▀▄░█░░
#                             ░▀▀▀░▀▀▀░▀░▀░▀░▀░▀▀▀


# Set vi style bindings before sourcing fzf to prevent reset for TAB key binding
bindkey -v


# ┏━╸┏━┓┏┓╻┏━╸╻┏━╸┏━┓
# ┃  ┃ ┃┃┗┫┣╸ ┃┃╺┓┗━┓
# ┗━╸┗━┛╹ ╹╹  ╹┗━┛┗━┛
# Configs

_load shell/zsh/plugins.zsh

# ensure EXTENDED_GLOB is set before looking for expired zcompdump with glob
# qualifiers
_load shell/zsh/config.zsh

_load shell/zsh/completion.zsh
_load shell/zsh/keybinds.zsh

# load configs from enabled topics
_load_all config.zsh

# _load shell/zsh/speedup.zsh


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
