#                           ░▀▀█░█▀▀░█░█░█▀▀░█▀█░█░█
#                           ░▄▀░░▀▀█░█▀█░█▀▀░█░█░▀▄▀
#                           ░▀▀▀░▀▀▀░▀░▀░▀▀▀░▀░▀░░▀░
#
# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.


unsetopt GLOBAL_RCS  # disable global zsh config; we'll handle it ourselves
source $(cd ${${(%):-%x}:A:h}/../../.. && pwd -P)/env

# Move ZDOTDIR from $HOME to reduce dotfile pollution.
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
export ZSH_CACHE="$XDG_CACHE_HOME/zsh"
export ZINIT_DIR="$ZSH_CACHE/zinit"

local -A ZINIT # initial Zinit's hash definition
export ZINIT[ZCOMPDUMP_PATH]="$ZSH_CACHE/zcompdump_$ZSH_VERSION"
export ZINIT[HOME_DIR]="$ZINIT_DIR"

# These 2 variables need to be set in our local machine since they are passed
# down to the remote host when we connect via SSH. Otherwise, we will be getting
# an annoying perl warning from time to time.
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export FIGLET_FONTDIR="$ZDOTDIR/figlet-fonts"

export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

# Ensure path arrays do not contain duplicates
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath

path=( /usr/local/{,s}bin /usr/{,s}bin /{,s}bin )

BREW_PATH="/opt/homebrew/bin/brew"
[[ -f "$BREW_PATH" ]] && eval "$($BREW_PATH shellenv)"

# [[ $(_os) == macos ]] && path=($(brew --prefix coreutils/libexec/gnubin 2>/dev/null) $path)
path=( $XDG_BIN_HOME $DOTFILES_DATA/*.topic/bin(N) $path )

fpath=( $ZDOTDIR/functions $XDG_BIN_HOME $fpath )

# initialize enabled topics
_load_all env.bash
