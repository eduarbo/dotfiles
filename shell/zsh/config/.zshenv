# This file is sourced by all instances of Zsh, and thus, it should be kept as
# small as possible and should only define environment variables.

# unsetopt GLOBAL_RCS  # disable global zsh config; we'll handle it ourselves
source $(cd ${${(%):-%x}:A:h}/../../.. && pwd -P)/env

# Do not call compinit too early (happening in Debian)
skip_global_compinit=1

export ZSH_DATA_HOME="$XDG_DATA_HOME/zsh"
export ZSH_CACHE="$XDG_CACHE_HOME/zsh"
export ZINIT_DIR="$ZSH_CACHE/zinit"

export LANG=${LANG:-en_US.UTF-8}
export MANPAGER="/bin/sh -c \"col -b | vim -c 'set ft=man ts=8 nomod nolist nonu noma' -\""
export PAGER=less
export LESS='-R -i -w -M -z-4'
export LESSHISTFILE="$XDG_DATA_HOME/lesshst"

# Changing pinentry for SSH logins
GPG_TTY=$(tty)
export GPG_TTY

if [[ -n "$SSH_CONNECTION" ]] ;then
  export PINENTRY_USER_DATA="USE_CURSES=1"
fi

# initial Zinit's hash definition
local -A ZINIT

export ZINIT[ZCOMPDUMP_PATH]="$ZSH_CACHE/zcompdump_$ZSH_VERSION"
export ZINIT[HOME_DIR]="$ZINIT_DIR"
export ZINIT[COMPINIT_OPTS]="-C"

# These 2 variables need to be set in our local machine since they are passed
# down to the remote host when we connect via SSH. Otherwise, we will be getting
# an annoying perl warning from time to time.
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export FIGLET_FONTDIR="$ZDOTDIR/figlet-fonts"

export POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

export _FASD_DATA="$XDG_DATA_HOME/zsh/fasd"

# Ensure path arrays do not contain duplicates
typeset -gU cdpath fpath mailpath manpath path
typeset -gUT INFOPATH infopath

BREW_PATH=$( [ -x /opt/homebrew/bin/brew ] && echo /opt/homebrew/bin/brew || echo /usr/local/bin/brew )

if [ -x "$BREW_PATH" ]; then
  # NOTE this reorders the PATH, make sure to update the PATH after homebrew init
  eval "$($BREW_PATH shellenv)"
  path=( $HOMEBREW_PREFIX/opt/grep/libexec/gnubin $path )
fi

path=( $path /usr/local/{,s}bin /usr/{,s}bin /{,s}bin )
path=( $XDG_BIN_HOME $DOTFILES_DATA/*.topic/bin(N) $path )

fpath=( $ZDOTDIR/functions $XDG_BIN_HOME $fpath )

case $(_os) in
  debian)
    export GNUPGHOME="$XDG_CONFIG_HOME/gpg"
    export TERMINFO="$XDG_CONFIG_HOME/terminfo"
    ;;
esac

_cache ssh-agent -s >/dev/null

# initialize enabled topics
_load_all env.zsh

# If you have host-local configuration, this is where you'd put it
_source $ZDOTDIR/zshenv.local
