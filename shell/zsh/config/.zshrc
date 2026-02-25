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

# Enable Powerlevel10k instant prompt. Must be near the top of .zshrc — before
# any output or code that may produce output. The prompt will render immediately
# while the rest of zsh initialization continues in the background.
if [[ -r "$XDG_CACHE_HOME/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "$XDG_CACHE_HOME/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Set vi style bindings before sourcing fzf to prevent reset for TAB key binding
bindkey -v

# NOTE ensure EXTENDED_GLOB is set before looking for expired zcompdump with glob
# qualifiers
_load $ZDOTDIR/config.zsh
_load $ZDOTDIR/plugins.zsh

# NOTE: Ensure to source the following files after invoking compinit (done in
# plugins.zsh after loading the last completion-related plugin)
_load $ZDOTDIR/completion.zsh
_load $ZDOTDIR/keybinds.zsh

_load $ZDOTDIR/cursor.zsh

# load configs and aliases from enabled topics
_load_all config.zsh
_load_all aliases.zsh

# If you have host-local configuration, put it here
_source $ZDOTDIR/zshrc.local

function _set_terminal_title() {
    local title="$(basename "$PWD")"
    if [[ -n $SSH_CONNECTION ]]; then
        title="$title \xE2\x80\x94 $HOSTNAME"
    fi
    echo -ne "\033]0;$title\007"
}
add-zsh-hook precmd _set_terminal_title
