# The surround module wasn't working if KEYTIMEOUT was <= 10. Specifically,
# (delete|change)-surround immediately abort into insert mode if KEYTIMEOUT <=
# 8. If <= 10, then add-surround does the same. At 11, all these issues vanish.
# Very strange!
export KEYTIMEOUT=15

# Open current prompt in external editor
autoload -Uz edit-command-line; zle -N edit-command-line
bindkey '^ ' edit-command-line

# History search
# -----------------------------------------------------------

# Color in which to highlight matched, searched text (default bg=17 on 256-color terminals)
zstyle ":history-search-multi-word" highlight-color "fg=yellow,bold"

# bind UP and DOWN arrow keys
bindkey '^[[A' history-substring-search-up
bindkey '^[[B' history-substring-search-down
bindkey "^[OA" history-substring-search-up
bindkey "^[OB" history-substring-search-down

bindkey "^R" history-search-multi-word

# Fancy Ctrl+Z
# -----------------------------------------------------------

# C-z to toggle current process (background/foreground)
fancy-ctrl-z () {
  if [[ $#BUFFER -eq 0 ]]; then
    BUFFER="fg"
    zle accept-line
  else
    zle push-input
    zle clear-screen
  fi
}
zle -N fancy-ctrl-z
bindkey '^Z' fancy-ctrl-z

# vi-mode
# -----------------------------------------------------------

bindkey -M viins '^u' backward-kill-line
bindkey -M viins '^w' backward-kill-word
bindkey -M viins '^b' backward-word
bindkey -M viins '^f' forward-word
bindkey -M viins '^g' push-line-or-edit
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^d' push-line-or-edit

bindkey -M viins '^J' down-line
bindkey -M viins '^K' up-line

bindkey -M vicmd '^k' kill-line
bindkey -M vicmd 'H'  run-help

# Shift + Tab
bindkey -M viins '^[[Z' reverse-menu-complete

# Vim's C-x C-l in zsh
history-beginning-search-backward-then-append() {
  zle history-beginning-search-backward
  zle vi-add-eol
}
zle -N history-beginning-search-backward-then-append
bindkey -M viins '^x^l' history-beginning-search-backward-then-append

# Fix the DEL key
bindkey -M vicmd "^[[3~" delete-char
bindkey "^[[3~" delete-char

# Fix vimmish ESC
bindkey -sM vicmd '^[' '^G'
bindkey -rM viins '^X'
bindkey -M viins '^X,' _history-complete-newer \
  '^X/' _history-complete-older \
  '^X`' _bash_complete-word

# Surround
# -----------------------------------------------------------

autoload -Uz surround
zle -N delete-surround surround
zle -N add-surround surround
zle -N change-surround surround
bindkey -a cs change-surround
bindkey -a ds delete-surround
bindkey -a ys add-surround

autoload -U is-at-least

# <5.0.8 doesn't have visual map
if is-at-least 5.0.8; then
  bindkey -M visual S add-surround

  # add vimmish text-object support to zsh
  autoload -U select-quoted; zle -N select-quoted
  for m in visual viopp; do
    for c in {a,i}{\',\",\`}; do
      bindkey -M $m $c select-quoted
    done
  done
  autoload -U select-bracketed; zle -N select-bracketed
  for m in visual viopp; do
    for c in {a,i}${(s..)^:-'()[]{}<>bB'}; do
      bindkey -M $m $c select-bracketed
    done
  done
fi

# Completing words in buffer in tmux
# -----------------------------------------------------------

if [ -n "$TMUX" ]; then
  _tmux_pane_words() {
    local expl
    local -a w
    if [[ -z "$TMUX_PANE" ]]; then
      _message "not running inside tmux!"
      return 1
    fi
    w=( ${(u)=$(tmux capture-pane \; show-buffer \; delete-buffer)} )
    _wanted values expl 'words from current tmux pane' compadd -a w
  }

  zle -C tmux-pane-words-prefix   complete-word _generic
  zle -C tmux-pane-words-anywhere complete-word _generic

  bindkey -M viins '^x^n' tmux-pane-words-prefix
  bindkey -M viins '^x^o' tmux-pane-words-anywhere

  zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' completer _tmux_pane_words
  zstyle ':completion:tmux-pane-words-(prefix|anywhere):*' ignore-line current
  zstyle ':completion:tmux-pane-words-anywhere:*' matcher-list 'b:=* m:{A-Za-z}={a-zA-Z}'
fi
