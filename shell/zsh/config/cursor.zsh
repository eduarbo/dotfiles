# Use a steady beam in insert mode, steady block in normal mode
function _cursor_vi_mode() {
  if [[ $KEYMAP = vicmd ]]; then
    # Normal mode: block
    print -n '\e[2 q'
  else
    # Insert mode: beam
    print -n '\e[6 q'
  fi
}

function zle-keymap-select() { _cursor_vi_mode }
zle -N zle-keymap-select

function zle-line-init() {
  zle -K viins
  print -n '\e[6 q'   # start with beam
}
zle -N zle-line-init

function zle-line-finish() {
  print -n '\e[2 q'   # fall back to block when the line finishes
}
zle -N zle-line-finish
