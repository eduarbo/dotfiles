path=( $XDG_CONFIG_HOME/emacs/bin "${path[@]}" )

# export EDITOR="emacsclient -c -nw"

# For better perfomance use plists for deserialization
# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
export LSP_USE_PLISTS=true
