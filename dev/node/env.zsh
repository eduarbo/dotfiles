# XDG-compliant paths for npm/pnpm/bun
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/config"
export NODE_REPL_HISTORY="$XDG_CACHE_HOME/node/repl_history"
export PNPM_HOME="$XDG_DATA_HOME/pnpm"
export BUN_INSTALL="$XDG_DATA_HOME/bun"

path=( $BUN_INSTALL/bin $PNPM_HOME $path )

# bun completions
_source "$BUN_INSTALL/_bun"
