export NPM_CONFIG_USERCONFIG=$XDG_CONFIG_HOME/npm/config
export NODE_REPL_HISTORY=$XDG_CACHE_HOME/node/repl_history
export NODENV_ROOT=$XDG_DATA_HOME/nodenv
export NODENV_PLUGINS=$NODENV_ROOT/plugins
export BUN_INSTALL="$HOME/.bun"
export PNPM_HOME="${XDG_DATA_HOME}/pnpm"

path=($NODENV_ROOT/bin $BUN_INSTALL/bin $PNPM_HOME $path)
