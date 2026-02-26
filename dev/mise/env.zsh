# XDG-compliant paths for mise
export MISE_DATA_DIR="$XDG_DATA_HOME/mise"
export MISE_CACHE_DIR="$XDG_CACHE_HOME/mise"
export MISE_CONFIG_DIR="$XDG_CONFIG_HOME/mise"

# Auto-trust mise config files under ~/dev
export MISE_TRUSTED_CONFIG_PATHS="$HOME/dev"

# On Linux, mise installs to ~/.local/bin when using the curl installer
path=( "$XDG_BIN_HOME" $path )
