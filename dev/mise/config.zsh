# Activate mise with caching and stderr suppression during init.
# We can't use `_cache` here because we need to suppress stderr from both the
# initial `_mise_hook` call (inside the cached file) and the first
# `_mise_hook_precmd` invocation — both run within p10k's instant prompt capture
# window and would trigger the "console output during zsh initialization" warning.
if _is_callable mise; then
  local cache_dir="$XDG_CACHE_HOME/zsh"
  local cache="$cache_dir/mise"

  if [[ ! -f "$cache" || ! -s "$cache" ]]; then
    mkdir -p "$cache_dir"
    mise activate zsh >| "$cache"
  fi

  source "$cache" 2>/dev/null

  # Wrap the precmd hook so its first invocation also suppresses stderr
  # (p10k is still capturing at that point). Restore the original after.
  if (( $+functions[_mise_hook_precmd] )); then
    functions[_mise_hook_precmd_real]=$functions[_mise_hook_precmd]
    _mise_hook_precmd() {
      _mise_hook_precmd_real 2>/dev/null
      functions[_mise_hook_precmd]=$functions[_mise_hook_precmd_real]
      unset -f _mise_hook_precmd_real
    }
  fi
fi
