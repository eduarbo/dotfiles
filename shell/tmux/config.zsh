if (( $+functions[_is_callable] && $+functions[_cache] )) && _is_callable tmuxifier tmux; then
  _cache tmuxifier init -
fi
