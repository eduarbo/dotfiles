eval "$(nodenv init -)"

if _is_callable yarn; then
    path=($(yarn global bin 2>/dev/null) $path)
fi
