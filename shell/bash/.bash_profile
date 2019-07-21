_init_path() {
  echo "LOADING BASH_PROFILE"
  # if running bash
  if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
      . "$HOME/.bashrc"
    fi
  fi
}

_init_path
