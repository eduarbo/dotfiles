export PIP_CONFIG_FILE="$XDG_CONFIG_HOME/pip/pip.conf"
export PIP_LOG_FILE="$XDG_DATA_HOME/pip/log"
export PYLINTHOME="$XDG_DATA_HOME/pylint"
export PYLINTRC="$XDG_CONFIG_HOME/pylint/pylintrc"
export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"
export IPYTHONDIR="$XDG_CONFIG_HOME/ipython"
export PYENV_ROOT="$HOME/.pyenv"

command -v pyenv >/dev/null || path=( "$PYENV_ROOT/bin" "${path[@]}" )
eval "$(pyenv init - --no-rehash)"