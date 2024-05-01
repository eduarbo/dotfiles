alias py='python3'
alias pip='python3 -m pip'

if _is_callable python3; then
    alias python='python3'
    alias py3='python3'
fi

if _is_callable python2; then
    alias py2='python2'
fi

alias pye='pyenv'
alias ipy='ipython'
alias ipylab='ipython --pylab=qt5 --no-banner'

alias server='python -m SimpleHTTPServer'
