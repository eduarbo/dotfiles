if [[ $(_os) == macos ]]; then
  alias emacs='emacs -nw'
  alias e='emacs'
else
  alias e='emacsclient -n'
fi

ediff() {
  e --eval "(ediff-files \"$1\" \"$2\")";
}
