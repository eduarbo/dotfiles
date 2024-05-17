alias ai='sudo apt install'
alias aiy='sudo apt install -y'
alias ar='sudo apt remove -y'
alias ac='sudo apt clean all && sudo dnf autoremove'

alias fd='fdfind'

alias sc='systemctl'
alias jc='journalctl'
alias ssc='sudo systemctl'

alias reboot='sudo systemctl reboot'
alias shutdown='sudo systemctl poweroff'
alias clear_cached_memory='sync; sudo sh -c "echo 3 > /proc/sys/vm/drop_caches"'

if _is_callable notify-send; then
  # Add an "alert" alias for long running commands.  Use like so:
  #   sleep 10; alert
  alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
fi

o() {
  if [ -n "$1" ]; then
    xdg-open "$@"
  else
    xdg-open .
  fi
}

# Clipboard pipes
if _is_callable xclip; then
  alias y='xclip -selection clipboard -in'
  alias p='xclip -selection clipboard -out'
elif _is_callable xsel; then
  alias y='xsel -i --clipboard'
  alias p='xsel -o --clipboard'
fi
