alias y='pbcopy'
alias p='pbpaste'

alias b='brew'
alias bi='brew install'
alias br='brew remove'
alias bu='brew update && brew outdated && brew upgrade && brew cleanup'

alert() {
  osascript -e 'display notification "$*"'
}

o() {
  if [ -n "$1" ]; then
    open "$@"
  else
    open .
  fi
}

# Empty the Trash on all mounted volumes and the main HDD
# Also, clear Apple’s System Logs to improve shell startup speed
alias emptytrash='sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl'

# Get macOS Software Updates and update Homebrew packages
alias update='sudo softwareupdate -i -a; brew update; brew upgrade; brew cleanup'

# Recursively delete `.DS_Store` files
alias cleanup='find . -type f -name "*.DS_Store" -ls -delete'

alias unfuckcamera='sudo killall VDCAssistant'

# OS X has no `md5sum`, so use `md5` as a fallback
_is_callable md5sum  || alias md5sum='md5'
# OS X has no `sha1sum`, so use `shasum` as a fallback
_is_callable sha1sum || alias sha1sum='shasum'

# Kill orphan Node processes spawned by Cursor (usage: cursor-nuke)
cursor-nuke() {
  echo "Node processes spawned by Cursor:"
  local pids=$(pgrep -f "Cursor Helper" | xargs -I {} pgrep -P {} -f node 2>/dev/null | sort -u)
  if [ -z "$pids" ]; then
    echo "None. Clean."
    return
  fi
  ps -p $(echo $pids | tr ' ' ',') -o pid,ppid,etime,command
  echo ""
  read "?Kill all? [y/N] " c
  [[ "$c" == "y" || "$c" == "Y" ]] && echo $pids | xargs kill
}

# Diagnose and fix DHCP when Wi-Fi can't get an IP
net-doctor() {
  echo "=== Processes binding UDP:68 (DHCP client port) ==="
  local offenders
  offenders=$(sudo lsof -nP -iUDP:68 2>/dev/null | awk 'NR>1 && $1 != "configd"')

  if [ -z "$offenders" ]; then
    echo "Nothing suspicious on port 68. Issue is elsewhere."
    echo "Forcing DHCP renewal anyway..."
    sudo ipconfig set en0 DHCP
    return
  fi

  echo "$offenders"
  echo ""
  read "?Kill these processes and renew DHCP? [y/N] " confirm
  if [[ "$confirm" == "y" || "$confirm" == "Y" ]]; then
    echo "$offenders" | awk '{print $2}' | sort -u | while read pid; do
      echo "Killing PID $pid..."
      kill "$pid" 2>/dev/null || sudo kill "$pid"
    done
    sleep 2
    sudo ipconfig set en0 DHCP
    sleep 3
    echo ""
    echo "=== Current state ==="
    ifconfig en0 | grep 'inet '
  fi
}
