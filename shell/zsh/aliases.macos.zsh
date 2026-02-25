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
