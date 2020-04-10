# NOTE This script is also sourced in the zsh module

# Allow aliases to be sudo'ed
alias sudo='sudo '

# Clear the screen AND scrollback buffer
# Works for kitty, iTerm2 and maybe other terminals
alias cl='SIMPL_NEWLINE_BEFORE_PROMPT= && printf "\033[2J\033[3J\033[1;1H"'

alias mk='make'
alias q='exit'

alias wget='wget -c'  # Resume dl if possible
alias scp='rsync --rsh=ssh -CarvP'
alias ssh='TERM=xterm-256color ssh'

alias ls="ls -Fh --color --group-directories-first"


## Files & Directories

alias pd='popd'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias -- -='cd -'

alias ln='ln -v'  # verbose ln

alias l='ls -1'
alias ll='ls -l' # List all files in long format
alias la='ls -lA' # List all files in long format, including dot files
alias lsd='ls -l | grep "^d"' # List only directories

alias mkdir='mkdir -p'
alias md='mkdir -p'
alias rd='rmdir'
alias RM='rm -vrf'

# notify me before clobbering files
alias rm='rm -i'
alias cp='cp -i'

# Securely Erase Files
alias shred='shred -zuvn5'
alias vanish='shred'

# Always enable colored `grep` output
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias agg='ag -S --hidden --line-number'
alias rgg='rg -S --hidden --line-number'


## Misc

alias gurl='curl --compressed'
alias rsyncd='rsync -va --delete'   # Hard sync two directories

alias encrypt='gpg --encrypt'
alias decrypt='gpg --decrypt'
alias sign='gpg --sign'
alias verify='gpg --verify'

alias pathlist='tr : "\n" <<<$PATH'

alias please='sudo $(fc -ln -1)'  # rerun last command with sudo, please!
alias mine='sudo chown -R $USER:$GROUPS'
alias fuck='killall -9'

alias top='top -o cpu'

alias d='docker'
alias dc='docker-compose'

# Intuitive map function
# For example, to list all directories that contain a certain file: find . -name
# .gitattributes | map dirname
alias map='xargs -n1'

alias dush='du -csh * | sort -h'

alias publicip='dig +short myip.opendns.com @resolver1.opendns.com'
alias localip='ipconfig getifaddr en1'
alias whois='whois -h whois-servers.net'
alias ipinfo="curl ipinfo.io/json"

# FIXME make it work on Debian/Linux
# Copy my public key to my clipboard
alias pubkey='more ~/.ssh/id_rsa.pub | pbcopy | echo "=> Public key copied to pasteboard"'

# quick way to serve a directory, very handy
alias server='python -m SimpleHTTPServer'

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill='ps ux | grep "[C]hrome Helper --type=renderer" | grep -v extension-process | tr -s " " | cut -d " " -f2 | xargs kill'

# Reload the current shell
alias reload='exec $CURRENT_SHELL -l'
alias rl='reload'

# Reload the current shell and return the load time
loadtime() {
  export DISABLE_LOAD_TIME=1
  local times=${1:-1}

  for _ in $(seq 1 "$times"); do
    # FIXME wrong format in linux
    /usr/bin/time "$CURRENT_SHELL" -lic exit;
  done
  unset DISABLE_LOAD_TIME
}
alias lt=loadtime

# Lists the 50 most used commands.
historystat() {
  history 0 | awk "{print $2}" | sort | uniq -c | sort -n -r | head -n 50
}

rvu() {
  local offset=${2:=0}
  local output

  output=$($output | md5)
  output=${output:$offset:15}
  echo "$output" | y
}

rvp() {
  echo "$1" | md5 | y
}
