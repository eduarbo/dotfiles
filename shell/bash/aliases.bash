# NOTE This script is also sourced in the zsh module

# Allow aliases to be sudo'ed
alias sudo='sudo '

# Clear the screen AND scrollback buffer
# Works for kitty, iTerm2 and maybe other terminals
alias cl='SIMPL_NEWLINE_BEFORE_PROMPT= && printf "\033[2J\033[3J\033[1;1H"'

alias mk='make'
alias q='exit'
alias d='docker'

alias wget='wget -c'  # Resume dl if possible
alias scp='rsync --rsh=ssh -CarvP'
alias ssh='TERM=xterm-256color ssh'


## Files & Directories

alias pd='popd'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
alias -- -='cd -'

alias ln='${aliases[ln]:-ln} -v'  # verbose ln

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
alias mv='mv -i'

# Securely Erase Files
alias shred='gshred -zuvn5'
alias vanish='shred'

# Always enable colored `grep` output
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias agg='ag -S --hidden --line-number'
alias rgg='rg -S --hidden --line-number'


## Misc

# Reload the shell (i.e. invoke as a login shell)
alias reload='exec $SHELL -l'
alias rl='reload'

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

# Intuitive map function
# For example, to list all directories that contain a certain file: find . -name
# .gitattributes | map dirname
alias map='xargs -n1'

alias dush='du -csh * | sort -h'

alias publicip='dig +short myip.opendns.com @resolver1.opendns.com'
alias localip='ipconfig getifaddr en1'
alias whois='whois -h whois-servers.net'

# Copy my public key to my clipboard
alias pubkey='more ~/.ssh/id_rsa.pub | pbcopy | echo "=> Public key copied to pasteboard"'

# quick way to serve a directory, very handy
alias server='python -m SimpleHTTPServer'

# Kill all the tabs in Chrome to free up memory
# [C] explained: http://www.commandlinefu.com/commands/view/402/exclude-grep-from-your-grepped-output-of-ps-alias-included-in-description
alias chromekill='ps ux | grep "[C]hrome Helper --type=renderer" | grep -v extension-process | tr -s ' ' | cut -d ' ' -f2 | xargs kill'

# Lists the 50 most used commands.
alias historystat='history 0 | awk "{print $2}" | sort | uniq -c | sort -n -r | head -n 50'

if [[ $OSTYPE == darwin* ]]; then
  alert() {
    osascript -e 'display notification "$*"'
  }
elif _is_callable notify-send; then
  # Add an "alert" alias for long running commands.  Use like so:
  #   sleep 10; alert
  alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
fi


# fasd & fzf change directory
j() {
  local dir
  dir="$(fasd -Rdl "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

# fd & fzf change directory
jj() {
  local dir
  dir="$(fd --type directory --follow --hidden "$1" | fzf -1 -0 --no-sort +m)" && cd "${dir}" || return 1
}

# Schedule sleep in X minutes, use like: sleep-in 60
sleep-in() {
  local minutes=$1
  local datetime=$(date -v+${minutes}M +"%m/%d/%y %H:%M:%S")
  sudo pmset schedule sleep "$datetime"
}

# Reload the shell and return the load time
loadtime() {
  export DISABLE_LOAD_TIME=1
  local times=${1:-1}
  for i in $(seq 1 "$times"); do
    /usr/bin/time $SHELL -lic exit;
  done
  unset DISABLE_LOAD_TIME
}

ix() {
  local opts
  local OPTIND
  [ -f "$HOME/.netrc" ] && opts='-n'
  while getopts ":hd:i:n:" x; do
    case $x in
      h) echo "ix [-d ID] [-i ID] [-n N] [opts]"; return;;
      d) echo curl $opts -X DELETE ix.io/$OPTARG; return;;
      i) opts="$opts -X PUT"; local id="$OPTARG";;
      n) opts="$opts -F read:1=$OPTARG";;
    esac
  done
  shift $(($OPTIND - 1))
  [ -t 0 ] && {
    local filename="$1"
    shift
    [ "$filename" ] && {
      curl $opts -F f:1=@"$filename" $* ix.io/$id
      return
    }
    echo "^C to cancel, ^D to send."
  }
  curl $opts -F f:1='<-' $* ix.io/$id
}
