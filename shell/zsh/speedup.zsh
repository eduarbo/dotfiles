# ┏━┓┏━┓┏━╸┏━╸╺┳┓   ╻ ╻┏━┓   ╺━┓┏━┓╻ ╻   ╻  ┏━┓┏━┓╺┳┓
# ┗━┓┣━┛┣╸ ┣╸  ┃┃   ┃ ┃┣━┛   ┏━┛┗━┓┣━┫   ┃  ┃ ┃┣━┫ ┃┃
# ┗━┛╹  ┗━╸┗━╸╺┻┛   ┗━┛╹     ┗━╸┗━┛╹ ╹   ┗━╸┗━┛╹ ╹╺┻┛
# Speed up zsh load


# Compinit should be called after loading of all plugins and before possibly calling cdreplay
autoload -Uz compinit

# Load and initialize the completion system with a cache time of 20 hours, so it
# should almost always regenerate the first time a shell is opened each day.
# The globbing is a little complicated here:
#
# - '#q' is an explicit glob qualifier that makes globbing work within zsh's [[ ]] construct.
# - 'N' makes the glob pattern evaluate to nothing when it doesn't match (rather than throw a globbing error)
# - '.' matches "regular files"
# - 'mh+20' matches files (or directories or whatever) that are older than 20 hours.
if [[ -n $ZINIT[ZCOMPDUMP_PATH](#qN.mh+20) ]]; then
	compinit -d $ZINIT[ZCOMPDUMP_PATH];
  # update the timestamp on compdump file
  compdump
else
	compinit -C -d $ZINIT[ZCOMPDUMP_PATH];
fi;

# Execute code that does not affect the current session in the background.
{
  # Compile the completion dump to increase startup speed
  if [[ "${ZINIT[ZCOMPDUMP_PATH]}" -nt "${ZINIT[ZCOMPDUMP_PATH]}.zwc" || ! -s "${ZINIT[ZCOMPDUMP_PATH]}.zwc" ]]; then
    zcompile "${ZINIT[ZCOMPDUMP_PATH]}"
  fi
} &!

# execute compdefs provided by rest of plugins
zinit cdreplay -q # -q is for quiet
