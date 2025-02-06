# ─── General Settings ──────────────────────────────────────────────────────────

# Define which characters are considered part of a word
WORDCHARS='*?[]~&.;!#$%^(){}<>'

# When using Ctrl+w, treat slashes, dots, and some other characters as delimiters
autoload -U select-word-style
select-word-style bash

# Enable/Disable specific shell behaviors
unsetopt BRACE_CCL          # Disable brace character class list expansion
setopt COMBINING_CHARS      # Combine zero-length punctuation (accents) with base characters
setopt RC_QUOTES            # Allow "Henry''s Garage" instead of "Henry'\''s Garage"
setopt HASH_LIST_ALL        # Include all matches when completing
unsetopt CORRECT_ALL        # Disable automatic spelling correction
unsetopt NOMATCH            # Prevent errors when a glob pattern doesn't match anything
unsetopt MAIL_WARNING       # Don't show warnings for mail file access
unsetopt BEEP               # Disable terminal beep sound
setopt IGNOREEOF            # Prevent accidental shell exit via Ctrl+D


# ─── Job Control & Process Management ──────────────────────────────────────────

setopt LONG_LIST_JOBS       # Show job list in long format
setopt AUTO_RESUME          # Try to resume an existing job before starting a new one
setopt NOTIFY               # Show job status updates immediately
unsetopt BG_NICE            # Don't lower priority of background jobs
unsetopt HUP                # Don't send SIGHUP to background jobs when the shell exits
unsetopt CHECK_JOBS         # Don't warn about running jobs when exiting the shell


# ─── Command History and Execution Control ─────────────────────────────────────

HISTFILE="$ZSH_DATA_HOME/zhistory"  # Location of history file
HISTSIZE=10000                      # Max number of events stored in memory
SAVEHIST=10000                      # Max number of events stored in the history file

# Avoid storing failed commands in history
zshaddhistory() { whence ${${(z)1}[1]} >| /dev/null || return 1 }

# Configure history behavior
setopt BANG_HIST                # Don't treat '!' specially during expansion
setopt EXTENDED_HISTORY         # Store timestamps and execution times in history
setopt APPEND_HISTORY           # Append history instead of overwriting it
setopt INC_APPEND_HISTORY       # Write commands to history file as they are entered
setopt SHARE_HISTORY            # Synchronize history between multiple Zsh sessions
setopt HIST_EXPIRE_DUPS_FIRST   # Remove older duplicate entries first when trimming history
setopt HIST_IGNORE_DUPS         # Don't record duplicate consecutive commands
setopt HIST_IGNORE_ALL_DUPS     # Remove all previous duplicate commands
setopt HIST_FIND_NO_DUPS        # Prevent duplicate entries from appearing in history search
setopt HIST_IGNORE_SPACE        # Ignore commands that start with a space
setopt HIST_SAVE_NO_DUPS        # Don't write duplicates to the history file
setopt HIST_VERIFY              # Show history expansion before executing
unsetopt HIST_BEEP              # Don't beep when accessing a non-existent history entry


# ─── Directory Navigation ──────────────────────────────────────────────────────

DIRSTACKSIZE=9                  # Max number of directories in the stack

setopt AUTO_CD                  # Change directory without using 'cd'
setopt AUTO_PUSHD               # Push old directory onto the stack when changing directories
setopt PUSHD_IGNORE_DUPS        # Avoid duplicate entries in directory stack
setopt PUSHD_SILENT             # Suppress directory stack output after pushd/popd
setopt PUSHD_TO_HOME            # Go to home directory when pushd/popd is used without arguments
setopt CDABLE_VARS              # Allow changing to directories stored in variables
setopt MULTIOS                  # Allow writing to multiple outputs
setopt EXTENDED_GLOB            # Enable extended globbing (for complex filename matching)
setopt INTERACTIVE_COMMENTS     # Allow comments in interactive shell sessions
unsetopt GLOB_DOTS              # Prevent globbing from matching dotfiles by default
unsetopt AUTO_NAME_DIRS         # Don't auto-assign variables for frequently used directories


# ─── Terminal Behavior ─────────────────────────────────────────────────────────

# Prevent pasted text from getting a different background color
unset zle_bracketed_paste

export PS2=$'%F{8}❭%f '
