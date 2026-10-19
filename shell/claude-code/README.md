[dotfiles](../../README.md) › shell › claude-code

# Claude Code

Installs [Claude Code](https://claude.com/claude-code) and a status line that matches the
Powerlevel10k prompt.

```sh
dot shell/claude-code
```

On macOS it installs `jq` and the stable Homebrew cask when `claude` is missing; on Debian
it installs `jq` and asks you to install Claude Code from an official package. Both link
[`settings.json`](settings.json) and [`statusline.sh`](statusline.sh) into `~/.claude/`.

## Status line

```text
in 󱚟 Opus 5.5 at 📁 dotfiles [━━━━━━━─── 75%] $0.35
```

It shows the model, the current directory, how full the context window is and the session
cost. The context bar turns from cyan to yellow to red as it fills, using Powerlevel10k's
8-color palette. Edit `statusline.sh` to change what it shows.

## Sign-in stays local

Run `claude` and complete the browser login on each machine, and turn off usage credits
or overage before enabling Claude in Personal Ops. Never copy `~/.claude` auth or session
files between computers.
