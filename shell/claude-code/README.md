# Claude Code Configuration

Status line configuration for [Claude Code](https://claude.com/claude-code) CLI, integrated with Powerlevel10k theme.

## What's included

- **statusline.sh**: Custom status line script that displays:
  - Current AI model (e.g., 󱚟 Sonnet 4.5)
  - Current directory (📁 dotfiles)
  - Context usage with visual progress bar
  - Session cost
  - Color-coded warnings based on context usage (cyan → yellow → red)

- **settings.json**: Claude Code settings that enable the custom status line

## Installation

```sh
deploy shell/claude-code
```

This will:
1. Install `jq` (required for JSON parsing)
2. Create symlinks from this directory to `~/.claude/`

## Customization

Edit `statusline.sh` to customize:
- Colors and formatting
- Information displayed
- Progress bar style
- Context usage thresholds

The status line uses Powerlevel10k's 8-color palette for consistency with your zsh theme.

## Status Line Preview

```
in 󱚟 Sonnet 4.5 at 📁 dotfiles [━━━━━━━─── 75%] $0.35
```

Colors are applied automatically based on your terminal theme.
