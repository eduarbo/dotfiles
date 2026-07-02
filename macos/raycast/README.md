# Raycast

Raycast stores everything in **encrypted SQLite** under
`~/Library/Application Support/com.raycast.macos/`, so it can't be symlinked
like the rest of the dotfiles. This topic instead versions a single **encrypted
snapshot** (`raycast.rayconfig`) and ships two helpers to move it around.

> The `.rayconfig` is AES-encrypted by Raycast with your **export password**.
> Since this repo is public, that password is the *only* thing protecting the
> file — use a strong random one (≥20 chars) and keep it in Bitwarden. Raycast
> asks you to set it the first time you export (min 8 chars).

## Commands

Both live in `bin/` and are on your `PATH` once the topic is enabled.

- `raycast-export` — launches Raycast's export, then copies the resulting
  `.rayconfig` into this topic. Pass a path to skip autodetection:
  `raycast-export ~/Downloads/foo.rayconfig`.
- `raycast-import` — points Raycast's import command at the snapshot (path copied
  to clipboard + revealed in Finder).

Neither is fully automatic: on export you tick the categories, set/enter the
password, and choose where to save; on import you choose the file, tick which
categories to restore, and enter the password. The helpers only remove the
busywork around that.

## New machine

1. Enable the topic: `dot macos/raycast` (installs Raycast if missing).
2. Install the rest of your apps (`dot macos/apps`) so aliases/hotkeys resolve.
3. `raycast-import`, then enter the passphrase from Bitwarden.

## What's in the snapshot

The export dialog has a checkbox per category, so you pick what goes in. By
default everything is ticked: settings/aliases/hotkeys, extensions, AI
chats/presets/commands, Clipboard History, MCP servers, quicklinks, Focus
categories, notes, script directories, and more. Screenshots are never exported.

Heads-up for a public repo: **Clipboard History** can hold copied
passwords/tokens. Simplest fix — **untick "Clipboard History"** in the export
dialog so it never enters the snapshot. (Even if left in, it's encrypted behind
your export password.)
