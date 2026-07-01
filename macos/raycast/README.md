# Raycast

Raycast stores everything in **encrypted SQLite** under
`~/Library/Application Support/com.raycast.macos/`, so it can't be symlinked
like the rest of the dotfiles. This topic instead versions a single **encrypted
snapshot** (`raycast.rayconfig`) and ships two helpers to move it around.

> The `.rayconfig` is AES-encrypted by Raycast with your **export passphrase**.
> Since this repo is public, that passphrase is the *only* thing protecting the
> file. Set a strong random one in **Raycast Settings (⌘,) → Advanced → Export**
> — it's a field you fill in there, not a popup (the inline prompt only shows up
> the first time you run *Export Settings & Data*). Store it in Bitwarden and
> don't leave it at the `12345678` default.

## Commands

Both live in `bin/` and are on your `PATH` once the topic is enabled.

- `raycast-export` — launches Raycast's export, then copies the resulting
  `.rayconfig` into this topic. Pass a path to skip autodetection:
  `raycast-export ~/Downloads/foo.rayconfig`.
- `raycast-import` — points Raycast's import command at the snapshot (path copied
  to clipboard + revealed in Finder).

Neither is fully automatic: on export you choose where to save and (first time)
set the passphrase; on import you choose the file, tick which categories to
restore, and type the passphrase. The helpers only remove the busywork around
that.

## New machine

1. Enable the topic: `dot macos/raycast` (installs Raycast if missing).
2. Install the rest of your apps (`dot macos/apps`) so aliases/hotkeys resolve.
3. `raycast-import`, then enter the passphrase from Bitwarden.

## What's in the snapshot

The export bundles all 11 categories into the encrypted file and you **can't
deselect any at export time** (category selection only happens on import).
Included: settings/aliases/hotkeys, extensions, snippets, quicklinks, notes,
window layouts, MCP servers, AI chats/commands, **Clipboard History**, and
Emoji & Symbol History. Only Screenshots are left out.

Heads-up for a public repo: your Clipboard History (which can hold copied
passwords/tokens) rides inside the encrypted snapshot and can't be excluded on
export. The strong passphrase is what protects it — if that worries you, clear
it first (Raycast → Clipboard History → `Delete All Entries`) before exporting.
