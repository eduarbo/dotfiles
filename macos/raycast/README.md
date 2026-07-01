# Raycast

Raycast stores everything in **encrypted SQLite** under
`~/Library/Application Support/com.raycast.macos/`, so it can't be symlinked
like the rest of the dotfiles. This topic instead versions a single **encrypted
snapshot** (`raycast.rayconfig`) and ships two helpers to move it around.

> The `.rayconfig` is AES-encrypted by Raycast with your **export passphrase**.
> Since this repo is public, that passphrase is the *only* thing protecting the
> file. Set a strong random one in **Raycast Settings → Advanced → Export** and
> store it in Bitwarden. Do **not** leave it at the `12345678` default.

## Commands

Both live in `bin/` and are on your `PATH` once the topic is enabled.

- `raycast-export` — launches Raycast's export, then copies the resulting
  `.rayconfig` into this topic. Pass a path to skip autodetection:
  `raycast-export ~/Downloads/foo.rayconfig`.
- `raycast-import` — points Raycast's import command at the snapshot (path copied
  to clipboard + revealed in Finder).

Neither is fully automatic: Raycast requires you to pick categories, choose the
file, and type the passphrase in its UI. The helpers only remove the busywork
around that.

## New machine

1. Enable the topic: `dot macos/raycast` (installs Raycast if missing).
2. Install the rest of your apps (`dot macos/apps`) so aliases/hotkeys resolve.
3. `raycast-import`, then enter the passphrase from Bitwarden.

## Not included in the export

Per Raycast's migration guide, the snapshot does **not** carry clipboard
history, screenshots, or the emoji picker's custom keywords. Everything else
(settings, extensions, hotkeys, aliases, snippets, quicklinks, floating notes,
script directories…) is included.
