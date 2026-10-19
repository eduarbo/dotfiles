[dotfiles](../../README.md) › editor › emacs

# Emacs

[Doom Emacs](https://github.com/doomemacs/doomemacs) with Evil, tuned to my keyboard
layout. On macOS it installs `emacs-plus@30`; on Debian, the distribution's Emacs. Both
get the language servers, linters, fonts and Dired previews that the config uses.

```sh
dot editor/emacs
```

- **[Keybindings →](KEYBINDINGS.md)** The leader is `,` and the local leader is `⌘M`.
- **Config:** [`doom/`](doom) is linked to `~/.config/doom`; Doom itself is cloned to
  `~/.config/emacs` and `doom` is on the `PATH`.
- **Snippets** come from a Git submodule. Clone with `--recurse-submodules`, or run
  `git submodule update --init` before enabling this topic.

## Shell helpers

| Command | Does |
| --- | --- |
| `e FILE` | Open in the running Emacs, or in a terminal Emacs if none is running |
| `ediff A B` | Compare two files with ediff |
| `eman PAGE` | Read a man page in Emacs |
| `ekill` | Stop the Emacs server |

## Forge tokens

Forge, Magit's GitHub and GitLab helper, reads tokens from `~/.config/doom/authinfo.gpg`.
Git ignores that file, so it never reaches this repository:

1. Copy [`doom/authinfo.gpg.example`](doom/authinfo.gpg.example) to `doom/authinfo.gpg`.
2. Open it in Emacs, add your tokens and save; Emacs encrypts it with GPG.
