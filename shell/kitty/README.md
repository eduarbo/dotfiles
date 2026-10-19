[dotfiles](../../README.md) › shell › kitty

# kitty

My terminal: JetBrains Mono, the Debussy theme, Vim-aware splits and scrollback in
Neovim. `⌘` is kitty's modifier for everything below.

```sh
dot shell/kitty
```

## Keys

| Keys | Action |
| --- | --- |
| `⌘N` / `⌘W` | New split in the same directory / close it |
| `⌃H` `⌃J` `⌃K` `⌃L` | Move between splits; inside Vim they reach Vim instead |
| `⌘Z` | Zoom the current split |
| `⌘L` / `⌘;` | Next layout / last used layout |
| `⌘T` / `⌘K` | New tab / close tab |
| `⌘1`…`⌘9` / `⌘J` | Go to a tab / previous tab |
| `⌘F` | Open the scrollback in Neovim |
| `⌘G` | Open the last command's output in Neovim |

The rest lives in [`config/bindings.conf`](config/bindings.conf).

## Remote control for local scripts

kitty listens on a Unix socket in a private, mode-`700` directory and ignores control
sequences sent through the terminal or over SSH. Scripts drive it with `kitty-remote`,
which takes the same arguments as `kitty @`:

```sh
kitty-remote ls
```

It uses the current kitty connection when there is one and otherwise finds your own
socket. This does not lift restrictions that external automation tools impose.
