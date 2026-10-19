[dotfiles](../../README.md) › editor › nvim

# Neovim

A light Neovim for quick edits, `git` messages and man pages, with the same muscle memory
as my [Emacs keybindings](../emacs/KEYBINDINGS.md) where it matters. It is `$EDITOR`,
`$VISUAL` and `$MANPAGER`, and `v` and `vim` open it.

```sh
dot editor/nvim
```

Plugins load lazily through lazy.nvim from [`config/init.lua`](config/init.lua): leap,
surround, abolish, exchange, matchup, wordmotion, targets, expand-region, visual-multi,
Comment, Telescope, the kitty navigator and kitty scrollback, with the Catppuccin theme.
The leader is `,`.

## Keys

| Keys | Action |
| --- | --- |
| `j` / `k` | Down / up by visual line |
| `↑` / `↓` | Up / down 10 visual lines |
| `←` / `→` | Start / end of the line |
| `⇧↑` / `⇧↓` | Move the line or selection |
| `Return` | Jump to the matching pair |
| `⇧Return` | Comment the line or selection |
| `Space` | Search the word under the cursor, or the selection |
| `Tab` | Toggle the fold; creates one in visual mode |
| `S` | Leap to a two-character match; `⌃.` / `⌃,` repeat it |
| `f` `F` `t` `T` | Clever motions: press again to repeat |
| `;` | Command line |
| `⌫` | Join with the next line |
| `<` / `>` | Indent, keeping the selection in visual mode |
| `v` / `V` in visual | Expand / shrink the selection |
| `⇧R` | Select every match of the word (multiple cursors) |
| `cr` + `c` `s` `u` `k` `.` | Change the word to camelCase, snake_case, UPPER_CASE, kebab-case or dot.case |
| `,,` / `,.` | Find a buffer / a file |
| `,w` + `s` `v` `d` | Split / vertical split / close the window |
| `,t` + `l` `r` `w` | Toggle line numbers / relative numbers / wrap |
| `⌃H` `⌃J` `⌃K` `⌃L` | Move between windows, and out to kitty splits |

In insert mode, `⌃A` `⌃E` go to the start or end of the line, `⌃F` `⌃B` move by word,
`⌃D` deletes to the end of the line, `⇧Return` / `⌃Return` open a line above / below and
`⇧Space` inserts a tab. The command line has the same `⌃A` `⌃E` `⌃F` `⌃B`.

## Still different from Emacs

| Action | Emacs | Neovim |
| --- | --- | --- |
| Find a project file | `, Space` | `,.` |
| Search the project | `⇧Space` | — |
| New line above / below in insert | `⌃Return` / `⌘Return` | `⇧Return` / `⌃Return` |
| Repeat `f` or `t` | `→` / `←` | Press the motion again |
| Join lines with `⌫` | With the previous line | With the next line |

For plain Vim motions and operators, see the [Vim cheatsheet on devhints](https://devhints.io/vim).
