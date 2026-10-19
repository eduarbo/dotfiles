[dotfiles](../../README.md) › [emacs](README.md) › keybindings

# Emacs keybindings

What differs from Doom's Evil defaults, from [`doom/+bindings.el`](doom/+bindings.el).
`⌘` is Super (`s-`), the leader is `,` and the local leader is `⌘M`.

## Thumb keys

| Keys | Normal and visual | Insert |
| --- | --- | --- |
| `Return` | Jump to the matching pair | |
| `⇧Return` | Comment the line or region | |
| `⌃Return` | LSP code action | New line above |
| `⌘Return` | New line below | New line below; sends the prompt in GPTel |
| `⇧Space` | Search the project | Insert a tab |
| `Tab` | Toggle the fold, or pick a snippet for a selected line | Next snippet field, or complete |

## Motions and editing

| Keys | Action |
| --- | --- |
| `j` / `k` | Down / up by visual line |
| `↑` / `↓` | Up / down 10 visual lines |
| `←` / `→` | Start / end of the visual line; after `f`, `t` or a snipe they repeat it |
| `⇧↑` `⇧↓` `⇧←` `⇧→` | Drag the line or region |
| `H` / `L` | Previous / next error; `X` lists them |
| `;` / `:` | Ex command / evaluate an expression |
| `s` | Surround: an operator in normal mode, wraps the region in visual mode |
| `S` | Snipe to a two-character match |
| `v` / `V` in visual | Expand / contract the region |
| `⌫` / `⇧⌫` | Join with the previous line / split the line |
| `⌃A` / `⌃X` | Increment / decrement the number |
| `⌃,` / `⌃.` | Repeat the snipe backward / forward; previous / next snippet field |
| `⌃H` `⌃J` `⌃K` `⌃L` | Move between windows |
| `⌃F` `⌃B` `⌃D` in insert | Word forward / back, delete to the end of the line |

`⌃Z` is unbound, so Emacs state never traps you.

## `g` prefix

| Keys | Action |
| --- | --- |
| `gj` / `gk` | Jump to a line below / above |
| `go` / `gO` | Jump to a character, in this window / every window |
| `gs` / `gS` | Snipe forward / backward |
| `gn` | Narrow or widen |
| `gw` | Transpose words |
| `gC` | Comment box |
| `g.` | Last change |

## Super

| Key | Action | Key | Action |
| --- | --- | --- | --- |
| `⌘A` | Select all | `⌘N` | New buffer |
| `⌘B` | Jump to a bookmark | `⌘O` | Switch project |
| `⌘C` | Copy | `⌘P` | File tree in a side window |
| `⌘D` | File manager (Dirvish) | `⌘Q` | Quit |
| `⌘E` | `M-x` | `⌘R` | REPL; evaluates the region in visual mode |
| `⌘F` | Search in the buffer | `⌘S` | Save |
| `⌘G` | Magit status | `⌘T` | New workspace |
| `⌘H` | Describe a key | `⌘U` | Most recently used window |
| `⌘I` | ESLint fix and format | `⌘V` | Paste |
| `⌘J` | Last buffer | `⌘W` | Close the window or workspace |
| `⌘K` | Kill the buffer | `⌘X` / `⌘Z` | Scratch buffer / project scratch buffer |
| `⌘L` | GPTel menu | `⌘Y` | Paste from the kill ring |
| `⌘M` | Local leader | `⌘⇧Z` | Redo |

| Key | Action |
| --- | --- |
| `⌘[` / `⌘]` | Back / forward through buffers, help, Info or Git history |
| `⌃Tab` / `⌃⇧Tab` | Next / previous workspace |
| `⌘1`…`⌘9` | Go to a workspace |
| `⌘;` / `⌘,` / `⌘.` | Evaluate an expression / resume the last search / toggle the last popup |
| `⌘/` | Comment |
| `⌘=` / `⌘-` / `⌘0` | Bigger / smaller / reset font |

## Leader

Additions to Doom's leader map:

| Keys | Action |
| --- | --- |
| `, Space` / `, Return` / `, Tab` | Find a project file / open GPTel / switch workspace |
| `, ;` / `, :` / `, X` | `M-x` / evaluate / project scratch buffer |
| `, l` | LLM: add context, explain, rewrite, send, abort |
| `, k` | Workspaces: new, load, save, rename, restore, kill |
| `, y` | Copy: buffer, file name or path, directory, Org heading link |
| `, d` | Delete: buffer, buried or other buffers, workspace, bookmark, session |
| `, w b` / `, w o` / `, w w` | Balance windows / enlargen mode / pick a window |
| `, g b` / `, g B` | Magit blame / switch branch |
| `, c x` / `, c X` | Errors in the buffer / in the project |
| `, t e` / `, t l` | Toggle enlargen mode / line numbers |
| `, o d` / `, o /` | Dirvish / Dired |

## Minibuffer and file manager

| Where | Keys | Action |
| --- | --- | --- |
| Completion | `⇧Space` / `⇧Return` | Preview / export to an editable buffer |
| Completion | `Tab` / `⇧Tab` | Insert the candidate / go up a directory |
| Completion | `⌃.` / `⌃,` and `⌃L` / `⌃H` | Next / previous group and scroll |
| Minibuffer | `⌃Space` / `⌃Return` | Embark actions / export |
| Dired | `Space` | Toggle the mark |
| Dired | `⌘↓` / `⌘↑` | Open / go to the parent directory |
| Dirvish | `←` `→`, `⌫` `⌘⌫` | Back and forward in history |

Markdown buffers add insert commands under `⌘M` (bold, code, link, table, headings 1–6
and more).
