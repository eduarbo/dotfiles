# Emacs bindings Cheatsheet

## The Emacs notation

- `M-`: Meta (i.e. alt or option)
- `s-`: Super (i.e. command or windows key)
- `S-`: Shift
- `C-`: Ctrl

## Text Objects

| Key                                              | Description                                               |
|--------------------------------------------------|-----------------------------------------------------------|
| `'`, `` ` ``, `"`                                | Quotes                                                    |
| `a`                                              | C-style function arguments                                |
| `b`, `B`, `(`, `)`, `{`, `}`, `[`, `]`, `<`, `>` | Braces, parentheses, brackets                             |
| `c`                                              | Comments                                                  |
| `f`                                              | Functions                                                 |
| `g`                                              | Entire buffer                                             |
| `i`, `j`, `k`                                    | Indentation (variations include lines above and/or below) |
| `o`                                              | Symbols                                                   |
| `p`                                              | Paragraphs                                                |
| `q`                                              | Quotes of any kind                                        |
| `s`                                              | Sentences                                                 |
| `t`                                              | Tags                                                      |
| `u`                                              | URLs                                                      |
| `w`, `W`                                         | Words                                                     |
| `x`                                              | XML attributes                                            |

## Thumb keys

|     key | mode | description                                |
|--------:|:----:|--------------------------------------------|
|   `SPC` | nv   | Visualstar search forward                  |
|  `,SPC` | nv   | Find file in project                       |
| `C-SPC` | in   | LSP Rename                                 |
| `S-SPC` | nv   | Search in project                          |
| `S-SPC` | i    | Insert TAB                                 |
|   `RET` | m    | Jump between the matched pair              |
|  `,RET` | nv   | Open or create bookmark                    |
| `C-RET` | in   | LSP Code action                            |
| `S-RET` | nv   | Comment or uncomment line                  |
| `S-RET` | i    | Insert an indented new line above          |
|   `TAB` | nv   | Toggle fold                                |
|   `TAB` | i    | Indent the current line or autocomplete    |
| `S-TAB` | m    | Choose a snippet to expand                 |
| `S-TAB` | i    | Expand snippet or choose one from the list |

## Qwerty

|     key | mode | description                                               |
|--------:|:----:|-----------------------------------------------------------|
|     `↑` | nv   | Move cursor up 10 visual lines                            |
|     `↓` | nv   | Move cursor down 10 visual lines                          |
|     `→` | nv   | Move cursor to End of Line                                |
|     `←` | nv   | Move cursor to Beginning of Line                          |
|     `a` | n    | Append text after cursor                                  |
|     `A` | n    | Append text at end of line                                |
|     `b` | nv   | Move to beginning of previous word                        |
|     `B` | nv   | Move to beginning of previous WORD (includes punctuation) |
|     `c` | nv   | Change command                                            |
|     `C` | n    | Change to the end of line                                 |
|     `d` | nv   | Delete command                                            |
|     `D` | n    | Delete to the end of line                                 |
|     `e` | nv   | Move to end of word                                       |
|     `E` | nv   | Move to end of WORD (includes punctuation)                |
|     `f` | nv   | Find character after cursor in line                       |
|     `F` | nv   | Find character before cursor in line                      |
|     `g` | nv   | Additional go-to bindings                                 |
|     `G` | nv   | End of file                                               |
|     `h` | nv   | Move cursor left                                          |
|     `H` | nv   | Jumpt to previous error                                   |
|     `i` | n    | Insert before cursor                                      |
|     `I` | n    | Insert at start of line                                   |
|     `j` | nv   | Move cursor down                                          |
|     `J` | nv   | Join lines                                                |
|     `k` | nv   | Move cursor up                                            |
|     `K` | nv   | Show documentation                                        |
|     `l` | nv   | Move cursor right                                         |
|     `L` | nv   | Jumpt to next error                                       |
|     `m` | n    | Set mark                                                  |
|     `M` | nv   | Move the cursor to the middle line in the window          |
|     `n` | nv   | Repeat previous search                                    |
|     `N` | nv   | Repeat previous search in reverse direction               |
|     `o` | n    | Open line below and enter insert mode                     |
|     `O` | n    | Open line above and enter insert mode                     |
|     `p` | nv   | Paste after cursor                                        |
|     `P` | nv   | Paste before cursor                                       |
|     `q` | n    | Start/stop recording macro                                |
|     `Q` | m    | Execute last recorded keyboard macro                      |
|     `r` | n    | Replace single character under cursor                     |
|     `R` | n    | Switch to Replace state at point                          |
|     `R` | v    | Highlight all matches as multiedit regions                |
|     `s` | nv   | surround                                                  |
|     `S` | nv   | surround with additional new-lines                        |
|     `t` | nv   | Till before the character after cursor in line            |
|     `T` | nv   | Till before the character before cursor in line           |
|     `u` | n    | Undo last change                                          |
|     `u` | v    | lowercase                                                 |
|     `U` | v    | UPPERCASE                                                 |
|     `v` | nv   | Start/Expand selected region by semantic units            |
|     `V` | nv   | Linewise selection or Shrink selected region if expanded  |
|     `w` | nv   | Move to beginning of next word                            |
|     `W` | nv   | Move to beginning of next WORD (includes punctuation)     |
|     `x` | nv   | Delete character under cursor                             |
|     `X` | nv   | List errors in buffer                                     |
|     `y` | nv   | Yank                                                      |
|     `Y` | n    | Yank to End of Line                                       |
|     `z` | n    | Additional fold/scroll bindings                           |
|     `Z` | nv   | --                                                        |
|     `/` | nv   | Start a forward search                                    |
|     `;` | nv   | Ex command                                                |
|     `:` | nv   | Eval                                                      |
|     `~` | nv   | Invert case of character                                  |
|     `'` | nv   | Go to the line of the marker specified by CHAR            |
| `` ` `` | nv   | Go to the marker specified by CHAR                        |
|     `.` | nv   | Repeat last change                                        |
|     `"` | nv   | Use REGISTER for the next command                         |
|     `<` | nv   | Promote or indent line/region                             |
|     `>` | nv   | Demote or unindent line/region                            |
|     `=` | nv   | Indent line/region                                        |
|     `@` | nv   | Execute macro                                             |
|     `!` | nv   | Execute a shell command                                   |
|     `&` | nv   | Repeat last substitute command (same as `:s//~/`)         |
|   `BSP` | n    | Join line to previous                                     |
| `S-BSP` | n    | Split line                                                |

## Goto prefix

|      key | mode | description                                                             |
|---------:|:----:|:------------------------------------------------------------------------|
| `g<SPC>` | nv   | --                                                                      |
| `g<RET>` | nv   | --                                                                      |
| `g<TAB>` | nv   | --                                                                      |
|     `ga` | nv   | Print info on cursor position                                           |
|     `gb` | nv   | --                                                                      |
|     `gc` | nv   | Comments text                                                           |
|     `gd` | nv   | Goto local definition                                                   |
|     `ge` | nv   | End of previous word                                                    |
|     `gE` | nv   | End of previous WORD (includes punctuation)                             |
|     `gf` | nv   | Open file under cursor                                                  |
|     `gF` | nv   | Open file and line under cursor                                         |
|     `gg` | nv   | Start of file                                                           |
|     `gh` | nv   | --                                                                      |
|     `gi` | nv   | Insert mode at last insert location                                     |
|     `gj` | nv   | Goto visible line below the cursor                                      |
|     `gk` | nv   | Goto visible line above the cursor                                      |
|     `gl` | nv   | Align the text in the given region using CHAR                           |
|     `gm` | nv   | **CAN REBIND** Move the cursor to the middle of the current visual line |
|     `gn` | nv   | Narrow/widen region or block                                            |
|     `go` | nv   | Read one or many consecutive chars and jump to the first one            |
|     `gp` | nv   | Return to visual mode and reselect the last pasted region               |
|     `gq` | nv   | Fill text and move point to the end of the filled region                |
|     `gr` | nv   | Evaluate selection or sends it to the open REPL, if available           |
|     `gs` | m    | Jumps to the next 2-char match                                          |
|     `gS` | m    | Jumps to the previous 2-char match                                      |
|     `gt` | m    | **CAN REBIND** Switch to next workspace                                 |
|     `gT` | m    | **CAN REBIND** Switch to previous workspace                             |
|     `gu` | nv   | Lowercase                                                               |
|     `gU` | nv   | Uppercase                                                               |
|     `gv` | n    | Restore previous selection                                              |
|     `gw` | n    | Transpose words                                                         |
|     `gx` | m    | Exchange two regions                                                    |
|     `gy` | nv   | Yank the current selection or line without leading indentation          |
|     `gz` | nv   | Additional multiple cursors bindings                                    |
|     `g~` | nv   | Swap case                                                               |
|     `g?` | nv   | Caesar cipher rotate                                                    |
|     `g;` | nv   | Eval                                                                    |
|     `g,` | nv   | Go to last change backwards                                             |
|     `g.` | nv   | Go to last change                                                       |

## Control

|   key | mode       | description                                            |
|------:|:----------:|--------------------------------------------------------|
| `C-a` | nv         | Decrement number                                       |
| `C-a` | i          | Start of line _(after whitespace)_                     |
| `C-b` | m          | Scroll up in the window                                |
| `C-b` | i          | Previous word                                          |
| `C-c` |            |                                                        |
| `C-d` | m          | Scroll down in the window                              |
| `C-d` | i          | Delete to end of line                                  |
| `C-e` | m          | Scroll window one line up                              |
| `C-e` | i          | End of line                                            |
| `C-f` | m          | Scroll down one screen                                 |
| `C-f` | i          | Next word                                              |
| `C-g` | g          | Cancel                                                 |
| `C-h` | inv        | Move to left window                                    |
| `C-i` | nv         | Jump forward                                           |
| `C-i` | i          | Same as TAB                                            |
| `C-j` | inv        | Move to lower window                                   |
| `C-k` | inv        | Move to upper window                                   |
| `C-l` | inv        | Move to right window                                   |
| `C-m` | invc       | Same as Enter                                          |
| `C-n` | nv         | Switch to next kill-ring entry after pasting           |
| `C-n` | ic         | Auto-complete next match                               |
| `C-o` | nv         | Jump backward                                          |
| `C-o` | i          | Execute one command, return to Insert mode             |
| `C-p` | nv         | Switch to previous kill-ring entry after pasting       |
| `C-p` | ic         | Auto-complete previous match                           |
| `C-q` | inv        | **CAN REBIND** Insert CTRL character                   |
| `C-r` | inv        | Redo                                                   |
| `C-s` | inv        | **CAN REBIND** Incremental search forward              |
| `C-t` | nv         | **CAN REBIND** Create a new workspace                  |
| `C-t` | i          | Shift the current line COUNT times to the right        |
| `C-u` | nv         | Scroll half-page up                                    |
| `C-u` | i          | Delete to start of line                                |
| `C-v` | nv         | Visual block mode                                      |
| `C-v` | i          | **CAN REBIND** Insert CTRL character                   |
| `C-w` | nv         | Additional window bindings                             |
| `C-w` | i          | Delete word backward                                   |
| `C-x` | i          | Additional bindings                                    |
| `C-x` | nv         | Decrement number                                       |
| `C-y` | nv         | Scroll window one line down                            |
| `C-y` | i          | Copies the text of the same column from the line above |
| `C-z` | inv        | Suspend frame                                          |
| `C-;` | g          | Open an `embark-act` menu to chose a useful action     |
| `C-;` | evil-snipe | Quickly hop into `evil-easymotion` right after a snipe |
| `C-.` | inv        | Repeat the last evil-snipe                             |
| `C-,` | inv        | Repeat the inverse of the last evil-snipe              |

## Command

|     key | mode | description                                                    |
|--------:|:----:|----------------------------------------------------------------|
|   `s-a` | g    | Select whole buffer                                            |
|   `s-b` | -    | -                                                              |
|   `s-c` | g    | Copy                                                           |
|   `s-d` | g    | **CAN REBIND** Repeat incremental search backwards             |
|   `s-e` | g    | M-x                                                            |
|   `s-f` | g    | Search in file                                                 |
|   `s-g` | g    | Magit status                                                   |
|   `s-h` | g    | Describe key                                                   |
|   `s-i` | g    | Format region or buffer                                        |
|   `s-j` | g    | Switch to the last open buffer                                 |
|   `s-k` | g    | Kill current buffer                                            |
|   `s-l` | g    | Goto line                                                      |
|   `s-m` | g    | Minimize                                                       |
|   `s-n` | g    | New buffer                                                     |
|   `s-o` | g    | Open Project                                                   |
|   `s-p` | g    | Toggle Treemacs                                                |
|   `s-q` | -    | -                                                              |
|   `s-r` | g    | Open REPL                                                      |
|   `s-s` | g    | Save buffer                                                    |
|   `s-t` | g    | New workspace                                                  |
|   `s-u` | g    | Move to most recently used buffer                              |
|   `s-v` | g    | Paste                                                          |
|   `s-w` | g    | Close window                                                   |
|   `s-x` | g    | Localleader                                                    |
|   `s-y` | g    | Insert or Replace the active visual region with a yanked entry |
|   `s-z` | g    | Undo                                                           |
|   `s-;` | -    | -                                                              |
|   `s-,` | g    | Resume last search                                             |
|   `s-.` | g    | Toggle last popup                                              |
|   `s-[` | g    | Go back                                                        |
|   `s-]` | g    | Go forward                                                     |
|   `s-{` | g    | Previous workspace                                             |
|   `s-}` | g    | Next workspace                                                 |
| `s-RET` | g    | New indented line                                              |
| `s-SPC` | -    | -                                                              |
| `s-BSP` | g    | Raise popup window into a regular window                       |
