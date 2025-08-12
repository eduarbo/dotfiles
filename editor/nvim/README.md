# Vim Cheatsheet

## Add bindings
> ![IMPORTANT] sync with [My Doom Emacs Bindings](https://github.com/eduarbo/dotfiles/blob/main/editor/emacs/README.md)

- [ ]  `,SPC` | nv   | Find file in project
- [ ] `C-SPC` | in   | LSP Rename
- [ ] `S-SPC` | nv   | Search in project
- [ ]  `,RET` | nv   | Open or create bookmark
- [ ] `C-RET` | in   | LSP Code action
- [ ] `S-TAB` | m    | Choose a snippet to expand
- [ ] `S-TAB` | i    | Expand snippet or choose one from the list

## Exiting

|                |                               |
|:-------------- | ----------------------------- |
| `:q`           | Close file                    |
| `:qa`          | Close all files               |
| `:w`           | Save                          |
| `:wq` _/_ `:x` | Save and close file           |
| `ZZ`           | Save and quit                 |
| `ZQ`           | Quit without checking changes |

## Navigating

|                     |                             |
|:------------------- | --------------------------- |
| `h` _/_ `l`         | Left/Right                  |
| `j` _/_ `k`         | Down/Up _(Visual line)_     |
| `↓` _/_ `↑`         | Down/Up _(10 Visual lines)_ |
| `<C-U>` _/_ `<C-D>` | Half-page up/down           |
| `<C-B>` _/_ `<C-F>` | Page up/down                |

### Words
	
|              |                           |
|:------------ | ------------------------- |
| `b` _/_ `w`  | Previous/next subWord        |
| `ge` _/_ `e` | Previous/next end of subWord |
| `B` _/_ `W`  | Previous/next word        |
| `gE` _/_ `E` | Previous/next end of word |

### Line

|              |                                    |
|:------------ | ---------------------------------- |
| `0` _(zero)_ | Start of line                      |
| `H` _/_ `^`  | Start of line _(after whitespace)_ |
| `L` _/_ `$`  | End of line                        |

### Document

|             |                          |
|:----------- | ------------------------ |
| `gg`        | First line               |
| `G`         | Last line                |
| `:{number}` | Go to line `{number}`    |
| `{number}G` | Go to line `{number}`    |
| `{number}j` | Go down `{number}` lines |
| `{number}k` | Go up `{number}` lines   |

### Window

|      |         |                |
|:---- | --------|--------------- |
| `zz` | Center this line         |

|      |                          |
|:---- | ------------------------ |
| `zz` | Center this line         |
| `zt` | Top this line            |
| `zb` | Bottom this line         |
| `M`  | Move to middle of screen |

### Search

|                                                         |                                                   |
|:------------------------------------------------------- | ------------------------------------------------- |
| `n` _/_ `N`                                             | Next/Previous matching search pattern             |
| `*` _/_ `#`                                             | Next/Previous whole word under cursor             | 
| `f{char}` _/_ `F{char}`                                 | Move forward/backward to the given char           |
| `t{char}` _/_ `T{char}`                                 | Move forward/backward to before the given char    |
| `<Space>{c1}{c2}` _/_ `<S-Space>{c1}{c2}`               | Move forward/backward to the given 2-char pattern |
| `f→` _/_ `f←`, `t→` _/_ `t←`, `<Space>→` _/_ `<Space>←` | Repeat motion forward/backward                    |
| `→` _/_ `←`                                             | Next/Previous matching 2-char pattern pattern     |

### In insert mode

|                     |                                    |
|:------------------- | ---------------------------------- |
| `<C-A>`             | Start of line _(after whitespace)_ |
| `<C-E>`             | End of line                        |
| `<C-B>` _/_ `<C-F>` | Previous/next word                 |

## Editing

|         |                                     |
|:------- | ----------------------------------- |
| `a`     | Append                              |
| `A`     | Append from end of line             |
| `i`     | Insert                              |
| `o`     | Next line                           |
| `O`     | Previous line                       |
| `C`     | Delete until end of line and insert |
| `J`     | Join line below to the current one  |
| `K`     | Split line                          |
| `r`     | Replace one character               |
| `R`     | Enter Replace mode                  |
| `u`     | Undo changes                        |
| `<C-R>` | Redo changes                        |

### Coercion
Toggle the casing of the word under the cursor using the `cr{casing-char}` mapping (mnemonic: CoeRce)

|                 |                                                                        |
|:--------------- |:---------------------------------------------------------------------- |
| `crc`           | camelCase                                                              |
| `crp` _/_ `crm` | PascalCase/MixedCase                                                   |
| `cr_` _/_ `crs` | snake_case                                                             |
| `cru` _/_ `crU` | SNAKE_UPPERCASE                                                        |
| `crk` _/_ `cr-` | kebab-case (not usually reversible; see `abolish-coercion-reversible`) |
| `cr.`           | dot.case (not usually reversible; see `abolish-coercion-reversible`)   |

### In insert mode

|         |                         |
|:------- |:----------------------- |
| `<C-W>` | Delete word backward    |
| `<C-U>` | Delete to start of line |
| `<C-D>` | Delete to end of line   | 

## Exiting insert mode

|         |                                             |
|:------- |:------------------------------------------- |
| `Esc`   | Exit insert mode                            |
| `<C-C>` | Exit insert mode, and abort current command |

## Clipboard

|                 |                             |
|:--------------- |:--------------------------- |
| `x`             | Delete character            |
| `dd`            | Delete line _(Cut)_         |
| `yy`            | Yank line _(Copy)_          |
| `p`             | Paste                       |
| `P`             | Paste before                |
| `"*p` _/_ `"+p` | Paste from system clipboard |
| `"*y` _/_ `"+y` | Paste to system clipboard   |

## Visual mode

|         |                                                                                      |
|:--------|:-------------------------------------------------------------------------------------|
| `v`     | Enter visual mode _(continue pressing `v` to expand and `C-v` to shrink the region)_ |
| `V`     | Enter visual line mode                                                               |
| `<C-V>` | Enter visual block mode                                                              |

### In visual mode

|             |                         |
|:----------- |:----------------------- |
| `d` _/_ `x` | Delete selection        |
| `s`         | Surround selection      |
| `y`         | Yank selection _(Copy)_ |

See [Operators](https://devhints.io/vim#operators) for other things you can do.

## Find & Replace

|               |                                        |
|:------------- |:-------------------------------------- |
| :%s/foo/bar/g | Replace foo with bar in whole document |

## [#](https://devhints.io/vim#operators)Operators

### Usage

Operators let you operate in a range of text (defined by _motion_). These are performed in normal mode.

|          |        |
|:-------- |:------ |
| `d`      | `w`    |
| Operator | Motion |

### Operators list

|      |                                 |
|:---- |:------------------------------- |
| `d`  | Delete                          |
| `y`  | Yank _(copy)_                   |
| `c`  | Change _(delete then insert)_   |
| `>`  | Indent right                    |
| `<`  | Indent left                     |
| `=`  | Autoindent                      |
| `g~` | Swap case                       |
| `gU` | Uppercase                       |
| `gu` | Lowercase                       |
| `!`  | Filter through external program |
| `X`  | Exchange selection or current line |

See `:help operator`

### Examples

Combine operators with _motions_ to use them.

|                        |                                                   |
|:---------------------- |:------------------------------------------------- |
| `d`_d_                 | _(repeat the letter)_ Delete current line         |
| `d`_w_                 | Delete to next word                               |
| `d`_s"_                | Delete surrounding quotes                         |
| `c`_s"'_               | Change surrounding double quotes to single quotes |
| `d`_b_                 | Delete to beginning of word                       |
| _2_`dd`                | Delete 2 lines                                    |
| `d`_ip_                | Delete a text object _(inside paragraph)_         |
| _(in visual mode)_ `d` | Delete selection                                  |

See: `:help motion.txt`

## [#](https://devhints.io/vim#text-objects)Text objects

### Usage

Text objects let you operate (with an _operator_) in or around text blocks (_objects_).

|   |   |   |
|---|---|---|
|`v`|`i`|`p`|
|Operator|[i]nside or [a]round|Text object|

### Text objects

|                 |                       |
|:--------------- |:--------------------- |
| `p`             | Paragraph             |
| `w`             | Word                  |
| `s`             | Sentence              |
| `[` `(` `{` `<` | A [], (), or {} block |
| `'` `"` `` ` `` | A quoted string       |
| `b`             | A block \[(           |
| `B`             | A block in \[{        |
| `t`             | A XML tag block       |

### Examples

|             |                                    |
|:----------- |:---------------------------------- |
| `vip`       | Select paragraph                   |
| `vipipipip` | Select more                        |
| `yip`       | Yank inner paragraph               |
| `yap`       | Yank paragraph (including newline) |
| `dip`       | Delete inner paragraph             |
| `cip`       | Change inner paragraph             |

See [Operators](https://devhints.io/vim#operators) for other things you can do.

### Diff

|                                |                                       |
|:------------------------------ |:------------------------------------- |
| `gvimdiff file1 file2 [file3]` | See differences between files, in HMI |

## [#](https://devhints.io/vim#misc)Misc

### Folds

|                               |                              |
|:----------------------------- |:---------------------------- |
| `zo` _/_ `zO`                 | Open                         |
| `zc` _/_ `zC`                 | Close                        |
| `<S-Enter>` _/_ `za` _/_ `zA` | Toggle                       |
| `zv`                          | Open folds for this line     |
| `zM`                          | Close all                    |
| `zR`                          | Open all                     |
| `zm`                          | Fold more _(foldlevel += 1)_ |
| `zr`                          | Fold less _(foldlevel -= 1)_ |
| `zx`                          | Update folds                 |

Uppercase ones are recursive (eg, `zO` is open recursively).

### Navigation

|                 |                            |
|:--------------- |:-------------------------- |
| `<Tab>` _/_ `%` | Nearest/matching `{[()]}`  |
| `[(` `[{` `[<`  | Previous `(` or `{` or `<` |
| `])`            | Next                       |
| `[m`            | Previous method start      |
| `[M`            | Previous method end        |

### Jumping

|         |                              |
|:------- |:---------------------------- |
| `<C-O>` | Go back to previous location |
| `<C-I>` | Go forward                   |
| `gf`    | Go to file in cursor         |

### Counters

|                     |                            |
|:------------------- |:-------------------------- |
| `<C-A>` _/_ `<C-X>` | Increment/Decrement number |

### Case

|       |                                      |
|:----- |:------------------------------------ |
| `~`   | Toggle case (Case => cASE)           |
| `gU`  | Uppercase                            |
| `gu`  | Lowercase                            |
| `gUU` | Uppercase current line (also `gUgU`) |
| `guu` | Lowercase current line (also `gugu`) |

Do these in visual or normal mode.

### Marks

|             |                                                      |
|:----------- |:---------------------------------------------------- |
| `` `^ ``    | Last position of cursor in insert mode               |
| `` `. ``    | Last change in current buffer                        |
| `` `" ``    | Last exited current buffer                           |
| `` `0 ``    | In last file edited                                  |
| `''`        | Back to line in current buffer where jumped from     |
| ` `` `      | Back to position in current buffer where jumped from |
| `` `[ ``    | To beginning of previously changed or yanked text    |
| `` `] ``    | To end of previously changed or yanked text          |
| `` `< ``    | To beginning of last visual selection                |
| `` `> ``    | To end of last visual selection                      |
| `ma`        | Mark this cursor position as `a`                     |
| `` `a ``    | Jump to the cursor position `a`                      |
| `'a`        | Jump to the beginning of the line with position `a`  |
| `d'a`       | Delete from current line to line of mark `a`         |
| ``d`a``     | Delete from current position to position of mark `a` |
| `c'a`       | Change text from current line to line of `a`         |
| ``y`a``     | Yank text from current position to position of `a`   |
| `:marks`    | List all current marks                               |
| `:delm a`   | Delete mark `a`                                      |
| `:delm a-d` | Delete marks `a`, `b`, `c`, `d`                      |
| `:delm abc` | Delete marks `a`, `b`, `c`                           |

### IntelliSense

|                     |                                            |
|:------------------- |:------------------------------------------ |
| `<S-Space>`         | Quick suggestions                          |
| `<S-Tab>`           | Insert best suggestion                     |
| `<C-,>` _/_ `<C-.>` | Jump to next/prev snippet placeholder      |
| `<C-,>` _/_ `<C-.>` | Show next/prev inline suggestion           |
| `<S-Tab>`           | Dismiss inline suggestion                  |

### Misc

|             |                                           |
|:----------- |:----------------------------------------- |
| `.`         | Repeat last command                       |
| `]p`        | Paste under the current indentation level |
| `Enter`     | Line Comment _(Visual and normal mode)_   |
| `<S-Enter>` | Block Comment _(Visual and normal mode)_  |
