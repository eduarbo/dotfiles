" Sane defaults
" ------------------------------------------------------------------------------

set clipboard=unnamed           " Yank to system clipboard
set iskeyword+=-                " Treat hyphens as part of the word
set nostartofline               " Make j/k respect the columns
set scrolloff=10                " Start scrolling n lines before horizontal border of window
set gdefault                    " RegExp global by default
set magic                       " Enable extended regexes.
set hlsearch                    " highlight searches
set incsearch                   " show the `best match so far' astyped
set ignorecase                  " case-insensitive search
set smartcase                   " case-sensitive when upper-case letters are present


" Plugins
" ------------------------------------------------------------------------------

" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    \| PlugInstall --sync | source $MYVIMRC
  \| endif

call plug#begin()
" -- Tim Pope essentials
" Defaults everyone can agree on
Plug 'tpope/vim-sensible'
" Delete/change/add parentheses/quotes/XML-tags/much more with ease
Plug 'tpope/vim-surround'
" Repeat last change with `.`
Plug 'tpope/vim-repeat'
" Toggle the casing of the word under the cursor
Plug 'tpope/vim-abolish'

" -- Motions
" Jump to any spot on-screen with a 2-char search
Plug 'ggandor/leap.nvim'
" enhanced f/t motions
Plug 'ggandor/flit.nvim'
" Easy text exchange operator
Plug 'tommcdo/vim-exchange'
" extends vim's % key to language-specific words instead of just single characters
Plug 'andymass/vim-matchup'
" Better word navigation
Plug 'chaoren/vim-wordmotion' " something.cool[][0].CamelCaseACRONYMWords_unders-core1234()
" Additional text objects
Plug 'wellle/targets.vim'
" Expand text selection incrementally using the same key
Plug 'terryma/vim-expand-region'

" -- Utils
" Smart and Powerful commenting plugin
Plug 'numToStr/Comment.nvim'
" Turns off search highlighting post-search and reactivates it for the next search
Plug 'romainl/vim-cool'
" Allow seamless navigation between vim and kitty splits.
" Use in conjunction with the kittens `pass_keys.py` and `neighboring_window.py`
Plug 'knubie/vim-kitty-navigator'
call plug#end()


" Plugins settings
" ------------------------------------------------------------------------------

if &rtp =~ 'plugged/Comment'
    lua require('Comment').setup()
endif

if &rtp =~ 'plugged/leap'
    lua require('leap').add_repeat_mappings('<Right>', '<Left>', { relative_directions = false })
    lua require('leap').opts.special_keys.prev_target = '<Left>'
    lua require('leap').opts.special_keys.next_target = '<Right>'

    nmap <Space>   <Plug>(leap-forward-to)
    nmap <S-Space> <Plug>(leap-backward-to)
    xmap <Space>   <Plug>(leap-forward-to)
    xmap <S-Space> <Plug>(leap-backward-to)
    omap <Space>   <Plug>(leap-forward-to)
    omap <S-Space> <Plug>(leap-backward-to)
endif

if &rtp =~ 'plugged/flit'
    lua require('flit').setup()
endif

if &rtp =~ 'plugged/vim-exchange'
    " Exchange the current line
    xmap gx   <Plug>(Exchange)
    nmap gx   cxx
endif

" if &rtp =~ 'plugged/vim-wordmotion'
"     " Move SubWord mode to uppercase bindings
"     let g:wordmotion_mappings = {
"         \ 'w': 'W',
"         \ 'b': 'B',
"         \ 'e': 'E',
"         \ 'ge': 'gE',
"         \ 'aw': 'aW',
"         \ 'iw': 'iW',
"         \ 'W': '',
"         \ 'B': '',
"         \ 'E': '',
"         \ 'gE': '',
"         \ 'aW': '',
"         \ 'iW': ''
"     \ }
" endif

if &rtp =~ 'plugged/vim-surround'
    " Disable default mappings
    let g:surround_no_mappings= 1
    let g:surround_no_insert_mappings= 1

    xmap s   <Plug>VSurround
    xmap S   <Plug>VgSurround
    nmap ds  <Plug>Dsurround
    nmap cs  <Plug>Csurround
    nmap cS  <Plug>CSurround
    nmap ys  <Plug>Ysurround
    nmap yS  <Plug>YSurround
    nmap ySs <Plug>YSsurround
    nmap ySS <Plug>YSsurround
    nmap yss <Plug>Yssurround
endif

if &rtp =~ 'plugged/vim-expand-region'
    vmap v <Plug>(expand_region_expand)
    vmap V <Plug>(expand_region_shrink)
endif


" Bindings
" ------------------------------------------------------------------------------

let mapleader = ','
let maplocalleader = 'm'

" <S-...>  shift-key                      shift <S-
" <C-...>  control-key                    control ctrl <C-
" <M-...>  alt-key or meta-key            meta alt <M-
" <A-...>  same as <M-...>                <A-
" <D-...>  command-key (Macintosh only)   <D-

" map - normal, visual, select and operator pending modes, equivalent to `nvo`
" map! - insert and command-line modes, equivalent to `ic`
" map works recursively, noremap doesn't

" Prefixes:
" n - normal mode
" i - insert mode
" v - visual and select modes
" x - visual mode
" s - select mode
" c - command-line mode
" o - operator pending mode

" Ex Commands
noremap ; :

" pattern matching without jumping
nnoremap <silent> * :let @/= '\<' . expand('<cword>') . '\>' <bar> set hls <cr>
nnoremap <silent> # :let @/= '\<' . expand('<cword>') . '\>' <bar> set hls <cr>

" Comments
nmap <CR>   gcc
vmap <CR>   gc
vmap <S-CR> gb

" Have j and k navigate visual lines rather than logical ones
noremap j gj
noremap k gk

" Beginning/End of line
noremap H ^
noremap L $

" Jump to matching bracket
noremap <tab> %

" maintains the selection region when indenting/outdenting
vnoremap < <gv
vnoremap > >gv
nnoremap < <<
nnoremap > >>

"binds K to opposite of J
nnoremap K i<CR><ESC>k$

" Basic cursor movement and deletion keybindings from emacs

" Start of Line
inoremap <C-a> <C-o>^
" End of Line
inoremap <C-e> <C-o>$
" back one word
inoremap <C-b> <C-o>b
" forward one word
inoremap <C-f> <C-o>w
" delete to end of line
inoremap <C-d>  <C-o><S-d>

" Start of Line
cnoremap <C-a> <Home>
" End of Line
cnoremap <C-e> <End>
" back one word
cnoremap <C-b> <S-Left>
" forward one word
cnoremap <C-f> <S-Right>
" Open command-line window in normal mode
cnoremap <C-o> <C-f>

" FIXME <C-o> doesn't work in command-line mode and there doesn't seem to be
" any other way to support this 🙁
" delete to end of line
" cnoremap <C-d> <C-o><S-d>

nnoremap <Down> 10gj
nnoremap <Up> 10gk
vnoremap <Down> 10gj
vnoremap <Up> 10gk
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

" VSCode settings
" ------------------------------------------------------------------------------

if exists('g:vscode')
    " VSCode extension

    xmap gc  <Plug>VSCodeCommentary
    nmap gc  <Plug>VSCodeCommentary
    omap gc  <Plug>VSCodeCommentary
    nmap gcc <Plug>VSCodeCommentaryLine

    xmap gb <Cmd>call VSCodeNotifyVisual('editor.action.blockComment', 0)<CR>
    nmap gb <Cmd>call VSCodeNotify('editor.action.blockComment')<CR>
    nmap gbb <Cmd>call VSCodeNotifyRange('editor.action.blockComment', line("."), line("."), 0)<CR>

    noremap <leader>wv <Cmd>call VSCodeNotify('workbench.action.splitEditor')<CR>
    noremap <leader>ws <Cmd>call VSCodeNotify('workbench.action.splitEditorDown')<CR>
    noremap <leader>, <Cmd>call VSCodeNotify('workbench.action.showAllEditorsByMostRecentlyUsed')<CR>
    noremap <leader>< <Cmd>call VSCodeNotify('workbench.action.openPreviousEditorFromHistory')<CR>
    noremap <leader>. <Cmd>call VSCodeNotify('relativeFileNavigator.open')<CR>
    noremap <leader>= <Cmd>call VSCodeNotify('editor.action.formatDocument')<CR>
    noremap <leader><space> <Cmd>call VSCodeNotify('workbench.action.quickOpen')<CR>
    noremap <leader><tab> <Cmd>call VSCodeNotify('workbench.action.quickOpenPreviousRecentlyUsedEditorInGroup')<CR>
    nnoremap <leader>gy <Cmd>call VSCodeNotify('gitlens.copyRemoteFileUrlToClipboard', 1)<CR>
    xnoremap <leader>gy <Cmd>call VSCodeNotifyVisual('gitlens.copyRemoteFileUrlToClipboard', 1)<CR>

    " Toggles
    noremap <leader>to <Cmd>call VSCodeNotify('workbench.action.toggleSidebarVisibility')<CR>
    noremap <leader>tp <Cmd>call VSCodeNotify('workbench.files.action.showActiveFileInExplorer')<CR>
    noremap <leader>tl <Cmd>call VSCodeNotify('settings.cycle.lineNumbers')<CR>
    noremap <leader>ti <Cmd>call VSCodeNotify('settings.cycle.indentGuides')<CR>
    noremap <leader>tw <Cmd>call VSCodeNotify('editor.action.toggleWordWrap')<CR>

    " Open
    noremap <leader>oo <Cmd>call VSCodeNotify('revealFileInOS')<CR>

    " Files
    noremap <leader>fy <Cmd>call VSCodeNotify('copyRelativeFilePath')<CR>
    noremap <leader>ff <Cmd>call VSCodeNotify('editor.action.formatDocument.none')<CR>

    " Search and Replace in all files
    nnoremap gr <Cmd>call VSCodeNotify('workbench.action.replaceInFiles', { 'query': expand('<cword>')})<CR>
    xnoremap gr <Cmd>call VSCodeNotifyVisual('workbench.action.replaceInFiles', 0)<CR>

    " Search in all files
    nnoremap gs <Cmd>call VSCodeNotify('search.action.openNewEditor', { 'query': expand('<cword>')})<CR>
    xnoremap gs <Cmd>call VSCodeNotifyVisual('search.action.openNewEditor', 0)<CR>

    " Replace
    " FIXME `<cword>` doesn't work for `editor.action.startFindReplaceAction`
    nnoremap <M-r> <Cmd>call VSCodeNotify('editor.action.startFindReplaceAction', { 'query': expand('<cword>')})<CR>
    xnoremap <M-r> <Cmd>call VSCodeNotifyVisual('editor.action.startFindReplaceAction', 0)<CR>
else
    " ordinary Neovim

    " Split windows
    nnoremap <Leader>ws <C-w>s
    nnoremap <Leader>wv <C-w>v

    " Navigation between window
    " nnoremap <C-h> <C-w>h
    " nnoremap <C-j> <C-w>j
    " nnoremap <C-k> <C-w>k
    " nnoremap <C-l> <C-w>l

    " FIXME Doesn't work in kitty
    map <D-S-[> :bprevious<CR>
    map <D-S-]> :bnext<CR>
endif


" Misc
" ------------------------------------------------------------------------------

" highlighting selection on yank
au TextYankPost * silent! lua vim.highlight.on_yank {on_visual=false}
