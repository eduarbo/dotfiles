" Run PlugInstall if there are missing plugins
autocmd VimEnter * if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
    \| PlugInstall --sync | source $MYVIMRC
  \| endif

call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-repeat'
Plug 'andymass/vim-matchup'
Plug 'wellle/targets.vim'
Plug 'numToStr/Comment.nvim'
Plug 'terryma/vim-expand-region'
Plug 'tommcdo/vim-exchange'
Plug 'tpope/vim-surround'
Plug 'chaoren/vim-wordmotion'
Plug 'romainl/vim-cool'
call plug#end()

if &rtp =~ 'plugged/Comment'
    lua require('Comment').setup()
endif

let mapleader = ','
let maplocalleader = 'm'

let g:surround_no_mappings= 1
" this allows me to select envirionent variables
let g:wordmotion_uppercase_spaces = ['=', '[', ']', '[', ']', '{', '}', '(', ')', '<', '>', "'", '"', '.', ',', ':', '/', '\', '|', '#', '$', '%', '*', '+', '!']

set ignorecase
set smartcase
set incsearch
set iskeyword+=-
set scrolloff=10
set ignorecase           " case insensitive search
set smartcase            " case sensitive when uc present
set nostartofline

" highlighting selection on yank
au TextYankPost * silent! lua vim.highlight.on_yank {on_visual=false}

" Bindings

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

vnoremap y "+y

nmap ; :
vmap ; :
nmap # gcc
vmap # gc

"  nnoremap <S-r> :%s/<C-R><C-W>//g<Left><Left>
"  "  xnoremap <S-r> '"zy<Esc>:%s/<C-R>z//g<Left><Left>
"  vnoremap <S-r> :s/"/'/g<CR>

" replace the current search
nnoremap <S-r> :%s///g<left><left>
vnoremap <S-r> :s///g<left><left>

nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk
nnoremap <Down> gj
nnoremap <Up> gk
vnoremap <Down> gj
vnoremap <Up> gk
inoremap <Down> <C-o>gj
inoremap <Up> <C-o>gk

vmap v <Plug>(expand_region_expand)
vmap V <Plug>(expand_region_shrink)

xmap s <Plug>VSurround
nmap ss <Plug>Yssurround
nmap s  <Plug>Ysurround
nmap ds  <Plug>Dsurround
nmap cs  <Plug>Csurround

nnoremap <down> 10j
nnoremap <up> 10k

if exists('g:vscode')
    " VSCode extension

    xmap gc  <Plug>VSCodeCommentary
    nmap gc  <Plug>VSCodeCommentary
    omap gc  <Plug>VSCodeCommentary
    nmap gcc <Plug>VSCodeCommentaryLine

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

    nnoremap <space> <Cmd>call VSCodeNotify('search.action.openNewEditor')<CR>
    xnoremap <space> <Cmd>call VSCodeNotifyVisual('search.action.openNewEditor', 0)<CR>
else
    " ordinary Neovim

    " Split windows
    nnoremap <Leader>ws <C-w>s
    nnoremap <Leader>wv <C-w>v

    " Navigation between window
    nnoremap <C-h> <C-w>h
    nnoremap <C-j> <C-w>j
    nnoremap <C-k> <C-w>k
    nnoremap <C-l> <C-w>l

    " FIXME Doesn't work in kitty
    map <D-S-[> :bprevious<CR>
    map <D-S-]> :bnext<CR>
endif
