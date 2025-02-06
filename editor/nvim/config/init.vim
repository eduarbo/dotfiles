" Sane defaults
" ------------------------------------------------------------------------------

set clipboard=unnamed           " Yank to system clipboard
set iskeyword+=-                " Treat hyphens as part of the word
set nostartofline               " Make j/k respect the columns
set scrolloff=10                " Start scrolling n lines before horizontal border of window
set gdefault                    " Regular expressions are global by default
set magic                       " Enable extended regex patterns
set hlsearch                    " Highlight search results
set incsearch                   " Show incremental search results as you type
set ignorecase                  " Case-insensitive search
set smartcase                   " Case-sensitive search if query contains uppercase characters


" Plugins
" ------------------------------------------------------------------------------

" Auto-install vim-plug if missing
let s:plug_path = stdpath('data') . '/site/autoload/plug.vim'
if empty(glob(s:plug_path))
  echo "Installing vim-plug..."
  silent execute '!curl -fLo ' . s:plug_path . ' --create-dirs ' .
        \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin()
" -- Essential plugins from Tim Pope
Plug 'tpope/vim-sensible'       " Sensible default settings
Plug 'tpope/vim-surround'       " Easily change surrounding characters (quotes, brackets, etc.)
Plug 'tpope/vim-repeat'         " Enhance the repeat (.) command
Plug 'tpope/vim-abolish'        " Smart case substitution and abbreviation expansion

" -- Motion enhancements
Plug 'ggandor/leap.nvim'        " Jump to any position on screen with a 2-char search
Plug 'ggandor/flit.nvim'        " Enhanced f/t motions
Plug 'tommcdo/vim-exchange'     " Easy text swapping
Plug 'andymass/vim-matchup'     " Improves % navigation for matching pairs
Plug 'chaoren/vim-wordmotion'   " Better word-based navigation
Plug 'wellle/targets.vim'       " Extra text objects
Plug 'terryma/vim-expand-region'" Incrementally expand selection

" -- Utility plugins
Plug 'numToStr/Comment.nvim'    " Smart and powerful commenting plugin
Plug 'romainl/vim-cool'         " Turns off search highlighting after searching
Plug 'knubie/vim-kitty-navigator' " Seamless navigation between Vim and Kitty splits

" -- Theme
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
call plug#end()


" Plugin Settings
" ------------------------------------------------------------------------------

function! SetupPlugins()
    " Comment.nvim - Smart commenting
    if isdirectory(stdpath("data") . "/plugged/Comment.nvim")
        lua require('Comment').setup()
    endif

    " Leap.nvim - Fast 2-character jump navigation
    if isdirectory(stdpath("data") . "/plugged/leap.nvim")
        lua require("leap").add_repeat_mappings('<C-.>', '<C-,>', { relative_directions = false })
        lua require("leap").opts.special_keys.prev_target = '<C-.>'
        lua require("leap").opts.special_keys.next_target = '<C-.>'

        " Vimscript keybindings
        nmap gs <Plug>(leap-forward-to)
        nmap gS <Plug>(leap-backward-to)
        xmap gs <Plug>(leap-forward-to)
        xmap gS <Plug>(leap-backward-to)
        omap gs <Plug>(leap-forward-to)
        omap gS <Plug>(leap-backward-to)
    endif

    " Flit.nvim - Enhanced f/t motions
    if isdirectory(stdpath("data") . "/plugged/flit.nvim")
        lua require('flit').setup()
    endif

    " Vim-Exchange - Swap text objects
    if isdirectory(stdpath("data") . "/plugged/vim-exchange")
        xmap gx   <Plug>(Exchange)
        nmap gx   cxx
    endif

    " WordMotion - Better word navigation
    if isdirectory(stdpath("data") . "/plugged/vim-wordmotion")
        let g:wordmotion_mappings = {
            \ 'w': 'W',
            \ 'b': 'B',
            \ 'e': 'E',
            \ 'ge': 'gE',
            \ 'aw': 'aW',
            \ 'iw': 'iW',
            \ 'W': '',
            \ 'B': '',
            \ 'E': '',
            \ 'gE': '',
            \ 'aW': '',
            \ 'iW': ''
        \ }
    endif

    " Vim-Surround - Change surrounding characters easily
    if isdirectory(stdpath("data") . "/plugged/vim-surround")
        let g:surround_no_mappings= 1
        let g:surround_no_insert_mappings= 1

        xmap s   <Plug>VSurround
        xmap S   <Plug>VgSurround
        nmap ds  <Plug>Dsurround
        nmap cs  <Plug>Csurround
        nmap cS  <Plug>CSurround
        nmap s   <Plug>Ysurround
        nmap S   <Plug>YSurround
        nmap Ss  <Plug>YSsurround
        nmap SS  <Plug>YSsurround
        nmap ss  <Plug>Yssurround
    endif

    " Expand Region - Incrementally expand selection
    if isdirectory(stdpath("data") . "/plugged/vim-expand-region")
        vmap v <Plug>(expand_region_expand)
        vmap V <Plug>(expand_region_shrink)
    endif

    " Theme: Catppuccin
    if isdirectory(stdpath("data") . "/plugged/catppuccin")
        lua require("catppuccin").setup({ flavour = "mocha", transparent_background = true })
        colorscheme catppuccin
    endif
endfunction

autocmd VimEnter * call SetupPlugins()


" Keybindings
" ------------------------------------------------------------------------------

let mapleader = ','
let maplocalleader = 'm'

" Map ; to : for quicker command mode
noremap ; :

" Highlight the current word without jumping
nnoremap <silent> * :let @/= '\<' . expand('<cword>') . '\>' <bar> set hls <cr>
nnoremap <silent> # :let @/= '\<' . expand('<cword>') . '\>' <bar> set hls <cr>

" Toggle folds with Tab
nnoremap <tab> za
onoremap <tab> <C-C>za
vnoremap <tab> zf

" Have j/k navigate visual lines instead of logical lines
noremap j gj
noremap k gk

" Beginning/End of line
noremap <Left>  ^
noremap <Right> $

" Indentation while keeping selection
vnoremap < <gv
vnoremap > >gv
nnoremap < <<
nnoremap > >>

" Highlight text on yank
au TextYankPost * silent! lua vim.highlight.on_yank {on_visual=false}
