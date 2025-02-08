-- ─── Sane defaults ────────────────────────────────────────────────────────────

vim.opt.clipboard = "unnamed"          -- Yank to system clipboard
vim.opt.iskeyword:append("-")          -- Treat hyphens as part of words
vim.opt.scrolloff = 10                 -- Start scrolling n lines before window border
vim.opt.gdefault = true                -- Regular expressions are global by default
vim.opt.magic = true                   -- Enable extended regex patterns
vim.opt.hlsearch = true                -- Highlight search results
vim.opt.incsearch = true               -- Show incremental search results as you type
vim.opt.ignorecase = true              -- Case-insensitive search
vim.opt.smartcase = true               -- Case-sensitive search if query contains uppercase
vim.opt.list = true                    -- Enable indication characters

-- ─── Profiling ───────────────────────────────────────────────────────────────

local start_time = vim.loop.hrtime()

vim.api.nvim_create_autocmd("VimEnter", {
    callback = function()
        local end_time = vim.loop.hrtime()
        local elapsed_time = (end_time - start_time) / 1e6 -- Convertir a milisegundos
        print("Tiempo de carga: " .. elapsed_time .. " ms")
    end,
})

-- ─── Ensure package manager (vim-plug equivalent) ─────────────────────────────

local plug_path = vim.fn.stdpath("data") .. "/site/autoload/plug.vim"
if vim.fn.empty(vim.fn.glob(plug_path)) > 0 then
    print("Installing vim-plug...")
    vim.fn.system({
        "curl", "-fLo", plug_path, "--create-dirs",
        "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    })
    vim.cmd("autocmd VimEnter * PlugInstall --sync | source $MYVIMRC")
end

-- ─── Install and configure plugins ────────────────────────────────────────────

vim.cmd [[
call plug#begin()
" Defaults
Plug 'tpope/vim-sensible'                           " Sensible default settings
Plug 'tpope/vim-surround'                           " Surround characters manipulation
Plug 'tpope/vim-repeat'                             " Enhances . command
Plug 'tpope/vim-abolish'                            " Case-aware substitution

" Motions
Plug 'ggandor/leap.nvim'                            " Jump to any spot on-screen
Plug 'ggandor/flit.nvim'                            " Enhanced f/t motions
Plug 'tommcdo/vim-exchange'                         " Swap text objects
Plug 'andymass/vim-matchup'                         " Smarter % navigation
Plug 'chaoren/vim-wordmotion'                       " Better word navigation
Plug 'wellle/targets.vim'                           " Extra text objects
Plug 'terryma/vim-expand-region'                    " Incrementally expand selection
Plug 'mg979/vim-visual-multi', {'branch': 'master'} " Multiple cursors

" Utilities
Plug 'numToStr/Comment.nvim'                        " Smart commenting
Plug 'romainl/vim-cool'                             " Disable search highlight automatically
Plug 'knubie/vim-kitty-navigator'                   " Seamless Vim-Kitty navigation
Plug 'mikesmithgh/kitty-scrollback.nvim'            " Proper Kitty scrollback navigation
Plug 'nvim-lua/plenary.nvim'                        " Dependency for telescope
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.8' } " Fuzzy finder

" Theme
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
call plug#end()
]]

-- ─── Plugin Configurations ────────────────────────────────────────────────────

local function setup_plugins()
    vim.opt.listchars = {
        tab = "→ ",
        trail = "·",
        extends = "»",
        precedes = "«",
        nbsp = "␣"
    }
    -- expand-region - Incrementally expand selection
    vim.api.nvim_set_keymap("v", "v", "<Plug>(expand_region_expand)", {})
    vim.api.nvim_set_keymap("v", "V", "<Plug>(expand_region_shrink)", {})

    -- comment.nvim - Smart commenting
    if pcall(require, "Comment") then
        require('Comment').setup()
    end

    -- leap.nvim - Fast 2-character jump navigation
    if pcall(require, "leap") then
        local leap = require("leap")
        require('leap.user').set_repeat_keys('<C-.>', '<C-,>', { relative_directions = false })
        leap.opts.special_keys.prev_target = '<C-,>'
        leap.opts.special_keys.next_target = '<C-.>'

        vim.keymap.set({'n', 'x'}, 'S', '<Plug>(leap)') -- Bidirectional
    end

    -- flit.nvim - Enhanced f/t motions
    if pcall(require, "flit") then
        require('flit').setup()
    end

    -- vim-visual-multi - Multiple cursors
    if pcall(require, "vim-visual-multi") then
        vim.keymap.set("n", "<S-r>", ":VMSearchAll<CR>", { noremap = true, silent = true })
    end

    -- telescope.nvim - Fuzzy finder
    if pcall(require, "telescope") then
        local actions = require("telescope.actions")
        require('telescope').setup({
                defaults = {
                    mappings = {
                        i = {
                            ["<Esc>"] = actions.close,
                            ["<C-n>"] = actions.move_selection_next,
                            ["<C-p>"] = actions.move_selection_previous,
                        },
                    },
                },
                                  })
        vim.keymap.set("n", "<leader>,", ":Telescope buffers<CR>", { noremap = true, silent = true })
        vim.keymap.set("n", "<leader>.", ":Telescope find_files<CR>", { noremap = true, silent = true })
    end

    -- Kitty Scrollback - Open Kitty scrollback in Neovim with proper ANSI color support
    if pcall(require, "kitty-scrollback") then
        vim.keymap.set("n", "<Esc>", "<Plug>(KsbCloseOrQuitAll)", {})
        -- FIXME config is being ignored
        require("kitty-scrollback").setup({
                -- global configuration
                {
                    status_window = {
                        -- enabled = false,
                        -- autoclose = true,
                        -- style_simple = true,
                        icons = {
                            kitty = '', -- variants 󰄛
                            heart = '', -- variants 󰣐 |  |  | ♥ |  | 󱢠 | 
                            nvim = '', -- variants  |  |  | 
                        },
                    },
                    paste_window = {
                        -- yank_register_enabled = false,
                        -- hide_footer = true,
                    },
                    kitty_get_text = {
                        -- ansi = false,
                    },
                },
                -- builtin configuration override
                ksb_builtin_get_text_all = {
                }
                                         })
    end

    -- Theme: Catppuccin
    if pcall(require, "catppuccin") then
        require("catppuccin").setup({ flavour = "mocha", transparent_background = true })
        vim.cmd("colorscheme catppuccin")
    end
end

-- Load Plugins on VimEnter
vim.api.nvim_create_autocmd("VimEnter", { callback = setup_plugins })

-- ─── Keybindings ──────────────────────────────────────────────────────────────

vim.g.mapleader = ","
vim.g.maplocalleader = "m"

-- Map ; to : for faster command mode access
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })

-- Search the word under cursor without jumping
vim.api.nvim_set_keymap("n", "*", ":let @/='\\<' .. expand('<cword>') .. '\\>' | set hls<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "#", ":let @/='\\<' .. expand('<cword>') .. '\\>' | set hls<CR>", { noremap = true, silent = true })

-- Toggle folds using Tab
vim.api.nvim_set_keymap("n", "<Tab>", "za", { noremap = true })
vim.api.nvim_set_keymap("o", "<Tab>", "<C-C>za", { noremap = true })
vim.api.nvim_set_keymap("v", "<Tab>", "zf", { noremap = true })

-- Make j/k navigate visual lines instead of logical ones
vim.api.nvim_set_keymap("n", "j", "gj", { noremap = true })
vim.api.nvim_set_keymap("n", "k", "gk", { noremap = true })

-- Move to the beginning/end of the line
vim.api.nvim_set_keymap("n", "<Left>", "^", { noremap = true })
vim.api.nvim_set_keymap("n", "<Right>", "$", { noremap = true })
vim.api.nvim_set_keymap("v", "<Left>", "^", { noremap = true })
vim.api.nvim_set_keymap("v", "<Right>", "$", { noremap = true })

-- Indentation while keeping selection
vim.api.nvim_set_keymap("v", "<", "<gv", { noremap = true })
vim.api.nvim_set_keymap("v", ">", ">gv", { noremap = true })
vim.api.nvim_set_keymap("n", "<", "<<", { noremap = true })
vim.api.nvim_set_keymap("n", ">", ">>", { noremap = true })

-- Buffer navigation
vim.api.nvim_set_keymap("n", "<M-left>", ":bprevious<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<M-right>", ":bnext<CR>", { noremap = true, silent = true })

-- Visual mode replace selection with surround
vim.api.nvim_set_keymap("v", "s", "S", { noremap = false })

-- Join lines with Backspace
vim.api.nvim_set_keymap("n", "<BS>", "J", { noremap = true, silent = true })

-- Scroll faster with Up/Down keys
vim.api.nvim_set_keymap("n", "<Up>", "10gk", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<Down>", "10gj", { noremap = true, silent = true })

-- Toggle comments
vim.api.nvim_set_keymap("n", "<S-CR>", ":lua require('Comment.api').toggle.linewise.current()<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("v", "<S-CR>", ":lua require('Comment.api').toggle.linewise(vim.fn.visualmode())<CR>", { noremap = true, silent = true })

-- Emulate Doom Emacs C-a and C-e behavior in insert mode
local function smart_home()
    local col = vim.fn.col(".")
    local first_non_blank = vim.fn.match(vim.fn.getline("."), "\\S") + 1

    if col == 1 or col == first_non_blank then
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>0", true, false, true), "n", false)
    else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-o>^", true, false, true), "n", false)
    end
end

-- Emacs-style bindings in Insert mode
vim.keymap.set("i", "<C-a>", smart_home, { noremap = true, silent = true })
vim.keymap.set("i", "<C-e>", "<C-o>$", { noremap = true, silent = true })
vim.keymap.set("i", "<C-d>", "<C-o>D", { noremap = true, silent = true })
vim.keymap.set("i", "<C-f>", "<C-right>", { noremap = true, silent = true })
vim.keymap.set("i", "<C-b>", "<C-left>", { noremap = true, silent = true })

-- Emulate Emacs-like navigation in command-line mode
vim.cmd([[
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-f> <C-right>
cnoremap <C-b> <C-left>
]])

-- Window management - Splits
vim.keymap.set("n", "<leader>ws", ":split<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>wv", ":vsplit<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>wd", ":close<CR>", { noremap = true, silent = true })

-- Window management - Movement
vim.keymap.set("n", "<leader>wh", "<C-w>h", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>wj", "<C-w>j", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>wk", "<C-w>k", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>wl", "<C-w>l", { noremap = true, silent = true })

-- New line above/below
vim.keymap.set("i", "<S-CR>", "<C-o>O", { noremap = true, silent = true })
vim.keymap.set("i", "<C-CR>", "<C-o>o", { noremap = true, silent = true })

-- Insert a tab at current position (Shift+Space)
vim.keymap.set("i", "<S-Space>", "<C-v><Tab>", { noremap = true, silent = true })

-- Search word under cursor with Space
vim.keymap.set("n", "<Space>", "*N", { noremap = true, silent = true })
vim.keymap.set("v", "<Space>", "y/\\V<C-r>=escape(@\",'/\\')<CR><CR>N", { noremap = true, silent = true })

-- Match bracket jumping with Return
vim.keymap.set("n", "<CR>", "%", { noremap = true, silent = true })

-- Drag lines up/down
vim.keymap.set("n", "<S-Up>", ":m .-2<CR>==", { noremap = true, silent = true })
vim.keymap.set("n", "<S-Down>", ":m .+1<CR>==", { noremap = true, silent = true })
vim.keymap.set("v", "<S-Up>", ":m '<-2<CR>gv=gv", { noremap = true, silent = true })
vim.keymap.set("v", "<S-Down>", ":m '>+1<CR>gv=gv", { noremap = true, silent = true })

-- Toggle options
vim.keymap.set("n", "<leader>tl", ":set invnumber<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tr", ":set invrelativenumber<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>tw", function()
    vim.opt.wrap = not vim.opt.wrap:get()
    vim.opt.list = not vim.opt.wrap:get()  -- Solo muestra `listchars` cuando wrap está desactivado
end, { noremap = true, silent = true, desc = "Toggle line wrap" })

-- Highlight text on yank
vim.api.nvim_create_autocmd("TextYankPost", {
    pattern = "*",
    callback = function()
        vim.highlight.on_yank({ on_visual = false })
    end,
})
