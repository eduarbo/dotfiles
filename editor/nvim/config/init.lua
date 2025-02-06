-- Sane defaults
-- -----------------------------------------------------------------------------
vim.opt.clipboard = "unnamed"          -- Yank to system clipboard
vim.opt.iskeyword:append("-")          -- Treat hyphens as part of words
vim.opt.scrolloff = 10                 -- Start scrolling n lines before window border
vim.opt.gdefault = true                -- Regular expressions are global by default
vim.opt.magic = true                    -- Enable extended regex patterns
vim.opt.hlsearch = true                -- Highlight search results
vim.opt.incsearch = true                -- Show incremental search results as you type
vim.opt.ignorecase = true               -- Case-insensitive search
vim.opt.smartcase = true                -- Case-sensitive search if query contains uppercase

-- Ensure package manager (vim-plug equivalent)
-- -----------------------------------------------------------------------------
local plug_path = vim.fn.stdpath("data") .. "/site/autoload/plug.vim"
if vim.fn.empty(vim.fn.glob(plug_path)) > 0 then
    print("Installing vim-plug...")
    vim.fn.system({
        "curl", "-fLo", plug_path, "--create-dirs",
        "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim"
    })
    vim.cmd("autocmd VimEnter * PlugInstall --sync | source $MYVIMRC")
end

-- Install and configure plugins
-- -----------------------------------------------------------------------------
vim.cmd [[
call plug#begin()
Plug 'tpope/vim-sensible'        " Sensible default settings
Plug 'tpope/vim-surround'        " Surround characters manipulation
Plug 'tpope/vim-repeat'          " Enhances . command
Plug 'tpope/vim-abolish'         " Case-aware substitution

" Motions
Plug 'ggandor/leap.nvim'         " Jump to any spot on-screen
Plug 'ggandor/flit.nvim'         " Enhanced f/t motions
Plug 'tommcdo/vim-exchange'      " Swap text objects
Plug 'andymass/vim-matchup'      " Smarter % navigation
Plug 'chaoren/vim-wordmotion'    " Better word navigation
Plug 'wellle/targets.vim'        " Extra text objects
Plug 'terryma/vim-expand-region' " Incrementally expand selection

" Utilities
Plug 'numToStr/Comment.nvim'     " Smart commenting
Plug 'romainl/vim-cool'          " Disable search highlight automatically
Plug 'knubie/vim-kitty-navigator' " Seamless Vim-Kitty navigation
Plug 'mikesmithgh/kitty-scrollback.nvim' " Proper Kitty scrollback navigation

" Theme
Plug 'catppuccin/nvim', { 'as': 'catppuccin' }
call plug#end()
]]

-- Plugin Configurations
-- -----------------------------------------------------------------------------
local function setup_plugins()
    -- Comment.nvim - Smart commenting
    if pcall(require, "Comment") then
        require('Comment').setup()
    end

    -- Leap.nvim - Fast 2-character jump navigation
    if pcall(require, "leap") then
        require("leap").add_repeat_mappings('<C-.>', '<C-,>', { relative_directions = false })
        require("leap").opts.special_keys.prev_target = '<C-.>'
        require("leap").opts.special_keys.next_target = '<C-.>'
    end

    -- Flit.nvim - Enhanced f/t motions
    if pcall(require, "flit") then
        require('flit').setup()
    end

    -- WordMotion - Better word navigation
    vim.g.wordmotion_mappings = {
        w = "W", b = "B", e = "E", ge = "gE",
        aw = "aW", iw = "iW", W = "", B = "", E = "", gE = "", aW = "", iW = ""
    }

    -- Surround - Modify surrounding characters easily
    vim.g.surround_no_mappings = 1
    vim.g.surround_no_insert_mappings = 1

    -- Expand Region - Incrementally expand selection
    vim.api.nvim_set_keymap("v", "v", "<Plug>(expand_region_expand)", {})
    vim.api.nvim_set_keymap("v", "V", "<Plug>(expand_region_shrink)", {})

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

-- Keybindings
-- -----------------------------------------------------------------------------
vim.g.mapleader = ","
vim.g.maplocalleader = "m"

-- Map ; to : for faster command mode access
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })

-- Pattern search without jumping
vim.api.nvim_set_keymap("n", "*", ":let @/='\\<' .. expand('<cword>') .. '\\>' | set hls<CR>", { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "#", ":let @/='\\<' .. expand('<cword>') .. '\\>' | set hls<CR>", { noremap = true, silent = true })

-- Toggle folds with Tab
vim.api.nvim_set_keymap("n", "<Tab>", "za", { noremap = true })
vim.api.nvim_set_keymap("o", "<Tab>", "<C-C>za", { noremap = true })
vim.api.nvim_set_keymap("v", "<Tab>", "zf", { noremap = true })

-- Make j/k navigate visual lines instead of logical ones
vim.api.nvim_set_keymap("n", "j", "gj", { noremap = true })
vim.api.nvim_set_keymap("n", "k", "gk", { noremap = true })

-- Beginning/End of line
vim.api.nvim_set_keymap("n", "<Left>", "^", { noremap = true })
vim.api.nvim_set_keymap("n", "<Right>", "$", { noremap = true })

-- Indentation while keeping selection
vim.api.nvim_set_keymap("v", "<", "<gv", { noremap = true })
vim.api.nvim_set_keymap("v", ">", ">gv", { noremap = true })
vim.api.nvim_set_keymap("n", "<", "<<", { noremap = true })
vim.api.nvim_set_keymap("n", ">", ">>", { noremap = true })

-- Highlight text on yank
vim.api.nvim_create_autocmd("TextYankPost", {
    pattern = "*",
    callback = function()
        vim.highlight.on_yank({ on_visual = false })
    end,
})
