--Install packer
local vim = vim
local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'


if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
        vim.fn.execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
end

local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", { command = "PackerCompile", group = packer_group, pattern = "init.lua" })

local use = require('packer').use
require('packer').startup(function()
        use {
                "lewis6991/gitsigns.nvim",
                config = function()
                        require('gitsigns').setup()
                end
        }
        use "wbthomason/packer.nvim"
        use "jalvesaq/Nvim-R"
        use "vim-pandoc/vim-pandoc-syntax"
        use "tpope/vim-surround"
        use "jiangmiao/auto-pairs"
        use "neovim/nvim-lspconfig"
        use "nvim-lua/plenary.nvim"
        use "nvim-lua/telescope.nvim"
        use "nvim-lua/popup.nvim"
        use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate",
                config = function()
                        require 'nvim-treesitter.configs'.setup {
                                ensure_installed = "all", -- one of "all", "maintained" (parsers with maintainers), or a list of languages
                                highlight = { enable = true } } -- false will disable the whole extension
                end }
        --	use "nvim-treesitter/completion-treesitter"
        use {
                "norcalli/nvim-colorizer.lua",
                config = function()
                        require 'colorizer'.setup()
                end
        }
        use "vim-pandoc/vim-pandoc"
        use "nvim-lua/lsp-status.nvim"
        use "vigoux/LanguageTool.nvim"
        use "kyazdani42/nvim-web-devicons"
        use "sainnhe/edge"
        use "dhruvasagar/vim-table-mode"
        use "hrsh7th/nvim-cmp" -- Autocompletion plugin
        use "hrsh7th/cmp-nvim-lsp" -- LSP source for nvim-cmp
        use "saadparwaiz1/cmp_luasnip" -- Snippets source for nvim-cmp
        use "L3MON4D3/LuaSnip"
end)

--------------------------------------------------------------------------------
vim.g.loaded_getscript = 1
vim.g.loaded_getscriptPlugin = 1
vim.g.loaded_vimball = 1
vim.g.loaded_vimballPlugin = 1
vim.g.loaded_2html_plugin = 1
vim.g.loaded_matchit = 1
vim.g.loaded_matchparen = 1
vim.g.loaded_logiPat = 1
vim.g.loaded_rrhelper = 1
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrwSettings = 1


--Set colorscheme
vim.o.termguicolors = true
vim.cmd [[colorscheme edge]]

vim.o.relativenumber = true
vim.o.mouse = 'a'
vim.o.expandtab = true

vim.o.list = true
vim.o.listchars = 'tab:» ,eol:↲'

vim.o.signcolumn = 'auto:2'

vim.o.clipboard = 'unnamed'
vim.o.breakindent = true
--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.completeopt = 'menuone,noinsert'
HOME = os.getenv("HOME")
vim.g.mapleader = " "
vim.g.maplocalleader = ","

--Highlight on yank
local highlight_group = vim.api.nvim_create_augroup("YankHighlight", { clear = true })
vim.api.nvim_create_autocmd("TextYankPost", { callback = function() vim.highlight.on_yank() end, group = highlight_group })

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next)
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist)

local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
for type, icon in pairs(signs) do
        local hl = "DiagnosticSign" .. type
        vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = hl })
end

-- LSP settings
local lspconfig = require 'lspconfig'
local on_attach = function(_, bufnr)
        local opts = { buffer = bufnr }
        vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
        vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
        vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
        vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
        vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, opts)
        vim.keymap.set('n', '<leader>wa', vim.lsp.buf.add_workspace_folder, opts)
        vim.keymap.set('n', '<leader>wr', vim.lsp.buf.remove_workspace_folder, opts)
        vim.keymap.set('n', '<leader>wl', function()
                vim.inspect(vim.lsp.buf.list_workspace_folders())
        end, opts)
        vim.keymap.set('n', '<leader>D', vim.lsp.buf.type_definition, opts)
        vim.keymap.set('n', '<leader>rn', vim.lsp.buf.rename, opts)
        vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
        vim.keymap.set('n', '<leader>ca', vim.lsp.buf.code_action, opts)
        vim.keymap.set('n', '<leader>so', require('telescope.builtin').lsp_document_symbols, opts)
        vim.api.nvim_create_user_command("Format", vim.lsp.buf.formatting, {})
end

-- nvim-cmp supports additional completion capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

-- Enable the following language servers
local servers = { 'clangd', 'rust_analyzer', 'sumneko_lua', 'tsserver', 'pylsp', 'r_language_server', 'bashls' }
for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup {
                on_attach = on_attach,
                capabilities = capabilities,
        }
end

-- luasnip setup
local luasnip = require 'luasnip'

-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup {
        snippet = {
                expand = function(args)
                        luasnip.lsp_expand(args.body)
                end,
        },
        mapping = cmp.mapping.preset.insert({
                ['<C-d>'] = cmp.mapping.scroll_docs(-4),
                ['<C-f>'] = cmp.mapping.scroll_docs(4),
                ['<C-Space>'] = cmp.mapping.complete(),
                ['<CR>'] = cmp.mapping.confirm {
                        behavior = cmp.ConfirmBehavior.Replace,
                        select = true,
                },
                ['<Tab>'] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                                cmp.select_next_item()
                        elseif luasnip.expand_or_jumpable() then
                                luasnip.expand_or_jump()
                        else
                                fallback()
                        end
                end, { 'i', 's' }),
                ['<S-Tab>'] = cmp.mapping(function(fallback)
                        if cmp.visible() then
                                cmp.select_prev_item()
                        elseif luasnip.jumpable(-1) then
                                luasnip.jump(-1)
                        else
                                fallback()
                        end
                end, { 'i', 's' }),
        }),
        sources = {
                { name = 'nvim_lsp' },
                { name = 'luasnip' },
        },
}

require("_statusline")
