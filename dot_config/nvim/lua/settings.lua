vim.o.termguicolors = true
vim.o.relativenumber = true
vim.o.mouse = 'a'
vim.o.expandtab = true
vim.o.list = true
vim.o.listchars = 'tab:» ,eol:↲'
vim.o.signcolumn = 'auto:2'
vim.o.breakindent = true
vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.completeopt = 'menuone,noinsert'


vim.g.mapleader = " "


vim.o.shiftwidth = 2                  -- Set amount of space characters, when we press "<" or ">"
vim.o.tabstop = 2                     -- 1 tab equal 2 spaces
vim.o.smartindent = true              -- Turn on smart indentation. See in the docs for more info

vim.o.clipboard = 'unnamedplus' -- Use system clipboard
vim.o.fixeol = false -- Turn off appending new line in the end of a file

vim.o.foldmethod = 'syntax'

vim.o.ignorecase = true               -- Ignore case if all characters in lower case
vim.o.joinspaces = false              -- Join multiple spaces in search
vim.o.smartcase = true                -- When there is a one capital letter search for exact match
vim.o.showmatch = true                -- Highlight search instances

vim.o.splitbelow = true               -- Put new windows below current
vim.o.splitright = true               -- Put new vertical splits to right

vim.o.wildmenu = true
vim.o.wildmode = "longest:full,full"

local disabled_built_ins = {
    "netrw",
    "netrwPlugin",
    "netrwSettings",
    "netrwFileHandlers",
    "gzip",
    "zip",
    "zipPlugin",
    "tar",
    "tarPlugin",
    "getscript",
    "getscriptPlugin",
    "vimball",
    "vimballPlugin",
    "2html_plugin",
    "logipat",
    "rrhelper",
    "spellfile_plugin",
    "matchit"
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end

if vim.fn.has('linux') == 1 then
   -- require("nvim-adapt")
   require("nvim-darkman")
end

-- Highlight on yank
local highlight_group = vim.api.nvim_create_augroup('YankHighlight', { clear = true })
vim.api.nvim_create_autocmd('TextYankPost', {
        callback = function()
                vim.highlight.on_yank()
        end,
        group = highlight_group,
        pattern = '*',
})

