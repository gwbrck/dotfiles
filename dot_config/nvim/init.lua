HOME = os.getenv("HOME")
vim.g.mapleader = " "
vim.g.maplocalleader = ","
vim.opt.termguicolors = true

local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
end

return require('packer').startup(function(use)
     use "tpope/vim-fugitive" 
     use "wbthomason/packer.nvim"
     use "jalvesaq/Nvim-R" 
     use "airblade/vim-gitgutter" 
     use  "nvim-lua/completion-nvim" 
     use "vim-pandoc/vim-pandoc-syntax"
     use "tpope/vim-surround"
     use  "jiangmiao/auto-pairs" 
     use  "neovim/nvim-lspconfig" 
     use  "nvim-lua/plenary.nvim" 
     use  "nvim-lua/telescope.nvim" 
     use  "nvim-lua/popup.nvim" 
     use  "nvim-lua/lsp-status.nvim" 
     use { "nvim-treesitter/nvim-treesitter", run = ":TSUpdate" }
     use  "nvim-treesitter/completion-treesitter" 
     use  "norcalli/nvim-colorizer.lua" 
     use  "vim-pandoc/vim-pandoc" 
     use  "vigoux/LanguageTool.nvim"
     use  "rebelot/kanagawa.nvim"
     use  "kyazdani42/nvim-web-devicons" 
     use  "dhruvasagar/vim-table-mode"
     if packer_bootstrap then
	require('packer').sync()
     end

     require("_statusline")
     require("configoflsp")
     vim.cmd("so " .. HOME .. "/.config/nvim/old.vim")
     vim.cmd("colorscheme kanagawa")
end)
