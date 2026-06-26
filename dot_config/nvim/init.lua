-- ~/.config/nvim/init.lua

vim.pack.add({
  { src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
})

-- helle Variante = latte, dunkle Variante = mocha
require("catppuccin").setup({
  background = {
    light = "latte",
    dark = "mocha",
  },
})

vim.cmd.colorscheme("catppuccin")

