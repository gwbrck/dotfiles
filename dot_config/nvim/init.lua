vim.pack.add({
  { src = "https://github.com/catppuccin/nvim", name = "catppuccin" },
})

require("catppuccin").setup({
  background = {
    light = "latte",
    dark = "mocha",
  },
})

vim.cmd.colorscheme("catppuccin")
vim.o.clipboard = "unnamedplus"
