return {
  {
    "EdenEast/nightfox.nvim",
    lazy = false, -- make sure we load this during startup if it is your main colorscheme
    priority = 1000,
  },
  {
    "f-person/auto-dark-mode.nvim",
    lazy = false,
    priority = 1001,
    cond = vim.fn.has("Mac") == 1,
    opts = {
      update_interval = 1000,
      set_dark_mode = function()
        vim.cmd("colorscheme nightfox")
        -- vim.cmd("colorscheme edge")
      end,
      set_light_mode = function()
        vim.cmd("colorscheme dayfox")
        -- vim.cmd("colorscheme edge")
      end,
    },
    init = function()
      require("auto-dark-mode").init()

      vim.api.nvim_create_autocmd("OptionSet", {
        pattern = "background",
        callback = function()
          -- Get the current filetype
          local ft = vim.bo.filetype
          -- Reset the filetype to trigger syntax highlighting
          vim.cmd("set filetype=" .. ft)
        end,
      })

      vim.api.nvim_create_autocmd("ColorScheme", {
        callback = function()
          -- Get the current filetype
          local ft = vim.bo.filetype
          -- Reset the filetype to trigger syntax highlighting
          vim.cmd("set filetype=" .. ft)
        end,
      })
    end,
  },
  {
    "NvChad/nvim-colorizer.lua",
    enabled = true,
    opts = {
      filetypes = { "*" },
      user_default_options = {
        RGB = true,          -- #RGB hex codes
        RRGGBB = true,       -- #RRGGBB hex codes
        names = true,        -- "Name" codes like Blue or blue
        RRGGBBAA = true,     -- #RRGGBBAA hex codes
        AARRGGBB = false,    -- 0xAARRGGBB hex codes
        rgb_fn = false,      -- CSS rgb() and rgba() functions
        hsl_fn = false,      -- CSS hsl() and hsla() functions
        css = false,         -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
        css_fn = false,      -- Enable all CSS *functions*: rgb_fn, hsl_fn
        -- Available modes for `mode`: foreground, background,  virtualtext
        mode = "background", -- Set the display mode.
        -- Available methods are false / true / "normal" / "lsp" / "both"
        -- True is same as normal
        tailwind = false,                               -- Enable tailwind colors
        -- parsers can contain values used in |user_default_options|
        sass = { enable = false, parsers = { "css" } }, -- Enable sass colors
        virtualtext = "■",
        -- update color values even if buffer is not focused
        -- example use: cmp_menu, cmp_docs
        always_update = false,
        -- all the sub-options of filetypes apply to buftypes
      },
      buftypes = {},
    },
  },
}
