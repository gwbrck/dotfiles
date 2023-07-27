return {
        -- the colorscheme should be available when starting Neovim
        {
                "sainnhe/edge",
                lazy = false,    -- make sure we load this during startup if it is your main colorscheme
                priority = 1000, -- make sure to load this before all the other start plugins
        },
        {
                "f-person/auto-dark-mode.nvim",
                lazy = false, 
                priority = 100,
                cond = vim.fn.has('Mac') == 1,
                config = {
                        update_interval = 1000,
                        set_dark_mode = function()
                                vim.api.nvim_set_option("background", "dark")
                                vim.cmd("colorscheme edge")
                        end,
                        set_light_mode = function()
                                vim.api.nvim_set_option("background", "light")
                                vim.cmd("colorscheme edge")
                        end,
                },
                init = function()
                        require("auto-dark-mode").init()
                end,
        },
        {
                "nvim-lualine/lualine.nvim",
                opts = {
                        options = {
                                component_separators = { left = '|', right = '|' },
                                section_separators = { left = '', right = '' },
                        },

                },
        },
        {       "lewis6991/gitsigns.nvim",
                event = { "BufReadPre", "BufNewFile" },
        },
        {
                "norcalli/nvim-colorizer.lua"
        },
}
