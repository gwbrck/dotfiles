return {
  {
    'neovim/nvim-lspconfig',
    config = function()
      local lspconfig = require('lspconfig') -- Lazy.nvim will handle the loading
      -- Enable the following language servers
      local servers = { 'clangd', 'rust_analyzer', 'lua_ls', 'tsserver', 'pylsp', 'r_language_server', 'bashls',
        'jsonls' }
      for _, lsp in ipairs(servers) do
        lspconfig[lsp].setup {}
      end
    end,
    event = "BufReadPre",
  },
  {
    'hrsh7th/nvim-cmp',
    event = "InsertEnter",
    dependencies = {
      'L3MON4D3/LuaSnip',
      'saadparwaiz1/cmp_luasnip',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-path',
      'hrsh7th/cmp-emoji',
      'hrsh7th/cmp-nvim-lsp-signature-help',
      'hrsh7th/cmp-nvim-lua',
      'rafamadriz/friendly-snippets',
    },
    config = function()
      local cmp = require('cmp')
      local lspkind = require('lspkind')
      local luasnip = require('luasnip')
      cmp.setup {
        snippet = {
          expand = function(args)
            require 'luasnip'.lsp_expand(args.body) -- Luasnip expand
          end,
        },
        -- Mappings for cmp
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
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },                -- LSP
          { name = 'nvim_lsp_signature_help' }, -- LSP for parameters in functions
          { name = 'nvim_lua' },                -- Lua Neovim API
          { name = 'luasnip' },                 -- Luasnip
          { name = 'buffer' },                  -- Buffers
          { name = 'path' },                    -- Paths
          { name = "emoji" },                   -- Emoji
        }, {
        }),
        formatting = {
          format = lspkind.cmp_format({
            mode = 'symbol', -- Show only symbol annotations
            maxwidth = 50,   -- Prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
          })
        }
      }

      -- Add snippets from Friendly Snippets
      require("luasnip/loaders/from_vscode").lazy_load()
    end,
  },

  {
    'onsails/lspkind-nvim',
    lazy = true,
  },

  {
    "folke/trouble.nvim",
    lazy = true,
    dependencies = "kyazdani42/nvim-web-devicons",
  },

}
