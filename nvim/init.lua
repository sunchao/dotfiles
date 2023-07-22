-- ensure the packer plugin manager is installed
local ensure_packer = function()
  local fn = vim.fn
  local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
    vim.cmd([[packadd packer.nvim]])
    return true
  end
  return false
end

local packer_bootstrap = ensure_packer()

require("packer").startup(function(use)
  -- Color Theme
  use "rebelot/kanagawa.nvim"
  -- Packer can manage itself
  use("wbthomason/packer.nvim")
  -- Collection of common configurations for the Nvim LSP client
  use("neovim/nvim-lspconfig")
  -- Visualize lsp progress
  use({
    "j-hui/fidget.nvim",
    config = function()
      require("fidget").setup()
    end
  })

  -- Autocompletion framework
  use("hrsh7th/nvim-cmp")
  use({
    -- cmp LSP completion
    "hrsh7th/cmp-nvim-lsp",
    -- cmp Snippet completion
    "hrsh7th/cmp-vsnip",
    -- cmp Path completion
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-buffer",
    after = { "hrsh7th/nvim-cmp" },
    requires = { "hrsh7th/nvim-cmp" },
  })
  -- See hrsh7th other plugins for more great completion sources!
  -- Snippet engine
  use('hrsh7th/vim-vsnip')
  -- Adds extra functionality over rust analyzer
  use("simrat39/rust-tools.nvim")

  -- Optional
  use("nvim-lua/popup.nvim")
  use("nvim-lua/plenary.nvim")
  use("nvim-telescope/telescope.nvim")

  -- Some color scheme other then default
  use("arcticicestudio/nord-vim")
  
  -- Navitage to any word in the file
  use { 'phaazon/hop.nvim', branch = 'v2' } 

  -- Move working directory to the project root
  use {
    'notjedi/nvim-rooter.lua',
    config = function() require'nvim-rooter'.setup() end
  }

  -- Git
  use "sindrets/diffview.nvim"

  -- Better Escape
  use {
    "max397574/better-escape.nvim",
    config = function()
    require("better_escape").setup()
    end,
  }

  -- Auto pair parenthesis
  use {
    "windwp/nvim-autopairs",
    config = function() require("nvim-autopairs").setup {} end
  }

  -- Status line
  use {
    'nvim-lualine/lualine.nvim',
    requires = { 'nvim-tree/nvim-web-devicons', opt = true }
  }

  -- -----
  -- Rust configurations
  -- -----

  -- 1. Mason
  use 'williamboman/mason.nvim'
  use 'williamboman/mason-lspconfig.nvim'


  -- 2. Attach Neovim to rust-analyzer
  use 'neovim/nvim-lspconfig'
  use 'simrat39/rust-tools.nvim'

  -- 3. Install the Complete and Snippets plugins

  -- Completion framework:
  use 'hrsh7th/nvim-cmp'

  -- LSP completion source:
  use 'hrsh7th/cmp-nvim-lsp'

  -- Useful completion sources:
  use 'hrsh7th/cmp-nvim-lua'
  use 'hrsh7th/cmp-nvim-lsp-signature-help'

  use 'puremourning/vimspector'

  use 'nvim-treesitter/nvim-treesitter' -- better syntax
  use 'folke/todo-comments.nvim' -- better TODO comments
  use 'BurntSushi/ripgrep'
  use 'numToStr/Comment.nvim'
  use 'RRethy/vim-illuminate' 
  use { 'kyazdani42/nvim-tree.lua', -- Filesystem navigation
        requires = 'kyazdani42/nvim-web-devicons' } -- Filesystem icons

end)


require('kanagawa').setup({
    keywordStyle = { italic = false, bold = true },
})
vim.cmd("colorscheme kanagawa")

require("mason").setup({
  ui = {
      icons = {
	  package_installed = "",
	  package_pending = "",
	  package_uninstalled = "",
      },
  }
})
require("mason-lspconfig").setup()

local rt = require("rust-tools")

rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})

-- LSP Diagnostics Options Setup
local sign = function(opts)
  vim.fn.sign_define(opts.name, {
    texthl = opts.name,
    text = opts.text,
    numhl = ''
  })
end

sign({name = 'DiagnosticSignError', text = ''})
sign({name = 'DiagnosticSignWarn', text = ''})
sign({name = 'DiagnosticSignHint', text = ''})
sign({name = 'DiagnosticSignInfo', text = ''})

vim.diagnostic.config({
    virtual_text = false,
    signs = true,
    update_in_insert = true,
    underline = true,
    severity_sort = false,
    float = {
	border = 'rounded',
	source = 'always',
	header = '',
	prefix = '',
    },
})

vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])

-- Completion Plugin Setup
local cmp = require'cmp'
cmp.setup({
  -- Enable LSP snippets
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-S-f>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },
  -- Installed sources:
  sources = {
    { name = 'path' },                              -- file paths
    { name = 'nvim_lsp', keyword_length = 3 },      -- from language server
    { name = 'nvim_lsp_signature_help'},            -- display function signatures with current parameter emphasized
    { name = 'nvim_lua', keyword_length = 2},       -- complete neovim's Lua runtime API such vim.lsp.*
    { name = 'buffer', keyword_length = 2 },        -- source current buffer
    { name = 'vsnip', keyword_length = 2 },         -- nvim-cmp source for vim-vsnip
    { name = 'calc'},                               -- source for math calculation
  },
  window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
  },
  formatting = {
      fields = {'menu', 'abbr', 'kind'},
      format = function(entry, item)
          local menu_icon ={
              nvim_lsp = 'λ',
              vsnip = '⋗',
              buffer = 'Ω',
              path = '🖫',
          }
          item.menu = menu_icon[entry.source.name]
          return item
      end,
  },
})

require('lualine').setup {
    options = {
        theme = 'kanagawa',
        fmt = string.lower,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
    }
}

-- Better Escape
require("better_escape").setup {
    mapping = { "jj", "jk" }, -- a table with mappings to use
    timeout = vim.o.timeoutlen, -- the time in which the keys must be hit in ms. Use option timeoutlen by default
    clear_empty_lines = false, -- clear line after escaping if there is only whitespace
    keys = "<Esc>", -- keys used for escaping, if it is a function will use the result everytime
}

require 'hop'.setup {
    keys = 'etovxqpdygfblzhckisuran',
    jump_on_sole_occurrence = false,
}

require 'nvim-autopairs'.setup {}

require('todo-comments').setup {
    colors = {
        error = { "DiagnosticError", "ErrorMsg", "#de5d68" },
        warning = { "DiagnosticWarning", "WarningMsg", "#eeb927" },
        info = { "DiagnosticInfo", "#57a5e5" },
        hint = { "DiagnosticHint", "#bb70d2" },
        default = { "Identifier", "#de5d68" },
    },
}

require('nvim-tree').setup {
    sort_by = "case_sensitive",
    view = {
        adaptive_size = false,
        mappings = {
            list = {
                { key = "u", action = "dir_up" },
            },
        },
    },
    renderer = {
        group_empty = true,
        icons = {
            git_placement = "after",
            glyphs = {
                git = {
                    unstaged = "-",
                    staged = "s",
                    untracked = "u",
                    renamed = "r",
                    deleted = "d",
                    ignored = "i",
                },
            },
        },
    },
    filters = {
        dotfiles = false,
    },
    git = {
        ignore = false,
    },
}

require('nvim-treesitter.configs').setup {
  ensure_installed = { "lua", "rust", "toml" },
  auto_install = true,
  highlight = {
    enable = true,
    additional_vim_regex_highlighting=false,
  },
  ident = { enable = true }, 
  rainbow = {
    enable = true,
    extended_mode = true,
    max_file_lines = nil,
  }
}

-- Comment Setup
require('Comment').setup({
    mappings = {
        basic = false,
        extra = false,
        extended = false,
    },
})

require('opts')
require('keys')

-- the first run will install packer and our plugins
if packer_bootstrap then
  require("packer").sync()
  return
end

