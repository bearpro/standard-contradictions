return {
  {
    "nvim-treesitter/nvim-treesitter",
    ft = { "fsharp" },
    build = ":TSUpdate",
    opts = {},
    config = function(_, opts)
      local treesitter = require("nvim-treesitter")

      treesitter.setup(opts)
      treesitter.install({ "fsharp" })

      local group = vim.api.nvim_create_augroup("bearpro_treesitter_fsharp", { clear = true })

      vim.api.nvim_create_autocmd("FileType", {
        group = group,
        pattern = { "fsharp" },
        callback = function(args)
          vim.treesitter.start(args.buf)
          vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end,
      })
    end,
  },
  {
    "neovim/nvim-lspconfig",
    ft = { "fsharp", "python" },
    config = function()
      local function glob_matches(dir, patterns)
        for _, pattern in ipairs(patterns) do
          if #vim.fn.globpath(dir, pattern, false, true) > 0 then
            return true
          end
        end

        return false
      end

      local function find_root(fname, patterns)
        local path = vim.fs.normalize(fname)
        local dir = vim.fs.dirname(path)

        if not dir or dir == "" then
          return nil
        end

        if glob_matches(dir, patterns) then
          return dir
        end

        for parent in vim.fs.parents(dir) do
          if glob_matches(parent, patterns) then
            return parent
          end
        end

        return nil
      end

      local fs_root = function(fname)
        return find_root(fname, { "*.sln", "*.fsproj" })
      end

      local py_root = function(fname)
        return find_root(fname, {
          "pyproject.toml",
          "uv.lock",
          "requirements.txt",
          "setup.py",
          "setup.cfg",
        }) or vim.fs.dirname(vim.fs.normalize(fname))
      end

      vim.lsp.config("fsautocomplete", {
        root_dir = function(bufnr, on_dir)
          local fname = vim.api.nvim_buf_get_name(bufnr)
          on_dir(fs_root(fname))
        end,
        single_file_support = false,
      })

      vim.lsp.config("basedpyright", {
        root_dir = function(bufnr, on_dir)
          local fname = vim.api.nvim_buf_get_name(bufnr)
          on_dir(py_root(fname))
        end,
        single_file_support = true,
      })

      vim.lsp.enable("fsautocomplete")
      vim.lsp.enable("basedpyright")
    end,
  },
}
