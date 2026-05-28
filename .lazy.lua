return {
  {
    "neovim/nvim-lspconfig",
    ft = { "python" },
    config = function()
      local function glob_matches(dir, patterns)
        for _, pattern in ipairs(patterns) do
          if #vim.fn.globpath(dir, pattern, false, true) > 0 then
            return true
          end
        end

        return false
      end

      local function py_root(fname)
        local path = vim.fs.normalize(fname)
        local dir = vim.fs.dirname(path)

        if not dir or dir == "" then
          return nil
        end

        local patterns = {
          "pyproject.toml",
          "uv.lock",
          "requirements.txt",
          "setup.py",
          "setup.cfg",
        }

        if glob_matches(dir, patterns) then
          return dir
        end

        for parent in vim.fs.parents(dir) do
          if glob_matches(parent, patterns) then
            return parent
          end
        end

        return dir
      end

      vim.lsp.config("basedpyright", {
        root_dir = function(bufnr, on_dir)
          local fname = vim.api.nvim_buf_get_name(bufnr)
          on_dir(py_root(fname))
        end,
        single_file_support = true,
      })

      vim.lsp.enable("basedpyright")
    end,
  },
}
