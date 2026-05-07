local root = vim.fn.getcwd()
local parser_path = root .. "/parser/mdl.so"

local diagnostic_config = {
  virtual_text = false,
  signs = true,
  underline = true,
  severity_sort = true,
}

local function configure_diagnostics(namespace, bufnr)
  vim.diagnostic.config(diagnostic_config, namespace)

  vim.api.nvim_set_hl(0, "DiagnosticUnderlineError", { undercurl = true, sp = "#e86671" })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineWarn", { bg = "#3a3000", undercurl = true, sp = "#d7a65f" })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineInfo", { undercurl = true, sp = "#6cb6ff" })
  vim.api.nvim_set_hl(0, "DiagnosticUnderlineHint", { undercurl = true, sp = "#7ee787" })
  vim.api.nvim_set_hl(0, "@attribute.mdl", { fg = "#d7a65f", bold = true })
  vim.api.nvim_set_hl(0, "@constructor.mdl", { link = "@type" })

  if namespace and bufnr then
    vim.schedule(function()
      vim.diagnostic.hide(namespace, bufnr)
      vim.diagnostic.show(namespace, bufnr)
    end)
  end
end

local function setup_mdl_demo()
  vim.opt.runtimepath:prepend(root)

  configure_diagnostics(nil, nil)

  if vim.fn.filereadable(parser_path) == 0 then
    vim.fn.system({ root .. "/scripts/build-parser.sh" })
  end

  vim.filetype.add({
    extension = {
      mdl = "mdl",
    },
  })

  vim.treesitter.language.add("mdl", {
    path = parser_path,
  })

  vim.api.nvim_create_autocmd("FileType", {
    group = vim.api.nvim_create_augroup("mdl_demo_lang", { clear = true }),
    pattern = "mdl",
    callback = function(args)
      vim.treesitter.start(args.buf, "mdl")

      local client_id = vim.lsp.start({
        name = "mdl-demo-lsp",
        cmd = { "python3", root .. "/lsp/server.py" },
        root_dir = root,
      }, {
        bufnr = args.buf,
      })

      if client_id then
        local namespace = vim.lsp.diagnostic.get_namespace(client_id)
        configure_diagnostics(namespace, args.buf)
      end
    end,
  })
end

return {
  {
    name = "mdl-demo-lang",
    dir = root,
    lazy = false,
    init = setup_mdl_demo,
  },
}
