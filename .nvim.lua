local has_rd, rd = pcall(require, "rainbow-delimiters")

if has_rd then
    rd.toggle()
end

local has_MiniDeps, MiniDeps = pcall(require, "mini.deps")

if has_MiniDeps then
    local local_deps_path = vim.fn.fnamemodify(debug.getinfo(1, "S").source:sub(2), ":h") .. "/.nvim/deps"
    MiniDeps.setup({ path = { package = local_deps_path } })
    MiniDeps.now(function()
        MiniDeps.add({ source = "Olical/conjure" })
        MiniDeps.add({ source = "julienvincent/nvim-paredit" })

        local has_ts, parsers = pcall(require, "nvim-treesitter.parsers")
        if has_ts and not parsers.has_parser("scheme") then
            vim.notify("Installing Scheme parser...", vim.log.levels.INFO)
            vim.cmd("TSInstallSync scheme")
        end

        vim.schedule(function()
            if vim.bo.filetype ~= "" then
                vim.cmd("doautocmd FileType")
            end
        end)
    end)
end

local function find_root(args)
    local buf = args and args.buf or vim.api.nvim_get_current_buf()
    local buf_name = vim.api.nvim_buf_get_name(buf)
    if buf_name == "" then
        return vim.fn.getcwd()
    end
    local found = vim.fs.find({ "info.rkt", ".devenv", ".git" }, {
        upward = true,
        path = vim.fs.dirname(buf_name),
    })[1]
    if found then
        return vim.fs.dirname(found)
    end
    return vim.fs.dirname(buf_name)
end

local function start_scheme_lsp(args)
    if vim.fn.executable("scheme-langserver") ~= 1 then
        vim.notify("scheme-langserver not found in PATH", vim.log.levels.WARN)
        return
    end
    local root_dir = find_root(args)
    vim.lsp.start({
        name = "scheme_langserver",
        cmd = { "scheme-langserver" },
        root_dir = root_dir,
    })
end

local function start_racket_lsp(args)
    if vim.fn.executable("racket") ~= 1 then
        vim.notify("racket not found in PATH", vim.log.levels.WARN)
        return
    end
    local root_dir = find_root(args)
    vim.lsp.start({
        name = "racket_langserver",
        cmd = { "racket", "--lib", "racket-langserver" },
        root_dir = root_dir,
    })
end

local function start_local_lsps(buf)
    if vim.bo[buf].filetype == "scheme" then
        start_scheme_lsp({ buf = buf })
    elseif vim.bo[buf].filetype == "racket" then
        start_racket_lsp({ buf = buf })
    end
end

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "scheme" },
    callback = start_scheme_lsp,
})

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "racket" },
    callback = start_racket_lsp,
})

local has_paredit, paredit = pcall(require, "nvim-paredit")
if has_paredit then
    paredit.setup({
        indent = { enabled = true },
        keys = {
            ["<localleader>w"] = {
                function()
                    paredit.cursor.place_cursor(
                        paredit.wrap.wrap_element_under_cursor("( ", ")"),
                        { placement = "inner_start", mode = "insert" }
                    )
                end,
                "Wrap element insert head",
            },
            ["<localleader>W"] = {
                function()
                    paredit.cursor.place_cursor(
                        paredit.wrap.wrap_element_under_cursor("(", ")"),
                        { placement = "inner_end", mode = "insert" }
                    )
                end,
                "Wrap element insert tail",
            },
            ["<localleader>i"] = {
                function()
                    paredit.cursor.place_cursor(
                        paredit.wrap.wrap_enclosing_form_under_cursor("( ", ")"),
                        { placement = "inner_start", mode = "insert" }
                    )
                end,
                "Wrap form insert head",
            },

            ["<localleader>I"] = {
                function()
                    paredit.cursor.place_cursor(
                        paredit.wrap.wrap_enclosing_form_under_cursor("(", ")"),
                        { placement = "inner_end", mode = "insert" }
                    )
                end,
                "Wrap form insert tail",
            },
        },
    })
end

local has_conform, conform = pcall(require, "conform")
if has_conform then
    conform.formatters.schemat = { command = "schemat" }
    conform.setup({
        format_on_save = { timeout_ms = 3000, lsp_fallback = true },
    })
    conform.formatters_by_ft.scheme = { "schemat" }
    conform.formatters_by_ft.racket = { "racketfmt" }
end

local has_wk, wk = pcall(require, "which-key")
if has_wk then
    wk.add({ { "<leader>e", desc = "[E]val", icon = { icon = "", color = "purple" } } })
end

start_local_lsps(vim.api.nvim_get_current_buf())

vim.notify("SICP Local Config Loaded", vim.log.levels.INFO)
