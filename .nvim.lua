local has_rd, rd = pcall(require, "rainbow-delimiters")

if has_rd then
    rd.toggle()
end

local has_MiniDeps, MiniDeps = pcall(require, "mini.deps")

if has_MiniDeps then
    local local_deps_path = vim.fn.getcwd() .. "/.nvim/deps"
    MiniDeps.setup({ path = { package = local_deps_path } })
    MiniDeps.now(function()
        MiniDeps.add({ source = "Olical/conjure" })
        MiniDeps.add({
            source = "eraserhd/parinfer-rust",
            hooks = {
                post_install = function(params)
                    vim.notify("Building parinfer-rust...", vim.log.levels.INFO)
                    os.execute("cd " .. params.path .. " && cargo build --release > build.log 2>&1")
                    vim.notify("Parinfer build complete", vim.log.levels.INFO)
                end,
                post_checkout = function(params)
                    vim.notify("Updating parinfer-rust...", vim.log.levels.INFO)
                    os.execute("cd " .. params.path .. " && cargo build --release > build.log 2>&1")
                    vim.notify("Parinfer update complete", vim.log.levels.INFO)
                end,
            },
        })

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

vim.api.nvim_create_autocmd("FileType", {
    pattern = { "racket", "scheme" },
    callback = function(args)
        local root_dir = vim.fs.dirname(vim.fs.find({ "info.rkt", ".git" }, { upward = true })[1])
        if not root_dir then
            root_dir = vim.fs.dirname(vim.api.nvim_buf_get_name(args.buf))
        end
        vim.lsp.start({
            name = "racket_langserver",
            cmd = { "racket", "--lib", "racket-langserver" },
            root_dir = root_dir,
        })
    end,
})

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

vim.notify("SICP Local Config Loaded", vim.log.levels.INFO)
