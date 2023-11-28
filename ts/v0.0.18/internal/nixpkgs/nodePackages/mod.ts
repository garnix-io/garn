import { mkPackage } from "../../../package.ts";
import { nixRaw } from "../../../nix.ts";

/**
 * CLI tool for Angular
 */
export const _angular_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@angular/cli`,
  "CLI tool for Angular",
);

/**
 * Use the right package manager
 */
export const _antfu_ni = mkPackage(
  nixRaw`pkgs.nodePackages.@antfu/ni`,
  "Use the right package manager",
);

/**
 * The Astro language server, implement the [language server protocol](https://microsoft.github.io/language-server-protocol/)
 */
export const _astrojs_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.@astrojs/language-server`,
  "The Astro language server, implement the [language server protocol](https://microsoft.github.io/language-server-protocol/)",
);

/**
 * Babel command line.
 */
export const _babel_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@babel/cli`,
  "Babel command line.",
);

/**
 * Lint your commit messages
 */
export const _commitlint_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@commitlint/cli`,
  "Lint your commit messages",
);

/**
 * Shareable commitlint config enforcing conventional commits
 */
export const _commitlint_config_conventional = mkPackage(
  nixRaw`pkgs.nodePackages.@commitlint/config-conventional`,
  "Shareable commitlint config enforcing conventional commits",
);

/**
 * A complete tool for building modern Electron applications
 */
export const _electron_forge_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@electron-forge/cli`,
  "A complete tool for building modern Electron applications",
);

/**
 * CLI implementation of the GitLab API.
 */
export const _gitbeaker_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@gitbeaker/cli`,
  "CLI implementation of the GitLab API.",
);

/**
 * A professional solution for consolidating all your JavaScript projects in one Git repo
 */
export const _microsoft_rush = mkPackage(
  nixRaw`pkgs.nodePackages.@microsoft/rush`,
  "A professional solution for consolidating all your JavaScript projects in one Git repo",
);

/**
 * Prisma Language Server
 */
export const _prisma_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.@prisma/language-server`,
  "Prisma Language Server",
);

/**
 * A CLI tool to build for the Shopify platform
 */
export const _shopify_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@shopify/cli`,
  "A CLI tool to build for the Shopify platform",
);

/**
 * A plugin that provides a composable API for giving elements a fixed aspect ratio.
 */
export const _tailwindcss_aspect_ratio = mkPackage(
  nixRaw`pkgs.nodePackages.@tailwindcss/aspect-ratio`,
  "A plugin that provides a composable API for giving elements a fixed aspect ratio.",
);

/**
 * A plugin that provides a basic reset for form styles that makes form elements easy to override with utilities.
 */
export const _tailwindcss_forms = mkPackage(
  nixRaw`pkgs.nodePackages.@tailwindcss/forms`,
  "A plugin that provides a basic reset for form styles that makes form elements easy to override with utilities.",
);

/**
 * Tailwind CSS Language Server
 */
export const _tailwindcss_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.@tailwindcss/language-server`,
  "Tailwind CSS Language Server",
);

/**
 * A plugin that provides utilities for visually truncating text after a fixed number of lines.
 */
export const _tailwindcss_line_clamp = mkPackage(
  nixRaw`pkgs.nodePackages.@tailwindcss/line-clamp`,
  "A plugin that provides utilities for visually truncating text after a fixed number of lines.",
);

/**
 * A Tailwind CSS plugin for automatically styling plain HTML content with beautiful typographic defaults.
 */
export const _tailwindcss_typography = mkPackage(
  nixRaw`pkgs.nodePackages.@tailwindcss/typography`,
  "A Tailwind CSS plugin for automatically styling plain HTML content with beautiful typographic defaults.",
);

/**
 * OAuth helper and remote fetcher for Uppy's (https://uppy.io) extensible file upload widget with support for drag&drop, resumable uploads, previews, restrictions, file processing/encoding, remote providers like Dropbox and Google Drive, S3 and more :dog:
 */
export const _uppy_companion = mkPackage(
  nixRaw`pkgs.nodePackages.@uppy/companion`,
  "OAuth helper and remote fetcher for Uppy's (https://uppy.io) extensible file upload widget with support for drag&drop, resumable uploads, previews, restrictions, file processing/encoding, remote providers like Dropbox and Google Drive, S3 and more :dog:",
);

export const _volar_vue_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.@volar/vue-language-server`,
  "",
);

/**
 * Command line interface for rapid Vue.js development
 */
export const _vue_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@vue/cli`,
  "Command line interface for rapid Vue.js development",
);

/**
 * Toolbox for WebAssembly
 */
export const _webassemblyjs_cli_1_11_1 = mkPackage(
  nixRaw`pkgs.nodePackages.@webassemblyjs/cli-1.11.1`,
  "Toolbox for WebAssembly",
);

/**
 * WebAssembly REPL
 */
export const _webassemblyjs_repl_1_11_1 = mkPackage(
  nixRaw`pkgs.nodePackages.@webassemblyjs/repl-1.11.1`,
  "WebAssembly REPL",
);

/**
 * > Strips custom sections
 */
export const _webassemblyjs_wasm_strip = mkPackage(
  nixRaw`pkgs.nodePackages.@webassemblyjs/wasm-strip`,
  "> Strips custom sections",
);

/**
 * Emit documentation/code for your WASM binary Edit
 */
export const _webassemblyjs_wasm_text_gen_1_11_1 = mkPackage(
  nixRaw`pkgs.nodePackages.@webassemblyjs/wasm-text-gen-1.11.1`,
  "Emit documentation/code for your WASM binary Edit",
);

/**
 * WAST refmt
 */
export const _webassemblyjs_wast_refmt_1_11_1 = mkPackage(
  nixRaw`pkgs.nodePackages.@webassemblyjs/wast-refmt-1.11.1`,
  "WAST refmt",
);

export const _withgraphite_graphite_cli = mkPackage(
  nixRaw`pkgs.nodePackages.@withgraphite/graphite-cli`,
  "",
);

/**
 * nginx-language-server extension for coc.nvim
 */
export const _yaegassy_coc_nginx = mkPackage(
  nixRaw`pkgs.nodePackages.@yaegassy/coc-nginx`,
  "nginx-language-server extension for coc.nvim",
);

/**
 * Full access to zwave-js driver through Websockets
 */
export const _zwave_js_server = mkPackage(
  nixRaw`pkgs.nodePackages.@zwave-js/server`,
  "Full access to zwave-js driver through Websockets",
);

/**
 * Catch insensitive, inconsiderate writing
 */
export const alex = mkPackage(
  nixRaw`pkgs.nodePackages.alex`,
  "Catch insensitive, inconsiderate writing",
);

/**
 * Concat small audio files into single file and export in many formats.
 */
export const audiosprite = mkPackage(
  nixRaw`pkgs.nodePackages.audiosprite`,
  "Concat small audio files into single file and export in many formats.",
);

/**
 * Command line tool for generating a changelog from git tags and commit history
 */
export const auto_changelog = mkPackage(
  nixRaw`pkgs.nodePackages.auto-changelog`,
  "Command line tool for generating a changelog from git tags and commit history",
);

/**
 * Parse CSS and add vendor prefixes to CSS rules using values from the Can I Use website
 */
export const autoprefixer = mkPackage(
  nixRaw`pkgs.nodePackages.autoprefixer`,
  "Parse CSS and add vendor prefixes to CSS rules using values from the Can I Use website",
);

/**
 * Linter for Awesome lists
 */
export const awesome_lint = mkPackage(
  nixRaw`pkgs.nodePackages.awesome-lint`,
  "Linter for Awesome lists",
);

/**
 * CDK Toolkit, the command line tool for CDK apps
 */
export const aws_cdk = mkPackage(
  nixRaw`pkgs.nodePackages.aws-cdk`,
  "CDK Toolkit, the command line tool for CDK apps",
);

/**
 * A language server for Bash
 */
export const bash_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.bash-language-server`,
  "A language server for Bash",
);

/**
 * The browser package manager
 */
export const bower = mkPackage(
  nixRaw`pkgs.nodePackages.bower`,
  "The browser package manager",
);

/**
 * Generate nix expressions to fetch bower dependencies
 */
export const bower2nix = mkPackage(
  nixRaw`pkgs.nodePackages.bower2nix`,
  "Generate nix expressions to fetch bower dependencies",
);

/**
 * Live CSS Reload & Browser Syncing
 */
export const browser_sync = mkPackage(
  nixRaw`pkgs.nodePackages.browser-sync`,
  "Live CSS Reload & Browser Syncing",
);

/**
 * browser-side require() the node way
 */
export const browserify = mkPackage(
  nixRaw`pkgs.nodePackages.browserify`,
  "browser-side require() the node way",
);

/**
 * This is the command line tool for Cloud Development Kit (CDK) for Kubernetes (cdk8s).
 */
export const cdk8s_cli = mkPackage(
  nixRaw`pkgs.nodePackages.cdk8s-cli`,
  "This is the command line tool for Cloud Development Kit (CDK) for Kubernetes (cdk8s).",
);

/**
 * CDK for Terraform CLI
 */
export const cdktf_cli = mkPackage(
  nixRaw`pkgs.nodePackages.cdktf-cli`,
  "CDK for Terraform CLI",
);

/**
 * Access the system clipboard (copy/paste)
 */
export const clipboard_cli = mkPackage(
  nixRaw`pkgs.nodePackages.clipboard-cli`,
  "Access the system clipboard (copy/paste)",
);

/**
 * clangd extension for coc.nvim
 */
export const coc_clangd = mkPackage(
  nixRaw`pkgs.nodePackages.coc-clangd`,
  "clangd extension for coc.nvim",
);

/**
 * coc.nvim extension for cmake language
 */
export const coc_cmake = mkPackage(
  nixRaw`pkgs.nodePackages.coc-cmake`,
  "coc.nvim extension for cmake language",
);

/**
 * Css extension for coc.nvim
 */
export const coc_css = mkPackage(
  nixRaw`pkgs.nodePackages.coc-css`,
  "Css extension for coc.nvim",
);

/**
 * diagnostic-languageserver extension for coc.nvim
 */
export const coc_diagnostic = mkPackage(
  nixRaw`pkgs.nodePackages.coc-diagnostic`,
  "diagnostic-languageserver extension for coc.nvim",
);

/**
 * docker extension for coc
 */
export const coc_docker = mkPackage(
  nixRaw`pkgs.nodePackages.coc-docker`,
  "docker extension for coc",
);

/**
 * emmet extension for coc
 */
export const coc_emmet = mkPackage(
  nixRaw`pkgs.nodePackages.coc-emmet`,
  "emmet extension for coc",
);

/**
 * Eslint extension for coc.nvim
 */
export const coc_eslint = mkPackage(
  nixRaw`pkgs.nodePackages.coc-eslint`,
  "Eslint extension for coc.nvim",
);

/**
 * explorer for coc.nvim
 */
export const coc_explorer = mkPackage(
  nixRaw`pkgs.nodePackages.coc-explorer`,
  "explorer for coc.nvim",
);

/**
 * flutter support for (Neo)vim
 */
export const coc_flutter = mkPackage(
  nixRaw`pkgs.nodePackages.coc-flutter`,
  "flutter support for (Neo)vim",
);

/**
 * Git extension for coc.nvim
 */
export const coc_git = mkPackage(
  nixRaw`pkgs.nodePackages.coc-git`,
  "Git extension for coc.nvim",
);

/**
 * gopls extension for coc
 */
export const coc_go = mkPackage(
  nixRaw`pkgs.nodePackages.coc-go`,
  "gopls extension for coc",
);

/**
 * Haxe language server extension for coc.nvim
 */
export const coc_haxe = mkPackage(
  nixRaw`pkgs.nodePackages.coc-haxe`,
  "Haxe language server extension for coc.nvim",
);

/**
 * Highlight extension for coc.nvim
 */
export const coc_highlight = mkPackage(
  nixRaw`pkgs.nodePackages.coc-highlight`,
  "Highlight extension for coc.nvim",
);

/**
 * Html extension for coc.nvim
 */
export const coc_html = mkPackage(
  nixRaw`pkgs.nodePackages.coc-html`,
  "Html extension for coc.nvim",
);

/**
 * Java language extension for coc.nvim
 */
export const coc_java = mkPackage(
  nixRaw`pkgs.nodePackages.coc-java`,
  "Java language extension for coc.nvim",
);

/**
 * jest extension for coc.nvim
 */
export const coc_jest = mkPackage(
  nixRaw`pkgs.nodePackages.coc-jest`,
  "jest extension for coc.nvim",
);

/**
 * Json extension for coc.nvim
 */
export const coc_json = mkPackage(
  nixRaw`pkgs.nodePackages.coc-json`,
  "Json extension for coc.nvim",
);

/**
 * Basic list sources for coc.nvim
 */
export const coc_lists = mkPackage(
  nixRaw`pkgs.nodePackages.coc-lists`,
  "Basic list sources for coc.nvim",
);

/**
 * Grammar/spell checker using LanguageTool with support for LaTeX, Markdown, and others
 */
export const coc_ltex = mkPackage(
  nixRaw`pkgs.nodePackages.coc-ltex`,
  "Grammar/spell checker using LanguageTool with support for LaTeX, Markdown, and others",
);

/**
 * Markdownlint extension for coc.nvim
 */
export const coc_markdownlint = mkPackage(
  nixRaw`pkgs.nodePackages.coc-markdownlint`,
  "Markdownlint extension for coc.nvim",
);

/**
 * coc.nvim extension for Metals, the Scala language server
 */
export const coc_metals = mkPackage(
  nixRaw`pkgs.nodePackages.coc-metals`,
  "coc.nvim extension for Metals, the Scala language server",
);

/**
 * Auto pair extension for coc.nvim
 */
export const coc_pairs = mkPackage(
  nixRaw`pkgs.nodePackages.coc-pairs`,
  "Auto pair extension for coc.nvim",
);

/**
 * prettier extension for coc.nvim
 */
export const coc_prettier = mkPackage(
  nixRaw`pkgs.nodePackages.coc-prettier`,
  "prettier extension for coc.nvim",
);

/**
 * Pyright extension for coc.nvim, static type checker for Python
 */
export const coc_pyright = mkPackage(
  nixRaw`pkgs.nodePackages.coc-pyright`,
  "Pyright extension for coc.nvim, static type checker for Python",
);

/**
 * Python extension for coc.nvim, forked from vscode-python.
 */
export const coc_python = mkPackage(
  nixRaw`pkgs.nodePackages.coc-python`,
  "Python extension for coc.nvim, forked from vscode-python.",
);

/**
 * R language server extension for coc.nvim
 */
export const coc_r_lsp = mkPackage(
  nixRaw`pkgs.nodePackages.coc-r-lsp`,
  "R language server extension for coc.nvim",
);

/**
 * Rust language support - code completion, Intellisense, refactoring, reformatting, errors, snippets. A client for the Rust Language Server, built by the RLS team.
 */
export const coc_rls = mkPackage(
  nixRaw`pkgs.nodePackages.coc-rls`,
  "Rust language support - code completion, Intellisense, refactoring, reformatting, errors, snippets. A client for the Rust Language Server, built by the RLS team.",
);

/**
 * rust-analyzer for Vim/Neovim, works as an extension with coc.nvim
 */
export const coc_rust_analyzer = mkPackage(
  nixRaw`pkgs.nodePackages.coc-rust-analyzer`,
  "rust-analyzer for Vim/Neovim, works as an extension with coc.nvim",
);

/**
 * sh extension for coc
 */
export const coc_sh = mkPackage(
  nixRaw`pkgs.nodePackages.coc-sh`,
  "sh extension for coc",
);

/**
 * Smart find extension for coc.nvim
 */
export const coc_smartf = mkPackage(
  nixRaw`pkgs.nodePackages.coc-smartf`,
  "Smart find extension for coc.nvim",
);

/**
 * Snippets extension for coc.nvim
 */
export const coc_snippets = mkPackage(
  nixRaw`pkgs.nodePackages.coc-snippets`,
  "Snippets extension for coc.nvim",
);

/**
 * Ruby languageserver extension for coc.nvim, using solargraph
 */
export const coc_solargraph = mkPackage(
  nixRaw`pkgs.nodePackages.coc-solargraph`,
  "Ruby languageserver extension for coc.nvim, using solargraph",
);

/**
 * Spelling checker for source code
 */
export const coc_spell_checker = mkPackage(
  nixRaw`pkgs.nodePackages.coc-spell-checker`,
  "Spelling checker for source code",
);

/**
 * SQLFluff (A SQL linter and auto-formatter for Humans) extension for coc.nvim
 */
export const coc_sqlfluff = mkPackage(
  nixRaw`pkgs.nodePackages.coc-sqlfluff`,
  "SQLFluff (A SQL linter and auto-formatter for Humans) extension for coc.nvim",
);

/**
 * stylelint extension for coc.nvim
 */
export const coc_stylelint = mkPackage(
  nixRaw`pkgs.nodePackages.coc-stylelint`,
  "stylelint extension for coc.nvim",
);

/**
 * Lua extension using sumneko lua-language-server for coc.nvim
 */
export const coc_sumneko_lua = mkPackage(
  nixRaw`pkgs.nodePackages.coc-sumneko-lua`,
  "Lua extension using sumneko lua-language-server for coc.nvim",
);

/**
 * tabnine extension for coc.nvim
 */
export const coc_tabnine = mkPackage(
  nixRaw`pkgs.nodePackages.coc-tabnine`,
  "tabnine extension for coc.nvim",
);

/**
 * TexLab extension for coc.nvim
 */
export const coc_texlab = mkPackage(
  nixRaw`pkgs.nodePackages.coc-texlab`,
  "TexLab extension for coc.nvim",
);

/**
 * toml extension for coc.nvim
 */
export const coc_toml = mkPackage(
  nixRaw`pkgs.nodePackages.coc-toml`,
  "toml extension for coc.nvim",
);

/**
 * tslint extension for coc.nvim
 */
export const coc_tslint = mkPackage(
  nixRaw`pkgs.nodePackages.coc-tslint`,
  "tslint extension for coc.nvim",
);

/**
 * TSLint extension for coc.nvim as tsserver plugin
 */
export const coc_tslint_plugin = mkPackage(
  nixRaw`pkgs.nodePackages.coc-tslint-plugin`,
  "TSLint extension for coc.nvim as tsserver plugin",
);

/**
 * tsserver extension for coc.nvim
 */
export const coc_tsserver = mkPackage(
  nixRaw`pkgs.nodePackages.coc-tsserver`,
  "tsserver extension for coc.nvim",
);

/**
 * ultisnips source for coc.nvim
 */
export const coc_ultisnips = mkPackage(
  nixRaw`pkgs.nodePackages.coc-ultisnips`,
  "ultisnips source for coc.nvim",
);

/**
 * Vue language server extension for coc.nvim using vetur
 */
export const coc_vetur = mkPackage(
  nixRaw`pkgs.nodePackages.coc-vetur`,
  "Vue language server extension for coc.nvim using vetur",
);

/**
 * vim language server extension for coc.nvim
 */
export const coc_vimlsp = mkPackage(
  nixRaw`pkgs.nodePackages.coc-vimlsp`,
  "vim language server extension for coc.nvim",
);

/**
 * vimtex integration for coc.nvim
 */
export const coc_vimtex = mkPackage(
  nixRaw`pkgs.nodePackages.coc-vimtex`,
  "vimtex integration for coc.nvim",
);

/**
 * wxml language server extension for coc.nvim
 */
export const coc_wxml = mkPackage(
  nixRaw`pkgs.nodePackages.coc-wxml`,
  "wxml language server extension for coc.nvim",
);

/**
 * yaml extension for coc.nvim
 */
export const coc_yaml = mkPackage(
  nixRaw`pkgs.nodePackages.coc-yaml`,
  "yaml extension for coc.nvim",
);

/**
 * Yank extension for coc.nvim
 */
export const coc_yank = mkPackage(
  nixRaw`pkgs.nodePackages.coc-yank`,
  "Yank extension for coc.nvim",
);

/**
 * Convert any vscode theme with ease!
 */
export const code_theme_converter = mkPackage(
  nixRaw`pkgs.nodePackages.code-theme-converter`,
  "Convert any vscode theme with ease!",
);

/**
 * A cryptocurrency price monitoring tool
 */
export const coinmon = mkPackage(
  nixRaw`pkgs.nodePackages.coinmon`,
  "A cryptocurrency price monitoring tool",
);

/**
 * Run commands concurrently
 */
export const concurrently = mkPackage(
  nixRaw`pkgs.nodePackages.concurrently`,
  "Run commands concurrently",
);

/**
 * Generate a changelog from git metadata
 */
export const conventional_changelog_cli = mkPackage(
  nixRaw`pkgs.nodePackages.conventional-changelog-cli`,
  "Generate a changelog from git metadata",
);

/**
 * Copy files && directories with webpack
 */
export const copy_webpack_plugin = mkPackage(
  nixRaw`pkgs.nodePackages.copy-webpack-plugin`,
  "Copy files && directories with webpack",
);

/**
 * Copy files
 */
export const cpy_cli = mkPackage(
  nixRaw`pkgs.nodePackages.cpy-cli`,
  "Copy files",
);

/**
 * Create Cycle.js with no build configuration.
 */
export const create_cycle_app = mkPackage(
  nixRaw`pkgs.nodePackages.create-cycle-app`,
  "Create Cycle.js with no build configuration.",
);

/**
 * Create React Native apps with no build configuration.
 */
export const create_react_native_app = mkPackage(
  nixRaw`pkgs.nodePackages.create-react-native-app`,
  "Create React Native apps with no build configuration.",
);

/**
 * A Spelling Checker for Code!
 */
export const cspell = mkPackage(
  nixRaw`pkgs.nodePackages.cspell`,
  "A Spelling Checker for Code!",
);

/**
 * CSSLint
 */
export const csslint = mkPackage(
  nixRaw`pkgs.nodePackages.csslint`,
  "CSSLint",
);

/**
 * A DHCP server written in JavaScript
 */
export const dhcp = mkPackage(
  nixRaw`pkgs.nodePackages.dhcp`,
  "A DHCP server written in JavaScript",
);

/**
 * diagnostic language server
 */
export const diagnostic_languageserver = mkPackage(
  nixRaw`pkgs.nodePackages.diagnostic-languageserver`,
  "diagnostic language server",
);

/**
 * Fast Diff to colorized HTML
 */
export const diff2html_cli = mkPackage(
  nixRaw`pkgs.nodePackages.diff2html-cli`,
  "Fast Diff to colorized HTML",
);

/**
 * A global executable to run applications with the ENV variables loaded by dotenv
 */
export const dotenv_cli = mkPackage(
  nixRaw`pkgs.nodePackages.dotenv-cli`,
  "A global executable to run applications with the ENV variables loaded by dotenv",
);

/**
 * A secrets manager for .env files – from the same people that pioneered dotenv.
 */
export const dotenv_vault = mkPackage(
  nixRaw`pkgs.nodePackages.dotenv-vault`,
  "A secrets manager for .env files – from the same people that pioneered dotenv.",
);

/**
 * EAS command line tool
 */
export const eas_cli = mkPackage(
  nixRaw`pkgs.nodePackages.eas-cli`,
  "EAS command line tool",
);

/**
 * import and export tools for elasticsearch
 */
export const elasticdump = mkPackage(
  nixRaw`pkgs.nodePackages.elasticdump`,
  "import and export tools for elasticsearch",
);

/**
 * Query for information about values in elm source files.
 */
export const elm_oracle = mkPackage(
  nixRaw`pkgs.nodePackages.elm-oracle`,
  "Query for information about values in elm source files.",
);

/**
 * Find relevant emoji from text on the command-line
 */
export const emoj = mkPackage(
  nixRaw`pkgs.nodePackages.emoj`,
  "Find relevant emoji from text on the command-line",
);

/**
 * EmojiOne is a complete set of emojis designed for the web. It includes libraries to easily convert unicode characters to shortnames (:smile:) and shortnames to our custom emoji images. PNG formats provided for the emoji images.
 */
export const emojione = mkPackage(
  nixRaw`pkgs.nodePackages.emojione`,
  "EmojiOne is a complete set of emojis designed for the web. It includes libraries to easily convert unicode characters to shortnames (:smile:) and shortnames to our custom emoji images. PNG formats provided for the emoji images.",
);

/**
 * Escape RegExp special characters
 */
export const escape_string_regexp = mkPackage(
  nixRaw`pkgs.nodePackages.escape-string-regexp`,
  "Escape RegExp special characters",
);

/**
 * An AST-based pattern checker for JavaScript.
 */
export const eslint = mkPackage(
  nixRaw`pkgs.nodePackages.eslint`,
  "An AST-based pattern checker for JavaScript.",
);

/**
 * Package builder for esy.
 */
export const esy = mkPackage(
  nixRaw`pkgs.nodePackages.esy`,
  "Package builder for esy.",
);

/**
 * The command-line tool for creating and publishing Expo apps
 */
export const expo_cli = mkPackage(
  nixRaw`pkgs.nodePackages.expo-cli`,
  "The command-line tool for creating and publishing Expo apps",
);

/**
 * Test your download and upload speed using fast.com
 */
export const fast_cli = mkPackage(
  nixRaw`pkgs.nodePackages.fast-cli`,
  "Test your download and upload speed using fast.com",
);

/**
 * faunadb shell
 */
export const fauna_shell = mkPackage(
  nixRaw`pkgs.nodePackages.fauna-shell`,
  "faunadb shell",
);

/**
 * JSON fixer for humans using (relaxed) JSON5
 */
export const fixjson = mkPackage(
  nixRaw`pkgs.nodePackages.fixjson`,
  "JSON fixer for humans using (relaxed) JSON5",
);

/**
 * Fabulously kill processes. Cross-platform.
 */
export const fkill_cli = mkPackage(
  nixRaw`pkgs.nodePackages.fkill-cli`,
  "Fabulously kill processes. Cross-platform.",
);

/**
 * Fleek command line utilities
 */
export const fleek_cli = mkPackage(
  nixRaw`pkgs.nodePackages.fleek-cli`,
  "Fleek command line utilities",
);

/**
 * A simple CLI tool for ensuring that a given node script runs continuously (i.e. forever)
 */
export const forever = mkPackage(
  nixRaw`pkgs.nodePackages.forever`,
  "A simple CLI tool for ensuring that a given node script runs continuously (i.e. forever)",
);

/**
 * Command-line JSON viewer
 */
export const fx = mkPackage(
  nixRaw`pkgs.nodePackages.fx`,
  "Command-line JSON viewer",
);

/**
 * A library and cli to create a local blockchain for fast Ethereum development.
 */
export const ganache = mkPackage(
  nixRaw`pkgs.nodePackages.ganache`,
  "A library and cli to create a local blockchain for fast Ethereum development.",
);

/**
 * Gatsby command-line interface for creating new sites and running Gatsby commands
 */
export const gatsby_cli = mkPackage(
  nixRaw`pkgs.nodePackages.gatsby-cli`,
  "Gatsby command-line interface for creating new sites and running Gatsby commands",
);

/**
 * Downloads the GraphQL Schema of an GraphQL endpoint URL
 */
export const get_graphql_schema = mkPackage(
  nixRaw`pkgs.nodePackages.get-graphql-schema`,
  "Downloads the GraphQL Schema of an GraphQL endpoint URL",
);

/**
 * A tool for managing multiple git repositories
 */
export const git_run = mkPackage(
  nixRaw`pkgs.nodePackages.git-run`,
  "A tool for managing multiple git repositories",
);

/**
 * Recall what you did on the last working day. Psst! or be nosy and find what someone else in your team did ;-)
 */
export const git_standup = mkPackage(
  nixRaw`pkgs.nodePackages.git-standup`,
  "Recall what you did on the last working day. Psst! or be nosy and find what someone else in your team did ;-)",
);

/**
 * Command line grammar checker
 */
export const gramma = mkPackage(
  nixRaw`pkgs.nodePackages.gramma`,
  "Command line grammar checker",
);

/**
 * LSP server implementation for Grammarly
 */
export const grammarly_languageserver = mkPackage(
  nixRaw`pkgs.nodePackages.grammarly-languageserver`,
  "LSP server implementation for Grammarly",
);

export const graphite_cli = mkPackage(
  nixRaw`pkgs.nodePackages.graphite-cli`,
  "",
);

/**
 * A Query Language and Runtime which can target any service.
 */
export const graphql = mkPackage(
  nixRaw`pkgs.nodePackages.graphql`,
  "A Query Language and Runtime which can target any service.",
);

/**
 * Command line tool for common GraphQL development workflows
 */
export const graphql_cli = mkPackage(
  nixRaw`pkgs.nodePackages.graphql-cli`,
  "Command line tool for common GraphQL development workflows",
);

/**
 * An interface for building GraphQL language services for IDEs
 */
export const graphql_language_service_cli = mkPackage(
  nixRaw`pkgs.nodePackages.graphql-language-service-cli`,
  "An interface for building GraphQL language services for IDEs",
);

/**
 * The grunt command line interface
 */
export const grunt_cli = mkPackage(
  nixRaw`pkgs.nodePackages.grunt-cli`,
  "The grunt command line interface",
);

/**
 * The streaming build system.
 */
export const gulp = mkPackage(
  nixRaw`pkgs.nodePackages.gulp`,
  "The streaming build system.",
);

/**
 * Command line interface for gulp
 */
export const gulp_cli = mkPackage(
  nixRaw`pkgs.nodePackages.gulp-cli`,
  "Command line interface for gulp",
);

/**
 * A robust HTML entities encoder/decoder with full Unicode support.
 */
export const he = mkPackage(
  nixRaw`pkgs.nodePackages.he`,
  "A robust HTML entities encoder/decoder with full Unicode support.",
);

/**
 * Handshake airdrop redemption
 */
export const hs_airdrop = mkPackage(
  nixRaw`pkgs.nodePackages.hs-airdrop`,
  "Handshake airdrop redemption",
);

/**
 * A simple zero-configuration command-line http server
 */
export const http_server = mkPackage(
  nixRaw`pkgs.nodePackages.http-server`,
  "A simple zero-configuration command-line http server",
);

/**
 * IJavascript is a Javascript kernel for the Jupyter notebook
 */
export const ijavascript = mkPackage(
  nixRaw`pkgs.nodePackages.ijavascript`,
  "IJavascript is a Javascript kernel for the Jupyter notebook",
);

/**
 * Execute scripts on new messages using IDLE imap command
 */
export const imapnotify = mkPackage(
  nixRaw`pkgs.nodePackages.imapnotify`,
  "Execute scripts on new messages using IDLE imap command",
);

/**
 * Utility to inline images, CSS and JavaScript for a web page - useful for mobile sites
 */
export const inliner = mkPackage(
  nixRaw`pkgs.nodePackages.inliner`,
  "Utility to inline images, CSS and JavaScript for a web page - useful for mobile sites",
);

/**
 * High precision scientific calculator with support for physical units
 */
export const insect = mkPackage(
  nixRaw`pkgs.nodePackages.insect`,
  "High precision scientific calculator with support for physical units",
);

/**
 * A PHP language server
 */
export const intelephense = mkPackage(
  nixRaw`pkgs.nodePackages.intelephense`,
  "A PHP language server",
);

/**
 * Joplin CLI Client
 */
export const joplin = mkPackage(
  nixRaw`pkgs.nodePackages.joplin`,
  "Joplin CLI Client",
);

/**
 * beautifier.io for node
 */
export const js_beautify = mkPackage(
  nixRaw`pkgs.nodePackages.js-beautify`,
  "beautifier.io for node",
);

/**
 * YAML 1.2 parser and serializer
 */
export const js_yaml = mkPackage(
  nixRaw`pkgs.nodePackages.js-yaml`,
  "YAML 1.2 parser and serializer",
);

/**
 * An API documentation generator for JavaScript.
 */
export const jsdoc = mkPackage(
  nixRaw`pkgs.nodePackages.jsdoc`,
  "An API documentation generator for JavaScript.",
);

/**
 * Static analysis tool for JavaScript
 */
export const jshint = mkPackage(
  nixRaw`pkgs.nodePackages.jshint`,
  "Static analysis tool for JavaScript",
);

/**
 * a 'json' command for massaging and processing JSON on the command line
 */
export const json = mkPackage(
  nixRaw`pkgs.nodePackages.json`,
  "a 'json' command for massaging and processing JSON on the command line",
);

/**
 * JSON diff
 */
export const json_diff = mkPackage(
  nixRaw`pkgs.nodePackages.json-diff`,
  "JSON diff",
);

/**
 * Various utilities for JSON References (http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03).
 */
export const json_refs = mkPackage(
  nixRaw`pkgs.nodePackages.json-refs`,
  "Various utilities for JSON References (http://tools.ietf.org/html/draft-pbryan-zyp-json-ref-03).",
);

/**
 * Get a full fake REST API with zero coding in less than 30 seconds
 */
export const json_server = mkPackage(
  nixRaw`pkgs.nodePackages.json-server`,
  "Get a full fake REST API with zero coding in less than 30 seconds",
);

/**
 * Validate JSON
 */
export const jsonlint = mkPackage(
  nixRaw`pkgs.nodePackages.jsonlint`,
  "Validate JSON",
);

/**
 * A simple fake REST API server for testing and prototyping.
 */
export const jsonplaceholder = mkPackage(
  nixRaw`pkgs.nodePackages.jsonplaceholder`,
  "A simple fake REST API server for testing and prototyping.",
);

/**
 * CLI tools for Put.io
 */
export const kaput_cli = mkPackage(
  nixRaw`pkgs.nodePackages.kaput-cli`,
  "CLI tools for Put.io",
);

/**
 * Fast math typesetting for the web.
 */
export const katex = mkPackage(
  nixRaw`pkgs.nodePackages.katex`,
  "Fast math typesetting for the web.",
);

/**
 * CLI for Keyoxide
 */
export const keyoxide = mkPackage(
  nixRaw`pkgs.nodePackages.keyoxide`,
  "CLI for Keyoxide",
);

/**
 * Merges multiple lcov results into one
 */
export const lcov_result_merger = mkPackage(
  nixRaw`pkgs.nodePackages.lcov-result-merger`,
  "Merges multiple lcov results into one",
);

/**
 * Lerna is a fast, modern build system for managing and publishing multiple JavaScript/TypeScript packages from the same repository
 */
export const lerna = mkPackage(
  nixRaw`pkgs.nodePackages.lerna`,
  "Lerna is a fast, modern build system for managing and publishing multiple JavaScript/TypeScript packages from the same repository",
);

/**
 * Leaner CSS
 */
export const less = mkPackage(
  nixRaw`pkgs.nodePackages.less`,
  "Leaner CSS",
);

/**
 * clean-css plugin for less.js
 */
export const less_plugin_clean_css = mkPackage(
  nixRaw`pkgs.nodePackages.less-plugin-clean-css`,
  "clean-css plugin for less.js",
);

/**
 * simple development http server with live reload capability
 */
export const live_server = mkPackage(
  nixRaw`pkgs.nodePackages.live-server`,
  "simple development http server with live reload capability",
);

/**
 * Live Markdown previews for your favourite editor.
 */
export const livedown = mkPackage(
  nixRaw`pkgs.nodePackages.livedown`,
  "Live Markdown previews for your favourite editor.",
);

/**
 * Expose localhost to the world
 */
export const localtunnel = mkPackage(
  nixRaw`pkgs.nodePackages.localtunnel`,
  "Expose localhost to the world",
);

/**
 * Lodash modular utilities.
 */
export const lodash = mkPackage(
  nixRaw`pkgs.nodePackages.lodash`,
  "Lodash modular utilities.",
);

/**
 * Format Lua code
 */
export const lua_fmt = mkPackage(
  nixRaw`pkgs.nodePackages.lua-fmt`,
  "Format Lua code",
);

/**
 * Rasterize vector fonts for embedded use. Supports subsettings & merge.
 */
export const lv_font_conv = mkPackage(
  nixRaw`pkgs.nodePackages.lv_font_conv`,
  "Rasterize vector fonts for embedded use. Supports subsettings & merge.",
);

/**
 * Madoko is a fast scholarly Markdown processor written in Koka
 */
export const madoko = mkPackage(
  nixRaw`pkgs.nodePackages.madoko`,
  "Madoko is a fast scholarly Markdown processor written in Koka",
);

/**
 * The Makam metalanguage -- a tool for rapid language prototyping
 */
export const makam = mkPackage(
  nixRaw`pkgs.nodePackages.makam`,
  "The Makam metalanguage -- a tool for rapid language prototyping",
);

/**
 * checks the all of the hyperlinks in a markdown text to determine if they are alive or dead
 */
export const markdown_link_check = mkPackage(
  nixRaw`pkgs.nodePackages.markdown-link-check`,
  "checks the all of the hyperlinks in a markdown text to determine if they are alive or dead",
);

/**
 * Bot to publish twitter, tumblr or rss posts to an mastodon account.
 */
export const mastodon_bot = mkPackage(
  nixRaw`pkgs.nodePackages.mastodon-bot`,
  "Bot to publish twitter, tumblr or rss posts to an mastodon account.",
);

/**
 * Beautiful and accessible math in all browsers. MathJax is an open-source JavaScript display engine for LaTeX, MathML, and AsciiMath notation that works in all browsers. This package includes the packaged components (install mathjax-full to get the source 
 */
export const mathjax = mkPackage(
  nixRaw`pkgs.nodePackages.mathjax`,
  "Beautiful and accessible math in all browsers. MathJax is an open-source JavaScript display engine for LaTeX, MathML, and AsciiMath notation that works in all browsers. This package includes the packaged components (install mathjax-full to get the source ",
);

/**
 * CLI tools for calling mathjax-node
 */
export const mathjax_node_cli = mkPackage(
  nixRaw`pkgs.nodePackages.mathjax-node-cli`,
  "CLI tools for calling mathjax-node",
);

/**
 * Meeting room kiosk app for displaying meeting room schedules and booking rooms in your organization. Built against Google Apps, but other sources can be defined.
 */
export const meat = mkPackage(
  nixRaw`pkgs.nodePackages.meat`,
  "Meeting room kiosk app for displaying meeting room schedules and booking rooms in your organization. Built against Google Apps, but other sources can be defined.",
);

/**
 * MeshCommander web server
 */
export const meshcommander = mkPackage(
  nixRaw`pkgs.nodePackages.meshcommander`,
  "MeshCommander web server",
);

/**
 * simple, flexible, fun test framework
 */
export const mocha = mkPackage(
  nixRaw`pkgs.nodePackages.mocha`,
  "simple, flexible, fun test framework",
);

/**
 * Multi-file Swagger example
 */
export const multi_file_swagger = mkPackage(
  nixRaw`pkgs.nodePackages.multi-file-swagger`,
  "Multi-file Swagger example",
);

/**
 * Neovim client API and neovim remote plugin provider
 */
export const neovim = mkPackage(
  nixRaw`pkgs.nodePackages.neovim`,
  "Neovim client API and neovim remote plugin provider",
);

/**
 * An internal DSL for the Nix package manager in JavaScript
 */
export const nijs = mkPackage(
  nixRaw`pkgs.nodePackages.nijs`,
  "An internal DSL for the Nix package manager in JavaScript",
);

/**
 * Generate Nix expressions to build NPM packages
 */
export const node2nix = mkPackage(
  nixRaw`pkgs.nodePackages.node2nix`,
  "Generate Nix expressions to build NPM packages",
);

/**
 * Node.js native addon build tool
 */
export const node_gyp = mkPackage(
  nixRaw`pkgs.nodePackages.node-gyp`,
  "Node.js native addon build tool",
);

/**
 * Build tool and bindings loader for node-gyp that supports prebuilds
 */
export const node_gyp_build = mkPackage(
  nixRaw`pkgs.nodePackages.node-gyp-build`,
  "Build tool and bindings loader for node-gyp that supports prebuilds",
);

/**
 * Node.js native addon binary install tool
 */
export const node_pre_gyp = mkPackage(
  nixRaw`pkgs.nodePackages.node-pre-gyp`,
  "Node.js native addon binary install tool",
);

/**
 * Low-code programming for event-driven applications
 */
export const node_red = mkPackage(
  nixRaw`pkgs.nodePackages.node-red`,
  "Low-code programming for event-driven applications",
);

/**
 * Event-driven I/O framework for the V8 JavaScript engine
 */
export const nodejs = mkPackage(
  nixRaw`pkgs.nodePackages.nodejs`,
  "Event-driven I/O framework for the V8 JavaScript engine",
);

/**
 * Simple monitor script for use during development of a Node.js app.
 */
export const nodemon = mkPackage(
  nixRaw`pkgs.nodePackages.nodemon`,
  "Simple monitor script for use during development of a Node.js app.",
);

/**
 * A better `npm publish`
 */
export const np = mkPackage(
  nixRaw`pkgs.nodePackages.np`,
  "A better `npm publish`",
);

/**
 * a package manager for JavaScript
 */
export const npm = mkPackage(
  nixRaw`pkgs.nodePackages.npm`,
  "a package manager for JavaScript",
);

/**
 * git merge driver for automatically merging lockfiles
 */
export const npm_merge_driver = mkPackage(
  nixRaw`pkgs.nodePackages.npm-merge-driver`,
  "git merge driver for automatically merging lockfiles",
);

/**
 * NPM registry manager can help you easy and fast switch between different npm registries, now include: cnpm, taobao, nj(nodejitsu), edunpm
 */
export const nrm = mkPackage(
  nixRaw`pkgs.nodePackages.nrm`,
  "NPM registry manager can help you easy and fast switch between different npm registries, now include: cnpm, taobao, nj(nodejitsu), edunpm",
);

/**
 * A swagger client generator for typescript
 */
export const orval = mkPackage(
  nixRaw`pkgs.nodePackages.orval`,
  "A swagger client generator for typescript",
);

/**
 * Blazing fast, zero configuration web application bundler
 */
export const parcel = mkPackage(
  nixRaw`pkgs.nodePackages.parcel`,
  "Blazing fast, zero configuration web application bundler",
);

/**
 * A bidirectional runtime wikitext parser. Converts back and forth between wikitext and HTML/XML DOM with RDFa.
 */
export const parsoid = mkPackage(
  nixRaw`pkgs.nodePackages.parsoid`,
  "A bidirectional runtime wikitext parser. Converts back and forth between wikitext and HTML/XML DOM with RDFa.",
);

/**
 * Fix broken node modules with no fuss
 */
export const patch_package = mkPackage(
  nixRaw`pkgs.nodePackages.patch-package`,
  "Fix broken node modules with no fuss",
);

/**
 * Streaming torrent client for Node.js
 */
export const peerflix = mkPackage(
  nixRaw`pkgs.nodePackages.peerflix`,
  "Streaming torrent client for Node.js",
);

/**
 * Streaming torrent client for node.js with web ui.
 */
export const peerflix_server = mkPackage(
  nixRaw`pkgs.nodePackages.peerflix-server`,
  "Streaming torrent client for node.js with web ui.",
);

export const pgrok_build_deps_______tools_networking_pgrok_build_deps = mkPackage(
  nixRaw`pkgs.nodePackages.pgrok-build-deps-../../tools/networking/pgrok/build-deps`,
  "",
);

/**
 * Production process manager for Node.JS applications with a built-in load balancer.
 */
export const pm2 = mkPackage(
  nixRaw`pkgs.nodePackages.pm2`,
  "Production process manager for Node.JS applications with a built-in load balancer.",
);

/**
 * Fast, disk space efficient package manager
 */
export const pnpm = mkPackage(
  nixRaw`pkgs.nodePackages.pnpm`,
  "Fast, disk space efficient package manager",
);

/**
 * A T-SQL formatting utility in JS, transpiled from the C# library of the same name.
 */
export const poor_mans_t_sql_formatter_cli = mkPackage(
  nixRaw`pkgs.nodePackages.poor-mans-t-sql-formatter-cli`,
  "A T-SQL formatting utility in JS, transpiled from the C# library of the same name.",
);

/**
 * Tool for transforming styles with JS plugins
 */
export const postcss = mkPackage(
  nixRaw`pkgs.nodePackages.postcss`,
  "Tool for transforming styles with JS plugins",
);

/**
 * CLI for PostCSS
 */
export const postcss_cli = mkPackage(
  nixRaw`pkgs.nodePackages.postcss-cli`,
  "CLI for PostCSS",
);

/**
 * A command line tool to easily install prebuilt binaries for multiple version of node/iojs on a specific platform
 */
export const prebuild_install = mkPackage(
  nixRaw`pkgs.nodePackages.prebuild-install`,
  "A command line tool to easily install prebuilt binaries for multiple version of node/iojs on a specific platform",
);

/**
 * Prettier is an opinionated code formatter
 */
export const prettier = mkPackage(
  nixRaw`pkgs.nodePackages.prettier`,
  "Prettier is an opinionated code formatter",
);

/**
 * TOML Prettier Plugin
 */
export const prettier_plugin_toml = mkPackage(
  nixRaw`pkgs.nodePackages.prettier-plugin-toml`,
  "TOML Prettier Plugin",
);

/**
 * Prisma is an open-source database toolkit. It includes a JavaScript/TypeScript ORM for Node.js, migrations and a modern GUI to view and edit the data in your database. You can use Prisma in new projects or add it to an existing one.
 */
export const prisma = mkPackage(
  nixRaw`pkgs.nodePackages.prisma`,
  "Prisma is an open-source database toolkit. It includes a JavaScript/TypeScript ORM for Node.js, migrations and a modern GUI to view and edit the data in your database. You can use Prisma in new projects or add it to an existing one.",
);

/**
 * A lightweight editor experience for PureScript development
 */
export const pscid = mkPackage(
  nixRaw`pkgs.nodePackages.pscid`,
  "A lightweight editor experience for PureScript development",
);

/**
 * A build system for PureScript projects
 */
export const pulp = mkPackage(
  nixRaw`pkgs.nodePackages.pulp`,
  "A build system for PureScript projects",
);

/**
 * Language Server Protocol server for PureScript wrapping purs ide server functionality
 */
export const purescript_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.purescript-language-server`,
  "Language Server Protocol server for PureScript wrapping purs ide server functionality",
);

/**
 * Error/Warning reporting frontend for psc
 */
export const purescript_psa = mkPackage(
  nixRaw`pkgs.nodePackages.purescript-psa`,
  "Error/Warning reporting frontend for psc",
);

/**
 * A syntax tidy-upper (formatter) for PureScript.
 */
export const purs_tidy = mkPackage(
  nixRaw`pkgs.nodePackages.purs-tidy`,
  "A syntax tidy-upper (formatter) for PureScript.",
);

/**
 * PureScript pretty printer
 */
export const purty = mkPackage(
  nixRaw`pkgs.nodePackages.purty`,
  "PureScript pretty printer",
);

/**
 * Type checker for the Python language
 */
export const pyright = mkPackage(
  nixRaw`pkgs.nodePackages.pyright`,
  "Type checker for the Python language",
);

/**
 * chmod for human beings!
 */
export const remod_cli = mkPackage(
  nixRaw`pkgs.nodePackages.remod-cli`,
  "chmod for human beings!",
);

/**
 * The HTML Presentation Framework
 */
export const reveal_js = mkPackage(
  nixRaw`pkgs.nodePackages.reveal.js`,
  "The HTML Presentation Framework",
);

/**
 * A deep deletion module for node (like `rm -rf`)
 */
export const rimraf = mkPackage(
  nixRaw`pkgs.nodePackages.rimraf`,
  "A deep deletion module for node (like `rm -rf`)",
);

/**
 * Next-generation ES module bundler
 */
export const rollup = mkPackage(
  nixRaw`pkgs.nodePackages.rollup`,
  "Next-generation ES module bundler",
);

/**
 * A professional solution for consolidating all your JavaScript projects in one Git repo
 */
export const rush = mkPackage(
  nixRaw`pkgs.nodePackages.rush`,
  "A professional solution for consolidating all your JavaScript projects in one Git repo",
);

export const rust_analyzer_build_deps_______applications_editors_vscode_extensions_rust_lang_rust_analyzer_build_deps = mkPackage(
  nixRaw`pkgs.nodePackages.rust-analyzer-build-deps-../../applications/editors/vscode/extensions/rust-lang.rust-analyzer/build-deps`,
  "",
);

/**
 * A pure JavaScript implementation of Sass.
 */
export const sass = mkPackage(
  nixRaw`pkgs.nodePackages.sass`,
  "A pure JavaScript implementation of Sass.",
);

/**
 * The semantic version parser used by npm.
 */
export const semver = mkPackage(
  nixRaw`pkgs.nodePackages.semver`,
  "The semantic version parser used by npm.",
);

/**
 * Static file serving and directory listing
 */
export const serve = mkPackage(
  nixRaw`pkgs.nodePackages.serve`,
  "Static file serving and directory listing",
);

/**
 * Serverless Framework - Build web, mobile and IoT applications with serverless architectures using AWS Lambda, Azure Functions, Google CloudFunctions & more
 */
export const serverless = mkPackage(
  nixRaw`pkgs.nodePackages.serverless`,
  "Serverless Framework - Build web, mobile and IoT applications with serverless architectures using AWS Lambda, Azure Functions, Google CloudFunctions & more",
);

/**
 * The self-hosted Web IRC client
 */
export const shout = mkPackage(
  nixRaw`pkgs.nodePackages.shout`,
  "The self-hosted Web IRC client",
);

/**
 * sloc is a simple tool to count SLOC (source lines of code)
 */
export const sloc = mkPackage(
  nixRaw`pkgs.nodePackages.sloc`,
  "sloc is a simple tool to count SLOC (source lines of code)",
);

/**
 * Old Client SDK and CLI for the Joyent SmartDataCenter API
 */
export const smartdc = mkPackage(
  nixRaw`pkgs.nodePackages.smartdc`,
  "Old Client SDK and CLI for the Joyent SmartDataCenter API",
);

/**
 * node.js realtime framework server
 */
export const socket_io = mkPackage(
  nixRaw`pkgs.nodePackages.socket.io`,
  "node.js realtime framework server",
);

/**
 * Test your internet connection speed and ping using speedtest.net from the CLI
 */
export const speed_test = mkPackage(
  nixRaw`pkgs.nodePackages.speed-test`,
  "Test your internet connection speed and ping using speedtest.net from the CLI",
);

/**
 * Format whitespace in a SQL query to make it more readable
 */
export const sql_formatter = mkPackage(
  nixRaw`pkgs.nodePackages.sql-formatter`,
  "Format whitespace in a SQL query to make it more readable",
);

/**
 * Send metric data from statsd to Stackdriver
 */
export const stackdriver_statsd_backend = mkPackage(
  nixRaw`pkgs.nodePackages.stackdriver-statsd-backend`,
  "Send metric data from statsd to Stackdriver",
);

/**
 * Svelte Code Checker Terminal Interface
 */
export const svelte_check = mkPackage(
  nixRaw`pkgs.nodePackages.svelte-check`,
  "Svelte Code Checker Terminal Interface",
);

/**
 * A language server for Svelte
 */
export const svelte_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.svelte-language-server`,
  "A language server for Svelte",
);

/**
 * Nodejs-based tool for optimizing SVG vector graphics files
 */
export const svgo = mkPackage(
  nixRaw`pkgs.nodePackages.svgo`,
  "Nodejs-based tool for optimizing SVG vector graphics files",
);

/**
 * A utility-first CSS framework for rapidly building custom user interfaces.
 */
export const tailwindcss = mkPackage(
  nixRaw`pkgs.nodePackages.tailwindcss`,
  "A utility-first CSS framework for rapidly building custom user interfaces.",
);

/**
 * Programmer for TECK keyboards.
 */
export const teck_programmer = mkPackage(
  nixRaw`pkgs.nodePackages.teck-programmer`,
  "Programmer for TECK keyboards.",
);

/**
 * A JavaScript code analyzer for deep, cross-editor language support
 */
export const tern = mkPackage(
  nixRaw`pkgs.nodePackages.tern`,
  "A JavaScript code analyzer for deep, cross-editor language support",
);

/**
 * The pluggable linting tool for text and markdown.
 */
export const textlint = mkPackage(
  nixRaw`pkgs.nodePackages.textlint`,
  "The pluggable linting tool for text and markdown.",
);

/**
 * Latex plugin for [textlint](https://github.com/textlint/textlint &#34;textlint&#34;).
 */
export const textlint_plugin_latex = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-plugin-latex`,
  "Latex plugin for [textlint](https://github.com/textlint/textlint &#34;textlint&#34;).",
);

/**
 * textlint rule check that abbreviations within parentheses.
 */
export const textlint_rule_abbr_within_parentheses = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-abbr-within-parentheses`,
  "textlint rule check that abbreviations within parentheses.",
);

/**
 * textlint rule for alex
 */
export const textlint_rule_alex = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-alex`,
  "textlint rule for alex",
);

/**
 * textlint rule to check common misspellings
 */
export const textlint_rule_common_misspellings = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-common-misspellings`,
  "textlint rule to check common misspellings",
);

/**
 * Textlint rule to check correct usage of diacritics
 */
export const textlint_rule_diacritics = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-diacritics`,
  "Textlint rule to check correct usage of diacritics",
);

/**
 * textlint rule that specify the maximum word count of a sentence.
 */
export const textlint_rule_en_max_word_count = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-en-max-word-count`,
  "textlint rule that specify the maximum word count of a sentence.",
);

/**
 * textlint rule that limit maxinum comma(,) count of sentence.
 */
export const textlint_rule_max_comma = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-max-comma`,
  "textlint rule that limit maxinum comma(,) count of sentence.",
);

/**
 * textlint rule that check no start with duplicated conjunction.
 */
export const textlint_rule_no_start_duplicated_conjunction = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-no-start-duplicated-conjunction`,
  "textlint rule that check no start with duplicated conjunction.",
);

/**
 * textlint rule that check with or without period in list item.
 */
export const textlint_rule_period_in_list_item = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-period-in-list-item`,
  "textlint rule that check with or without period in list item.",
);

/**
 * Textlint rule to find filler words, buzzwords and chiches
 */
export const textlint_rule_stop_words = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-stop-words`,
  "Textlint rule to find filler words, buzzwords and chiches",
);

/**
 * TextLint rule to check correct terms spelling
 */
export const textlint_rule_terminology = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-terminology`,
  "TextLint rule to check correct terms spelling",
);

/**
 * textlint rule that check unexpanded acronym word.
 */
export const textlint_rule_unexpanded_acronym = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-unexpanded-acronym`,
  "textlint rule that check unexpanded acronym word.",
);

/**
 * textlint rule to check your English style with write good
 */
export const textlint_rule_write_good = mkPackage(
  nixRaw`pkgs.nodePackages.textlint-rule-write-good`,
  "textlint rule to check your English style with write good",
);

/**
 * The Lounge plugin to close all PMs on a network
 */
export const thelounge_plugin_closepms = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-plugin-closepms`,
  "The Lounge plugin to close all PMs on a network",
);

/**
 * Simple plugin for the irc client thelounge that allows you to quickly look up giphy-gifs
 */
export const thelounge_plugin_giphy = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-plugin-giphy`,
  "Simple plugin for the irc client thelounge that allows you to quickly look up giphy-gifs",
);

/**
 * Simple plugin for the irc client thelounge that allows you to register shortcuts/aliases for commands
 */
export const thelounge_plugin_shortcuts = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-plugin-shortcuts`,
  "Simple plugin for the irc client thelounge that allows you to register shortcuts/aliases for commands",
);

/**
 * A very nice theme for The Lounge
 */
export const thelounge_theme_abyss = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-abyss`,
  "A very nice theme for The Lounge",
);

/**
 * Black theme suitable for AMOLED displays
 */
export const thelounge_theme_amoled = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-amoled`,
  "Black theme suitable for AMOLED displays",
);

/**
 * Black theme suitable for AMOLED displays - with Source Code Pro
 */
export const thelounge_theme_amoled_sourcecodepro = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-amoled-sourcecodepro`,
  "Black theme suitable for AMOLED displays - with Source Code Pro",
);

/**
 * A bdefault theme
 */
export const thelounge_theme_bdefault = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-bdefault`,
  "A bdefault theme",
);

/**
 * A bmorning theme
 */
export const thelounge_theme_bmorning = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-bmorning`,
  "A bmorning theme",
);

/**
 * Darkly elegant theme for The Lounge
 */
export const thelounge_theme_chord = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-chord`,
  "Darkly elegant theme for The Lounge",
);

/**
 * Classic theme for The Lounge, to get a v2 look with The Lounge v3
 */
export const thelounge_theme_classic = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-classic`,
  "Classic theme for The Lounge, to get a v2 look with The Lounge v3",
);

/**
 * A common theme
 */
export const thelounge_theme_common = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-common`,
  "A common theme",
);

/**
 * Retro & high-contrast theme for The Lounge
 */
export const thelounge_theme_crypto = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-crypto`,
  "Retro & high-contrast theme for The Lounge",
);

/**
 * A discordapp like theme for thelounge
 */
export const thelounge_theme_discordapp = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-discordapp`,
  "A discordapp like theme for thelounge",
);

/**
 * Dracula theme for thelounge
 */
export const thelounge_theme_dracula = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-dracula`,
  "Dracula theme for thelounge",
);

/**
 * <a href="https://yarn.pm/thelounge-theme-dracula-official"><img alt="npm version" src="https://img.shields.io/npm/v/thelounge-theme-dracula-official.svg?style=flat-square"></a> <a href="https://npm-stat.com/charts.html?package=thelounge-theme-dracula-offi
 */
export const thelounge_theme_dracula_official = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-dracula-official`,
  "<a href=\"https://yarn.pm/thelounge-theme-dracula-official\"><img alt=\"npm version\" src=\"https://img.shields.io/npm/v/thelounge-theme-dracula-official.svg?style=flat-square\"></a> <a href=\"https://npm-stat.com/charts.html?package=thelounge-theme-dracula-offi",
);

/**
 * A simple flat theme with blue colours
 */
export const thelounge_theme_flat_blue = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-flat-blue`,
  "A simple flat theme with blue colours",
);

/**
 * A simple flat theme with dark colours
 */
export const thelounge_theme_flat_dark = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-flat-dark`,
  "A simple flat theme with dark colours",
);

/**
 * The only theme you'll ever need ;P
 */
export const thelounge_theme_gruvbox = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-gruvbox`,
  "The only theme you'll ever need ;P",
);

/**
 * Somtething like Solarized
 */
export const thelounge_theme_hexified = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-hexified`,
  "Somtething like Solarized",
);

/**
 * Colourful Material Design theme for The Lounge
 */
export const thelounge_theme_ion = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-ion`,
  "Colourful Material Design theme for The Lounge",
);

/**
 * A simple theme for The Lounge with custom highlights
 */
export const thelounge_theme_light = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-light`,
  "A simple theme for The Lounge with custom highlights",
);

/**
 * Midnight theme for the lounge with bubble layout on mobile devices
 */
export const thelounge_theme_midnight = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-midnight`,
  "Midnight theme for the lounge with bubble layout on mobile devices",
);

/**
 * A dark, minimal theme for thelounge
 */
export const thelounge_theme_mininapse = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-mininapse`,
  "A dark, minimal theme for thelounge",
);

/**
 * Monokai Colors with Classic Console Neue font
 */
export const thelounge_theme_monokai_console = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-monokai-console`,
  "Monokai Colors with Classic Console Neue font",
);

/**
 * A dark theme.
 */
export const thelounge_theme_mortified = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-mortified`,
  "A dark theme.",
);

/**
 * A dark theme for The Lounge
 */
export const thelounge_theme_neuron_fork = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-neuron-fork`,
  "A dark theme for The Lounge",
);

/**
 * Dark, modern looking theme with conversational message layout.
 */
export const thelounge_theme_new_morning = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-new-morning`,
  "Dark, modern looking theme with conversational message layout.",
);

/**
 * Compact version of New Morning theme.
 */
export const thelounge_theme_new_morning_compact = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-new-morning-compact`,
  "Compact version of New Morning theme.",
);

/**
 * A no-logo theme for The Lounge
 */
export const thelounge_theme_nologo = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-nologo`,
  "A no-logo theme for The Lounge",
);

/**
 * Nordify your lounge.
 */
export const thelounge_theme_nord = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-nord`,
  "Nordify your lounge.",
);

/**
 * One Dark theme for The Lounge
 */
export const thelounge_theme_onedark = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-onedark`,
  "One Dark theme for The Lounge",
);

/**
 * A lovely dark purple theme for The Lounge
 */
export const thelounge_theme_purplenight = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-purplenight`,
  "A lovely dark purple theme for The Lounge",
);

/**
 * A theme for The Lounge
 */
export const thelounge_theme_scoutlink = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-scoutlink`,
  "A theme for The Lounge",
);

/**
 * A personal theme, derived from thelounge-theme-nord.
 */
export const thelounge_theme_seraphimrp = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-seraphimrp`,
  "A personal theme, derived from thelounge-theme-nord.",
);

/**
 * A simple theme with Solarized colours
 */
export const thelounge_theme_solarized = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-solarized`,
  "A simple theme with Solarized colours",
);

/**
 * A simple theme with Solarized colors
 */
export const thelounge_theme_solarized_fork_monospace = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-solarized-fork-monospace`,
  "A simple theme with Solarized colors",
);

/**
 * Dark & low-contrast theme for The Lounge, based on the Vim's Zenburn color scheme
 */
export const thelounge_theme_zenburn = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-zenburn`,
  "Dark & low-contrast theme for The Lounge, based on the Vim's Zenburn color scheme",
);

/**
 * Dark & low-contrast theme for The Lounge, based on the Vim's Zenburn color scheme
 */
export const thelounge_theme_zenburn_monospace = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-zenburn-monospace`,
  "Dark & low-contrast theme for The Lounge, based on the Vim's Zenburn color scheme",
);

/**
 * Dark & low-contrast theme for The Lounge, based on the Vim's Zenburn color scheme
 */
export const thelounge_theme_zenburn_sourcecodepro = mkPackage(
  nixRaw`pkgs.nodePackages.thelounge-theme-zenburn-sourcecodepro`,
  "Dark & low-contrast theme for The Lounge, based on the Vim's Zenburn color scheme",
);

/**
 * a non-linear personal web notebook
 */
export const tiddlywiki = mkPackage(
  nixRaw`pkgs.nodePackages.tiddlywiki`,
  "a non-linear personal web notebook",
);

/**
 * TypeScript execution environment and REPL for node.js, with source map support
 */
export const ts_node = mkPackage(
  nixRaw`pkgs.nodePackages.ts-node`,
  "TypeScript execution environment and REPL for node.js, with source map support",
);

/**
 * TSUN: a repl for TypeScript Upgraded Node
 */
export const tsun = mkPackage(
  nixRaw`pkgs.nodePackages.tsun`,
  "TSUN: a repl for TypeScript Upgraded Node",
);

/**
 * Convert TTF font to EOT
 */
export const ttf2eot = mkPackage(
  nixRaw`pkgs.nodePackages.ttf2eot`,
  "Convert TTF font to EOT",
);

/**
 * Language Server Protocol (LSP) implementation for TypeScript using tsserver
 */
export const typescript_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.typescript-language-server`,
  "Language Server Protocol (LSP) implementation for TypeScript using tsserver",
);

/**
 * JavaScript parser, mangler/compressor and beautifier toolkit
 */
export const uglify_js = mkPackage(
  nixRaw`pkgs.nodePackages.uglify-js`,
  "JavaScript parser, mangler/compressor and beautifier toolkit",
);

/**
 * undollar strips the dollar sign from the beginning of the terminal command you just copied from StackOverflow when you were searching for what arguments to pass to `tar` (`xzf`? `xvfJ`? Or was it `xvf`? You never seem to remember).
 */
export const undollar = mkPackage(
  nixRaw`pkgs.nodePackages.undollar`,
  "undollar strips the dollar sign from the beginning of the terminal command you just copied from StackOverflow when you were searching for what arguments to pass to `tar` (`xzf`? `xvfJ`? Or was it `xvf`? You never seem to remember).",
);

/**
 * Language server for unified
 */
export const unified_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.unified-language-server`,
  "Language server for unified",
);

/**
 * OAuth helper and remote fetcher for Uppy's (https://uppy.io) extensible file upload widget with support for drag&drop, resumable uploads, previews, restrictions, file processing/encoding, remote providers like Dropbox and Google Drive, S3 and more :dog:
 */
export const uppy_companion = mkPackage(
  nixRaw`pkgs.nodePackages.uppy-companion`,
  "OAuth helper and remote fetcher for Uppy's (https://uppy.io) extensible file upload widget with support for drag&drop, resumable uploads, previews, restrictions, file processing/encoding, remote providers like Dropbox and Google Drive, S3 and more :dog:",
);

/**
 * Command line utilities for server-side Vega.
 */
export const vega_cli = mkPackage(
  nixRaw`pkgs.nodePackages.vega-cli`,
  "Command line utilities for server-side Vega.",
);

/**
 * Vega-Lite is a concise high-level language for interactive visualization.
 */
export const vega_lite = mkPackage(
  nixRaw`pkgs.nodePackages.vega-lite`,
  "Vega-Lite is a concise high-level language for interactive visualization.",
);

/**
 * The command-line interface for Vercel
 */
export const vercel = mkPackage(
  nixRaw`pkgs.nodePackages.vercel`,
  "The command-line interface for Vercel",
);

/**
 * vim language server
 */
export const vim_language_server = mkPackage(
  nixRaw`pkgs.nodePackages.vim-language-server`,
  "vim language server",
);

/**
 * Vue Language Server
 */
export const vls = mkPackage(
  nixRaw`pkgs.nodePackages.vls`,
  "Vue Language Server",
);

export const volar = mkPackage(
  nixRaw`pkgs.nodePackages.volar`,
  "",
);

/**
 * Binary version published on npm of vscode-css-languageserver extracted from VSCode tree
 */
export const vscode_css_languageserver_bin = mkPackage(
  nixRaw`pkgs.nodePackages.vscode-css-languageserver-bin`,
  "Binary version published on npm of vscode-css-languageserver extracted from VSCode tree",
);

/**
 * Binary version published on npm of vscode-html-languageserver extracted from VSCode tree
 */
export const vscode_html_languageserver_bin = mkPackage(
  nixRaw`pkgs.nodePackages.vscode-html-languageserver-bin`,
  "Binary version published on npm of vscode-html-languageserver extracted from VSCode tree",
);

/**
 * JSON language server
 */
export const vscode_json_languageserver = mkPackage(
  nixRaw`pkgs.nodePackages.vscode-json-languageserver`,
  "JSON language server",
);

/**
 * JSON language server
 */
export const vscode_json_languageserver_bin = mkPackage(
  nixRaw`pkgs.nodePackages.vscode-json-languageserver-bin`,
  "JSON language server",
);

/**
 * WaveDrom command-line interface (CLI)
 */
export const wavedrom_cli = mkPackage(
  nixRaw`pkgs.nodePackages.wavedrom-cli`,
  "WaveDrom command-line interface (CLI)",
);

/**
 * Packs ECMAScript/CommonJs/AMD modules for the browser. Allows you to split your codebase into multiple bundles, which can be loaded on demand. Supports loaders to preprocess files, i.e. json, jsx, es7, css, less, ... and your custom stuff.
 */
export const webpack = mkPackage(
  nixRaw`pkgs.nodePackages.webpack`,
  "Packs ECMAScript/CommonJs/AMD modules for the browser. Allows you to split your codebase into multiple bundles, which can be loaded on demand. Supports loaders to preprocess files, i.e. json, jsx, es7, css, less, ... and your custom stuff.",
);

/**
 * CLI for webpack & friends
 */
export const webpack_cli = mkPackage(
  nixRaw`pkgs.nodePackages.webpack-cli`,
  "CLI for webpack & friends",
);

/**
 * Serves a webpack app. Updates the browser on changes.
 */
export const webpack_dev_server = mkPackage(
  nixRaw`pkgs.nodePackages.webpack-dev-server`,
  "Serves a webpack app. Updates the browser on changes.",
);

/**
 * WebTorrent, the streaming torrent client. For the command line.
 */
export const webtorrent_cli = mkPackage(
  nixRaw`pkgs.nodePackages.webtorrent-cli`,
  "WebTorrent, the streaming torrent client. For the command line.",
);

/**
 * Command-line interface for all things Cloudflare Workers
 */
export const wrangler = mkPackage(
  nixRaw`pkgs.nodePackages.wrangler`,
  "Command-line interface for all things Cloudflare Workers",
);

/**
 * Extract content from websites using CSS Selectors and XPath
 */
export const wring = mkPackage(
  nixRaw`pkgs.nodePackages.wring`,
  "Extract content from websites using CSS Selectors and XPath",
);

/**
 * Work with npm/yarn packages locally like a boss.
 */
export const yalc = mkPackage(
  nixRaw`pkgs.nodePackages.yalc`,
  "Work with npm/yarn packages locally like a boss.",
);

/**
 * 📦🐈 Fast, reliable, and secure dependency management.
 */
export const yarn = mkPackage(
  nixRaw`pkgs.nodePackages.yarn`,
  "📦🐈 Fast, reliable, and secure dependency management.",
);
