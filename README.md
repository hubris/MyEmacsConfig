# Emacs for TypeScript and C++/Wasm

`.emacs` is the live configuration (linked from `~/.emacs`). It targets Emacs
30 and keeps the existing light theme, Helm, Projectile, Treemacs, Magit,
Company, Flycheck, and lsp-mode workflow.

Open a TypeScript, TSX, JavaScript, C, or C++ file to start its language server.
The config uses a globally installed `typescript-language-server` from
`PATH` or an existing NVM installation. C and C++ use `clangd` (including versioned `clangd-N`
executables). A missing server simply leaves normal editing available.

Install the TypeScript server and formatter globally, outside any project:

```sh
npm install --global typescript@6 typescript-language-server prettier
```

Install `clangd` and Emscripten as machine tools. On Ubuntu, use
`sudo apt install clangd emscripten`, or use a globally installed Emscripten
SDK if your projects need its version. Launch Emacs with a `PATH` containing
`clangd`, `emcmake`, and `em++` for C++ diagnostics and Wasm builds.

Semgrep is an optional extra language server. This config uses the user-wide
`~/.local/bin/semgrep` installation (`uv tool install semgrep`) because the
older Snap build cannot start Git from its language server. If that executable
is absent, the Semgrep client stays disabled.

Tree-sitter grammars are used when present, with classic modes as fallbacks.
Run `M-x my/install-development-grammars` once to install any missing
grammars, then restart Emacs. The config does not download grammars during
startup.

For CMake projects targeting Wasm, run `C-c w c` to configure with
`emcmake` in `build-wasm/`, then `C-c w b` to build. Configuration exports
`compile_commands.json`; clangd is pointed at that directory when editing
the project. If LSP started before the first configure completed, run
`C-c w r` from a C/C++ source buffer to pick up the database. For other build systems,
provide a `compile_commands.json` or `compile_flags.txt` that represents
your Emscripten flags.

Useful keys:

| Key | Action |
| --- | --- |
| `C-c f` in JS/TS | Format with global Prettier |
| `C-c p` | Projectile project commands |
| `C-x g` | Magit status |
| `C-x t t` | Treemacs |
| `F7` | Compile current project |
| `F5` | Repeat last compilation |
| `C-c w c` / `C-c w b` | Configure / build Wasm CMake project |
| `C-c w r` | Reload the Wasm compile commands in clangd |

The Wasm workflow follows the [Emscripten CMake guidance](https://emscripten.org/docs/compiling/Building-Projects.html).
The compilation database and Emscripten driver setup follow
[clangd's configuration](https://clangd.llvm.org/config) and
[compile command documentation](https://clangd.llvm.org/design/compile-commands).
