;;; .emacs --- TypeScript and C++/Wasm development -*- lexical-binding: t; -*-

(require 'package)
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file) (load custom-file nil t))
(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      backup-directory-alist `(("." . ,(locate-user-emacs-file "backups/")))
      auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "auto-save/") t))
      compilation-scroll-output 'first-error)
(dolist (dir '("backups/" "auto-save/"))
  (make-directory (locate-user-emacs-file dir) t))
(setq-default indent-tabs-mode nil tab-width 4 fill-column 100)
(when (and (display-graphic-p) (find-font (font-spec :family "Source Code Pro")))
  (set-face-attribute 'default nil :family "Source Code Pro" :height 120))
(show-paren-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(unless noninteractive (desktop-save-mode 1))
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(use-package doom-themes :config (load-theme 'doom-one-light t))
(use-package which-key :ensure nil :hook (after-init . which-key-mode))
(use-package company
  :hook (after-init . global-company-mode)
  :custom (company-idle-delay 0.15)
          (company-minimum-prefix-length 2))
(use-package flycheck :hook (prog-mode . flycheck-mode))
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.mts\\'" . typescript-mode)
         ("\\.cts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; Grammar installation is explicit, so startup does not depend on GitHub.
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src")
        (json "https://github.com/tree-sitter/tree-sitter-json")))
(defun my/install-development-grammars ()
  "Install missing grammars used by this configuration."
  (interactive)
  (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
    (user-error "This Emacs was built without tree-sitter"))
  (dolist (language '(c cpp javascript typescript tsx json))
    (unless (treesit-language-available-p language)
      (treesit-install-language-grammar language)))
  (message "Grammars installed; restart Emacs to enable the new modes"))
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  (dolist (entry '((c c-mode c-ts-mode)
                   (cpp c++-mode c++-ts-mode)
                   (javascript js-mode js-ts-mode)
                   (javascript javascript-mode js-ts-mode)
                   (typescript typescript-mode typescript-ts-mode)
                   (json json-mode json-ts-mode)))
    (when (treesit-language-available-p (car entry))
      (add-to-list 'major-mode-remap-alist (cons (cadr entry) (caddr entry))))))
(add-to-list 'auto-mode-alist
             (cons "\\.tsx\\'"
                   (if (and (fboundp 'treesit-language-available-p)
                            (treesit-language-available-p 'tsx))
                       'tsx-ts-mode 'typescript-mode)))
(dolist (extension '("mjs" "cjs"))
  (add-to-list 'auto-mode-alist (cons (concat "\\." extension "\\'") 'js-mode)))
(add-to-list 'auto-mode-alist
             (cons "\\.jsx\\'"
                   (if (and (fboundp 'treesit-language-available-p)
                            (treesit-language-available-p 'javascript))
                       'js-ts-mode 'js-jsx-mode)))
(setq c-basic-offset 4
      c-ts-mode-indent-offset 4
      js-indent-level 2
      js-ts-mode-indent-offset 2
      typescript-indent-level 2
      typescript-ts-mode-indent-offset 2)

(defun my/project-root ()
  "Return the current project root, or `default-directory'."
  (if-let ((project (project-current nil)))
      (project-root project)
    default-directory))
(defun my/node-global-tool (name)
  "Find globally installed Node tool NAME, including the newest NVM version."
  (or (executable-find name)
      (when-let
          ((tool
            (car (sort
                  (seq-filter
                   #'file-executable-p
                   (file-expand-wildcards
                    (expand-file-name
                     (concat "~/.nvm/versions/node/*/bin/" name))))
                  (lambda (a b)
                    (cl-flet ((node-version (path)
                                (if (string-match "/node/v\\([0-9][^/]*\\)/bin/" path)
                                    (match-string 1 path)
                                  "0")))
                      (version< (node-version b) (node-version a))))))))
        ;; A GUI session may not inherit NVM's PATH. Its scripts use env node.
        (let ((bin (file-name-directory tool)))
          (add-to-list 'exec-path bin)
          (setenv "PATH" (concat bin path-separator (or (getenv "PATH") ""))))
        tool)))
(defun my/wasm-build-dir ()
  "Find the nearest configured Emscripten CMake build directory."
  (when-let ((root (locate-dominating-file
                    default-directory "build-wasm/compile_commands.json")))
    (expand-file-name "build-wasm" root)))
(defun my/typescript-language-server ()
  "Find the globally installed TypeScript language server."
  (my/node-global-tool "typescript-language-server"))
(defun my/clangd-executable ()
  "Find clangd, including versioned distribution binaries."
  (or (executable-find "clangd")
      (seq-find #'executable-find
                '("clangd-21" "clangd-20" "clangd-19" "clangd-18"
                  "clangd-17" "clangd-16" "clangd-15" "clangd-14"))))
(defun my/start-lsp ()
  "Start the relevant language server when its binary is available."
  (let* ((cpp (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode))
         (server (if cpp (my/clangd-executable) (my/typescript-language-server))))
    (when server
      (require 'lsp-mode)
      (if cpp
          (progn
            (require 'lsp-clangd)
            (setq-local lsp-clients-clangd-executable server)
            (when-let ((emxx (executable-find "em++")))
              (setq-local lsp-clients-clangd-args
                          (cons (concat "--query-driver=" emxx)
                                lsp-clients-clangd-args)))
            (when-let ((build (my/wasm-build-dir)))
              (setq-local lsp-clients-clangd-args
                          (cons (concat "--compile-commands-dir=" build)
                                lsp-clients-clangd-args))))
        (require 'lsp-javascript)
        (setq-local lsp-clients-typescript-tls-path server))
      (lsp-deferred))))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js-mode js-ts-mode typescript-mode typescript-ts-mode tsx-ts-mode
                   c-mode c++-mode c-ts-mode c++-ts-mode) . my/start-lsp)
  :custom (lsp-prefer-flymake nil)
          (lsp-completion-provider :capf)
          (lsp-idle-delay 0.5)
  :config
  ;; The older Snap cannot spawn Git from its LSP; use the user-wide tool.
  (let ((semgrep (expand-file-name "~/.local/bin/semgrep")))
    (if (file-executable-p semgrep)
        (with-eval-after-load 'lsp-semgrep
          (setq lsp-semgrep-server-command (list semgrep "lsp")))
      (add-to-list 'lsp-disabled-clients 'semgrep-ls)))
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
(use-package lsp-ui :after lsp-mode :commands lsp-ui-mode)

;; Formatting uses a globally installed Prettier and project configuration.
(use-package prettier-js :commands prettier-js)
(defvar prettier-js-command)
(defun my/format-typescript ()
  "Format this JS/TS buffer with globally installed Prettier."
  (interactive)
  (unless buffer-file-name
    (user-error "Save this buffer to a file before formatting"))
  (let ((prettier (my/node-global-tool "prettier")))
    (unless prettier
      (user-error "Install Prettier globally (npm install -g prettier)"))
    (let ((prettier-js-command prettier))
      (prettier-js))))
(dolist (hook '(js-mode-hook js-ts-mode-hook typescript-mode-hook
                typescript-ts-mode-hook tsx-ts-mode-hook))
  (add-hook hook (lambda ()
                   (local-set-key (kbd "C-c f") #'my/format-typescript))))

(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini))
  :config (helm-mode 1))
(use-package projectile
  :config (projectile-mode 1)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package treemacs :commands treemacs :bind (("C-x t t" . treemacs)))
(use-package magit :commands magit-status :bind (("C-x g" . magit-status)))

(require 'ansi-color)
(defun my/colorize-compilation-output ()
  "Interpret ANSI colors in the latest compilation output."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'my/colorize-compilation-output)
(defun my/wasm-cmake-configure ()
  "Configure the current CMake project for Emscripten and clangd."
  (interactive)
  (unless (executable-find "emcmake")
    (user-error "Activate the Emscripten SDK so emcmake is on PATH"))
  (let* ((root (or (locate-dominating-file default-directory "CMakeLists.txt")
                   (my/project-root)))
         (build (expand-file-name "build-wasm" root)))
    (unless (file-exists-p (expand-file-name "CMakeLists.txt" root))
      (user-error "No CMakeLists.txt in %s" root))
    (let ((default-directory root))
      (compile (format "emcmake cmake -S %s -B %s -DCMAKE_EXPORT_COMPILE_COMMANDS=ON"
                       (shell-quote-argument root)
                       (shell-quote-argument build))))))
(defun my/wasm-cmake-build ()
  "Build the current project's Emscripten CMake build directory."
  (interactive)
  (let ((default-directory
         (or (locate-dominating-file default-directory "build-wasm")
             (my/project-root))))
    (compile "cmake --build build-wasm -j")))
(defun my/wasm-lsp-refresh ()
  "Make clangd use the Wasm compilation database and restart its workspace."
  (interactive)
  (unless (derived-mode-p 'c-mode 'c++-mode 'c-ts-mode 'c++-ts-mode)
    (user-error "Open a C or C++ source file first"))
  (let ((build (my/wasm-build-dir)))
    (unless build
      (user-error "No build-wasm/compile_commands.json above this file; run C-c w c first"))
    (require 'lsp-clangd)
    (setq-local lsp-clients-clangd-args
                (cons (concat "--compile-commands-dir=" build)
                      (seq-remove (lambda (arg)
                                    (string-prefix-p "--compile-commands-dir=" arg))
                                  lsp-clients-clangd-args)))
    (if (bound-and-true-p lsp-mode)
        (dolist (workspace (lsp-workspaces))
          (lsp-workspace-restart workspace))
      (my/start-lsp))))
(global-set-key (kbd "<f5>") #'recompile)
(global-set-key (kbd "<f7>") #'project-compile)
(global-set-key (kbd "C-c w c") #'my/wasm-cmake-configure)
(global-set-key (kbd "C-c w b") #'my/wasm-cmake-build)
(global-set-key (kbd "C-c w r") #'my/wasm-lsp-refresh)

;;; .emacs ends here
