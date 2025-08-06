
;;; init.el --- Opinionated, compact rebuild of your original config -*- lexical-binding: t; -*-

;;; ────────────────────────────────────────────────────────────────────
;;; 0.  Bootstrap package system & use-package
;;; ────────────────────────────────────────────────────────────────────
(require 'package)
(setq package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-always-ensure t
      use-package-compute-statistics t)

;;; ────────────────────────────────────────────────────────────────────
;;; 1.  Interface – theme, modeline, UI bits
;;; ────────────────────────────────────────────────────────────────────
(set-face-attribute 'default nil :family "Source Code Pro" :height 120)
(setq inhibit-startup-screen t
      ring-bell-function     'ignore
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold   t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package which-key
  :hook (after-init . which-key-mode)
  :custom (which-key-idle-delay 0.3))

(global-display-line-numbers-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(desktop-save-mode 1)

;;; ────────────────────────────────────────────────────────────────────
;;; 2.  Editing defaults – indentation, whitespace, tabs
;;; ────────────────────────────────────────────────────────────────────
(setq-default indent-tabs-mode nil  ; spaces, not tabs
              tab-width 4
              fill-column 100)

(dolist (var '((c-basic-offset              . 4)
               (c-ts-mode-indent-offset     . 4)
               (python-indent-offset        . 4)
               (python-ts-mode-indent-offset . 4)
               (js-indent-level             . 4)
               (js-ts-mode-indent-offset    . 4)
               (typescript-indent-level     . 4)
               (typescript-ts-mode-indent-offset . 4)
               (css-indent-offset           . 4)
               (web-mode-markup-indent-offset . 4)
               (web-mode-code-indent-offset   . 4)
               (web-mode-css-indent-offset    . 4)
               (json-ts-mode-indent-offset    . 4)
               (yaml-indent-offset            . 4)))
  (set (car var) (cdr var)))

;;; ────────────────────────────────────────────────────────────────────
;;; 3.  Completion & syntax checking
;;; ────────────────────────────────────────────────────────────────────
;; (use-package company
;;   :hook (after-init . global-company-mode)
;;   :custom
;;   (company-idle-delay 0.0)
;;   (company-minimum-prefix-length 1))

;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode))

;;; ────────────────────────────────────────────────────────────────────
;;; 4.  LSP, DAP, Tree-sitter
;;; ────────────────────────────────────────────────────────────────────
 ;; (use-package lsp-mode
 ;;   :commands (lsp lsp-deferred)
 ;;   :hook ((js-mode          . lsp-deferred)
 ;;          (typescript-mode  . lsp-deferred)
 ;;          (tsx-ts-mode      . lsp-deferred)
 ;;          (typescript-ts-mode . lsp-deferred)
 ;;          (json-ts-mode     . lsp-deferred))
 ;;   :custom
 ;;   (lsp-prefer-flymake nil)
 ;;   (lsp-file-watch-ignored-directories
 ;;    '("[/\\\\]node_modules\\'" "[/\\\\]dist\\'" "[/\\\\]build\\'" "[/\\\\]coverage\\'"))
 ;;   :config
 ;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
 ;;   )

;; (use-package lsp-ui   :after lsp-mode :commands lsp-ui-mode)
;; (use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))

;; Tree-sitter grammars
(use-package treesit
  :config
  (defun my/ensure-treesit-grammars ()
    "Install supported grammars once."
    (interactive)
    (dolist (g '((bash "https://github.com/tree-sitter/tree-sitter-bash")
                 (c "https://github.com/tree-sitter/tree-sitter-c" "v0.24.1")
                 (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                 (css "https://github.com/tree-sitter/tree-sitter-css" "v0.20.0")
                 (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1")
                 (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                 (json "https://github.com/tree-sitter/tree-sitter-json" "v0.20.2")
                 (python "https://github.com/tree-sitter/tree-sitter-python" "v0.20.4")
                 (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.2.3" "tsx/src"))
                 (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))))
      (add-to-list 'treesit-language-source-alist g)
      (unless (treesit-language-available-p (car g))
        (treesit-install-language-grammar (car g)))))
  (my/ensure-treesit-grammars)

  ;; Remap classic → ts modes
  (dolist (pair '((javascript  . typescript-ts-mode)
                  (js-mode     . typescript-ts-mode)
                  (js2-mode    . typescript-ts-mode)
                  (typescript-mode . typescript-ts-mode)
                  (python-mode . python-ts-mode)
                  (c-mode      . c-ts-mode)
                  (c++-mode    . c++-ts-mode)))
    (add-to-list 'major-mode-remap-alist pair)))

;;; ────────────────────────────────────────────────────────────────────
;;; 5.  JavaScript / TypeScript extras
;;; ────────────────────────────────────────────────────────────────────
(use-package prettier-js
  :hook ((js-mode            . prettier-js-mode)
         (typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode        . prettier-js-mode)))

;; (add-hook 'typescript-ts-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'lsp-organize-imports nil t)))
;; (add-hook 'tsx-ts-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

(use-package npm-mode :hook (typescript-ts-mode . npm-mode))

;;; ────────────────────────────────────────────────────────────────────
;;; 6.  Project navigation – Helm, Projectile, Treemacs
;;; ────────────────────────────────────────────────────────────────────
 (use-package helm
   :demand t
   :bind (("C-x C-f" . helm-find-files)
          ("M-x"     . helm-M-x)
          ("C-x b"   . helm-mini)
          ("C-p"     . helm-browse-project))
   :config (helm-mode 1))

 (use-package projectile
   :after helm
   :init  (projectile-mode)
   :bind  (:map projectile-mode-map
                ("C-c p" . projectile-command-map)))

(use-package treemacs
  :bind (("M-0"     . treemacs-select-window)
         ("C-x t t" . treemacs))
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (set-window-scroll-bars (selected-window) nil)
              (text-scale-decrease 1))))

(use-package treemacs-nerd-icons
  :after (treemacs nerd-icons))

(with-eval-after-load 'treemacs
  ;; Set single click to open files/directories
  (setq treemacs-default-visit-action 'treemacs-visit-node-default)
  ;; Configure mouse click behavior
  (define-key treemacs-mode-map [mouse-1] 'treemacs-single-click-expand-action))

(add-hook 'emacs-startup-hook 'treemacs)
;;; ────────────────────────────────────────────────────────────────────
;;; 7.  Version control
;;; ────────────────────────────────────────────────────────────────────
(use-package magit
  :bind (("C-x g" . magit-status)))

;;; ────────────────────────────────────────────────────────────────────
;;; 8.  Compilation & ANSI colours
;;; ────────────────────────────────────────────────────────────────────
;;(use-package ansi-color
 ;; :hook (compilation-filter . ansi-color-apply-on-region))

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  "Interpret ANSI color codes in the `compilation-buffer'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region compilation-filter-start (point))))
(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

(setq compilation-search-path '(nil "." "../" "../../"))
(add-hook 'compilation-mode-hook #'compilation-shell-minor-mode)
(global-set-key [f7] #'projectile-compile-project)

;;; ────────────────────────────────────────────────────────────────────
;;; 9.  Convenience bindings
;;; ────────────────────────────────────────────────────────────────────
(global-set-key [f1] #'desktop-save)
(global-unset-key (kbd "C-z")) ; disable suspend
(global-unset-key (kbd "C-x C-z"))

;;; init.el ends here

