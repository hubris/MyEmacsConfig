(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-ts-mode-indent-offset 4 t)
 '(global-display-line-numbers-mode t)
 '(package-selected-packages
   '(company devdocs doom-themes helm magit npm-mode prettier-js
             rjsx-mode tide treesit-auto typescript-mode web-mode
             yasnippet))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "ADBO" :slant normal :weight medium :height 120 :width normal)))))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(setq package-selected-packages '(xterm-color nerd-icons npm-mode prettier-js treemacs flycheck typescript-mode lsp-ui js2-mode lsp-mode yasnippet lsp-treemacs helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode zenburn-theme json-mode helm-ls-git))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(use-package doom-themes
  :ensure t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-one-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package treemacs
  :ensure t
  :config
  (treemacs-project-follow-mode t)
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple)
  (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))



(setq lsp-prefer-flymake nil) ;; Use Flycheck, not Flymake

(require 'treesit)

(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c" "v0.24.1")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.2.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))


         (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))


;; Install and enable company-mode globally
(use-package company
  :hook (after-init . global-company-mode))

;; Install and enable lsp-mode for TypeScript
;;(use-package lsp-mode
;;  :hook ((typescript-mode . lsp-deferred))
;;  :commands (lsp lsp-deferred))

;; Optional: lsp-ui for better UI
(use-package lsp-ui
  :commands lsp-ui-mode)



(helm-mode)
(require 'helm-xref)
(define-key global-map [remap list-directory] #'helm-browse-project)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'
(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'js-mode-hook #'lsp)
  (yas-global-mode)
  (dap-ui-mode 1)
  (dap-mode 1)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]dist\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]coverage\\'"))


(setq lsp-clients-typescript-prefer-use-project-ts-server t)
(setq lsp-eslint-enable t) ;; Enable ESLint integration
(setq lsp-eslint-format t) ;; Allow ESLint to format code


(add-hook 'typescript-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)
            (add-hook 'before-save-hook 'prettier-js nil 'local)))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-organize-imports nil t)
            (add-hook 'before-save-hook 'prettier-js nil 'local)))

;; Disable the default suspend binding (C-z)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(setq js-indent-level 4)
(setq typescript-indent-level 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil) ;; Use spaces, not tabs

(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(add-hook 'typescript-ts-mode-hook 'prettier-js-mode)
(add-hook 'tsx-ts-mode-hook 'prettier-js-mode)




;; JS and TS
(setq js-indent-level 4)
(setq typescript-indent-level 4)

;; Web mode (covers JSX, TSX, HTML, etc.)
(setq web-mode-code-indent-offset 4)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 4)

;; Python (PEP 8 typically wants 4)

(setq python-indent-offset 4);
; CSS
(setq css-indent-offset 4)

;; JSON
(setq json-ts-mode-indent-offset 4)

;; YAML (yaml-mode)
(setq yaml-indent-offset 4)

;; C/C++
(setq c-basic-offset 4)

(add-hook 'prog-mode-hook
          (lambda ()
            (setq tab-width 4
                  indent-tabs-mode nil)))


;(load-theme 'leuven t)
(savehist-mode 1)
(desktop-save-mode 1)
(define-key helm-map (kbd "C-r") 'helm-minibuffer-history)


(with-eval-after-load 'dap-mode
  (define-key dap-mode-map (kbd "<f5>")  'dap-continue)    ; Start/Continue debugging
  (define-key dap-mode-map (kbd "<f10>") 'dap-next)        ; Step over
  (define-key dap-mode-map (kbd "<f11>") 'dap-step-in)     ; Step into
  (define-key dap-mode-map (kbd "<f12>") 'dap-breakpoint-toggle)) ; Add/remove breakpoint


(define-key global-map (kbd "C-p") #'helm-browse-project)


;; General Emacs tab/display settings
(setq-default indent-tabs-mode nil) ; always use spaces, not tabs
(setq-default tab-width 4)

;; C/C++ (c-mode, c++-mode, c-ts-mode, c++-ts-mode)
(setq-default c-basic-offset 4)
(setq-default c-ts-mode-indent-offset 4)

;; Python (classic and treesit/ts-mode)
(setq-default python-indent-offset 4)
(setq-default python-ts-mode-indent-offset 4)

;; JavaScript & TypeScript
(setq-default js-indent-level 4)
(setq-default js-ts-mode-indent-offset 4)
(setq-default typescript-indent-level 4)
(setq-default typescript-ts-mode-indent-offset 4)

;; CSS and Web-related modes
(setq-default css-indent-offset 4)
(setq-default web-mode-markup-indent-offset 4)
(setq-default web-mode-code-indent-offset 4)
(setq-default web-mode-css-indent-offset 4)

;; JSON
(setq-default json-ts-mode-indent-offset 4)

;; YAML
(setq-default yaml-indent-offset 4)

(require 'npm-mode)
(npm-mode)

;(require 'whitespace)
(setq whitespace-style '(face tabs trailing tab-mark space-mark))
;(setq whitespace-display-mappings
;      '((space-mark 32 [183] [46])))
(custom-set-faces
 '(whitespace-space-trailing ((t (:foreground "#CCCCCC" :background nil)))))

;(set-face-attribute 'whitespace-space nil 
;                    :foreground "#CCCCCC" 
;                    :background nil)
;(global-whitespace-mode 1)

(global-display-line-numbers-mode 1)
(auto-compression-mode 1)

(global-set-key [f1] 'desktop-save)
(global-set-key [f7] 'projectile-compile-project)
(global-set-key "\M-n" 'next-error)

;(setq compilation-environment '("TERM=xterm-256color"))
;(defun my/advice-compilation-filter (f proc string)
;  (funcall f proc (xterm-color-filter string)))
;(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(require 'paren)
(show-paren-mode 1)

(add-hook 'treemacs-mode-hook (lambda () (text-scale-decrease 1)))
(add-hook 'treemacs-mode-hook
          (lambda () (set-window-scroll-bars (selected-window) nil)))
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode -1)))

(use-package magit
  :ensure t)

(setq compilation-search-path '(nil "." "../" "../../"))
(add-hook 'compilation-mode-hook 'compilation-shell-minor-mode)

(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


