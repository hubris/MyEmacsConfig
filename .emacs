;;ajoute le chemin vers emacs dans path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/povray"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/kde-emacs"))
(add-to-list 'load-path (expand-file-name "~/share/emacs/site-lisp"))

(transient-mark-mode 0)
(setq split-width-threshold 9999)
(autoload 'gtags-mode "gtags" "" t)

;;CLisp mode
(add-to-list 'load-path "~/.emacs.d/slime/")
(setq inferior-lisp-program "/usr/bin/cmucl")
(require 'slime)
(slime-setup)

;(require 'doxymacs)
;(defun my-doxymacs-font-lock-hook ()
;  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;      (doxymacs-font-lock)))
;(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)

;(require 'color-theme)
;(color-theme-initialize)
;(color-theme-deep-blue)

;;kde modes
;;(setq magic-keys-mode nil)
(require 'kde-emacs)
(load "cc-engine.elc")
;(load "~/.emacs.d/kde-emacs/kde-emacs.el")
(setq magic-keys-mode t)
(require 'cwarn)

(require 'ctypes)
(savehist-mode t)
;;mode css
;(require 'css-mode)
;(autoload 'css-mode "css-mode.el" "CSS mode" t)
;(setq auto-mode-alist
;      (append '(("\\.css$" . css-mode) 
;		) auto-mode-alist))


;;Semantic
;(setq semantic-load-turn-everything-on t)
;require 'semantic-load)

;(require 'semantic-ia)
;;Ecb
;(global-semantic-show-dirty-mode -1)
;(global-semantic-show-unmatched-syntax-mode -1)
;(require 'ecb)

;;Mode c++ pour glsl, cg, h 
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
		("\\.cg$" . c++-mode)
		("\\.cgfx$" . c++-mode)
		("\\.glsl$" . c++-mode)
		("\\.cu$" . c++-mode)
		("\\.fx$" . c++-mode)) auto-mode-alist))
(setq magic-keys-mode nil)
(cwarn-mode t)

;;Mode OpenGL
;;(autoload 'OpenGL-minor-mode "OpenGL" "OpenGL editing utilities." t)
;;(add-hook 'OpenGL-minor-mode-hook 'OpenGL-setup-keys)

;;Mode todo
(autoload 'todo-mode "todo-mode" "Major mode for editing TODO" t)
(autoload 'todo-show "todo-mode" "Show TODO items." t)
(autoload 'todo-insert-item "todo-mode" "Add TODO items" t)

;;D�sactive le bip
(setq visible-bell 't)

;;Suppresion de la toolbar
(tool-bar-mode -1)

;;Gestion de la molette
(cond (window-system
       (mwheel-install)
))

;;Chargement du package de gestion des sessions
;(require 'session)
;(add-hook 'after-init-hook 'session-initialize)

;;Chargement auto du mode nasm
(autoload 'nasm-mode "nasm-mode.el" "Nasm mode" t)
(setq auto-mode-alist
      (append '(("\\.asm$" . nasm-mode) 
		("\\.inc$" . nasm-mode)
		) auto-mode-alist))

;; Set autoloading of POV-mode for these file-types.
;(require 'pov-mode)
;(autoload 'pov-mode "pov-mode.el" "PoVray scene file mode" t)
;(setq auto-mode-alist
;      (append '(("\\.pov$" . pov-mode) 
;		) auto-mode-alist))
;(add-hook 'pov-mode-hook 'turn-on-font-lock)

;Charge les fonctions de creation auto de classes
;(load "ccext.el")

;;Affiche l'heure et le numero de ligne
(setq line-number-mode t)
(setq column-number-mode t)
(display-time)

;;Decompression automatique des fichiers
;;(toggle-auto-compression) ;version X
(auto-compression-mode 1)

;;Coloration automatique des buffers
;;(require 'font-lock)
;;(setq font-lock-maximum-decoration 't
;;      font-lock-background-mode 'dark)
(global-font-lock-mode t)
;;(lazy-lock-mode)
;;(setq font-lock-support-mode 'lazy-lock-mode)

;;delete efface les caracteres a l'avant 
(setq delete-key-deletes-forward 't)

;;Permet d'effacer une zone selectionnee avec delete
(cond 
 ((fboundp 'turn-on-pending-delete)
  (turn-on-pending-delete))
 ((fboundp 'pending-delete-on)
  (pendin-delete-on t)))

;;Charge desktop.el et chargement automatique du bureau
(load "desktop")
(desktop-load-default)
(desktop-read)

;;Pour sauver le bureau
(global-set-key [f1] 'desktop-save)

;;La commande untilisee par compile
(setq compile-command "make")
(global-set-key [f7] 'compile)
(global-set-key "\M-g" 'goto-line)
(global-set-key [(print)] 'font-lock-fontify-buffer)
(global-set-key "\M-n" 'next-error)

(define-key c++-mode-map [(f7)] 'compile)

;;parentheses colorees
(require 'paren)
(show-paren-mode 1)

;;indentation en mode bash
(setq sh-basic-offset 2)

;(setq c-default-style "stroustrup")
(setq c-basic-offset 2)
;;indentation en mode C
(defun my-c-mode-hook()
  (cwarn-mode)
  (c-set-style 'stroustrup)
  (setq c-basic-offset 2)
  (c-toggle-auto-newline)
  (setq c-hanging-braces-alist '(append '((substatement-open before after)) c-hanging-braces-alist)))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'c-mode-common-hook 'my-c-mode-hook)

;;indentation en mode Java
(add-hook 'java-mode-hook 'my-c-mode-hook)

;;Autofill pour fichiers tex
(add-hook 'tex-mode-hook 'auto-fill-mode)

;;Fichiers d'abbrevs
;(setq save-abbrevs t)
;(setq-default abbrev-mode t)
;(read-abbrev-file "~/.abbrev_defs")

;;Aspell
(setq ispell-program-name "aspell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling." t)
(require 'ispell)

;;Divers
(setq inhibit-startup-message t)
;(lazy-lock-mode 1)

(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))
(setq dired-omit-files-p t)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(gnuserv-program (concat exec-directory "/gnuserv"))
 '(load-home-init-file t t)
 '(magic-keys-mode nil)
 '(pc-select-meta-moves-sexps t)
 '(pc-select-selection-keys-only t)
 '(pc-selection-mode t nil (pc-select))
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#141312" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))

(load "gl.el")
(load "psvn.el")

;;Javascript
;(autoload 'javascript-mode "javascript-mode" "JavaScript mode" t)
;(setq auto-mode-alist (append '(("\\.as$" . javascript-mode))
;			      auto-mode-alist))
(require 'generic-x)
(add-to-list 'generic-extras-enable-list 'javascript-generic-mode)

;;Indente la ligne courante comme la precedente
(setq indent-line-function 'indent-relative) 
(setq indent-tabs-mode nil)


(setq diff-switches "-b -c")
'(compare-ignore-whitespace t)

(setq cvs-diff-flags
[cl-struct-cvs-flags
 (("-b" "-u" "-N")
  ("-b" "-c" "-N")
  ("-b" "-u" "-b")
  ("-b" "-u" "-N")
 nil nil nil nil)
 nil nil nil nil]
)

;; Make the % key jump to the matching []() if on another, like VI
(global-set-key "\M-$" 'match-paren)
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
    (interactive "p")
      (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
              ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
                      (t (self-insert-command (or arg 1)))))

;;Options de compile
(setq compilation-scroll-output t)
(setq compilation-window-height 14)
;;Utiliser lorsque la compilation est lanc�e avec -C ../../
(setq compilation-search-path '("." "../.." "../../.." "../../../../"))
;(setq compile-command "scons -C .")
;(global-set-key [f4] 'compile)

;;CMAKE
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))
(autoload 'cmake-mode "~/.emacs.d/cmake-mode.el" "cmake mode" t)
(defun cmake-rename-buffer ()
  "Renames a CMakeLists.txt buffer to cmake-<directory name>."
  (interactive)
  (when (and (buffer-file-name) (string-match "CMakeLists.txt" (buffer-name)))
      (setq parent-dir (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))
      (setq new-buffer-name (concat "cmake-" parent-dir))
      (rename-buffer new-buffer-name t)
      )
  )
(add-hook 'cmake-mode-hook (function cmake-rename-buffer))

;;switch cxx/h
(load "cxx-tools.el")
(define-key c-mode-map [(f6)] 'cxxtools-switch-cpp-h)
(define-key c++-mode-map [(f6)] 'cxxtools-switch-cpp-h)
(define-key c++-mode-map [(shift f6)] 'cxxtools-switch-to-included-file)
(define-key c-mode-map [(shift f6)] 'cxxtools-switch-to-included-file)
(define-key c++-mode-map [(control meta d)] 'cxx-tools-insert-cerr)

;(set-default-font "-adobe-courier-medium-r-normal-*-14-*-*-*-*-*-iso8859-1")
;(set-default-font "-adobe-courier-medium-r-normal-*-14-*-*-*-*-*-*")
(require 'midnight)

(global-set-key (kbd "C-x C-b") 'bs-show)

(setq dabbrev-case-fold-search nil)

;Avoid truncate long lines, usefull for compil windows
;(setq truncate-partial-width-windows nil)

(define-key global-map [(f10)] 'gud-next)
(define-key global-map [(f11)] 'gud-step)
(define-key global-map [(shift f11)] 'gud-finish)
(define-key global-map [(f9)] 'gud-break)
(define-key global-map [(shift f9)] 'gud-remove)
