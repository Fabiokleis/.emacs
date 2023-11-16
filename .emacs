;; custom configuration
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 130)
(global-display-line-numbers-mode t)
(setq make-backup-files nil)
;; Let's define the list of required package in a new variable: package-list
(setq package-list '(typescript-mode tree-sitter tree-sitter-langs lsp-mode lsp-ui))


;; `package` feature is part of Emacs builtin packages
;; This feature will be useful to install other packages like `dap-mode`
(require 'package)

;; Add Melpa to the list of Emacs package repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Load Emacs Lisp packages, and activate them
(package-initialize)


;; package-archive-contents variable
;; holds the list of known packages
(unless package-archive-contents (package-refresh-contents))

;; If a package of package-list is not installed, install it
(dolist (package package-list)
  (unless (package-installed-p package) (package-install package)))

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(require 'lsp-mode)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)


					; Loading tree-sitter package
(require 'tree-sitter-langs)
(require 'tree-sitter)

;; Activate tree-sitter globally (minor mode registered on every buffer)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)


(require 'prettier-js)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
			 "--single-quote"    "true"
			 "--trailing-comma"  "es5"
			 "--bracket-spacing" "true"
			 "--single-quote"    "true"
			 "--semi"            "true"
			 "--print-width"     "120"
			 "--tab-witdh"        "2"
			 "--use-tabs"          "false"
			 ))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dracula-alternate-mode-line-and-minibuffer t)
 '(package-selected-packages
   '(apheleia lsp-ui lsp-mode tree-sitter-langs tree-sitter typescript-mode dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
