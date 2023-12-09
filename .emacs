;; custom configuration
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 130)
(global-display-line-numbers-mode t)
(setq make-backup-files nil)
(setq column-number-mode t)
;; Let's define the list of required package in a new variable: package-list
(setq package-list '(typescript-mode tree-sitter tree-sitter-langs lsp-mode lsp-ui))

(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

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

(add-hook 'after-init-hook 'global-company-mode)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'kanagawa t)


(require 'lsp-mode)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)
(add-hook 'lua-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'lsp-deferred)

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

					; Loading tree-sitter package
(require 'tree-sitter-langs)
(require 'tree-sitter)

;; Activate tree-sitter globally (minor mode registered on every buffer)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)


  ;;(rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")))
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


(require 'prettier-js)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(setq prettier-js-args '(
			 "--single-quote"    "true"
			 "--trailing-comma"  "es5"
			 "--bracket-spacing" "true"
			 "--semi"            "true"
			 "--print-width"     "80"
			 "--tab-width"        "4"
			 "--use-tabs"          "false"
			 ))




(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(protobuf-mode go-mode company tide lua-mode rust-mode kanagawa-theme apheleia lsp-ui lsp-mode tree-sitter-langs tree-sitter typescript-mode dracula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
