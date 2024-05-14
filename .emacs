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
(setq package-list '(elixir-mode alchemist erlang dockerfile-mode protobuf-mode go-mode company tide lua-mode rust-mode kanagawa-theme apheleia lsp-ui lsp-mode tree-sitter-langs tree-sitter typescript-mode dracula-theme))


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

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(require 'lsp-mode)
(add-hook 'erlang-mode-hook #'lsp)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)
(add-hook 'lua-mode-hook 'lsp-deferred)
(add-hook 'rust-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'elixir-mode-hook 'lsp-deferred)

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
			 "--single-quote"    "true"
			 "--semi"            "true"
			 "--print-width"     "80"
			 "--tab-width"        "4"
			 "--use-tabs"          "false"
			 ))




(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Always show diagnostics at the bottom, using 1/3 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))


(use-package erlang
  :load-path ("/usr/lib64/erlang/lib/tools-3.5./emacs")
  :mode (("\\.erl?$" . erlang-mode)
         ("rebar\\.config$" . erlang-mode)
         ("relx\\.config$" . erlang-mode)
         ("sys\\.config\\.src$" . erlang-mode)
         ("sys\\.config$" . erlang-mode)
         ("\\.config\\.src?$" . erlang-mode)
         ("\\.config\\.script?$" . erlang-mode)
         ("\\.hrl?$" . erlang-mode)
         ("\\.app?$" . erlang-mode)
         ("\\.app.src?$" . erlang-mode)
         ("\\Emakefile" . erlang-mode)))


(require 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-startup-banner 'logo)
;; Value can be:
;;  - 'official which displays the official emacs logo.
;;  - 'logo which displays an alternative emacs logo.
;;  - an integer which displays one of the text banners
;;    (see dashboard-banners-directory files).
;;  - a string that specifies a path for a custom banner
;;    currently supported types are gif/image/text/xbm.
;;  - a cons of 2 strings which specifies the path of an image to use
;;    and other path of a text file to use if image isn't supported.
;;    ("path/to/image/file/image.png" . "path/to/text/file/text.txt").
;;  - a list that can display an random banner,
;;    supported values are: string (filepath), 'official, 'logo and integers.

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)
;; vertically center content
(setq dashboard-vertically-center-content t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.8 :foreground "#A3BE8C" :weight extra-bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4 :foreground "#EBCB8B" :weight extra-bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.2 :foreground "#D08770" :weight extra-bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.15 :foreground "#BF616A" :weight extra-bold))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.11 :foreground "#b48ead" :weight extra-bold))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.06 :foreground "#5e81ac" :weight extra-bold)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("249e100de137f516d56bcf2e98c1e3f9e1e8a6dce50726c974fa6838fbfcec6b" "0170347031e5dfa93813765bc4ef9269a5e357c0be01febfa3ae5e5fcb351f09" "9cd57dd6d61cdf4f6aef3102c4cc2cfc04f5884d4f40b2c90a866c9b6267f2b3" "b95f61aa5f8a54d494a219fcde9049e23e3396459a224631e1719effcb981dbd" "06ed754b259cb54c30c658502f843937ff19f8b53597ac28577ec33bb084fa52" "260f523c89c6c1afe2d16f810ff555fea00d1f28c84112ec4ed8f388b9c143f9" "788121c96b7a9b99a6f35e53b7c154991f4880bb0046a80330bb904c1a85e275" "b5fab52f16546a15f171e6bd450ff11f2a9e20e5ac7ec10fa38a14bb0c67b9ab" "74e2ed63173b47d6dc9a82a9a8a6a9048d89760df18bc7033c5f91ff4d083e37" "9724b3abaf500b227faa036dcf817abed9764802835ba6e8d1e475c877205157" default))
 '(package-selected-packages
   '(projectile page-break-lines all-the-icons nerd-icons dashboard python-isort lsp-pyright markdown-preview-mode scala-mode docker-compose-mode kaolin-themes eshell-vterm vterm rebecca-theme elixir-ts-mode elixir-mode alchemist erlang dockerfile-mode protobuf-mode go-mode company tide lua-mode rust-mode kanagawa-theme apheleia lsp-ui lsp-mode tree-sitter-langs tree-sitter typescript-mode dracula-theme))
 '(warning-suppress-types '((comp))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
