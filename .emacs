;; custom configuration
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :height 120)
;;(set-face-attribute 'default nil :font "Source Sans 3" :height 130)
;;(set-face-attribute 'default nil :font "SF Mono" :height 130)
;;(set-face-attribute 'default nil :font "FiraCode NerdFont Propo" :height 120)
;;(set-face-attribute 'default nil :font "Hack NerdFont" :height 120)
;;(set-face-attribute 'default nil :font "MesloLGL Nerd Font" :height 120)
(setq imagemagick-enabled-types t)
(imagemagick-register-types)
(setq image-use-external-converter t)
(global-display-line-numbers-mode t)
(setq make-backup-files nil)
(setq column-number-mode t)

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

(cua-mode t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

(require 'which-key)
(which-key-mode)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(add-hook 'after-init-hook 'global-company-mode)
(ido-mode)

;;(require 'kanagawa-themes)
(nyan-mode t)

(setq kanagawa-themes-custom-colors '((bg "#2A2A37") (bg-m3 "#2A2A37")))
(add-to-list 'load-path "~/.emacs.d/themes/kanagawa-emacs")
(require 'kanagawa-themes)
(load-theme 'kanagawa-wave t)

;; (use-package lsp-mode
;;   :ensure t
;;   :config
;;   (setq lsp-modeline-code-actions-segments '(count icon name))

;;   :init
;;   '(lsp-mode))

(require 'lsp-mode)
(add-hook 'go-mode-hook #'lsp-deferred)
;;(add-hook 'go-mode-hook #'yas-minor-mode)
(add-hook 'erlang-mode-hook #'lsp)
(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'javascript-mode-hook 'lsp-deferred)
(add-hook 'elixir-mode-hook 'lsp-deferred)
(add-hook 'nix-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)


;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

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


;; Always show diagnostics at the bottom, using 1/3 of the available space
(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
              (display-buffer-reuse-window
               display-buffer-in-side-window)
              (side            . bottom)
              (reusable-frames . visible)
              (window-height   . 0.33)))


(require 'dashboard)
(dashboard-setup-startup-hook)

(require 'multiple-cursors)

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


(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company avy which-key helm-xref dap-mode))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

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
   '("daa27dcbe26a280a9425ee90dc7458d85bd540482b93e9fa94d4f43327128077" default))
 '(package-selected-packages
   '(company-tabnine nyan-mode multiple-cursors pdf-tools image+ nerdtab all-the-icons-ivy nerd-icons-ivy-rich nerd-icons-ibuffer nerd-icons-corfu lsp-mode envrc flymake-elixir all-the-icons-gnus nerd-icons-dired projectile-codesearch all-the-icons-completion nerd-icons-completion lean-mode yasnippet-lean yasnippet-capf yasnippet-classic-snippets yasnippet-snippets go treemacs-all-the-icons tree-sitter-langs tree-sitter simple-mpc protobuf-mode dap-mode elixir-yasnippets wakatime-mode kanagawa-theme abyss-theme package-lint-flymake package-build package-lint autothemer which-key vterm rebecca-theme nix-mode mode-icons lsp-ui lsp-pyright ligature go-mode fontawesome erlang elixir-mode dashboard company)))

;;(put 'upcase-region 'disabled nil)
;;(put 'downcase-region 'disabled nil)
