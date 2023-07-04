;; custom configuration
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-linum-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-always-indent nil)
(setq-default tab-always-indent 'complete)
(setq-default tab-width 4)
(setq tab-width 4)
(set-face-attribute 'default nil :height 130)


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package powerline
  :ensure t
  :config
  (powerline-center-theme))

(use-package moe-theme
  :ensure t)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
         (python-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
        
(use-package lsp-ui :commands lsp-ui-mode)

(use-package nordic-night-theme
  :ensure t
  :config
  (load-theme 'nordic-night t))

(setq auto-save-default nil)
(setq make-backup-files nil)



(add-to-list 'load-path "/home/nhambu/.emacs.d/neotree")
(require 'neotree)
(setq neo-smart-open t)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(global-set-key [f8] 'neotree-toggle) ;; atom key

;;(require 'powerline)
;;(require 'moe-theme)
;;(powerline-moe-theme)
;;(setq moe-theme-highlight-buffer-id t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-mode nordic-night-theme all-the-icons moe-theme powerline neotree auto-complete which-key try use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
