(setq debug-on-error t)

(require 'package)
(require 'use-package)

;; Skip the default splash screen.
(setq inhibit-startup-message t)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq rust-format-on-save t)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load-theme 'monokai t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)

(use-package magit
  :bind (("C-c g" . magit-status)))

(use-package ace-window
  :bind (("C-." . ace-window)))

(use-package dimmer)

(use-package rust)

(use-package helm
  :bind ("M-x" . helm-M-x))
