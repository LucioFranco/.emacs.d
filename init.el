(require 'package)
(require 'use-package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq rust-format-on-save t)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(load-theme 'monokai t)

(menu-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)

(use-package magit
  :bind ("C-c g" magit-status))

(use-package ace-window
  :bind ("C-." . ace-window))
