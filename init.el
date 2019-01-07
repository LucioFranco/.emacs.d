;;; package --- My Personal Emacs config

;;; Comentary:

;;; Code:
;; Debug when errors happen
(setq debug-on-error nil)

(message "Loading configuration...")

;; Version Checking
(let ((minver "24.4"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; Global configuration constants
(defconst *is-a-mac* (eq system-type 'darwin))

;; Load boostrap file, this will initally load straight.el
;; if straight.el is not installed already it will go ahead
;; and do that.
(load (expand-file-name "bootstrap.el" user-emacs-directory) nil 'nomessage)

;; -------

;; Package Manager config
;; Import use-package
(straight-use-package 'use-package)

;; When configuring a feature with `use-package', also tell
;; straight.el to install a package of the same name, unless otherwise
;; specified using the `:straight' keyword.
(setq straight-use-package-by-default t)

;; Tell `use-package' to always load features lazily unless told
;; otherwise. It's nicer to have this kind of thing be deterministic:
;; if `:demand' is present, the loading is eager; otherwise, the
;; loading is lazy. See
;; https://github.com/jwiegley/use-package#notes-about-lazy-loading.
(setq use-package-always-defer t)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))



;; --------

;; Window config
;; Set frame to fullscre
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; TODO: to be replaced by toggle-frame-maximized

;; Set default font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 130
                    :weight 'normal
                    :width 'normal)

;; Disable alarms
(setq ring-bell-function 'ignore)

;; Hide menu, toolbar and the scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; Display keystrokes in the echo area immediately
(setq echo-keystrokes 1e-6)

(if *is-a-mac*
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

(if *is-a-mac*
    (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; Display line numbers
;;(global-display-line-numbers-mode)


(use-package blackout
  :straight (:host github :repo "raxod502/blackout")
  :demand t)

(blackout 'auto-revert-mode)
(blackout 'eldoc-mode)

;; Theme
(use-package monokai-theme
  :demand t)
(use-package github-modern-theme
  :defer t)

(use-package zenburn-theme
  :defer t)

(use-package solarized-theme
  :defer t)

(use-package organic-green-theme
  :defer t)

;; (use-package alect-light
;;   :defer t)

(load-theme 'solarized-dark t)

(global-hl-line-mode +1)

(use-package winum
  :bind
  (("C-x w 0" . select-window-0-or-10)
  ("C-1" . winum-select-window-1)
  ("C-2" . winum-select-window-2)
  ("C-3" . winum-select-window-3)
  ("C-4" . winum-select-window-4)
  ("C-5" . winum-select-window-5)))
(winum-mode)

(use-package all-the-icons)

;; Desktop saving
;; (desktop-save-mode nil)
;; (setq desktop-restore-eager 10)
;; (setq desktop-save nil)

;; Function for killing all buffers
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
       (delq (current-buffer)
       (remove-if-not 'buffer-file-name (buffer-list)))))
;; Shackle
(use-package shackle
  :config
  (progn
    (setq shackle-lighter "")
    (setq shackle-select-reused-windows t) ; default nil
    (setq shackle-default-alignment 'below) ; default below
    (setq shackle-default-size 0.4) ; default 0.5
    (setq shackle-rules
	  '((compilation-mode :noselect t :other f :align t)
	    (magit-status-mode :select nil :same f)
            (magit-log-mode :select nil :same t)
	    ("\\*Cargo.*\\*" :regexp t :noselect t :other t :inhibit-window-quit t)
	  )))
      ;; shackle-default-rule
      ;; '(:noselect t :other t :inhibit-window-quit t)))
  :init
  (shackle-mode))

;; -------

;; (use-package eyebrowse
;;   :init (eyebrowse-mode))

;; Shell Variable config
(use-package exec-path-from-shell
  :demand t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package xterm-color)

(use-package multi-term)

;; -------

;; Ivy/Counsel config
(use-package counsel
  :demand t
  :bind ("M-x" . counsel-M-x))

(use-package ivy
  :bind (("C-s" . 'swiper)
	 ("C-r" . 'swiper)
	 ("C-c C-r" . 'ivy-resume))
  :config (setq ivy-use-virtual-buffers t
		ivy-count-format "%d/%d ")
  :demand t
  :blackout)
(ivy-mode 1)

;; -------

;; Emacs directory config
;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(use-package no-littering
  :demand t)

;; Set auto save files to be placed in "/var/auto-save"
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Ediotor config
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  :blackout)

;; Projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :blackout)

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  :demand t)

(setq projectile-project-search-path '("~/code"))

;; TODO highlight
(use-package hl-todo
  :defer t
  :init (global-hl-line-mode 1)
  :blackout)

;; Treemacs
(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay   0.5
          treemacs-display-in-side-window     t
          treemacs-file-event-delay           5000
          treemacs-file-follow-delay          0.2
          treemacs-follow-after-init          t
          treemacs-follow-recenter-distance   0.1
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      t
          treemacs-no-png-images              nil
          treemacs-project-follow-cleanup     nil
          treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          t
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-space-between-root-nodes   t
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

  (treemacs-follow-mode nil)
  (treemacs-filewatch-mode t)
  (treemacs-git-mode 'simple))

  :bind
  (:map global-map
        ("C-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))
  

(use-package treemacs-projectile
  :after treemacs projectile
  :defer t)

;; Smartparens
(use-package smartparens
  :config

  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  :blackout)

;; Magit
(use-package magit
  :bind ("C-c g" . magit-status)
  :demand t
  :config (setq magit-completing-read-function 'ivy-completing-read))

;; Global minor mods
(use-package company
  :defer t
  :init (global-company-mode)
  :bind (:map company-active-map
	      ("<tab>" . company-complete-selection)
	      ("TAB" . company-complete-selection)
	      ("C-n" . company-select-next-or-abort)
	      ("C-p" . company-select-previous-or-abort)
	      ("RET" . nil)
	      ("<return>" . nil))
  :config
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-flip-when-above t)
  :blackout t)

  ;; (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)
  ;; (define-key company-active-map (kbd "TAB") #'company-complete-selection))

(use-package company-flx
  :defer t
  :blackout)

(with-eval-after-load 'company
  (company-flx-mode +1))

(use-package flycheck
  :defer t
  ;;:init (global-flycheck-mode)
  :blackout)

;; Elixir/Erlang
(use-package elixir-mode
  ;;:bind-keymap ("C-c" . elixir-mode-map))
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  (add-hook 'elixir-mode-hook #'smartparens-mode)
  (add-to-list 'elixir-mode-hook 'seancribbs/activate-alchemist-root-advice)
  (add-hook 'elixir-format-hook (lambda ()
                                  (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                    (setq elixir-format-arguments nil)))))



(use-package alchemist
  :defer t
  :bind (:map alchemist-mode-map
	      ("C-c C-c C-t" . alchemist-mix-test)
	      ("C-c C-c C-b" . alchemist-mix-compile)
	      ("C-c C-c C-w" . alchemist-goto-list-symbol-definitions)
	      ("M-w" . nil)) ;; Need this because for some reason alchemist wants to steal that bind :shrug:
  :init
  (progn
    (add-hook 'elixir-mode-hook #'alchemist-mode)
    (setq alchemist-project-compile-when-needed t
          alchemist-test-status-modeline nil))
  ;; setup company backends
  ;; (add-to-list 'company-backends 'alchemist-company))
  :blackout t)


(defadvice alchemist-project-root (around seancribbs/alchemist-project-root activate)
  (let ((alchemist-project-mix-project-indicator ".git"))
    ad-do-it))

(defun seancribbs/activate-alchemist-root-advice ()
  "Activates advice to override alchemist's root-finding logic"
  (ad-activate 'alchemist-project-root))



(use-package flycheck-mix
;;  :commands (flycheck-mix-setup)
  :init
  (progn
    (add-to-list 'safe-local-variable-values
                 (cons 'elixir-enable-compilation-checking nil))
    (add-to-list 'safe-local-variable-values
                 (cons 'elixir-enable-compilation-checking t))
    (add-hook 'elixir-mode-local-vars-hook
              'spacemacs//elixir-enable-compilation-checking)))

;; Rust
(use-package rust-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq rust-format-on-save t))

(use-package cargo
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package eglot
  :defer t
  :config
  (progn
    (add-hook 'rust-mode-hook 'eglot-ensure))
  :blackout)

(use-package flymake
  :defer t)

(use-package flymake-diagnostic-at-point
  :straight (:host github :repo "meqif/flymake-diagnostic-at-point")
  :demand t
;;  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  :init
  (setq flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup))



;; (use-package racer
;;   :defer t
;;   :init
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   ;;(add-hook 'racer-mode-hook #'company-mode)
;;   :config
;;   ;;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;;   (setq company-tooltip-align-annotations t))

;; (use-package flycheck-rust
;;   :defer t
;;   :init
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package flycheck-inline
;;   :defer t
;;   :init (flycheck-inline-mode))

(use-package glsl-mode
  :defer t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
    (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))))

(use-package protobuf-mode
  :defer)

;; Org
;;; Prevent Emacs-provided Org from being loaded

;; The following is a temporary hack until straight.el supports
;; building Org, see:
;;
;; * https://github.com/raxod502/straight.el/issues/211
;; * https://github.com/raxod502/radian/issues/410
;;
;; There are three things missing from our version of Org: the
;; functions `org-git-version' and `org-release', and the feature
;; `org-version'. We provide all three of those ourself, therefore.

;; Package `git' is a library providing convenience functions for
;; running Git.
(use-package git)

(defun org-git-version ()
  "The Git version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
  Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
(straight-use-package 'org)

(use-feature org
	     :bind* (;; Add the global keybindings for accessing Org Agenda and
		     ;; Org Capture that are recommended in the Org manual.
		     ("C-c a" . org-agenda)
		     ("C-c c" . org-capture)
		     ("C-c l" . org-store-link))
	     :config
	     (setq org-log-done t)
	     (setq org-agenda-window-setup 'current-window)
	     (setq org-agenda-restore-windows-after-quit t))

(use-package ivy-todo
  :bind ("C-c t" . ivy-todo)
  :commands ivy-todo)
 
(if *is-a-mac*
    (setq org-agenda-files (directory-files-recursively "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org" "\.org$"))
  (setq org-agenda-files (directory-files-recursively "~/iCloudDrive/iCloud~com~appsonthemove~beorg/org" "\.org$")))


(setq  org-toggle-tags-groups nil)

;; Gist
(use-package gist
  :defer t)

;; Terraform
(use-package terraform-mode
  :defer t
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :after terraform-mode
  :defer t
  :init
  (add-hook 'terraform-mode-hook #'company-terraform-init)
  :blackout)

;; Markdown
(use-package markdown-mode
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Java
(use-package gradle-mode
  :defer t
  :blackout)

(use-package meghanada
  :defer t
  :config
    (setq meghanada-javac-xlint "-Xlint:all,-processing")
    :blackout)

(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (flycheck-mode +1)
	    (gradle-mode 1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))

(use-package groovy-mode
  :defer t)


;; Yaml
(use-package yaml-mode
  :defer t)

;; Docker
(use-package dockerfile-mode
  :defer t)


;; PHP

(use-package php-mode
  :defer t)

(use-package web-mode
  :defer t)

(use-package vue-mode
  :defer t)


;; Direnv
(use-package direnv
  :defer t
  :config (setq direnv-always-show-summary nil))
(direnv-mode 1)

(message "Done loading configuration!")

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
