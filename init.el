;; Debug when errors happen
(setq debug-on-error t)

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
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
(global-display-line-numbers-mode)

;; Theme
(use-package monokai-theme
  :demand t)
(load-theme 'monokai t)

(use-package dimmer
  :demand t)
(dimmer-mode)

(use-package winum
  :bind
  ("C-x w 0" . select-window-0-or-10)
  ("C-1" . winum-select-window-1)
  ("C-2" . winum-select-window-2)
  ("C-3" . winum-select-window-3))
(winum-mode)

(use-package all-the-icons)

;; -------

;; TOOD: add dashboard
(use-package xkcd)
;;(xkcd)

;; (defun dashboard-buffer ()
;;   "Create a new empty buffer.
;; New buffer will be named *dashboard*"
;;   (interactive)
;;   (let ((buf (generate-new-buffer "*dashboard*")))
;;     (switch-to-buffer buf)
;;     (funcall initial-major-mode)
;;     (setq buffer-offer-save t)
;;     buf))

(setq inhibit-startup-screen t)
;; (setq initial-buffer-choice "*xkcd*")

;; -------

;; Shell Variable config
(use-package exec-path-from-shell
  :demand t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
  :demand t)
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

;; Projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  :demand t)

(setq projectile-project-search-path '("~/code"))

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
  (treemacs-filewatch-mode t))

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
  (show-smartparens-global-mode +1))

;; Magit
(use-package magit
  :bind ("C-c g" . magit-status)
  :demand t
  :config (setq magit-completing-read-function 'ivy-completing-read))

;; Elixir/Erlang
(use-package elixir-mode
;;:bind-keymap ("C-c" . elixir-mode-map))
  :config
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))
  
  (add-hook 'elixir-format-hook (lambda ()
                                  (if (projectile-project-p)
                                      (setq elixir-format-arguments
                                            (list "--dot-formatter"
                                                  (concat (locate-dominating-file buffer-file-name ".formatter.exs") ".formatter.exs")))
                                    (setq elixir-format-arguments nil)))))



(use-package alchemist)

;; Rust
(use-package company
  :defer t
  :init (global-company-mode))

(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package rust-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
  (setq rust-format-on-save t))

(use-package cargo
  :defer t
  :init
  (add-hook 'rust-mode-hook 'cargo-minor-mode))

(use-package racer
  :defer t
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  :config
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package flycheck-rust
  :defer t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package flycheck-inline
  :defer t
  :init (flycheck-inline-mode))

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
	     (setq org-log-done t))
 
(setq org-agenda-files (directory-files-recursively "~/iCloudDrive/iCloud~com~appsonthemove~beorg/org" "\.org$"))

(setq  org-toggle-tags-groups nil)

(message "Done loading configuration!")
