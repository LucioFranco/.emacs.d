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

;; --------

;; Window config
;; Set frame to fullscre
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Hide menu, toolbar and the scrollbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

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
	     :config
	     (setq ivy-use-virtual-buffers t
		   ivy-count-format "%d/%d ")
	     :demand t)

(use-package counsel-projectile
  :demand t)
(counsel-projectile-mode)


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

(setq projectile-project-search-path '("~/code"))

;; Magit
(use-package magit
  :bind ("C-c g" . magit-status)
  :demand t)

;; Elixir/Erlang
(use-package elixir-mode)
(use-package alchemist)

(message "Done loading configuration!")
