;necessary mechanicse
(when (>= emacs-major-version 24)
  (require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(ess
		      cider
		      clojure-mode
		      paredit
		      project-explorer
		      magit
		      git-gutter
		      helm
		      auctex
		      color-theme-sanityinc-tomorrow
		      autopair
		      popup
		      auto-complete))
(dolist (p my-packages)
  (when (not(package-installed-p p))
    (package-install p)))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(set-face-attribute 'default nil :font "Menlo-14")

;global modes 
(hl-line-mode t)
(show-paren-mode t)
;;custom pairs snippet; credit to Grabriel Elanaro
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key (kbd "M-x") 'smex)

(require 'auto-complete-config)
(ac-config-default)
(require 'project-explorer)

(global-git-gutter-mode t)
(setq git-gutter:modified-sign "  ")
(setq git-gutter:added-sign "  ")
(set-face-background 'git-gutter:modified "blue") ;; background color
(set-face-foreground 'git-gutter:added "green")
(add-to-list 'git-gutter:update-hooks 'magit-revert-buffer-hook)

;put web-mode for html editing here as no config file for web specific stuff
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


