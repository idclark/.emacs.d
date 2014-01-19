;necessary mechanics
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
		      ac-nrepl
		      magit
		      auctex
		      ido-ubiquitous
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
;(set-face-attribute 'default nil :font "Menlo-14")

;global modes 
(hl-line-mode t)
(show-paren-mode t)
;;custom pairs snippet; credit to Grabriel Elanaro
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

;(require 'powerline)
(setq powerline-arrow-shape 'arrow14)
;(powerline-default-theme t)
(require 'ido)
(setq ido-enable-flex-mathing t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'auto-complete-config)
(ac-config-default)
(require 'project-explorer)
