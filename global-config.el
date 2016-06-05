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
		      magit
		      ido
		      git-gutter		      
		      auctex
		      color-theme-sanityinc-tomorrow
		      autopair
		      popup
		      ))
(dolist (p my-packages)
  (when (not(package-installed-p p))
    (package-install p)))
; make sure LaTeX plays nice. add tex to PATH
;also adds bin to launch SML repl 
(getenv "PATH")
 (setenv "PATH"
(concat
 "/usr/X11/bin" ":"
 "/usr/texbin" ":"
 "/usr/local/bin" ":"
 "/usr/local/smlnj-110.75/bin" ":"
 "/Applications/Postgres.app/Contents/Versions/9.4/bin" ":"

(getenv "PATH")))
(setq exec-path (cons "/usr/local/smlnj-110.75/bin" exec-path))
(setq exec-path (cons "/usr/local/bin" exec-path))
(setq exec-path (cons "/Applications/Postgres.app/Contents/Versions/9.4/bin" exec-path))

(setenv "SHELL"
	"/usr/local/bin/zsh")
(getenv "SHELL")
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(cond (window-system 
       (set-face-attribute 'default nil :font "Menlo-14")))

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [C-tab] 'other-window)

;swap query replace, with abbrev. expansion
(global-set-key (kbd "M-/") 'query-replace)
(global-set-key (kbd "M-%") 'abbrev-expansion)
;same for regexp
(global-set-key (kbd "C-M-/") 'query-replace-regexp)

;global modes 
(paredit-mode t)
(global-hl-line-mode t)
(blink-cursor-mode 0)
(show-paren-mode t)
;;custom pairs snippet; credit to Grabriel Elanaro
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key (kbd "M-x") 'smex)

(require 'ido)
(ido-mode t)

;;nasty hack for ispell
(setq ispell-program-name "/usr/local/Cellar/ispell/3.4.00/bin/ispell")

(add-hook 'after-init-hook #'global-flycheck-mode)

(global-git-gutter-mode t)
(setq git-gutter:modified-sign "  ")
(setq git-gutter:added-sign "  ")
(set-face-background 'git-gutter:modified "blue") ;; background color
(set-face-foreground 'git-gutter:added "green")
(add-to-list 'git-gutter:update-hooks 'magit-revert-buffer-hook)

;put web-mode for html editing here as no config file for web specific stuff
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
