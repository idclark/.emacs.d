;;; init.el --- Emacs configuration of Ian Clark -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2008-2017 Ian Clark <idclark13@gmail.com>
;;
;; Author: Ian Clark <idclark13@gmail.com>
;; URL: https://gihub.com/idclark/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:

;; Emacs configuration of Ian Clark.  Parts of this configuration are
;; inspired by Sebastian Wiesner; https://gihub.com/lunaryorn/.emacs.d

;;; Code:


;;; Debugging
(setq message-log-max 10000)

;;; Package management

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      ;; Package archives, the usual suspects
      '(("GNU ELPA"     . "http://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      ;; Prefer MELPA Stable over GNU over MELPA.  IOW prefer MELPA's stable
      ;; packages over everything and only fall back to GNU or MELPA if
      ;; necessary.
      package-archive-priorities
      '(("MELPA Stable" . 10)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 0))
      )

(package-initialize)

;;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Requirements
(require 'use-package)
(require 'subr-x)
(require 'time-date)

;;; Move Customization file out of init.el
;(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file)

;;; Disable the site default settings
(setq inhibit-default-init t)

;;; Customization
(use-package validate                     ; Validate options
  :ensure t)

;;; Environment Fixup - grab PATH and friends from .zshenv
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (progn
    (setenv "SHELL"
	    "/usr/local/bin/zsh")
    (getenv "SHELL")
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-initialize)))

;;; OS X support
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)

(cond (window-system
       (set-face-attribute 'default nil :font "Menlo-14")))

;;; Key Bindings
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key [C-tab] 'other-window)

; Swap query replace, with abbrev. expansion
(global-set-key (kbd "M-/") 'query-replace)
(global-set-key (kbd "M-%") 'abbrev-expansion)
; Same for regexp
(global-set-key (kbd "C-M-/") 'query-replace-regexp)

; Ensure matching pairs are enabled
(electric-pair-mode 1)

;;; Global Configurations
(use-package ido                      ; Better file completion with C-x C-f
  :ensure t
  :config
  (custom-set-variables
   '(ido-everywhere t)
   '(ido-mode t)))

(use-package hl-line                  ; Line highlighting always on
  :ensure t
  :config
  (custom-set-variables
  '(global-hl-line-mode t)))

(use-package paredit                 ; Must have for lisps
  :defer t
  :config
   (add-hook 'clojure-mode-hook 'paredit-mode)
   (add-hook 'cider-repl-mode-hook 'paredit-mode)
   (add-hook 'lisp-mode-hook 'paredit-mode))

(use-package autopair                ; Auto matching of )}]" chars
  :ensure t)

(use-package magit                   ; Must have for git repos
  :ensure t
  :defer t)

(use-package git-gutter             ; Color the gutter with code diffs from last commit
  :ensure t
  :defer t
  :after magit
  :config
  (global-git-commit-mode t)
  (setq git-gutter:modified-sign "  ")
  (setq git-gutter:added-sign "  ")
  (set-face-background 'git-gutter:modified "blue") ;; background color
  (set-face-foreground 'git-gutter:added "green")
  (add-to-list 'git-gutter:update-hooks 'magit-revert-buffer-hook))

(use-package color-theme-sanityinc-tomorrow               ; My Color Theme
  :ensure t
  :init
  (load-theme 'sanityinc-tomorrow-eighties 'no-confirm))

(use-package flycheck               ; On the fly syntax checking for major modes
  :ensure t
  :defer 1
  :config
  (global-flycheck-mode))

;;; Autocompletion configurations
(use-package yasnippet              ; Snippets
  :ensure t
  :defer t)

(use-package company                ; Graphical auto completion
  :ensure t
  :defer 1
  :config
  (global-company-mode)
  (setq company-tooltip-limit 20)
  (setq company-idle-delay .4)

  (validate-setq
   company-tooltip-align-annotations t
   company-tooltip-flip-when-above t
   ;; Easy navigation to candidates with M-<n>
   company-show-numbers t)
  :diminish company-mode)

(use-package company-quickhelp      ; Show help in tooltip
  :ensure t
  :after company
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode)
  :config (company-quickhelp-mode))

;;; Major Modes Start Here

(use-package ess                    ; Major Mode for R and S+
  :ensure t
  :defer 1
  :init
  (require 'ess-site)
  :config
  (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)

  (if (display-graphic-p)
      (normal-erase-is-backspace-mode 1))
  )

(use-package python                  ; Python Major mode
  :ensure t
  :defer t
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (validate-setq fill-column 79)))

  (let ((ipython (executable-find "ipython")))
    (if ipython
        (validate-setq python-shell-interpreter ipython
		       python-shell-interpreter-args "--simple-prompt -i")
      (warn "IPython is missing, falling back to default python"))))

(use-package company-anaconda    ; Backend for Company
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pyenv-mode           ; Virtual Environ
  :ensure t
  :defer t
  :after python
  :init
  (pyenv-mode))

(use-package cider                  ; Clojure REPL and Major Mode
  ; https://github.com/clojure-emacs/cider/blob/master/doc/code_completion.md
  :defer t
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (eval-after-load 'flycheck '(flycheck-clojure-setup))

  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  )
	    
(use-package org                       ; Org Mode for plain text notes and agenda
  :ensure t
  :defer t
  :config
  ;insert timestamp on done todo items
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "PENDING" "DONE")))

  (setq org-todo-keyword-faces
	'(("IN-PROGRESS" . "orange") ("PENDING" . "yellow")))

  (setq org-tag-alist '(("@jobs" . ?j) ("@python" . ?p) ("@blogs" . ?b)
			("@ml-stats" . ?m) ("@finance" . ?f))))

(use-package go-mode                   ; Major Mode for Editing Golang
  
					; http://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
  :ensure t
  :defer t
  :bind (("M-." . godef-jump)
	 ("M-*" . pop-tag-mark))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package company-go                ; Backend for Company
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc                  ; Eldoc for Golang
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package web-mode                  ; HTML and CSS Editing
  :ensure t
  :defer t
  :mode "\\.html?\\'")

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

;; When building on OS X homebrew will put llvm
;; in a non standard location.
;; M-x irony-install-server troubleshooting
;; https://github.com/Sarcasm/irony-mode/issues/167

(use-package irony                     ; Major Mode for C/C++/Obj-C
  :ensure t
  :config
  (progn
    (use-package company-irony
      :ensure t
      :config
      (add-to-list 'company-backends 'company-irony))
    (add-hook 'c++-mode-hook 'irony-mode)
    (add-hook 'c-mode-hook 'irony-mode)
    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)))

;;; ENSIME is a framework for interactive Scala / Apache Spark
(use-package ensime                     ; Major Mode for Scala / Java
  :ensure t
  :defer t
  :mode "\\.scala$"
  :mode "\\.java$")

(use-package markdown-mode              ; Major Mode for Markdown
  :ensure t
  :defer t
  :mode "\\.md$"
  :mode "\\.markdown$")

(use-package js2-mode                   ; ECMAScript Major Mode
  :ensure t
  :defer t
  :mode "\\.js$")

(provide 'init);;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-hl-line-mode t)
 '(ido-everywhere t)
 '(ido-mode t nil (ido))
 '(package-selected-packages
   '(lsp-python-ms lsp-ui lsp-mode ensime company-irony irony web-mode go-eldoc company-go go-mode pyenv-mode ess company-quickhelp company yasnippet flycheck-pos-tip flycheck git-gutter magit autopair exec-path-from-shell validate use-package color-theme-sanityinc-tomorrow)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
