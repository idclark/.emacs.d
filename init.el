;;; init.el --- Emacs configuration of Ian Clark -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2017 Ian Clark <idclark13@gmail.com>
;;
;; Author: Ian Clark <idclark13@gmail.com
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

;;; Disable the site default settings
(setq inhibit-default-init t)

;;; Customization
(use-package validate                     ; Validate options
  :ensure t)

;;; Environment Fixup - grab PATH and friends from .zshenv
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "SHELL")
  (when (memq window-system '(mac ns))
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

;;; Global Configurations
(use-package ido                      ; Better file completion with C-x C-f
  :ensure t
  :config
  (ido-mode t))

(use-package hl-line                  ; Line highlighting always on
  :ensure t
  :config
  (custom-set-variables
  '(global-hl-line-mode t)))

(use-package paredit                 ; Must have for lisps
  :defer t
  :config
   (add-hook 'clojure-mode-hook 'paredit-mode)
   (add-hook 'cider-repl-mode-hook 'paredit-mode))

(use-package autopair                ; Auto matching of )}]" chars
  :ensure t)

(use-package magit                   ; Must have for git repos
  :ensure t
  :defer t)

(use-package git-gutter             ; Color the gutter with code diffs from last commit
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

(use-package flycheck-pos-tip      ; Flycheck error appear in tooltip
  :ensure t
  :after flycheck
  :config
   (flycheck-pos-tip-mode))
  

;;; Major Modes Start Here

(use-package ess                  ; Major Mode for R and S+
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

(use-package python               ; Python Major mode
  :defer t
  :config
  ;; PEP 8 compliant filling rules, 79 chars maximum
  (add-hook 'python-mode-hook (lambda () (validate-setq fill-column 79)))

  (let ((ipython (executable-find "ipython")))
    (if ipython
        (validate-setq python-shell-interpreter ipython)
      (warn "IPython is missing, falling back to default python"))))

(use-package anaconda-mode        ; Backend for Python mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook #'anaconda-mode-hook))

(use-package company-anaconda    ; Backend for Company
  :ensure t
  :after company
  :config (add-to-list 'company-backends 'company-anaconda))

(use-package pyenv-mode           ; Virtual Environ
  :esure t
  :defer t
  :after python
  :init
  (pyenv-mode))

(use-package ein                   ; Jupyter Notebook Support
  :ensure t
  :defer t
  :mode "\\.ipynb\\"
  :init (require 'ein))

(use-package cider                  ; Clojure REPL and Major Mode
  ; https://github.com/clojure-emacs/cider/blob/master/doc/code_completion.md
  :defer t
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
  (eval-after-load 'flycheck '(flycheck-clojure-setup))

  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  )
	    
  
(use-package web-mode                  ; HTML and CSS Editing
  :defer t
  :mode "\\.html?\\'")

(provide 'init);;; init.el ends here
