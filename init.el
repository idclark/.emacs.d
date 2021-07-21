;;; init.el --- Emacs configuration of Ian Clark -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2008-2021 Ian Clark <idclark13@gmail.com>
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
      '(("MELPA Stable" . 5)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 20))
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
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

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
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-gopls-use-placeholders nil))  ;; Or 'C-l', 's-l'

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package yasnippet              ; Snippets
  :ensure t
  :defer t)
 
(use-package company               ; graphical autocompletion
  :ensure t
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

 (use-package company-box
   :hook (company-mode . company-box-mode))

;; dap mode for attaching debugger to lsp sessions
(use-package dap-mode
:config
(dap-mode 1)
(require 'dap-go)
(require 'dap-hydra))

(use-package dap-ui
:ensure nil
:config
(dap-ui-mode 1))


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

(use-package python-mode        ; Major mode for Python
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  ;; (python-shell-interpreter "python3")
  ;; (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  ;:init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package pyenv-mode           ; Virtual Environ
  :after python-mode
  :config
  (pyenv-mode 1))

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

(use-package company-org-block
  :ensure t
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :hook ((org-mode . (lambda ()
                       (setq-local company-backends '(company-org-block))
                       (company-mode +1)))))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package go-mode                   ; Major Mode for Editing Golang
  					
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package rustic
  :ensure
  ;; :bind (:map rustic-mode-map
  ;;             ("M-j" . lsp-ui-imenu)
  ;;             ("M-?" . lsp-find-references)
  ;;             ("C-c C-c l" . flycheck-list-errors)
  ;;             ("C-c C-c a" . lsp-execute-code-action)
  ;;             ("C-c C-c r" . lsp-rename)
  ;;             ("C-c C-c q" . lsp-workspace-restart)
  ;;             ("C-c C-c Q" . lsp-workspace-shutdown)
  ;;             ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t))

(use-package web-mode                  ; HTML and CSS Editing
  :ensure t
  :defer t
  :mode "\\.html?\\'")

(use-package markdown-mode              ; Major Mode for Markdown
  :ensure t
  :defer t
  :mode "\\.md$"
  :mode "\\.markdown$")

(provide 'init);;; init.el ends here
