;;; init.el --- emacs configuration of Ian Clark -*- lexical-binding: t; -*-
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
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      ;; Prefer MELPA over MELPA Stable and GNU ELPA — most LSP/modern
      ;; packages are only on MELPA.
      package-archive-priorities
      '(("MELPA Stable" . 5)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 20)))

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

;;; Environment Fixup - grab PATH and friends from .zshenv
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setenv "SHELL" "/usr/local/bin/zsh")
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-initialize))

;;; OS X support
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(show-paren-mode t)

(cond (window-system
       (set-face-attribute 'default nil :font "Menlo-14")))

; Key Bindings
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

(use-package hl-line                  ; Line highlighting always on
  :ensure t
  :config
  (global-hl-line-mode 1))

(use-package paredit                  ; Must have for lisps
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'paredit-mode))

(use-package magit                    ; Must have for git repos
  :ensure t
  :defer t)

(use-package diff-hl                  ; Git diff indicators in the fringe
  :ensure t
  :config
  (global-diff-hl-mode)
  :hook
  (magit-pre-refresh  . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :init
  (load-theme 'sanityinc-solarized-light 'no-confirm))

(use-package which-key                ; Show available keybindings after prefix
  :ensure t
  :config
  (which-key-mode 1))

;;; Completion — vertico + orderless + marginalia + consult

(use-package vertico                  ; Vertical completion UI
  :ensure t
  :init
  (vertico-mode 1))

(use-package orderless                ; Space-separated fragment matching
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia               ; Annotations in the minibuffer
  :ensure t
  :init
  (marginalia-mode 1))

(use-package consult                  ; Enhanced commands using completion
  :ensure t
  :bind
  ("C-x b" . consult-buffer)
  ("M-s l" . consult-line)
  ("M-s r" . consult-ripgrep)
  ("M-g g" . consult-goto-line))

;;; Icons

(use-package nerd-icons               ; Run M-x nerd-icons-install-fonts once after install
  :ensure t
  :if (display-graphic-p))

;;; Tree-sitter — auto-switch to *-ts-mode variants when grammars are available

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; Breadcrumb — file path + symbol in the header line

(use-package breadcrumb
  :ensure t
  :hook (eglot-managed-mode . breadcrumb-mode))

;;; LSP — eglot (built-in since Emacs 29)

(use-package eglot
  :ensure nil
  :hook
  (python-mode    . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (go-mode        . eglot-ensure)
  (go-ts-mode     . eglot-ensure)
  (java-mode      . eglot-ensure)
  (java-ts-mode   . eglot-ensure)
  :bind (:map eglot-mode-map
         ("C-c C-a" . eglot-code-actions)
         ("C-c C-r" . eglot-rename)
         ("C-c C-f" . eglot-format-buffer)))

(use-package eglot-java               ; Handles JDT LS install for Java
  :ensure t
  :hook
  (java-mode    . eglot-java-mode)
  (java-ts-mode . eglot-java-mode))

;;; In-buffer completion — corfu

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 1)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu         ; Icons in the corfu popup
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;; Snippets

(use-package yasnippet
  :ensure t
  :init (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;; dap mode for attaching debugger — revisit when eglot-dap support matures
;; (use-package dap-mode
;; :config
;; (dap-mode 1)
;; (require 'dap-go)
;; (require 'dap-hydra))


;;; Major Modes Start Here

(use-package ess                      ; Major Mode for R and S+
  :ensure t
  :defer 1
  :init
  (require 'ess-site)
  :config
  (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
  (if (display-graphic-p)
      (normal-erase-is-backspace-mode 1)))

;;; Python uses the built-in python.el — the MELPA python-mode package
;;; pollutes completion-at-point-functions globally, breaking other modes.

;; (use-package pyenv-mode           ; Virtual Environ
;;   :after python-mode
;;   :config
;;   (pyenv-mode 1))

(use-package cider                    ; Clojure REPL and Major Mode
  ; https://github.com/clojure-emacs/cider/blob/master/doc/code_completion.md
  :defer t
  :config
  (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode))

(use-package org                      ; Org Mode for plain text notes and agenda
  :ensure t
  :defer t
  :config
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-agenda-files "/Users/ian/org-notes")
  (setq org-todo-keywords
	'((sequence "TODO" "IN-PROGRESS" "PENDING" "DONE")))
  (setq org-todo-keyword-faces
	'(("IN-PROGRESS" . "orange") ("PENDING" . "yellow")))
  (setq org-tag-alist '(("@jobs" . ?j) ("@python" . ?p) ("@blogs" . ?b)
			("@ml-stats" . ?m) ("@finance" . ?f))))

(defun ian/go-save-hooks ()
  (add-hook 'before-save-hook #'eglot-format-buffer t t))

(use-package go-mode                  ; Major Mode for Editing Golang
  :ensure t
  :defer t
  :hook
  (go-mode    . ian/go-save-hooks)
  (go-ts-mode . ian/go-save-hooks))

(use-package rustic                   ; Rust major mode — eglot provides LSP via rust-analyzer
  :ensure t
  :custom
  (rustic-lsp-client 'eglot)
  (rustic-format-on-save t))

(use-package web-mode                 ; HTML and CSS Editing
  :ensure t
  :defer t
  :mode "\\.html?\\'")

(use-package markdown-mode            ; Major Mode for Markdown
  :ensure t
  :defer t
  :mode "\\.md$"
  :mode "\\.markdown$")

(provide 'init);;; init.el ends here
