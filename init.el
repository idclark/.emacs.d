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
  :defer t)

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




(provide 'init);;; init.el ends here
