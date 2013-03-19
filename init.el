;necessary mechanics
(when (>= emacs-major-version 24)
  (require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(ess
		      nrepl
		      clojure-mode
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

;ess configs
(add-to-list 'load-path "~/.emacs.d/elpa/ess-20121220.2/lisp")
(require 'ess-site)
(global-set-key [C-tab] 'other-window)
(define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
(define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(if (display-graphic-p)
    (normal-erase-is-backspace-mode 1))

; make sure LaTeX plays nice. add tex to PATH
;also adds bin to launch SML repl 
(getenv "PATH")
 (setenv "PATH"
(concat
 "/usr/X11/bin" ":"
 "/usr/texbin" ":"
 "/usr/local/bin/" ":"
 "/usr/local/smlnj-110.75/bin" ":"

(getenv "PATH")))
(setq exec-path (cons "/usr/local/smlnj-110.75/bin" exec-path))
;;launch skim
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

(setq TeX-source-correlate-method 'synctex)

(add-hook 'LaTeX-mode-hook
      (lambda()
        (add-to-list 'TeX-expand-list
             '("%q" skim-make-url))))

(defun skim-make-url () (concat
        (TeX-current-line)
        " "
        (expand-file-name (funcall file (TeX-output-extension) t)
            (file-name-directory (TeX-master-file)))
        " "
        (buffer-file-name)))

(setq TeX-view-program-list
      '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

(setq TeX-view-program-selection '((output-pdf "Skim")))

;autoload nice things
(require 'ido)
(setq ido-enable-flex-mathing t)
(setq ido-everywhere t)
(ido-mode 1)

;;(require 'autopair)
;;(autopair-global-mode)

(require 'auto-complete-config)
(ac-config-default)

;;custom pairs snippet; credit to Grabriel Elanaro
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

(show-paren-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
