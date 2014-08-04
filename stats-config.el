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

(setq inferior-julia-program-name "/Applications/Julia-0.2.0.app/Contents/Resources/julia/bin/julia-basic" )

; make sure LaTeX plays nice. add tex to PATH
;also adds bin to launch SML repl 
(getenv "PATH")
 (setenv "PATH"
(concat
 "/usr/X11/bin" ":"
 "/usr/texbin" ":"
 "/usr/local/bin" ":"
 "/usr/local/smlnj-110.75/bin" ":"

(getenv "PATH")))
(setq exec-path (cons "/usr/local/smlnj-110.75/bin" exec-path))
(setq exec-path (cons "/usr/local/bin" exec-path))
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
