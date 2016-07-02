(require 'ein)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode-hook)

(defun python-shell-send-statement ()
  "send the current statement to inferior Python process"
  (interactive)
  (let ((start (save-excursion
          (python-nav-beginning-of-statement)
          (point)))
    (end (save-excursion
       (python-nav-end-of-statement)
       (point))))
  (when (and start end)
    (python-shell-send-region start end))))

(add-hook 'anaconda-mode-hook
      #'(lambda ()
          (define-key python-mode-map "\C-c\C-j" 'python-shell-send-statement)))
;set ipython as default editor for python.el
(setq
 python-shell-interpreter "/Users/idclark/anaconda/bin/ipython"
; python-shell-interpreter-args ""
 ; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  ;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
