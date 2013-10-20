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

(add-hook 'python-mode-hook
      #'(lambda ()
          (define-key python-mode-map "\C-c\C-j" 'python-shell-send-statement)))
;set ipython as default editor for python.el
(setq
 python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/2.7/bin/python"
; python-shell-interpreter-args ""
 ; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
  ;python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
  python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"
  python-shell-completion-module-string-code
    "';'.join(module_completion('''%s'''))\n"
  python-shell-completion-string-code
    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
