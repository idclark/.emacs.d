(add-hook 'python-mode-hook 'anaconda-mode)
;(add-hook 'python-mode-hook 'eldoc-mode-hook)
(add-hook 'python-mode-hook 'ac-anaconda-setup)

(setq jedi:server-args
      '("--sys-path" "/Users/idclark/anaconda/envs/python3/lib/python3.5/site-packages"
        ))
(add-hook 'python-mode-hook 'jedi:ac-setup)
;setup jupyter notebook
(require 'ein)
(setq ein:use-auto-complete-superpack t)
(setq ein:use-smartrep t)

(defun 'python-shell-send-statement
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
 python-shell-interpreter "ipython"
 python-shell-interpreter-args "-i"
)

(provide 'python-config)
