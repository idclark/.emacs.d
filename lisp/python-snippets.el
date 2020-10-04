;;; Python.el - Custom snippets for python

;;; Commentary

;;; Code

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

(provide 'python);;; Python ends here
