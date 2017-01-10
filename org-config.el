;insert timestamp on done todo items
(setq org-log-done 'time)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "PENDING" "DONE")))

(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "orange") ("PENDING" . "yellow")))

(setq org-tag-alist '(("@jobs" . ?j) ("@python" . ?p) ("blogs" . ?b)
     ("@ml-stats" . ?ml) ("finance" . ?f)))

(provide 'org-config)
