;insert timestamp on done todo items
(setq org-log-done 'time)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "PENDING" "DONE")))
(setq org-tag-alist '(("@jobs" . ?j) ("@python" . ?p) ("blogs" . ?b)
		      ("@ml-stats" . ?ml) ("finance" . ?f)))
