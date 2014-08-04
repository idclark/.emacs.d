Emacs configuration scripts
===========================

One central configuration for work, home, and virtual machines. 

In `~/.emacs` drop in 
`(add-to-list 'load-path "~/.emacs.d/")`

`(load-file "~/.emacs.d/init.el")`

In `init.el`

`(load "~/.emacs.d/global-config")`
`(load "~/.emacs.d/stats-config")`
`(load "~/.emacs.d/python-config")`
`(load "~/.emacs.d/clojure-config")`

Supported Modes: Requires Emacs 24 or package.el
---------------

Currently supports

* LaTeX with Auctex
* Clojure mode with Nrepl and paredit
* ESS for R
* Currently uses a modified Python.el
* Magit and Org mode 
* Web mode for html / jinja templating


