Emacs configuration scripts
===========================

One central configuration for work, home, and virtual machines. 

In ~/.emacs drop in 
(add-to-list 'load-path "~/.emacs.d/")
(load-file "~/.emacs.d/init.el")

Supported Modes
---------------

Currently supports

* LaTeX with Auctex
* Clojure mode and Nrepl
* ESS for R
* Currently uses a vanilla Python.el
* Magit and Org mode 
Requires Emacs 24 or package.el

