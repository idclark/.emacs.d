Emacs configuration scripts
===========================

One central configuration for work, home, and virtual machines.

I build Emacs from `https://github.com/d12frosted/homebrew-emacs-plus`, but any version >= 27 that 
suppots `package.el` should work just fine. This configuration has been tested on both OS X 
and Linux. For linux, complile direclty against the `master` branch. Windows... you're on your own. 

The configuration consists of a single `init.el` containing only `use-package` declarations. 
Custom snippets can be found in `/lisp` and added to `load-path` as needed. 

I've refactored any use of `Auto-Complete` in favor of [LSP](https://emacs-lsp.github.io/lsp-mode/). 
The backends are more robust and I've found customization to be more consistent and easier to tweak. 

Supported Modes
---------------

* Python mode with the pyright lsp server / poetry / pyenv virtual environments
* Rust support with Rustic and lsp
* LaTeX with Auctex
* Clojure mode with Cider
* ESS for R	
* Magit and Org mode 
* Web mode for html / jinja templating
* Golang 

License
========

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with GNU Emacs; see the file COPYING. If not, write to the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
