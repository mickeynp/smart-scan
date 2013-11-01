smartscan-mode
==============

Jumps between other symbols found at point in Emacs.

Installation
============
```lisp
(package-install 'smartscan)
```
Enable it globally
```lisp
(global-smartscan-mode 1)
```
or just for specific modes
```lisp
(add-hook 'prog-mode-hook #'smartscan-mode)
```

Usage
=====
`M-n` and `M-p` move between symbols.

For more information on how to use Smart Scan and how to master movement in Emacs, read my article on [Effective Editing I: Movement](http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/).
