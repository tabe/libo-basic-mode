libo-basic-mode
===============
A major mode for editing LibreOffice Basic programs

Quick installation
------------------
Put libo-basic-mode.el in one of your `load-path`, and append the following
lines to your .emacs:
```
(autoload 'libo-basic-mode "libo-basic-mode" "A major mode for LibreOffice Basic." t)
(push '("\\.bas\\'" . libo-basic-mode) auto-mode-alist)
```

License
-------
GPLv3

Acknowledgements
----------------
This program owes a lot to authors of visual-basic-mode.el.
