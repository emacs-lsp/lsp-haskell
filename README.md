lsp-haskell
===========

[![Join the chat at https://gitter.im/emacs-lsp/lsp-mode](https://badges.gitter.im/emacs-lsp/lsp-mode.svg)](https://gitter.im/emacs-lsp/lsp-mode?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

An Emacs Lisp library for interacting with
a [haskell-ide-engine](https://github.com/alanz/haskell-ide-engine/tree/lsp)
server using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library is designed to integrate with existing Emacs IDE frameworks
(completion-at-point, xref (beginning with Emacs 25.1), flycheck,, haskell-mode, intero, etc).


*This package is still under development, and is not recommended for daily use.*
## Installation

Clone this repository andhttps://github.com/emacs-lsp/lsp-mode to suitable paths, and add
```emacs-lisp
(add-to-list 'load-path "<path to lsp-haskell>")
(add-to-list 'load-path "<path to lsp-mode>")
(require 'lsp-haskell)
(require 'lsp-mode)
(global-lsp-mode t)
```
to your .emacs.

It needs the HIE server in your path, so

```bash
git clone https://github.com/alanz/haskell-ide-engine
cd haskell-ide-engine
git checkout lsp
stack install
```

