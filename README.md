lsp-haskell
===========

[![MELPA](https://melpa.org/packages/lsp-haskell-badge.svg)](https://melpa.org/#/lsp-haskell)

An Emacs Lisp library for interacting with
a [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine/)
server using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library is designed to integrate with existing Emacs IDE frameworks
(completion-at-point, xref (beginning with Emacs 25.1), flycheck, haskell-mode, intero, etc).


*This package is still under development, and is not recommended for daily use.*

## Emacs Configuration

Clone this repository, https://github.com/emacs-lsp/lsp-mode and https://github.com/emacs-lsp/lsp-ui
to suitable paths, and add the following to your `.emacs` or `init.el` 
config file.

```emacs-lisp
(add-to-list 'load-path "<path to lsp-haskell>")
(add-to-list 'load-path "<path to lsp-mode>")
(add-to-list 'load-path "<path to lsp-ui>")

(require 'lsp-ui)
(require 'lsp-haskell)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'haskell-mode-hook #'lsp-haskell-enable)
(add-hook 'haskell-mode-hook 'flycheck-mode)
```

Note: All three packages are also available via MELPA.

It needs the HIE server in your path, so follow the appropriate
OSX or Linux section below accordingly.

## Hie Installation (OSX)

The following steps are recommended to bootstrap `lsp-haskell` on OSX.

```bash
git clone https://github.com/haskell/haskell-ide-engine
cd haskell-ide-engine
make
```

We need to run `make` here instead of `stack install` to ensure that 
all versions of `ghc` are available to support various project 
requirements.

After this, we need to instruct Emacs to prefer `hie-wrapper` over
`hie` so Hie can infer which version of ghc we need for a particular
project.

```elisp
(setq lsp-haskell-process-path-hie "hie-wrapper")
```

## Hie Installation (Linux)

Or of you are on linux (only place it has been tested, reports please)

```bash
git clone https://github.com/haskell/haskell-ide-engine
cd haskell-ide-engine
make
```
This will build executables called `hie-8.0.2`, `hie-8.2.2` etc which can be selected by using`M-x customize` for `lsp-haskell`
