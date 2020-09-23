lsp-haskell
===========

[![MELPA](https://melpa.org/packages/lsp-haskell-badge.svg)](https://melpa.org/#/lsp-haskell) [![Build Status](https://travis-ci.com/emacs-lsp/lsp-haskell.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-haskell)

An Emacs Lisp library for interacting with
a Haskell language server such as [`haskell-language-server`](https://github.com/haskell/haskell-language-server/)
or [`ghcide`](https://github.com/haskell/ghcide/)
using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library acts as a client for [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode).

## Emacs Configuration

Install [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) first, and either clone
this repository, or install from MELPA. Add the following to your `.emacs`:

```emacs-lisp
(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
```

Note: All three packages are also available via MELPA.

It needs the Haskell language server that you plan to use in your path, so follow the appropriate
OSX or Linux section below accordingly.

## Language server installation

Follow the instructions on the [`haskell-language-server`](https://github.com/haskell/haskell-language-server)
or [`ghcide`](https://github.com/haskell/ghcide/) repositories to install your server of choice.

If you have installed a server other than `haskell-language-server`, make sure to
customize the `lsp-haskell-server-path` variable to point to the executable you
have installed (see below).

## Server configuration

`lsp-haskell` exposes a number of configuration options under the `lsp-haskell` 
customization group, which should be set like normal customization variables.
Use `M-x customize-group` to get started.

This includes a few options for for setting the server executable
and arguments, and numerous settings for configuring the server itself (`hlint`,
choice of formatting provider, etc.).

Note that server configuration settings will currently [not](https://github.com/emacs-lsp/lsp-mode/issues/1174) 
be applied until the server is restarted.
