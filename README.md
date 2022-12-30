lsp-haskell
===========

[![MELPA](https://melpa.org/packages/lsp-haskell-badge.svg)](https://melpa.org/#/lsp-haskell) [![Build Status](https://travis-ci.com/emacs-lsp/lsp-haskell.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-haskell)

An Emacs Lisp library for interacting with a Haskell language server such as [`haskell-language-server`](https://github.com/haskell/haskell-language-server/) using Microsoft's [Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library acts as a client for [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode).

## Functionality

This main functionality of `lsp-haskell` is:
- Finding and launching the `haskell-language-server` binary
- Providing configuration options to `haskell-language-server`

If you would like to know about how LSP functionality is provided in Emacs and how to use it, please consult [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode).
If you would like to know about the LSP functionality which is provided by the Haskell language server, please consult [`haskell-language-server`](https://github.com/haskell/haskell-language-server).

## Installation

Follow the `lsp-mode` [installation instructions](https://emacs-lsp.github.io/lsp-mode/page/installation/).
Do not skip this!
It has important information.

`lsp-mode` automatically requires the `lsp-haskell` package , so you do not need to `require` `lsp-haskell` unless you like being explicit.
Similarly, `lsp-haskell` automatically requires the `haskell-mode` package, so you do not need to `require` `haskell-mode`.

You will need to set some hooks to ensure that `lsp-mode` is triggered when the `haskell-mode` major mode is entered.

```emacs-lisp
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
```

## Configuration

HLS has some [configuration](https://haskell-language-server.readthedocs.io/en/latest/configuration.html) you can modify using either the `lsp-haskell` or `lsp` customization groups:

```emacs-lisp
(customize-group 'lsp-haskell)

; or

(customize-group 'lsp)
```

If you want to modify some configuration that this project doesn't support yet you can customize things explicitly yourself using `lsp--set-configuration`. For example:

```emacs-lisp
(add-hook 'lsp-after-initialize-hook
          '(lambda ()
             (lsp--set-configuration
              '(:haskell (:plugin (:tactics (:config (:timeout_duration 5)))))
              )))
```


## Language server installation

Follow the instructions on the [`haskell-language-server`](https://github.com/haskell/haskell-language-server) repositories to install the server binaries.

If you have installed the binaries to a location that is not on your `PATH`, or you are not using the `haskell-language-server-wrapper`, then you will need to customize the `lsp-haskell-server-path` variable to point to the executable you want to launch.

## Server configuration

`lsp-haskell` provides a few options for for setting the server executable and arguments, and numerous settings for configuring the server itself (`hlint`, choice of formatting provider, etc.).

Note that server configuration settings will currently [not](https://github.com/emacs-lsp/lsp-mode/issues/1174) be applied until the server is restarted.

## Troubleshooting

`lsp-haskell` is *just* the glue between `lsp-mode` and `haskell-language-server`.

If you have an issue with Emacs, it is probably a `lsp-mode` issue.
If you have an issue with the Haskell functionality itself, it is probably a `haskell-language-server` issue.
If you have an issue specifically with how the two are joined together, then it belongs here!

If you're not sure what the problem is, the `haskell-language-server` README provides some troubleshooting instructions.
