lsp-haskell
===========

[![MELPA](https://melpa.org/packages/lsp-haskell-badge.svg)](https://melpa.org/#/lsp-haskell) [![Build Status](https://travis-ci.com/emacs-lsp/lsp-haskell.svg?branch=master)](https://travis-ci.com/emacs-lsp/lsp-haskell)

An Emacs Lisp library for interacting with
a [haskell-ide-engine](https://github.com/haskell/haskell-ide-engine/)
server using Microsoft's
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol/).

The library is designed to integrate with existing Emacs IDE frameworks
(completion-at-point, xref (beginning with Emacs 25.1), flycheck, haskell-mode, intero, etc).


*This package is still under development, and is not recommended for daily use.*

## Emacs Configuration

Install [`lsp-mode`](https://github.com/emacs-lsp/lsp-mode) first, and either clone
this repository, or install from MELPA. Add the following to your `.emacs`:

```emacs-lisp
(require 'lsp)
(require 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
```

Note: All three packages are also available via MELPA.

It needs the HIE server in your path, so follow the appropriate
OSX or Linux section below accordingly.

## Hie Installation (OSX, Linux)

The following steps are recommended to bootstrap `lsp-haskell` on OSX.

```bash
git clone https://github.com/haskell/haskell-ide-engine
cd haskell-ide-engine
./install.hs hie
```

After this, we need to instruct Emacs to prefer `hie-wrapper` over
`hie` so Hie can infer which version of ghc we need for a particular
project.

```elisp
(setq lsp-haskell-process-path-hie "hie-wrapper")
```

## Per project configuration

HIE has some settings that can be changed on the fly.  These are
exposed via a set of interactive functions.

- `lsp-haskell-set-hlint-on` / `lsp-haskell-set-hlint-off` Turn hlint
  checks on or off.
- `lsp-haskell-set-max-number-of-problems` Set the maximum number of
  diagnostics reported.
- `lsp-haskell-set-liquid-on` / `lsp-haskell-set-liquid-off` Turn
  liquid haskell checks on save on or off.
- `lsp-haskell-set-completion-snippets-on` /
  `lsp-haskell-set-completion-snippets-off` Whether completion should
  return plain text or snippets.

There are also non-interactive versions that do not actually send the
settings to the live server, but are suitable for use in `.dir-locals`
for a specific project.

- `lsp-haskell-set-hlint`
- `lsp-haskell-set-max-problems`
- `lsp-haskell-set-liquid`
- `lsp-haskell-set-completion-snippets`
