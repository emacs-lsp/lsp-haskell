# Contributing to lsp-haskell

## Updating the configuration options for lsp-haskell

1. Generate the vscode settings file

```sh
haskell-language-server-9.6.2 vscode-extension-schema > /tmp/schema.json
```
Add `{"contributes": {"configuration": { "properties":`
to the top and
`}}}` to the bottom.

2. Generate the settings snippet

Make sure you have https://github.com/emacs-lsp/lsp-mode/blob/master/scripts/lsp-generate-settings.el
on your machine somewhere. In this example it is at `/tmp/lsp-generate-settings.el`

In emacs, activate IELM, `M-x ielm`

In IELM,

```elisp
(load-library "/tmp/lsp-generate-settings.el")
;; Do direct output to buffer
(defun p (x) (move-end-of-line 0) (insert (format "\n%s" x)))
(p (lsp-generate-settings "/tmp/schema.json" 'lsp-haskell))
```

This will emit settings that do not currently exist in the running
emacs instance.

Edit them into [lsp-haskell.el](./lsp-haskell.el)
