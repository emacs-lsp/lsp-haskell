(package "lsp-haskell"
         "1.0"
         "Haskell support for lsp-mode")

(package-file "lsp-haskell.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "24.3")
(depends-on "lsp-mode")

(development
 (depends-on "f")
 (depends-on "ecukes")
 (depends-on "ert-runner")
 (depends-on "el-mock")
 (depends-on "spinner")
 (depends-on "haskell-mode"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
