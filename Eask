;; -*- mode: eask; lexical-binding: t -*-

(package "lsp-haskell"
         "1.1"
         "Haskell support for lsp-mode")

(website-url "https://github.com/emacs-lsp/lsp-haskell")
(keywords "haskell")

(package-file "lsp-haskell.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "28.1")
(depends-on "lsp-mode")

(development
 (depends-on "f")
 (depends-on "ecukes")
 (depends-on "ert-runner")
 (depends-on "el-mock")
 (depends-on "spinner"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
