;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((lsp-mode "2.0") (haskell-mode "1.0"))
;; Keywords: haskell
;; URL: https://github.com/emacs-lsp/lsp-haskell

(require 'haskell)
(require 'lsp-mode)

(defun lsp--haskell-session-cabal-dir ()
  "Get the session cabal-dir."
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-dir (if cabal-file
                        (file-name-directory cabal-file)
                      "" ;; no cabal file, use directory only
                      )))
    (progn
      (message "cabal-dir: %s" cabal-dir)
      cabal-dir)))

(defun lsp--haskell-get-root ()
  "TODO: use projectile directory"
  (let ((dir (lsp--haskell-session-cabal-dir)))
    (if (string= dir "/")
        (user-error (concat "Couldn't find cabal file, using:" dir))
      dir)))

;;;###autoload
(lsp-define-client 'haskell-mode "haskell" 'stdio #'lsp--haskell-get-root
  :command '("hie" "--lsp" "-d" "-l" "/tmp/hie.log")
  :name "Haskell Language Server")

(provide 'lsp-haskell)
