;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((lsp-mode "2.0") (haskell-mode "1.0"))
;; Keywords: haskell
;; URL: https://github.com/emacs-lsp/lsp-haskell

(require 'haskell)
(require 'lsp-mode)

(defun lsp-haskell--session-cabal-dir ()
  "Get the session cabal-dir."
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-dir (if cabal-file
                        (file-name-directory cabal-file)
                      "" ;; no cabal file, use directory only
                      )))
    (progn
      (message "cabal-dir: %s" cabal-dir)
      cabal-dir)))

(defun lsp-haskell--get-root ()
  "TODO: use projectile directory"
  (let ((dir (lsp-haskell--session-cabal-dir)))
    (if (string= dir "/")
        (user-error (concat "Couldn't find cabal file, using:" dir))
      dir)))

;; ---------------------------------------------------------------------
;; HaRe functions

(defun lsp-demote ()
  "Demote a function to the level it is used"
  (interactive)
  (lsp--send-execute-command
   "hare:demote"
   (vector `(:file (:textDocument ,(lsp-text-document-identifier)))
           `(:start_pos (:position ,(lsp-point-to-position (point)))))))

(defun lsp-lift-to-top ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (user-error "Not implemented")
  )

;; ---------------------------------------------------------------------

;;;###autoload
(lsp-define-stdio-client 'haskell-mode "haskell" 'stdio #'lsp-haskell--get-root
			  "Haskell Language Server"
			 '("hie" "--lsp" "-d" "-l" "/tmp/hie.log"))

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
