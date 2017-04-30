;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((lsp-mode "2.0") (haskell-mode "1.0"))
;; Keywords: haskell
;; URL: https://github.com/emacs-lsp/lsp-haskell

(require 'haskell)
(require 'lsp-mode)

;; ---------------------------------------------------------------------
;; HaRe functions

(defun lsp-demote ()
  "Demote a function to the level it is used"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:demote"
   (vector `(:file (:textDocument ,(lsp-text-document-identifier)))
           `(:start_pos (:position ,(lsp-point-to-position (point)))))))

(defun lsp-duplicate-definition (newname)
  "Duplicate a definition"
  (interactive "sNew definition name: ")
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:dupdef"
   (vector `(:file (:textDocument ,(lsp-text-document-identifier)))
           `(:start_pos (:position ,(lsp-point-to-position (point))))
           `(:name (:text ,newname)))))

(defun lsp-lift-to-top ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:lifttotoplevel"
   (vector `(:file (:textDocument ,(lsp-text-document-identifier)))
           `(:start_pos (:position ,(lsp-point-to-position (point)))))))

(defun lsp-lift-level ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:liftonelevel"
   (vector `(:file (:textDocument ,(lsp-text-document-identifier)))
           `(:start_pos (:position ,(lsp-point-to-position (point)))))))

;; ---------------------------------------------------------------------

;;;###autoload
(lsp-define-stdio-client 'haskell-mode "haskell" 'stdio #'lsp-haskell--get-root
			  "Haskell Language Server"
			 '("hie" "--lsp" "-d" "-l" "/tmp/hie.log"))
        ;; '("lsp-hello"))

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
