;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((lsp-mode "3.0") (haskell-mode "1.0"))
;; Keywords: haskell
;; URL: https://github.com/emacs-lsp/lsp-haskell

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Code:

(require 'haskell)
(require 'lsp-mode)
(require 'projectile nil 'noerror)

;; ---------------------------------------------------------------------
;; Configuration

(defgroup lsp-haskell nil
  "Customization group for ‘lsp-haskell’."
  :group 'lsp-mode)

;; ---------------------------------------------------------------------
;; Language server options

;; These are registered with lsp-mode below, which handles preparing them for the server.
;; Originally generated from the vscode extension's package.json using lsp-generate-bindings.
;; Should ideally stay in sync with what's offered in the vscode extension.

(defcustom lsp-haskell-hlint-on
  t
  "Get suggestions from hlint."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-max-number-of-problems
  100
  "Controls the maximum number of problems produced by the server."
  :group 'lsp-haskell
  :type 'number)
(defcustom
  lsp-haskell-diagnostics-on-change
  t
  "Compute diagnostics continuously as you type. Turn off to only generate diagnostics on file save."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-liquid-on
  nil
  "Get diagnostics from liquid haskell."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-completion-snippets-on
  t
  "Show snippets with type information when using code completion."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-format-on-import-on
  t
  "When adding an import, use the formatter on the result."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-formatting-provider
  "ormolu" 
  "The formatter to use when formatting a document or range."
  :group 'lsp-haskell
  :type '(choice (const :tag "brittany" "brittany") 
                 (const :tag "floskell" "floskell") 
                 (const :tag "fourmolu" "fourmolu") 
                 (const :tag "ormolu" "ormolu") 
                 (const :tag "stylish-haskell" "stylish-haskell") 
                 (const :tag "none" "none")))

;; ---------------------------------------------------------------------
;; Non-language server options

(defcustom lsp-haskell-server-path
  "haskell-language-server"
  "The language server executable. Can be something on the $PATH (e.g. 'ghcide') or a path to an executable itself."
  :group 'lsp-haskell
  :type 'string)

(defcustom lsp-haskell-server-args
  '("-d" "-l" "/tmp/hls.log")
  "The arguments for starting the language server.
For a debug log when using haskell-language-server, use `-d -l /tmp/hls.log'."
  :group 'lsp-haskell
  :type '(repeat (string :tag "Argument")))

(defcustom lsp-haskell-server-wrapper-function
  #'identity
  "Use this to wrap the language server process started by lsp-haskell.
For example, use the following the start the process in a nix-shell:
(lambda (argv)
  (append
   (append (list \"nix-shell\" \"-I\" \".\" \"--command\" )
           (list (mapconcat 'identity argv \" \"))
           )
   (list (concat (lsp-haskell--get-root) \"/shell.nix\"))
   )
  )"
  :group 'lsp-haskell
  :type '(choice
          (function-item :tag "None" :value identity)
          (function :tag "Custom function")))

;; ---------------------------------------------------------------------
;; HaRe functions

(defun lsp-demote ()
  "Demote a function to the level it is used"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:demote"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-duplicate-definition (newname)
  "Duplicate a definition"
  (interactive "sNew definition name: ")
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:dupdef"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))
             :text ,newname))))

(defun lsp-if-to-case ()
  "Convert an if statement to a case statement"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:iftocase"
   (vector `(:file      ,(concat "file://" buffer-file-name)
             :start_pos ,(lsp-get-start-position)
             :end_pos   ,(lsp-get-end-position)))))

(defun lsp-lift-level ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:liftonelevel"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-lift-to-top ()
  "Lift a function to the top level"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:lifttotoplevel"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-delete-definition ()
  "Delete a definition"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:deletedef"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

(defun lsp-generalise-applicative ()
  "Generalise a monadic function to use applicative"
  (interactive)
  (lsp--cur-workspace-check)
  (lsp--send-execute-command
   "hare:genapplicative"
   (vector `(:file ,(concat "file://" buffer-file-name)
             :pos  ,(lsp-point-to-position (point))))))

;; ---------------------------------------------------------------------
;; Miscellaneous useful functions

(defun lsp-haskell--session-cabal-dir ()
  "Get the session cabal-dir."
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-dir (if cabal-file
                        (file-name-directory cabal-file)
                      "." ;; no cabal file, use directory only
                      )))
    (progn
      (message "cabal-dir: %s" cabal-dir)
      cabal-dir)))

(defun lsp-haskell--get-root ()
  "Get project root directory.

First searches for root via projectile.  Tries to find cabal file
if projectile way fails"
  ;; (if (and (fboundp 'projectile-project-root) (projectile-project-root))
  (if nil
      (projectile-project-root)
    (let ((dir (lsp-haskell--session-cabal-dir)))
      (if (string= dir "/")
          (user-error (concat "Couldn't find cabal file, using:" dir))
        dir))))

;; ---------------------------------------------------------------------
;; Starting the server and registration with lsp-mode

(defun lsp-haskell--server-command ()
  "Command and arguments for launching the inferior language server process.
These are assembled from the customizable variables `lsp-haskell-server-path'
and `lsp-haskell-server-args'."
  (append (list lsp-haskell-server-path "--lsp") lsp-haskell-server-args) )

;; Register all the language server settings with lsp-mode.
;; Note that customizing these will currently *not* send the updated configuration to the server, 
;; users must manually restart. See https://github.com/emacs-lsp/lsp-mode/issues/1174.
(lsp-register-custom-settings '(
                                ("haskell.formattingProvider" lsp-haskell-formatting-provider)
                                ("haskell.formatOnImportOn" lsp-haskell-format-on-import-on t)
                                ("haskell.completionSnippetsOn" lsp-haskell-completion-snippets-on t)
                                ("haskell.liquidOn" lsp-haskell-liquid-on t)
                                ("haskell.diagnosticsOnChange" lsp-haskell-diagnostics-on-change t)
                                ("haskell.maxNumberOfProblems" lsp-haskell-max-number-of-problems)
                                ("haskell.hlintOn" lsp-haskell-hlint-on t)))

;; Register the client itself
(lsp-register-client
  (make-lsp--client
    :new-connection (lsp-stdio-connection (lambda () (lsp-haskell--server-command)))
    :major-modes '(haskell-mode)
    ;; This is arbitrary.
    :server-id 'lsp-haskell
    ;; We need to manually pull out the configuration section and set it. Possibly in
    ;; the future lsp-mode will asssociate servers with configuration sections more directly.
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "haskell"))))
    ;; No need to set :language-id, since there isn't one for Haskell and we
    ;; don't support multiple languages
    ))

;; ---------------------------------------------------------------------

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
