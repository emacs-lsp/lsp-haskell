;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.0
;; Package-Requires: ((emacs "24.3") (lsp-mode "3.0") (haskell-mode "1.0"))
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

;;; Commentary:

;; Haskell specific adapter for LSP mode

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
(defcustom lsp-haskell-diagnostics-on-change
  t
  "Compute diagnostics continuously as you type.
Turn off to only generate diagnostics on file save."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-liquid-on
  nil
  "Get diagnostics from liquid haskell."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-completion-snippets-on
  lsp-enable-snippet
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
;; Plugin-specific configuration
(defgroup lsp-haskell-plugins nil
  "Customization group for 'lsp-haskell' plugins."
  :group 'lsp-haskell)

(defcustom lsp-haskell-ghcide-on
  t
  "Turn on the ghcide plugins."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-pragmas-on
  t
  "Turn on the pragmas plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-floskell-on
  t
  "Turn on the floskell plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-fourmolu-on
  t
  "Turn on the fourmolu plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-ormolu-on
  t
  "Turn on the ormolu plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-stylish-haskell-on
  t
  "Turn on the stylish-haskell plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-brittany-on
  t
  "Turn on the brittany plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-tactic-on
  t
  "Turn on the tactic plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-retrie-on
  t
  "Turn on the retrie plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-eval-on
  t
  "Turn on the eval plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-importlens-on
  t
  "Turn on the explicit import lens."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-modulename-on
  t
  "Turn on the moduleName plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-hlint-on
  t
  "Turn on the hlint plugin."
  :group 'lsp-haskell-plugins
  :type 'boolean)

;; ---------------------------------------------------------------------
;; Non-language server options

(defcustom lsp-haskell-server-path
  "haskell-language-server-wrapper"
  "The language server executable.
Can be something on the $PATH (e.g. 'ghcide') or a path to an executable itself."
  :group 'lsp-haskell
  :type 'string)

(defcustom lsp-haskell-server-log-file
  (expand-file-name "hls.log" temporary-file-directory)
  "The log file used by the server.
Note that this is passed to the server via 'lsp-haskell-server-args', so if
you override that setting then this one will have no effect."
  :group 'lsp-haskell
  :type 'string)

(defcustom lsp-haskell-server-args
  `("-d" "-l" ,lsp-haskell-server-log-file)
  "The arguments for starting the language server.
For a debug log when using haskell-language-server, use `-d -l /tmp/hls.log'."
  :group 'lsp-haskell
  :type '(repeat (string :tag "Argument")))

(defcustom lsp-haskell-server-wrapper-function
  #'identity
  "Use this to wrap the language server process started by lsp-haskell.
For example, use the following the start the process in a nix-shell:
\(lambda (argv)
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
;; Miscellaneous useful functions

(defun lsp-haskell--session-cabal-dir ()
  "Get the session cabal-dir."
  (let* ((cabal-file (haskell-cabal-find-file))
         (cabal-dir (if cabal-file
                        (file-name-directory cabal-file)
                      "." ;; no cabal file, use directory only
                      )))
    (message "cabal-dir: %s" cabal-dir)
    cabal-dir))

(defun lsp-haskell--get-root ()
  "Get project root directory.

First searches for root via projectile.  Tries to find cabal file
if projectile way fails"
  ;; (if (and (fboundp 'projectile-project-root) (projectile-project-root))
  (if nil
      (projectile-project-root)
    (let ((dir (lsp-haskell--session-cabal-dir)))
      (if (string= dir "/")
          (user-error "Couldn't find cabal file, using: %s" dir)
        dir))))

;; ---------------------------------------------------------------------
;; Starting the server and registration with lsp-mode

(defun lsp-haskell--server-command ()
  "Command and arguments for launching the inferior language server process.
These are assembled from the customizable variables `lsp-haskell-server-path'
and `lsp-haskell-server-args' and `lsp-haskell-server-wrapper-function'."
  (funcall lsp-haskell-server-wrapper-function (append (list lsp-haskell-server-path "--lsp") lsp-haskell-server-args) ))

;; Register all the language server settings with lsp-mode.
;; Note that customizing these will currently *not* send the updated configuration to the server,
;; users must manually restart. See https://github.com/emacs-lsp/lsp-mode/issues/1174.
(lsp-register-custom-settings
 '(("haskell.formattingProvider" lsp-haskell-formatting-provider)
   ("haskell.formatOnImportOn" lsp-haskell-format-on-import-on t)
   ("haskell.completionSnippetsOn" lsp-haskell-completion-snippets-on t)
   ("haskell.liquidOn" lsp-haskell-liquid-on t)
   ("haskell.diagnosticsOnChange" lsp-haskell-diagnostics-on-change t)
   ("haskell.maxNumberOfProblems" lsp-haskell-max-number-of-problems)
   ("haskell.hlintOn" lsp-haskell-hlint-on t)

   ("haskell.plugin.ghcide.globalOn"          lsp-haskell-ghcide-on t)
   ("haskell.plugin.pragmas.globalOn"         lsp-haskell-pragmas-on t)
   ("haskell.plugin.floskell.globalOn"        lsp-haskell-floskell-on t)
   ("haskell.plugin.fourmolu.globalOn"        lsp-haskell-fourmolu-on t)
   ("haskell.plugin.ormolu.globalOn"          lsp-haskell-ormolu-on t)
   ("haskell.plugin.stylish-haskell.globalOn" lsp-haskell-stylish-haskell-on t)
   ("haskell.plugin.brittany.globalOn"        lsp-haskell-brittany-on t)
   ("haskell.plugin.tactic.globalOn"          lsp-haskell-tactic-on t)
   ("haskell.plugin.retrie.globalOn"          lsp-haskell-retrie-on t)
   ("haskell.plugin.eval.globalOn"            lsp-haskell-eval-on t)
   ("haskell.plugin.importLens.globalOn"      lsp-haskell-importlens-on t)
   ("haskell.plugin.moduleName.globalOn"      lsp-haskell-modulename-on t)
   ("haskell.plugin.hlint.globalOn"           lsp-haskell-hlint-on t)))

;; This mapping is set for 'haskell-mode -> haskell' in the lsp-mode repo itself. If we move
;; it there, then delete it from here.
;; It also isn't *too* important: it only sets the language ID, see
;; https://microsoft.github.io/language-server-protocol/specification#textDocumentItem
(add-to-list 'lsp-language-id-configuration '(haskell-literate-mode . "haskell"))

;; Register the client itself
(lsp-register-client
  (make-lsp--client
    :new-connection (lsp-stdio-connection (lambda () (lsp-haskell--server-command)))
    ;; Should run under haskell-mode and haskell-literate-mode. We need to list the
    ;; latter even though it's a derived mode of the former
    :major-modes '(haskell-mode haskell-literate-mode)
    ;; This is arbitrary.
    :server-id 'lsp-haskell
    ;; We need to manually pull out the configuration section and set it. Possibly in
    ;; the future lsp-mode will asssociate servers with configuration sections more directly.
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "haskell"))))
    ;; This is somewhat irrelevant, but it is listed in lsp-language-id-configuration, so
    ;; we should set something consistent here.
    :language-id "haskell"
    ;; This is required for completions to works inside language pragma statements
    :completion-in-comments? t
    ))

;; ---------------------------------------------------------------------

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
