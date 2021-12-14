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

(defcustom lsp-haskell-formatting-provider
  "ormolu"
  "The formatter to use when formatting a document or range. Ensure the plugin is enabled."
  :group 'lsp-haskell
  :type '(choice (const "brittany") (const "floskell") (const "fourmolu") (const "ormolu") (const "stylish-haskell") (const "none")))
(defcustom lsp-haskell-check-project
  t
  "Whether to typecheck the entire project on load. It could lead to bad perfomance in large projects."
  :group 'lsp-haskell
  :type 'boolean)
(defcustom lsp-haskell-max-completions
  40
  "Maximum number of completions sent to the editor."
  :group 'lsp-haskell
  :type 'number)

;; ---------------------------------------------------------------------
;; Plugin-specific configuration
(defgroup lsp-haskell-plugins nil
  "Customization group for 'lsp-haskell' plugins."
  :group 'lsp-haskell)

(defcustom lsp-haskell-plugin-import-lens-code-actions-on
  t
  "Enables explicit imports code actions"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-import-lens-code-lens-on
  t
  "Enables explicit imports code lenses"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-hlint-code-actions-on
  t
  "Enables hlint code actions (apply hints)"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-hlint-diagnostics-on
  t
  "Enables hlint diagnostics"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-hlint-config-flags
  nil
  "Flags used by hlint"
  :group 'lsp-haskell-plugins
  :type 'lsp-string-vector)
(defcustom lsp-haskell-plugin-eval-global-on
  t
  "Enables eval plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-module-name-global-on
  t
  "Enables module name plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-splice-global-on
  t
  "Enables splice plugin (expand template haskell definitions)"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-haddock-comments-global-on
  t
  "Enables haddock comments plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-class-global-on
  t
  "Enables type class plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-retrie-global-on
  t
  "Enables retrie plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-tactics-global-on
  t
  "Enables Wingman (tactics) plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-tactics-config-auto-gas
  4
  "The depth of the search tree when performing \"Attempt to fill hole\". Bigger values will be able to derive more solutions, but will take exponentially more time."
  :group 'lsp-haskell-plugins
  :type 'number)
(defcustom lsp-haskell-plugin-tactics-config-hole-severity
  nil
  "The severity to use when showing hole diagnostics."
  :group 'lsp-haskell-plugins
  :type '(choice (const 1) (const 2) (const 3) (const 4) (const nil)))
(defcustom lsp-haskell-plugin-tactics-config-max-use-ctor-actions
  5
  "Maximum number of `Use constructor <x>` code actions that can appear"
  :group 'lsp-haskell-plugins
  :type 'number)
(defcustom lsp-haskell-plugin-tactics-config-timeout-duration
  2
  "The timeout for Wingman actions, in seconds"
  :group 'lsp-haskell-plugins
  :type 'number)
(defcustom lsp-haskell-plugin-tactics-config-proofstate-styling
  t
  "Should Wingman emit styling markup when showing metaprogram proof states?"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-pragmas-code-actions-on
  t
  "Enables pragmas code actions"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-pragmas-completion-on
  t
  "Enables pragmas completions"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-ghcide-completions-config-auto-extend-on
  t
  "Extends the import list automatically when completing a out-of-scope identifier"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-ghcide-completions-config-snippets-on
  lsp-enable-snippet
  "Inserts snippets when using code completions"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-ghcide-type-lenses-global-on
  t
  "Enables type lenses plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean)
(defcustom lsp-haskell-plugin-ghcide-type-lenses-config-mode
  t
  "Control how type lenses are shown"
  :group 'lsp-haskell-plugins
  :type '(choice (const "always") (const "exported") (const "diagnostics")))
(defcustom lsp-haskell-plugin-refine-imports-global-on
  t
  "Enables refine imports plugin"
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
;; Miscellaneous options
;;
(defcustom lsp-haskell-completion-in-comments
  t
  "Whether to trigger completions in comments.
Note that this must be set to true in order to get completion of pragmas."
  :group 'lsp-haskell
  :type 'boolean)

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
 '(("haskell.plugin.refineImports.globalOn" lsp-haskell-plugin-refine-imports-global-on t)
   ("haskell.plugin.ghcide-type-lenses.config.mode" lsp-haskell-plugin-ghcide-type-lenses-config-mode)
   ("haskell.plugin.ghcide-type-lenses.globalOn" lsp-haskell-plugin-ghcide-type-lenses-global-on t)
   ("haskell.plugin.ghcide-completions.config.snippetsOn" lsp-haskell-plugin-ghcide-completions-config-snippets-on t)
   ("haskell.plugin.ghcide-completions.config.autoExtendOn" lsp-haskell-plugin-ghcide-completions-config-auto-extend-on t)
   ("haskell.plugin.pragmas.completionOn" lsp-haskell-plugin-pragmas-completion-on t)
   ("haskell.plugin.pragmas.codeActionsOn" lsp-haskell-plugin-pragmas-code-actions-on t)
   ("haskell.plugin.tactics.config.proofstate_styling" lsp-haskell-plugin-tactics-config-proofstate-styling t)
   ("haskell.plugin.tactics.config.timeout_duration" lsp-haskell-plugin-tactics-config-timeout-duration)
   ("haskell.plugin.tactics.config.max_use_ctor_actions" lsp-haskell-plugin-tactics-config-max-use-ctor-actions)
   ("haskell.plugin.tactics.config.hole_severity" lsp-haskell-plugin-tactics-config-hole-severity)
   ("haskell.plugin.tactics.config.auto_gas" lsp-haskell-plugin-tactics-config-auto-gas)
   ("haskell.plugin.tactics.globalOn" lsp-haskell-plugin-tactics-global-on t)
   ("haskell.plugin.retrie.globalOn" lsp-haskell-plugin-retrie-global-on t)
   ("haskell.plugin.class.globalOn" lsp-haskell-plugin-class-global-on t)
   ("haskell.plugin.haddockComments.globalOn" lsp-haskell-plugin-haddock-comments-global-on t)
   ("haskell.plugin.splice.globalOn" lsp-haskell-plugin-splice-global-on t)
   ("haskell.plugin.moduleName.globalOn" lsp-haskell-plugin-module-name-global-on t)
   ("haskell.plugin.eval.globalOn" lsp-haskell-plugin-eval-global-on t)
   ("haskell.plugin.hlint.config.flags" lsp-haskell-plugin-hlint-config-flags)
   ("haskell.plugin.hlint.diagnosticsOn" lsp-haskell-plugin-hlint-diagnostics-on t)
   ("haskell.plugin.hlint.codeActionsOn" lsp-haskell-plugin-hlint-code-actions-on t)
   ("haskell.plugin.importLens.codeLensOn" lsp-haskell-plugin-import-lens-code-lens-on t)
   ("haskell.plugin.importLens.codeActionsOn" lsp-haskell-plugin-import-lens-code-actions-on t)
   ("haskell.maxCompletions" lsp-haskell-max-completions)
   ("haskell.checkProject" lsp-haskell-check-project t)
   ("haskell.formattingProvider" lsp-haskell-formatting-provider)))

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
    :completion-in-comments? lsp-haskell-completion-in-comments
    ))

;; ---------------------------------------------------------------------

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
