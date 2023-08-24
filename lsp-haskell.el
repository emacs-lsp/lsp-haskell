;;; lsp-haskell.el --- Haskell support for lsp-mode

;; Version: 1.1
;; Package-Requires: ((emacs "27.1") (lsp-mode "3.0") (haskell-mode "16.1"))
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

(require 'lsp-mode)
(require 'haskell-mode)

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

(defcustom-lsp lsp-haskell-formatting-provider
  "ormolu"
  "The formatter to use when formatting a document or range.
Ensure the plugin is enabled."
  :group 'lsp-haskell
  :type '(choice (const "brittany") (const "floskell") (const "fourmolu") (const "ormolu") (const "stylish-haskell") (const "none"))
  :lsp-path "haskell.formattingProvider")
(defcustom-lsp lsp-haskell-check-project
  t
  "Whether to typecheck the entire project on load.
It could lead to bad perfomance in large projects."
  :group 'lsp-haskell
  :type 'boolean
  :lsp-path "haskell.checkProject")
(defcustom-lsp lsp-haskell-max-completions
  40
  "Maximum number of completions sent to the editor."
  :group 'lsp-haskell
  :type 'number
  :lsp-path "haskell.maxCompletions")

;; ---------------------------------------------------------------------
;; Plugin-specific configuration
(defgroup lsp-haskell-plugins nil
  "Customization group for 'lsp-haskell' plugins."
  :group 'lsp-haskell)

(defcustom-lsp lsp-haskell-plugin-import-lens-code-actions-on
  t
  "Enables explicit imports code actions"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.importLens.codeActionsOn")
(defcustom-lsp lsp-haskell-plugin-import-lens-code-lens-on
  t
  "Enables explicit imports code lenses"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.importLens.codeLensOn")
(defcustom-lsp lsp-haskell-plugin-hlint-code-actions-on
  t
  "Enables hlint code actions (apply hints)"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.hlint.codeActionsOn")
(defcustom-lsp lsp-haskell-plugin-hlint-diagnostics-on
  t
  "Enables hlint diagnostics"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.hlint.diagnosticsOn")
(defcustom-lsp lsp-haskell-plugin-hlint-config-flags
  nil
  "Flags used by hlint"
  :group 'lsp-haskell-plugins
  :type 'lsp-string-vector
  :lsp-path "haskell.plugin.hlint.config.flags")
(defcustom-lsp lsp-haskell-plugin-eval-global-on
  t
  "Enables eval plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.eval.globalOn")
(defcustom-lsp lsp-haskell-plugin-module-name-global-on
  t
  "Enables module name plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.moduleName.globalOn")
(defcustom-lsp lsp-haskell-plugin-splice-global-on
  t
  "Enables splice plugin (expand template haskell definitions)"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.splice.globalOn")
(defcustom-lsp lsp-haskell-plugin-haddock-comments-global-on
  t
  "Enables haddock comments plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.haddockComments.globalOn")
(defcustom-lsp lsp-haskell-plugin-class-global-on
  t
  "Enables type class plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.class.globalOn")
(defcustom-lsp lsp-haskell-plugin-retrie-global-on
  t
  "Enables retrie plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.retrie.globalOn")
(defcustom-lsp lsp-haskell-plugin-stan-global-on
  t
  "Enables stan plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.stan.globalOn")
(defcustom-lsp lsp-haskell-plugin-tactics-global-on
  t
  "Enables Wingman (tactics) plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.tactics.globalOn")
(defcustom-lsp lsp-haskell-plugin-tactics-config-auto-gas
  4
  "The depth of the search tree when performing \"Attempt to fill hole\".
Bigger values will be able to derive more solutions,
but will take exponentially more time."
  :group 'lsp-haskell-plugins
  :type 'number
  :lsp-path "haskell.plugin.tactics.config.auto_gas")
(defcustom-lsp lsp-haskell-plugin-tactics-config-hole-severity
  nil
  "The severity to use when showing hole diagnostics."
  :group 'lsp-haskell-plugins
  :type '(choice (const 1) (const 2) (const 3) (const 4) (const nil))
  :lsp-path "haskell.plugin.tactics.config.hole_severity")
(defcustom-lsp lsp-haskell-plugin-tactics-config-max-use-ctor-actions
  5
  "Maximum number of `Use constructor <x>` code actions that can appear"
  :group 'lsp-haskell-plugins
  :type 'number
  :lsp-path "haskell.plugin.tactics.config.max_use_ctor_actions")
(defcustom-lsp lsp-haskell-plugin-tactics-config-timeout-duration
  2
  "The timeout for Wingman actions, in seconds"
  :group 'lsp-haskell-plugins
  :type 'number
  :lsp-path "haskell.plugin.tactics.config.timeout_duration")
(defcustom-lsp lsp-haskell-plugin-tactics-config-proofstate-styling
  t
  "Should Wingman emit styling markup when showing metaprogram proof states?"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.tactics.config.proofstate_styling")
(defcustom-lsp lsp-haskell-plugin-pragmas-code-actions-on
  t
  "Enables pragmas code actions"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.pragmas.codeActionsOn")
(defcustom-lsp lsp-haskell-plugin-pragmas-completion-on
  t
  "Enables pragmas completions"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.pragmas.completionsOn")
(defcustom-lsp lsp-haskell-plugin-ghcide-completions-config-auto-extend-on
  t
  "Extends the import list automatically when completing a out-of-scope identifier"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.ghcide-completions.config.autoExtendOn")
(defcustom-lsp lsp-haskell-plugin-ghcide-completions-config-snippets-on
  lsp-enable-snippet
  "Inserts snippets when using code completions"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.ghcide-completions.config.snippetsOn")
(defcustom-lsp lsp-haskell-plugin-ghcide-type-lenses-global-on
  t
  "Enables type lenses plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.ghcide-type-lenses.globalOn")
(defcustom-lsp lsp-haskell-plugin-ghcide-type-lenses-config-mode
  t
  "Control how type lenses are shown"
  :group 'lsp-haskell-plugins
  :type '(choice (const "always") (const "exported") (const "diagnostics"))
  :lsp-path "haskell.plugin.ghcide-type-lenses.config.mode")
(defcustom-lsp lsp-haskell-plugin-refine-imports-global-on
  t
  "Enables refine imports plugin"
  :group 'lsp-haskell-plugins
  :type 'boolean
  :lsp-path "haskell.plugin.refineImports.globalOn")

;; Updated for haskell-language-server 2.1.0.0

(lsp-defcustom lsp-haskell-plugin-cabal-code-actions-on t
  "Enables cabal code actions"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.cabal.codeActionsOn")

(lsp-defcustom lsp-haskell-plugin-cabal-completion-on t
  "Enables cabal completions"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.cabal.completionOn")

(lsp-defcustom lsp-haskell-plugin-pragmas-suggest-global-on t
  "Enables pragmas-suggest plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.pragmas-suggest.globalOn")

(lsp-defcustom lsp-haskell-plugin-alternate-number-format-global-on t
  "Enables alternateNumberFormat plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.alternateNumberFormat.globalOn")

(lsp-defcustom lsp-haskell-plugin-call-hierarchy-global-on t
  "Enables callHierarchy plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.callHierarchy.globalOn")

(lsp-defcustom lsp-haskell-plugin-change-type-signature-global-on t
  "Enables changeTypeSignature plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.changeTypeSignature.globalOn")

(lsp-defcustom lsp-haskell-plugin-class-code-actions-on t
  "Enables class code actions"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.class.codeActionsOn")

(lsp-defcustom lsp-haskell-plugin-class-code-lens-on t
  "Enables class code lenses"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.class.codeLensOn")

(lsp-defcustom lsp-haskell-plugin-eval-config-diff t
  "Enable the diff output (WAS/NOW) of eval lenses"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.eval.config.diff")

(lsp-defcustom lsp-haskell-plugin-eval-config-exception nil
  "Enable marking exceptions with `*** Exception:` similarly to doctest and GHCi."
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.eval.config.exception")

(lsp-defcustom lsp-haskell-plugin-explicit-fields-global-on t
  "Enables explicit-fields plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.explicit-fields.globalOn")

(lsp-defcustom lsp-haskell-plugin-explicit-fixity-global-on t
  "Enables explicit-fixity plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.explicit-fixity.globalOn")

(lsp-defcustom lsp-haskell-plugin-fourmolu-config-external nil
  "Call out to an external \"fourmolu\" executable, rather than using the bundled library"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.fourmolu.config.external")

(lsp-defcustom lsp-haskell-plugin-gadt-global-on t
  "Enables gadt plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.gadt.globalOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-code-actions-bindings-global-on t
  "Enables ghcide-code-actions-bindings plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-code-actions-bindings.globalOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-code-actions-fill-holes-global-on t
  "Enables ghcide-code-actions-fill-holes plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-code-actions-fill-holes.globalOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-code-actions-imports-exports-global-on t
  "Enables ghcide-code-actions-imports-exports plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-code-actions-imports-exports.globalOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-code-actions-type-signatures-global-on t
  "Enables ghcide-code-actions-type-signatures plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-code-actions-type-signatures.globalOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-completions-global-on t
  "Enables ghcide-completions plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-completions.globalOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-hover-and-symbols-hover-on t
  "Enables ghcide-hover-and-symbols hover"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-hover-and-symbols.hoverOn")

(lsp-defcustom lsp-haskell-plugin-ghcide-hover-and-symbols-symbols-on t
  "Enables ghcide-hover-and-symbols symbols"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.ghcide-hover-and-symbols.symbolsOn")

(lsp-defcustom lsp-haskell-plugin-overloaded-record-dot-global-on t
  "Enables overloaded-record-dot plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.overloaded-record-dot.globalOn")

(lsp-defcustom lsp-haskell-plugin-pragmas-completion-global-on t
  "Enables pragmas-completion plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.pragmas-completion.globalOn")

(lsp-defcustom lsp-haskell-plugin-pragmas-disable-global-on t
  "Enables pragmas-disable plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.pragmas-disable.globalOn")

(lsp-defcustom lsp-haskell-plugin-qualify-imported-names-global-on t
  "Enables qualifyImportedNames plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.qualifyImportedNames.globalOn")

(lsp-defcustom lsp-haskell-plugin-rename-config-cross-module nil
  "Enable experimental cross-module renaming"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.rename.config.crossModule")

(lsp-defcustom lsp-haskell-plugin-rename-global-on t
  "Enables rename plugin"
  :type 'boolean
  :group 'lsp-haskell-plugins
  :package-version '(lsp-mode . "8.0.1")
  :lsp-path "haskell.plugin.rename.globalOn")

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
   (list (concat (lsp--suggest-project-root) \"/shell.nix\"))
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
;; Starting the server and registration with lsp-mode

(defun lsp-haskell--server-command ()
  "Command and arguments for launching the inferior language server process.
These are assembled from the customizable variables `lsp-haskell-server-path'
and `lsp-haskell-server-args' and `lsp-haskell-server-wrapper-function'."
  (funcall lsp-haskell-server-wrapper-function (append (list lsp-haskell-server-path "--lsp") lsp-haskell-server-args) ))

;; This mapping is set for 'haskell-mode -> haskell' in the lsp-mode repo itself. If we move
;; it there, then delete it from here.
;; It also isn't *too* important: it only sets the language ID, see
;; https://microsoft.github.io/language-server-protocol/specification#textDocumentItem
(add-to-list 'lsp-language-id-configuration '(haskell-literate-mode . "haskell"))
(add-to-list 'lsp-language-id-configuration '(haskell-tng-mode . "haskell"))
(add-to-list 'lsp-language-id-configuration '(haskell-cabal-mode . "haskell"))

;; Register the client itself
(lsp-register-client
  (make-lsp--client
    :new-connection (lsp-stdio-connection (lambda () (lsp-haskell--server-command)))
    ;; Should run under haskell-mode, haskell-literate-mode and haskell-tng-mode. We need to list haskell-literate-mode even though it's a derived mode of haskell-mode.
    :major-modes '(haskell-mode haskell-literate-mode haskell-tng-mode haskell-cabal-mode)
    ;; This is arbitrary.
    :server-id 'lsp-haskell
    ;; HLS does not currently send 'workspace/configuration' on startup (https://github.com/haskell/haskell-language-server/issues/2762),
    ;; so we need to push the configuration to it manually on startup. We should be able to
    ;; get rid of this once the issue is fixed in HLS.
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "haskell"))))
    :synchronize-sections '("haskell")
    ;; This is somewhat irrelevant, but it is listed in lsp-language-id-configuration, so
    ;; we should set something consistent here.
    :language-id "haskell"
    :completion-in-comments? lsp-haskell-completion-in-comments
    ))

;; ---------------------------------------------------------------------

(provide 'lsp-haskell)
;;; lsp-haskell.el ends here
