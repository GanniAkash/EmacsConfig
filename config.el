;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; LSP-Mode Keybindings
(after! lsp-mode
  ;; Start lsp-mode
  (define-key lsp-mode-map (kbd "C-c l") 'lsp)
  ;; Format buffer (changed from C-c C-f to C-c C-b)
  (define-key lsp-mode-map (kbd "C-c C-b") 'lsp-format-buffer)
  ;; Rename symbol
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-rename)
  ;; Execute code action
  (define-key lsp-mode-map (kbd "C-c C-a") 'lsp-execute-code-action)
  ;; Go to definition
  (define-key lsp-mode-map (kbd "C-c C-j") 'lsp-find-definition)
  ;; Describe thing at point
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  ;; Go to type definition
  (define-key lsp-mode-map (kbd "C-c C-t") 'lsp-find-type-definition)
  ;; Go to implementation
  (define-key lsp-mode-map (kbd "C-c C-i") 'lsp-find-implementation)
  ;; Show errors list
  (define-key lsp-mode-map (kbd "C-c C-e") 'lsp-treemacs-errors-list)
  ;; Show documentation
  (define-key lsp-mode-map (kbd "C-c C-k") 'lsp-ui-doc-show)
  ;; Show symbols
  (define-key lsp-mode-map (kbd "C-c C-c") 'lsp-treemacs-symbols)
  ;; Restart LSP server
  (define-key lsp-mode-map (kbd "C-c C-s") 'lsp-restart-workspace)
  ;; Shutdown LSP server
  (define-key lsp-mode-map (kbd "C-c C-q") 'lsp-workspace-shutdown)
  ;; Diagnostics list
  (define-key lsp-mode-map (kbd "C-c C-l") 'lsp-ui-flycheck-list)
  ;; Find references
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-find-references)
  ;; Hover documentation
  (define-key lsp-mode-map (kbd "C-c C-h") 'lsp-ui-doc-glance)
  ;; Next diagnostic
  (define-key lsp-mode-map (kbd "C-c C-n") 'flycheck-next-error)
  ;; Previous diagnostic
  (define-key lsp-mode-map (kbd "C-c C-p") 'flycheck-previous-error)
)

(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)

;; Changing default sartup directory
;; (setq default-directory "C:\\Users\\gakas\\OneDrive\\Documents" )


;; For gnupg
(setq package-check-signature nil)
(setq package-gnupghome-dir (expand-file-name "~/.gnupg"))
(setq package-gnupghome-dir (expand-file-name "~/.gnupg" user-emacs-directory))

;; Path for git
(setq magit-git-executable "D:\\Applications\\MSYS2\\usr\\bin\\git.exe") ;; replace with the correct path to git.exe

(require 'nasm-mode)

;; ;;For asm-lsp
;; (after! lsp-mode
;;   (add-hook 'asm-mode-hook
;;             (lambda ()
;;               (setq-local lsp-ui-doc-enable t            ;; Enable LSP UI documentation for asm-lsp
;;                           lsp-ui-doc-show-with-cursor t  ;; Show hover with cursor movement
;;                           lsp-ui-doc-delay 0.2))))     ;; Set delay before hover shows
;; (after! lsp-ui-doc
;;   (add-hook 'asm-mode-hook
;;             (lambda ()
;;               (setq-local lsp-ui-doc-max-width 100    ;; Set max width for asm-lsp hover docs
;;                           lsp-ui-doc-max-height 50
;;                           lsp-ui-doc-position 'at-point))))   ;; Set max height for asm-lsp hover docs

;; (after! lsp-mode
;;   (add-hook 'nasm-mode-hook
;;             (lambda ()
;;               (setq-local lsp-ui-doc-enable t            ;; Enable LSP UI documentation for asm-lsp
;;                           lsp-ui-doc-show-with-cursor t  ;; Show hover with cursor movement
;;                           lsp-ui-doc-delay 0.2))))     ;; Set delay before hover shows
;; (after! lsp-ui-doc
;;   (add-hook 'nasm-mode-hook
;;             (lambda ()
;;               (setq-local lsp-ui-doc-max-width 100    ;; Set max width for asm-lsp hover docs
;;                           lsp-ui-doc-max-height 50   ;; Set max height for asm-lsp hover docs
;;                           lsp-ui-doc-position 'at-point)))) ;; Position docs at point for asm-lsp
