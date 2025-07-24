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


;;For font
(set-face-attribute 'default nil :height 130)
;;(setq doom-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

;;For python indent
(setq-default python-indent-offset 4)

;;For fullscreen display
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;For eshell
(defun split-window-vertically-short ()
  "Split the window into two panes vertically, with the bottom one shorter."
  (interactive)
  (split-window-vertically)
  ;;(balance-windows)
  ;;(enlarge-window (- (window-height) 10))
  ;;(other-window 1)
  (eshell))

(global-set-key (kbd "C-x -") 'split-window-vertically-short)

;;For Company-mode
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

;; LSP-Mode
(with-eval-after-load 'lsp-mode
  ;; :global/:workspace/:file
  (setq lsp-modeline-diagnostics-scope :workspace)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(setq lsp-keymap-prefix  "s-l")



;;For VHDL
(defconst my-vhdl-style
  '((vhdl-tab-always-indent        . t)
    (vhdl-comment-only-line-offset . 4)
    (vhdl-offsets-alist            . ((arglist-close    . vhdl-lineup-arglist)
                                      (statement-cont   . 0)
                                      (case-alternative . 4)
                                      (block-open       . 0)))
    (vhdl-echo-syntactic-information-p . t)
    )
  "My VHDL Programming Style")

;; Customizations for vhdl-mode
(defun my-vhdl-mode-hook ()
  ;; add my personal style and set it for the current buffer
  (vhdl-add-style "PERSONAL" my-vhdl-style t)
  ;; offset customizations not in my-vhdl-style
  (vhdl-set-offset 'statement-case-intro '++)
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  ;; key bindings for VHDL are put in vhdl-mode-map
  (define-key vhdl-mode-map "\C-m" 'newline-and-indent)
  )

(add-hook 'vhdl-mode-hook 'my-vhdl-mode-hook)

(defun my-shell-hook ()
      (local-set-key "\C-cl" 'erase-buffer))

    (add-hook 'shell-mode-hook 'my-shell-hook)

;; For LaTeX
;;(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
;;(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

;; For verilog
(add-hook 'verilog-mode-hook #'lsp-deferred)
(add-hook 'systemverilog-mode-hook #'lsp-deferred)

(add-hook 'verilog-mode-hook #'yas-minor-mode)
(add-hook 'systemverilog-mode-hook #'yas-minor-mode)

;; (after! verilog-mode
;;   (setq major-mode-remap-alist
;;         '((verilog-mode . verilog-ts-mode)))
;;   (add-hook 'verilog-ts-mode-hook #'tree-sitter-hl-mode))

(use-package! yasnippet-snippets
  :after yasnippet)


 ;;Verilog Formatting
(setq lsp-verilog-verible-format-enable t
      lsp-verilog-verible-format-command "verible-verilog-format"
      lsp-verilog-verible-format-style "google")

  ;;Format on save
(add-hook 'verilog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

  ;;Format on save
(add-hook 'systemverilog-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

;; For Emmet
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;;For C/C++
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)
