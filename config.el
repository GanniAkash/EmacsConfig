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
  (define-key lsp-mode-map (kbd "C-c b") 'lsp-format-buffer)
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
  (define-key lsp-mode-map (kbd "C-c c") 'lsp-treemacs-symbols)
  ;; Restart LSP server
  (define-key lsp-mode-map (kbd "C-c s") 'lsp-restart-workspace)
  ;; Shutdown LSP server
  (define-key lsp-mode-map (kbd "C-c C-q") 'lsp-workspace-shutdown)
  ;; Diagnostics list
  (define-key lsp-mode-map (kbd "C-c C-l") 'lsp-ui-flycheck-list)
  ;; Find references
  (define-key lsp-mode-map (kbd "C-c C-r") 'lsp-find-references)
  ;; Hover documentation
  (define-key lsp-mode-map (kbd "C-c C-h") 'lsp-ui-doc-glance)
  ;; Next diagnostic
  (define-key lsp-mode-map (kbd "C-c n") 'flycheck-next-error)
  ;; Previous diagnostic
  (define-key lsp-mode-map (kbd "C-c p") 'flycheck-previous-error)
)


;;For font
(set-face-attribute 'default nil :height 130)
;;(setq doom-font (font-spec :family "Symbols Nerd Font Mono" :size 14))

;;For python indent
(setq-default python-indent-offset 4)

;;For fullscreen display
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;For eshell
(defvar my/bottom-vterm-buffer "*vterm*")

(defun my/vterm-bottom-toggle ()
  "Toggle a persistent vterm at the bottom of the frame."
  (interactive)
  (let ((buf (get-buffer my/bottom-vterm-buffer)))
    (cond
     ;; If window is visible → hide it
     ((and buf (get-buffer-window buf))
      (delete-window (get-buffer-window buf)))

     ;; If buffer exists but hidden → show it
     (buf
      (display-buffer
       buf
       '((display-buffer-at-bottom)
         (window-height . 10)))
      (select-window (get-buffer-window buf)))

     ;; Buffer does not exist → create it once
     (t
      (let ((default-directory (or (project-root (project-current))
                                   default-directory)))
        (vterm my/bottom-vterm-buffer))
      (display-buffer
       (get-buffer my/bottom-vterm-buffer)
       '((display-buffer-at-bottom)
         (window-height . 10)))
      (select-window (get-buffer-window my/bottom-vterm-buffer))))))

(global-set-key (kbd "C-x -") #'my/vterm-bottom-toggle)

;;Inline Shell
(defvar my/async-process-buffer "*Project Async Process*")
(defvar my/async-process nil)

(defun my/projectile-async-shell-command (command)
  "Run shell COMMAND asynchronously at Projectile project root.
If another process is running, ask before killing it."
  (interactive
   (list (read-shell-command "Async command (project root): "
                             nil nil
                             'shell-command-history)))
  (let* ((default-directory
          (or (projectile-project-root)
              default-directory))
         (buf (get-buffer-create my/async-process-buffer)))

    ;; If a process is already running, ask before killing it
    (when (and my/async-process
               (process-live-p my/async-process))
      (unless (y-or-n-p
               (format "Process \"%s\" is running. Kill it and start a new one? "
                       (process-name my/async-process)))
        (user-error "Aborted"))
      (kill-process my/async-process))

    ;; Prepare buffer (read-only, output-only)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer))
      (special-mode))   ;; <-- THIS is the key line

    ;; Start async process
    (setq my/async-process
          (start-process-shell-command
           "project-async"
           buf
           command))

    ;; Notify on exit
    (set-process-sentinel
     my/async-process
     (lambda (proc event)
       (message "[%s] %s"
                (process-name proc)
                (string-trim event))))

    (message "Started async process: %s" command)))

(defun my/kill-project-async-process ()
  "Kill the running project async process."
  (interactive)
  (if (and my/async-process
           (process-live-p my/async-process))
      (progn
        (kill-process my/async-process)
        (message "Async process killed"))
    (message "No running async process")))

(global-set-key (kbd "C-c ,") #'my/projectile-async-shell-command)
(global-set-key (kbd "C-c k") #'my/kill-project-async-process)

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


(with-eval-after-load 'lsp-mode
  (require 'dap-cpptools))

;;For GDB
(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gdb-restore-window-configuration-after-quit t)
(setq gdb-confirm-quit nil)
(setq gdb-non-stop-setting nil)
(setq gud-gdb-command-name "gdb-multiarch -q --annotate=3")

(defun my/projectile-gdb ()
  "Run gdb from Projectile project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively 'gdb)))

(global-set-key (kbd "C-c d") #'my/projectile-gdb)

(setq gdb-use-colon-colon-notation t)
(setq gdb-use-separate-io-buffer t)

(defun my/gdb-running-p ()
  "Return non-nil if a GDB process is running."
  (and (boundp 'gud-comint-buffer)
       gud-comint-buffer
       (buffer-live-p gud-comint-buffer)
       (comint-check-proc gud-comint-buffer)))

(defun my/gdb-send (cmd)
  "Send CMD to GDB if it is running."
  (when (my/gdb-running-p)
    (with-current-buffer gud-comint-buffer
      (goto-char (point-max))
      (insert cmd)
      (comint-send-input))))

(defun my/gdb-next ()
  (interactive)
  (my/gdb-send "n"))

(defun my/gdb-step ()
  (interactive)
  (my/gdb-send "s"))

(defun my/gdb-continue ()
  (interactive)
  (my/gdb-send "c"))

(defun my/gdb-get-breakpoints ()
  "Return GDB breakpoint info as string."
  (with-current-buffer gud-comint-buffer
    (gud-call "info breakpoints")))

(defun my/gdb-breakpoints-at (file line)
  "Return a list of breakpoint numbers at FILE:LINE."
  (let ((output (my/gdb-get-breakpoints))
        (regex (format "\\([0-9]+\\).*%s:%d"
                       (regexp-quote file) line))
        result)
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      (while (re-search-forward regex nil t)
        (push (match-string 1) result)))
    result)
  (message "%s" result)
  )

(defun my/gdb-toggle-breakpoint ()
  (interactive)
  (when (my/gdb-running-p)
    (let* ((file (file-name-nondirectory (buffer-file-name)))
           (line (line-number-at-pos))
           (bps (my/gdb-breakpoints-at file line)))
      (if bps
          ;; Delete all breakpoints at this line
          (dolist (bp bps)
            (my/gdb-send (format "delete %s" bp)))
        ;; No breakpoint → create
        (my/gdb-send (format "b %s:%d" file line))))))


  (with-eval-after-load 'cc-mode
  (define-key c-mode-base-map (kbd "C-c C-n") #'my/gdb-next)
  (define-key c-mode-base-map (kbd "C-c C-s") #'my/gdb-step)
  (define-key c-mode-base-map (kbd "C-c C-c") #'my/gdb-continue)
  (define-key c-mode-base-map (kbd "C-c C-b") #'my/gdb-toggle-breakpoint))


;; Load and use sr-speedbar
(require 'sr-speedbar)

(defun my-gdb-io-to-speedbar ()
  "Replace GDB IO window with SPEEDBAR and undedicate it."
  (when (bound-and-true-p gdb-many-windows)
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (when (and buf
                   (string-match-p "^\\*input/output of " (buffer-name buf)))
          ;; unpin window
          (set-window-dedicated-p win nil)
          ;; switch to SPEEDBAR
          (with-selected-window win
            (switch-to-buffer " SPEEDBAR")))))))

(add-hook
 'gdb-mode-hook
 (lambda ()
   (run-with-idle-timer 0.2 nil #'my-gdb-io-to-speedbar)))


(speedbar-frame-mode -1)
(setq speedbar-use-single-frame t)
(setq speedbar-frame-parameters nil)


;; Nerd Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono-12")

;;For org config

(use-package org
  :defer t
  :config
;; Resize Org headings
;; (custom-set-faces
;; '(org-document-title ((t (:height 1.6))))
;; '(outline-1          ((t (:height 1.25))))
;; '(outline-2          ((t (:height 1.2))))
;; '(outline-3          ((t (:height 1.2))))
;; '(outline-4          ((t (:height 1.2))))
;; '(outline-5          ((t (:height 1.2))))
;; '(outline-6          ((t (:height 1.2))))
;; '(outline-7          ((t (:height 1.2))))
;; '(outline-8          ((t (:height 1.2))))
;; '(outline-9          ((t (:height 1.2)))))
;; (org-indent-mode -1)
(setq org-startup-with-latex-preview t)
(plist-put org-format-latex-options :scale 1.35)
(let ((png (cdr (assoc 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter '("dvipng -D %D -T tight -bg Transparent -o %O %F")))
(setq org-startup-folded 'content)
(setq org-adapt-indentation t
      org-hide-leading-stars t
      org-pretty-entities t
      org-ellipsis "  ·")
(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-edit-src-content-indentation 0)
(setq org-log-done                       t
      org-auto-align-tags                t
      org-tags-column                    -80
      org-fold-catch-invisible-edits     'show-and-error
      org-special-ctrl-a/e               t
      org-insert-heading-respect-content t)
)

;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12:c%?-12t% s")  ; File name and time for agenda items
;;         (todo . " %i %b %-12:c")            ; File name for TODO items
;;         (tags . " %i %-12:c")            ; File name for tagged items
;;         (search . " %i %-12:c")))        ; File name for search results
(use-package org-agenda
  :ensure nil
  :after org
  :custom
  (org-agenda-include-diary t)
  (org-agenda-files '("/mnt/c/Users/gakas/Onedrive/Documents/Notes"))
  (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s")
                              ;; Indent todo items by level to show nesting
                              (todo . " %i %-12:c%l")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-start-on-weekday nil))

  (use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; (add-hook 'org-mode-hook 'olivetti-mode)
(setq org-startup-with-inline-images t)

;; (add-hook 'org-mode-hook 'variable-pitch-mode)

(use-package org-appear
  :commands (org-appear-mode)
  :hook     (org-mode . org-appear-mode)
  :config
  (setq org-hide-emphasis-markers t)  ;; Must be activated for org-appear to work
  (setq org-appear-autoemphasis   t   ;; Show bold, italics, verbatim, etc.
        org-appear-autolinks      t   ;; Show links
        org-appear-autosubmarkers t)) ;; Show sub- and superscripts

(use-package org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))

(use-package org-superstar
  :after org
  :config
  (setq org-superstar-leading-bullet " ")
  ;; (setq org-superstar-headline-bullets-list '("*" "*" "●" "○" "•"))
  (setq org-superstar-special-todo-items t) ;; Makes TODO header bullets into boxes
  (setq org-superstar-todo-bullet-alist '(("TODO"     . 9744)
                                          ("PROG"     . 9744)
                                          ("NEXT"     . 9744)
                                          ("WAIT"     . 9744)
                                          ("DROP"     . 9744)
                                          ("QUESTION" . 9744)
                                          ("DONE"     . 9745)))
  :hook (org-mode . org-superstar-mode))

(use-package svg-tag-mode
  :after org
  :config
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
	(svg-image (svg-lib-concat
				(svg-lib-progress-bar (/ (string-to-number value) 100.0)
			      nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
				(svg-lib-tag (concat value "%")
				  nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
	(let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
	  (svg-image (svg-lib-concat
				  (svg-lib-progress-bar (/ count total) nil
					:margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
				  (svg-lib-tag value nil
					:stroke 0 :margin 0)) :ascent 'center)))
  (setq svg-tag-tags
      `(;; Org tags
        ;; (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
        ;; (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

        ;; Task priority
        ("\\[#[A-Z]\\]" . ( (lambda (tag)
                              (svg-tag-make tag :face 'org-priority
                                            :beg 2 :end -1 :margin 0))))

        ;; Progress
        ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
          (svg-progress-percent (substring tag 1 -2)))))
        ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
          (svg-progress-count (substring tag 1 -1)))))

        ;; TODO / DONE
        ;; ("TODO" . ((lambda (tag) (svg-tag-make "TODO" :face 'org-todo
		;; 									           :inverse t :margin 0))))
        ;; ("DONE" . ((lambda (tag) (svg-tag-make "DONE" :face 'org-done :margin 0))))


        ;; Citation of the form [cite:@Knuth:1984]
        ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                          (svg-tag-make tag
                                                        :inverse t
                                                        :beg 7 :end -1
                                                        :crop-right t))))
        ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                                (svg-tag-make tag
                                                              :end -1
                                                              :crop-left t))))


        ;; Active date (with or without day name, with or without time)
        (,(format "\\(<%s>\\)" date-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :end -1 :margin 0))))
        (,(format "\\(<%s \\)%s>" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0))))
        (,(format "<%s \\(%s>\\)" date-re day-time-re) .
         ((lambda (tag)
            (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

        ;; Inactive date  (with or without day name, with or without time)
         (,(format "\\(\\[%s\\]\\)" date-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :end -1 :margin 0 :face 'org-date))))
         (,(format "\\(\\[%s \\)%s\\]" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :face 'org-date))))
         (,(format "\\[%s \\(%s\\]\\)" date-re day-time-re) .
          ((lambda (tag)
             (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date)))))))

(svg-tag-mode t)
(add-hook 'org-mode-hook 'svg-tag-mode)

(defun tt/prettify-symbols-setup ()
  "Beautify keywords"
  (interactive)
  (setq prettify-symbols-alist
		(mapcan (lambda (x)
                  (when (and (consp x) (stringp (car x)))
                    (list x (cons (upcase (car x)) (cdr x)))))
			        '(; Greek symbols
				  ("lambda" . ?λ)
				  ("delta"  . ?Δ)
				  ("gamma"  . ?Γ)
				  ("phi"    . ?φ)
				  ("psi"    . ?ψ)
                  ; Org headers
				  ("#+title:"  . ? )
				  ("#+author:" . ? )
                  ("#+date:"   . ? )
                  ; Checkboxes
				  ("[ ]" . ?)
				  ("[X]" . ?)
				  ("[-]" . ?)
                  ; Misc
                  ("->" . ?➜)
                  ; Blocks
				  ("#+begin_src"   . ?) ; 
				  ("#+end_src"     . ?)
				  ("#+begin_QUOTE" . ?‟)
				  ("#+end_QUOTE"   . ?”)
                  ; Drawers
				  (":properties:" . ?)
                  ; Agenda scheduling
				  ("SCHEDULED:"   . ?🕘)
				  ("DEADLINE:"    . ?⏰)
                  ; Agenda tags 
				  (":@projects:"  . ?☕)
				  (":work:"       . ?🚀)
				  (":@inbox:"     . ?✉)
				  (":goal:"       . ?🎯)
				  (":task:"       . ?📋)
				  (":@thesis:"    . ?📝)
				  (":thesis:"     . ?📝)
				  (":emacs:"      . ?)
				  (":learn:"      . ?🌱)
				  (":code:"       . ?💻)
				  (":fix:"        . ?🛠)
				  (":bug:"        . ?🚩)
				  (":read:"       . ?📚)
                  ; Roam tags
				  ("#+filetags:"  . ?📎)
				  (":wip:"        . ?🏗)
				  (":ct:"         . ?➡)    ; Category Theory
                  (":verb:"       . ?🌐) ; HTTP Requests in Org mode
                  )))
  (prettify-symbols-mode))

(setq org-latex-compiler "xelatex") ;; Ensure XeLaTeX is used

(setq org-latex-packages-alist
      '(("" "fontspec" t)
        ;; Add other packages if needed
        ))

(setq org-latex-header
      "\\setmainfont{Segoe UI Emoji}")

(defun tt/replace-inline-tags-with-glyphs (_backend)
  "Replace inline :tag: tokens with glyphs before Org export.
Do not touch trailing heading tags (the tag block at end of a headline)."
  (let* ((replacements '((":@projects:"  . ?☕)
				  (":work:"       . ?🚀)
				  (":@inbox:"     . ?✉)
				  (":goal:"       . ?🎯)
				  (":task:"       . ?📋)
				  (":@thesis:"    . ?📝)
				  (":thesis:"     . ?📝)
				  (":emacs:"      . ?)
				  (":learn:"      . ?🌱)
				  (":code:"       . ?💻)
				  (":fix:"        . ?🛠)
				  (":bug:"        . ?🚩)
				  (":read:"       . ?📚)))
         ;; Build alternation without word boundaries (colons are not word chars).
         (tags-regexp (concat "\\(?:" (mapconcat (lambda (t) (regexp-quote (car t)))
                                                 replacements "\\|")
                              "\\)")))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward tags-regexp nil t)
        (let* ((match (match-string 0))
               (glyph (cdr (assoc match replacements)))
               (pos   (match-beginning 0))
               (heading-p (org-at-heading-p))
               (line-beg (line-beginning-position))
               (line-end (line-end-position))
               (line (buffer-substring-no-properties line-beg line-end))
               (trailing-tags-start
                (and heading-p
                     (when (string-match " \\(:[[:alnum:]_@]+:\\)+\\s-*$" line)
                       (+ line-beg (match-beginning 1))))))
          (unless (and trailing-tags-start (>= pos trailing-tags-start))
            (replace-match glyph t t)))))))

;; Ensure the function runs during export preprocessing.
(add-hook 'org-export-before-processing-hook #'tt/replace-inline-tags-with-glyphs)

(add-hook 'org-mode-hook        #'tt/prettify-symbols-setup)
(add-hook 'org-agenda-mode-hook #'tt/prettify-symbols-setup)

(add-to-list 'font-lock-extra-managed-props 'display)
(font-lock-add-keywords 'org-mode
                        `(("^.*?\\( \\)\\(:[[:alnum:]_@#%:]+:\\)$"
                           (1 `(face nil
                                     display (space :align-to (- right ,(org-string-width (match-string 2)) 3)))
                              prepend))) t)
(add-hook 'org-mode-hook #'(lambda () (electric-indent-local-mode -1)))

(setq org-link-frame-setup
      '((vm      . vm-visit-folder-other-frame)
        (vm-imap . vm-visit-imap-folder-other-frame)
        (gnus    . org-gnus-no-new-news)
        (file    . find-file)
        (wl      . wl-other-frame)))

(setq org-return-follows-link t)

(setq org-blank-before-new-entry '((heading . nil)

                                   (plain-list-item . nil)))


(setq org-export-with-sub-superscripts '{})

(setq org-export-with-tags nil)  ;; or t to include
