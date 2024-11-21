;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company           ; the ultimate code completion backend
       ;;(corfu +orderless)  ; complete with cap(f), cape and a flying feather!
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;;ivy               ; a search engine for love and life
       vertico           ; the search engine of the future
       emmet

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       ;;(evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotatetext       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;(spell +flyspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;;collab            ; buffers with friends
       (debugger +lsp)          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)'
       eww
       lookup              ; navigate your code and its documentation
       lsp               ; M-x vscode
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf               ; pdf enhancements
       org-pdftools
       saveplace-pdf-view
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       tree-sitter       ; syntax and parsing, sitting in a tree...
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if (featurep :system 'macos) macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount         ; mind the GAAP
       (cc +lsp)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;dhall
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fortran           ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       ;;(haskell +lsp)    ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       json              ; At least it ain't XML
       (java +lsp)       ; the poster child for carpal tunnel syndrome
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       latex             ; writing papers in Emacs has never been so fun
       ;;lean              ; for folks with too much to prove
       ;;ledger            ; be audit you can be
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       org               ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       (python +lsp +pyright)     ; LSP mode for python
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;(scheme +guile)   ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web +html +css)               ; the tubes
       ;;yaml              ; JSON, but readable
       ;;zig               ; C, but simpler

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;emms
       ;;everywhere        ; *leave* Emacs!? You must be joking
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))

;;For font
(set-face-attribute 'default nil :height 140)

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
(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

;; For verilog
(add-hook 'verilog-mode-hook #'lsp-deferred)


;; For Emmet
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; For Web Mode
(after! web-mode
  (add-hook 'web-mode-hook 'emmet-mode)
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

;; For HTML Rendering
(defvar my-live-server-process nil
  "Process for the live-server, if running.")

(defun my-live-server-toggle ()
  "Toggle live-server for the current HTML file."
  (interactive)
  (if (and my-live-server-process (process-live-p my-live-server-process))
      (progn
        (kill-process my-live-server-process)
        (setq my-live-server-process nil)
        (message "Live-server stopped"))
    (let ((html-file (buffer-file-name)))
      (setq my-live-server-process
            (start-process "live-server" "*live-server*" "live-server" (file-name-directory html-file)))
      (message "Live-server started for %s" (file-name-directory html-file)))))

;; Add a key binding for HTML mode
(defun my-html-mode-keybindings ()
  "Custom keybinding for toggling live-server in HTML mode."
  (local-set-key (kbd "C-c C-o") 'my-live-server-toggle))

;; Add the custom keybinding hook to HTML mode
(add-hook 'web-mode-hook 'my-html-mode-keybindings)


;;For asm-mode
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

(after! lsp-mode
  ;; Associate nasm-mode with asm-lsp
  (add-to-list 'lsp-language-id-configuration '(nasm-mode . "nasm"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "asm-lsp")
    :major-modes '(nasm-mode)
    :server-id 'asm-lsp)))

(add-hook 'nasm-mode-hook #'lsp!)  ;; Enable LSP in nasm-mode


(after! lsp-mode
  ;; Associate nasm-mode with asm-lsp
  (add-to-list 'lsp-language-id-configuration '(asm-mode . "asm"))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "asm-lsp")
    :major-modes '(asm-mode)
    :server-id 'asm-lsp)))

(add-hook 'asm-mode-hook #'lsp!)  ;; Enable LSP in nasm-mode

(defun my-nasm-indent-on-enter ()

  (interactive)
  (nasm-indent-line)           ;; Indent the current line using nasm-indent-line
  ;; Remove trailing whitespace only if the line is empty
  (unless (string-blank-p (thing-at-point 'line t))  ;; Check if the new line is empty
    (delete-trailing-whitespace))

  (newline)                    ;; Insert a new line
  (let ((previous-line-indent (save-excursion
                                (forward-line -1)        ;; Move to the previous line
                                (current-indentation)))  ;; Get the indentation of the previous line
        (previous-line-text (save-excursion
                             (forward-line -1)
                             (thing-at-point 'line t)))) ;; Get the previous line's content
    (if (or (string-match-p "^[[:alnum:]_]+:$" previous-line-text)  ;; Check if the previous line has a label with a colon
            (string-match-p ".^[[:alnum:]_]+\\.$" previous-line-text))  ;; Check if the previous line has a label without a colon (e.g., .loop)
        (indent-line-to (* 1 tab-width))  ;; If the previous line is a label, indent with 2 tab widths
      (indent-line-to previous-line-indent))))  ;; Otherwise, indent normally
  


(defun setup-nasm-mode-hooks ()
  "Set up custom key bindings and hooks for NASM mode."
  (local-set-key (kbd "RET") #'my-nasm-indent-on-enter))

(add-hook 'nasm-mode-hook #'setup-nasm-mode-hooks)
