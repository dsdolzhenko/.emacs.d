;;; init.el -*- lexical-binding: t; -*-

;;
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Variable-Scoping.html
;; The dynamic binding was (and still is) the default in Emacs for many years,
;; but lately Emacs is moving towards using lexical binding in more and more places,
;; with the goal of eventually making that the default.

;;; Custom options

;; Overwrite location of the custom file and load it.
;; By default, Emacs uses ~init.el~ to store settings from Customization interface.
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Packages

;; Initialize and refresh package contents again if needed
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is available at compile time
(eval-when-compile
  (require 'use-package))

;; Uncomment to collect statistics
;; (setq use-package-compute-statistics t)

;;; Server

;; Run the emacs server
(use-package server
  :if window-system
  :init (add-hook 'after-init-hook 'server-start t))

;;; Desktop

;; Persist session state (opened files, buffers, etc) between Emacs restarts
(desktop-save-mode t)

;;; Environment

;; Get environment variables from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (dolist (var '("JAVA_HOME"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;;;
;;; MacOS
;;;

;; Avoid inadvertently changing font size when scrolling
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;;; Editing

;; Use Command key on MacOS to Meta key.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Newline at the and of a file
(setq require-final-newline t)

;; Use spaces instead of tabs when indenting
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Make kill-visual-line kill the whole line
(defalias 'kill-visual-line 'kill-line)

;; Require final newline
(setq require-final-newline t)

;; Replace selected text by typing
(setq delete-selection-mode t)

;; Replace selected text by typing
(delete-selection-mode t)

;; Enable narrow-to-region command
(put 'narrow-to-region 'disabled nil)

(defun dd/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'dd/keyboard-quit-dwim)

;; Translate input sequences into English.
;;
;; I have two Input Sources setup on my computer.
;; I don't want to use the input method switching
;; mechanism built in into Emacs. Instead, I want
;; key bindings to work no matter which Input Source
;; is currently active.
(use-package reverse-im
  :ensure t
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

;; Enable electric quote mode for text modes
(use-package electric
  :custom
  (electric-quote-replace-double t)
  (electric-quote-string nil)
  :hook (text-mode . electric-quote-mode))

;; Replace dabbrev-expand with hippie-expand
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package editorconfig
  :defer t
  :hook (prog-mode . editorconfig-mode))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char-2)))

(defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :config
;;  (require 'meow)
;;  (meow-setup)
;;  (meow-global-mode nil)
  )

;;; Backups

;; Disable lock files
(setq create-lockfiles nil)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(setq backup-by-copying t)   ;; Copy all files, don't rename them.
(setq delete-old-versions t) ;; Don't ask to delete excess backup versions.
(setq kept-new-versions 6)   ;; Number of newest versions to keep.
(setq kept-old-versions 2)   ;; Number of oldest versions to keep.
(setq version-control t)     ;; Use version numbers for backups.

;;; Buffers

;; Shortcut to kill the current buffer.
;;
;; Unlike the default, this one kills the buffer
;; *without confirmation* if it is not modified.
(defun dd/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'dd/kill-this-buffer)

(defun dd/kill-buffer-file-name ()
  (interactive)
  (kill-new buffer-file-name))

(global-set-key (kbd "C-c w") 'dd/kill-buffer-file-name)

;;; Minibuffer

;; Always use short variant of yes-or-no dialog
(defalias 'yes-or-no-p 'y-or-n-p)

;; As recommended by https://github.com/oantolin/embark
(setq y-or-n-p-use-read-key t)

;; Save minibuffer history
(savehist-mode)

;;; Complition

;; Vertical interactive completion
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; Enable orderless completion style that divides pattern
;; into space-separated components, and matches candidates
;; that match all of the components in any order
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Annotate completion candidates with additional information,
;; such as code documentation and file location
(use-package marginalia
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Allow typing spaces and question marks in the completion minibuffer
(define-key minibuffer-local-completion-map
            " " 'self-insert-command)
(define-key minibuffer-local-must-match-map
            " " 'self-insert-command)
(define-key minibuffer-local-completion-map
            "?" 'self-insert-command)
(define-key minibuffer-local-must-match-map
            "?" 'self-insert-command)

;; Display the key bindings following the currently entered
;; incomplete command (a prefix) in a popup
(use-package which-key
  :custom
  (which-key-max-description-length nil)
  :config (which-key-mode))

;; Consulting completing-read
(use-package consult
  :ensure t
  :bind
  (;; Buffers
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x p b" . consult-project-buffer)
   ;; Bookmarks
   ("C-x r b" . consult-bookmark)

   ;; Yank
   ("M-y" . consult-yank-pop)

   ;; imenu
   ("M-i" . consult-imenu)
   ("M-I" . consult-imenu-multi)
   ("M-o" . consult-outline)
   ("M-g i" . nil) ;; unset default binding for imenu

   ;; M-s bindings in `search-map'
   ("C-c s f" . consult-find)
   ("C-c s r" . consult-ripgrep))

  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :custom
  (wgrep-enable-key "e")
  :bind
  (:map grep-mode-map
        ("e" . #'wgrep-change-to-wgrep-mode)))

;;; Look and feel

;; Set the fringe a little wider to ensure the text isn’t too close to the frame borders
(fringe-mode 10)

;; Highlight current line
(global-hl-line-mode t)

;; Don't blink
(blink-cursor-mode -1)

;; Turn off alarms
(setq ring-bell-function 'ignore)

;; Narrow cursor
(set-default 'cursor-type '(bar . 2))

;; Set /JetBrains Mono/ as a default font face
(set-face-attribute 'default nil :height 160 :family "JetBrains Mono")

;; Use /modus-operandi/ as a default theme
(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-to-toggle '(modus-operandi modus-vivendi))
  :config
  (setq modus-themes-common-palette-overrides
        '((fg-mode-line-active fg-main)
          ;; Make the fringe invisible
          (fringe unspecified)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)))

  (load-theme 'modus-operandi :no-confirm)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Minimalistic layout for the modeline
(use-package mood-line
  :ensure t
  :config
  (mood-line-mode))

;; Highlight delimiters (parentheses, brakets, and braces)
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Show whitespaces in programming modes
(use-package whitespace
  :diminish whitespace-mode
  :hook
  (prog-mode . whitespace-mode)
  :custom
  (whitespace-style '(face spaces space-mark tabs tab-mark)))

;; Display line numbers
(use-package display-line-numbers
  :defer t
  :custom (display-line-numbers-type 'visual)
  :hook (prog-mode . display-line-numbers-mode))

(use-package outshine
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . outshine-mode)))

;;; Windows

(use-package windmove
  :ensure t
  :bind (("C-<" . shrink-window-horizontally)
         ("C->" . enlarge-window-horizontally)
         ("C-^" . enlarge-window))
  :config
  (windmove-default-keybindings))

(use-package transpose-frame
  :ensure t
  :defer t
  :commands (transpose-frame))

(use-package ace-window
  :ensure t
  :custom (aw-dispatch-always t)
  :bind ("C-c w" . ace-window))

;;; Bookmarks

(use-package bookmark
  :custom
  ;; Save bookmarks every time the list of bookmarks changes
  (bookmark-save-flag 1))

;;; Dired

;; Display only file and directory names in dired.
;; Press `(`, to switch it back.
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;; MacOS built-in ls command doesn't support "--dired" option
(when (memq window-system '(mac ns))
  (setq dired-use-ls-dired nil))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always))

(use-package dired-filter
  :ensure t)

(use-package dired-preview
  :ensure t
  :defer t
  :custom (dired-preview-delay 0)
  :commands (dired-preview-mode))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(defun dired-default-directory-on-left ()
  "Display `default-directory' in side window on left, hiding details."
  (interactive)
  (let ((buffer (dired-noselect default-directory)))
    (with-current-buffer buffer (dired-hide-details-mode t))
    (display-buffer-in-side-window
     buffer `((side . left) (slot . 0)
              (window-width . fit-window-to-buffer)
              (preserve-size . (t . nil))))))

;;; Compilation

(use-package compile
  :custom (compilation-scroll-output t))

;;; vterm

(use-package vterm
  :ensure t
  :defer t
  :commands (vterm vterm-other-window)
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-ignore-blink-cursor t)
  (vterm-buffer-name-string "vterm %s") ;; %s will be replaced with current directory
  ;; Commands used by https://github.com/pymander/vfish
  (vterm-eval-cmds '(("find-file" find-file)
                     ("message" message)
                     ("vterm-clear-scrollback" vterm-clear-scrollback)
                     ("dired" dired)
                     ("ediff-files" ediff-files)))
  :bind (("C-c v" . vterm)
         ("C-c V" . vterm-other-window))
  :hook
  ;; A hack to avoid flickering
  ;; https://github.com/akermu/emacs-libvterm/issues/432#issuecomment-894230991
  (vterm-mode . (lambda () (setq-local global-hl-line-mode nil)))
  (vterm-copy-mode . (lambda () (call-interactively 'hl-line-mode))))

;;; Crux

(use-package crux
  :ensure t
  :bind (("C-c r" . crux-rename-file-and-buffer)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)))

;;; Rg

(use-package rg
  :ensure t
  :bind ("C-c s b" . rg)
  :custom
  (rg-command-line-args '("-B 3" "-A 3")))

;;; Projectile

(use-package projectile
  :ensure t
  :defer t
  :custom
  (projectile-switch-project-action 'projectile-commander)
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :hook
  ;; Update projectile status in modeline whenever a new buffer appears.
  ;; Projectile updates its modeline status only on file with find-file-hook,
  ;; which doesn't work with non-file buffers, such as magit-*.
  (window-configuration-change . projectile-update-mode-line))

;;; Denote

(defun dd/denote-rename-on-save-using-front-matter ()
  "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields.

Add this function to the `after-save-hook'."
  (let ((denote-rename-confirmations nil)
        (denote-save-buffers t)) ; to save again post-rename
    (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
      (ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
      (message "Buffer saved; Denote file renamed"))))

(use-package denote
  :ensure t
  :custom
  (denote-file-type 'org)
  (denote-prompts '(title keywords))
  :bind (("C-c n n" . denote-open-or-create)
         ("C-c n i" . denote-link-or-create)
         ("C-c n b" . denote-backlinks)
         ("C-c n s" . denote-silo-dired))
  :hook
  (dired-mode . denote-dired-mode)
  (after-save . dd/denote-rename-on-save-using-front-matter)
  :config
  (denote-rename-buffer-mode)
  (put 'denote-file-type 'safe-local-variable 'symbolp))

(use-package denote-org
  :ensure t
  :custom
  (denote-org-store-link-to-heading 'id))

(use-package denote-silo
  :ensure t
  :commands (denote-silo-create-note
             denote-silo-open-or-create
             denote-silo-select-silo-then-command
             denote-silo-dired
             denote-silo-cd)
  :config
  (setq denote-silo-directories
        (list denote-directory
              "~/Documents/notes/personal/"
              "~/Documents/notes/dutch/"
              "~/Documents/notes/work/"
              "~/Documents/notes/recipes/")))

(use-package denote-journal
  :ensure t
  :preface

  (defun dd/denote-silo-journal-new-or-existing-entry (silo)
    "Select SILO and run `denote-journal-new-or-existing-entry' in it.
SILO is a file path from `denote-silo-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-path-is-silo-p'."
    (interactive (list (denote-silo-directory-prompt)))
    (denote-silo-with-silo silo
      (let* ((denote-directory silo)
             (denote-journal-directory (concat denote-directory "journal")))
        (call-interactively #'denote-journal-new-or-existing-entry))))

  :custom (denote-journal-title-format 'day-date-month-year))

(use-package consult-denote
  :ensure t
  :bind (("C-c n g" . consult-denote-grep)))

(use-package denote-menu
  :ensure t
  :defer t
  :bind (("C-c n z" . denote-menu-list-notes)
         (:map denote-menu-mode-map
               ("c" . denote-menu-clear-filters)
               ("/ r" . denote-menu-filter)
               ("/ k" . denote-menu-filter-by-keyword)
               ("/ o" . denote-menu-filter-out-keyword)
               ("e" . denote-menu-export-to-dired))))

;;; Org

(use-package ob-mermaid
  :ensure t
  :after org
  :custom (ob-mermaid-cli-path "mmdc"))

(use-package org
  :ensure t
  :custom

  (org-directory "~/Documents/org")

  ;; Hide blocks and drawers by default
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)

  (org-use-speed-commands t)

  ;; Align tags
  (org-tags-column 0)

  ;; Open image attachments in the default app
  (org-file-apps '((auto-mode . emacs)
                   (directory . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . default)
                   ("\\.pdf\\'" . default)
                   ("\\.png\\'" . default)
                   ("\\.jpe?g\\'" . default)))

  (org-id-link-to-org-use-id 'use-existing)

  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)

  (org-log-done 'time)
  (org-archive-subtree-save-file-p t)

  (org-startup-folded 'content)

  :bind
  ("C-x n t" . org-toggle-narrow-to-subtree)

  :hook
  (org-mode . org-indent-mode)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (mermaid . t))))

;; org-capture settings
(use-package org
  :ensure t
  :custom
  (org-directory "~/Documents/notes")
  (org-capture-templates '(("t" "Todo" entry (file "~/Documents/notes/inbox.org")
                            "* TODO %?")
                           ("s" "Scratch" entry (file "~/Documents/notes/scratchpad.org")
                            "* %U"
                            :immediate-finish t
                            :after-finalize (lambda ()
                                              (interactive)
                                              (org-capture '(16))
                                              (org-narrow-to-subtree)
                                              (org-end-of-line)
                                              (newline)
                                              (bookmark-set "Scratchpad")
                                              (save-buffer)))))

  :bind
  ("C-c c"   . org-capture))

;; Capture links to resources in other apps, such as Mail, Firefox, etc.
(use-package org-mac-link
  :load-path "contrib/"
  :defer t
  :after org
  :bind ("C-c g" . org-mac-link-get-link))

(use-package org-indent
  :defer t)

(use-package simple
  :defer t)

;; Wrap lines at fill-column in org-mode
(use-package visual-fill-column
  :ensure t
  :defer t
  :custom (fill-column 110)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . visual-fill-column-mode))

;; Handy utility to capture attachments from clipboard
(use-package org-download
  :ensure t
  :defer t
  :after org
  :custom (org-download-method 'attach))

;;; Pandoc

(defun dd/pandoc-convert-text (text from-format to-format)
  "Convert TEXT from FROM-FORMAT to TO-FORMAT using pandoc."
  (with-temp-buffer
    (insert text)
    (let ((exit-code (shell-command-on-region
                      (point-min) (point-max)
                      (format "pandoc -f %s -t %s --wrap=none" from-format to-format)
                      t t)))
      (if (zerop exit-code)
          (buffer-string)
        (error "Pandoc conversion failed with exit code %d" exit-code)))))

(defun dd/org-to-markdown-clipboard (start end)
  "Convert region from Org-mode to Markdown format and copy to clipboard using pandoc."
  (interactive "r")
  (let ((org-text (buffer-substring-no-properties start end)))
    (let ((converted-text (dd/pandoc-convert-text org-text "org" "markdown")))
      (kill-new converted-text)
      (message "Org text converted to Markdown and copied to clipboard"))))

(defun dd/markdown-to-org-region (start end)
  "Convert region from Markdown to Org-mode format using pandoc."
  (interactive "r")
  (let ((markdown-text (buffer-substring-no-properties start end)))
    (let ((converted-text (dd/pandoc-convert-text markdown-text "markdown" "org")))
      (delete-region start end)
      (insert converted-text)
      (message "Markdown text converted to Org format"))))

(defun dd/markdown-to-org-from-kill-ring ()
  "Convert last kill ring item from Markdown to Org-mode format and insert at point using pandoc."
  (interactive)
  (let ((markdown-text (current-kill 0)))
    (unless markdown-text
      (error "Kill ring is empty"))
    (let ((converted-text (dd/pandoc-convert-text markdown-text "markdown" "org")))
      (insert converted-text)
      (message "Markdown text from kill ring converted to Org format and inserted"))))

;;; Agenda & GTD

(use-package org
  :preface
  (defun dd/org-agenda (&optional arg)
    (interactive)
    (let* ((org-agenda-files (denote-directory-files "_agenda"))
           (org-agenda-files (append org-agenda-files '("~/Documents/notes/inbox.org"))))
      (org-agenda arg)))

  :custom
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "DELEGATED(f)" "CANCELED(c)")))

  (org-agenda-prefix-format '((agenda . "%-14:c")
                              (todo   . "%-14:c %-4e")
                              (tags   . "%-14:c")
                              (search . "%-14:c")))
  (org-agenda-window-setup 'current-window)

  (org-agenda-remove-tags t)

  (org-agenda-custom-commands
   '(("g" "Get Things Done (GTD)"
      ((agenda ""
               ((org-agenda-span 1)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'deadline))
                (org-deadline-warning-days 0)))
       (tags-todo "inbox"
                  ((org-agenda-prefix-format "%-14 s")
                   (org-agenda-overriding-header "\nInbox\n")))
       (todo "NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format "%-14:c")
              (org-agenda-overriding-header "\nNext tasks\n")))
       (todo "HOLD"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format "%-14:c")
              (org-agenda-overriding-header "\nTasks on hold\n")))
       (agenda nil
               ((org-agenda-entry-types '(:deadline))
                ;; (org-agenda-format-date "")
                (org-agenda-prefix-format "%-14:c")
                (org-deadline-warning-days 7)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                (org-agenda-overriding-header "\nDeadlines")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-prefix-format "%-14:c")
              (org-agenda-overriding-header "\nCompleted today\n")))
       (agenda "CLOSED>=\"<-1w>\""
               ((org-agenda-entry-types '(:closed))
                (org-agenda-overriding-header "\nCompleted this week\n")))))))

  :bind
  ("C-c a" . dd/org-agenda))

;;; Mail

(setq user-full-name "Dmitry Dolzhenko"
      user-mail-address "mailbox@dolzhenko.me")

;;; Spellchecking

;; Spellchecking in org and text modes
(use-package jinx
  :ensure t
  :defer t
  :custom
  (jinx-languages "en_GB")
  :hook
  (text-mode . jinx-mode)
  (org-mode . jinx-mode)
  :bind (("s-$" . jinx-correct)
         ("C-s-$" . jinx-languages)))

(use-package flymake-languagetool
  :ensure t
  :defer t
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load))
  :init
  (setq flymake-languagetool-server-jar nil)
  (setq flymake-languagetool-language "en-GB")
  (setq flymake-languagetool-url "https://api.languagetoolplus.com")
  (let ((auth (car (auth-source-search :host "api.languagetoolplus.com"))))
    (when auth
      (setq flymake-languagetool-api-username (plist-get auth :user))
      (setq flymake-languagetool-api-key (funcall  (plist-get auth :secret))))))

;;; LLM

(defun dd/gptel-quick-query ()
  "Prompt for user input and send it to LLM in a new gptel session."
  (interactive)
  (let ((user-prompt (read-string "Query: "))
        (session-name (format "*gptel-query-%s*"
                             (format-time-string "%H%M%S"))))
    (with-current-buffer (gptel session-name)
      (insert user-prompt)
      (gptel-send)
      (switch-to-buffer-other-window (current-buffer)))))

(use-package gptel
  :ensure t
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  (gptel-cache t)
  (gptel-directives
   '((rewrite . gptel--rewrite-directive-default)
     (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note.")
     (writing . "You are a large language model and a writing assistant. Respond concisely.")
     (chat . "You are a large language model and a conversation partner. Respond concisely.")))
  :bind
  (("C-c b b". gptel)
   ("C-c b q" . dd/gptel-quick-query)
   ("C-c b s" . gptel-send)
   ("C-c b m" . gptel-menu)
   ("C-c b r" . gptel-rewrite)
   ("C-c b a" . gptel-context-add)
   ("C-c b h" . gptel-org-set-topic))
  :config
  (gptel-make-gemini "Gemini" :key gptel-api-key :stream t)
  (gptel-make-anthropic "Claude" :stream t :key gptel-api-key))


(use-package yaml
  :ensure t)

(use-package templatel
  :ensure t)

(use-package gptel-prompts
  :vc (:url "https://github.com/jwiegley/gptel-prompts.git"
            :rev "dfe7fbcb0a54636843a1d522777c03b06d65840c")
  :ensure t
  :after (gptel)
  :config
  (gptel-prompts-update)
  ;; Ensure prompts are updated if prompt files change
  (gptel-prompts-add-update-watchers))

;;; Git

(use-package magit
  :ensure t
  :defer t
  :preface
  (defun dd/magit-kill-diff-buffer ()
    (when-let ((buffer (magit-get-mode-buffer 'magit-diff-mode)))
      (kill-buffer buffer)))
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration)
  :hook ((git-commit-post-finish . #'dd/magit-kill-diff-buffer)))

(use-package consult-vc-modified-files
  :ensure t
  :bind
  ;; choose any other key bindings you prefer
  (("C-x v /" . consult-vc-modified-files)
   ("C-x v ." . consult-vc-log-select-files)))

;;; LSP

(use-package eglot
  :defer t
  :custom
  ;; Disable logging as some language servers may be so noicy
  ;; that it affects performance
  (eglot-events-buffer-size 0)
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :bind
  ("s-<return>" . eglot-code-actions)
  :config
  ;; Otherwise my eyes are bleeding
  (set-face-attribute 'eglot-mode-line nil
                      :weight 'normal
                      :inherit '()))

;;; Auto-complete

(use-package company
  :ensure t
  :defer t
  :custom (company-idle-delay 0.5)
  :hook ((prog-mode . company-mode)
         (cider-repl-mode . company-mode))
  :bind (:map company-active-map
              ("C-j" . company-complete-selection)
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)))

;;; Snippets

(use-package yasnippet
  :ensure t
  :defer t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)

;;; Lisp

(use-package paredit
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . paredit-mode)
         (clojure-mode . paredit-mode)
         (cider-repl-mode . paredit-mode)
         (lisp-data-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode)))

;;; Clojure

(use-package cider
  :ensure t
  :defer t
  :commands (cider)
  :custom
   ;; Controls whether to pop to the REPL buffer on connect.
  (cider-repl-pop-to-buffer-on-connect nil)
  ;; Don't select the error buffer when it's displayed:
  (cider-auto-select-error-buffer nil))

;;; Assembler

(use-package asm-mode
  :defer t
  :preface
  (defun dd/asm-calculate-indentation (indentation)
    (or
     ;; Flush preprocessor macros to the left margin
     (and (looking-at "#\\w+") 0)
     ;; Flush labels with macro argument expansion
     (and (looking-at "
  \\(\\[\\sw\\\\\\]\\|\\s_\\)+:") 0)
     indentation))

  :config
  (advice-add 'asm-calculate-indentation :filter-return #'dd/asm-calculate-indentation))

;;; C/C++

(use-package gtags-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

;;; Markdown

(use-package markdown-mode
  :ensure t
  :defer t
  :custom (markdown-command "pandoc"))

;;; Bash

(use-package sh-script
  :ensure t
  :defer t
  :hook (sh-mode . flymake-mode))

;;; HTML

(use-package web-mode
  :ensure t
  :custom
  (web-mode-enable-front-matter-block t)
  :mode (("\\.html\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.liquid\\'" . web-mode)))

;;; Go

(use-package go-mode
  :ensure t
  :defer t)

;;; Rust

(use-package rust-mode
  :ensure t
  :defer t)

;;; Python

(use-package pyvenv
  :ensure t)

;;; YAML

(use-package yaml-mode
  :ensure t
  :defer t)

;;; CSV
(use-package csv-mode
  :ensure t
  :defer t)

;;; Jinja

(use-package jinja2-mode
  :ensure t
  :defer t)

;;; Docker

(use-package dockerfile-mode
  :ensure t
  :defer t)

;;; Tree-sitter

(use-package treesit
  :preface
  (defun dd/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.20.0"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go" "v0.20.0"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.20.1" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown" "v0.7.1"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust" "v0.21.2"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml" "v0.5.1"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (js-mode . js-ts-mode)
             (sh-mode . bash-ts-mode)
             (conf-toml-mode . toml-ts-mode)
             (go-mode . go-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode))
  :config
  (dd/setup-install-grammars))

;;; init.el ends here
