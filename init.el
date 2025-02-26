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

;;; Networking

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;;; package.el

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

;;; System

;; Persist session state (opened files, buffers, etc) between Emacs restarts
(desktop-save-mode t)

;;; Environment

;; Get environment variables from shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; Input

;; Use Command key on MacOS to Meta key.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Translate input sequences into English.
;;
;; I have two Input Sources setup on my computer.
;; I don't want to use the input method switching
;; mechanism built in into Emacs. Instead, I want
;; key bindings to work no matter which Input Source
;; is currently active.
;;
(use-package reverse-im
  :ensure t
  :custom (reverse-im-input-methods '("russian-computer"))
  :config (reverse-im-mode t))

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

;;; Editor

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

(use-package electric
  :custom
  (electric-quote-replace-double t)
  (electric-quote-string nil)
  :hook (text-mode . electric-quote-mode))

;; Replace dabbrev-expand with hippie-expand
;; https://www.masteringemacs.org/article/text-expansion-hippie-expand
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package editorconfig
  :ensure t
  :defer t
  :hook (prog-mode . editorconfig-mode))

(use-package avy
  :ensure t
  :bind (("C-:" . avy-goto-char-2)))

;;; Files

;; Disable lock files
(setq create-lockfiles nil)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/")))
(setq backup-by-copying t)   ;; Copy all files, don't rename them.
(setq delete-old-versions t) ;; Don't ask to delete excess backup versions.
(setq kept-new-versions 6)   ;; Number of newest versions to keep.
(setq kept-old-versions 2)   ;; Number of oldest versions to keep.
(setq version-control t)     ;; Use version numbers for backups.

;;; Frames

;; Resizing the Emacs frame can be costly when changing the font. Disable this
;; to improve startup times with fonts larger than the system default.
(setq frame-resize-pixelwise t)

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

;;; Buffers

;; Shortcut to kill the current buffer.
;;
;; Unlike the default, this one kills the buffer
;; *without confirmation* if it is not modified.
(defun dd/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'dd/kill-this-buffer)

;;; Minibuffer

;; Always use short variant of yes-or-no dialog
(defalias 'yes-or-no-p 'y-or-n-p)

;; As recommended by https://github.com/oantolin/embark
(setq y-or-n-p-use-read-key t)

;; Save minibuffer history
(savehist-mode)

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
     ("M-s d" . consult-find)
     ("M-s r" . consult-ripgrep))

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

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

;;; Help

;; Display the key bindings following the currently entered
;; incomplete command (a prefix) in a popup
(use-package which-key
  :ensure t
  :config (which-key-mode))

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

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;; Crux

(use-package crux
  :ensure t
  :bind (("C-c r" . crux-rename-file-and-buffer)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c d" . crux-duplicate-current-line-or-region)))

;;; Roam

(defun dd/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun dd/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (dd/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun dd/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (dd/org-roam-list-notes-by-tag "agenda")))

(defun dd/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* %?\n/Entered on/ %U"
                                   :if-new (file+head "inbox.org" "#+TITLE: Inbox\n")))))

(defun dd/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'dd/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun dd/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'dd/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (dd/org-roam-filter-by-tag "project")
   nil
   :templates
   '(("p" "project" plain (file "~/Documents/roam/templates/project.org")
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "")
      :unnarrowed t))))

(defun dd/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'dd/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (dd/org-roam-filter-by-tag "project"))
                     :templates `(("p" "project" plain ,(concat "** TODO %?\n"
                                                                "/Entered on/ %U")
                                   :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+TITLE: ${title}\n#+CATEGORY: ${title}\n#+FILETAGS: :project:\n"
                                                          ("Tasks"))))))

(defun dd/org-roam-update-filename-on-title-change ()
  "Update filename when the title of an org-roam buffer changes."
  (let* ((old-filename (buffer-file-name))
         ;; Check if the node is daily note
         (is-not-daily (not (org-roam-dailies--daily-note-p)))
         ;; Check if filename matches the pattern of regular org-roam nodes
         (is-matching-file (string-match "^[0-9]\\{14\\}-.*\\.org$"
                                         (file-name-nondirectory old-filename))))
    ;; Update org-roam cache
    (org-roam-db-update-file old-filename)
    (when (and is-not-daily is-matching-file)
      (let* ((node (org-roam-node-at-point))
             (slug (org-roam-node-slug node))
             ;; Extract the timestamp from the current filename
             (timestamp (when (string-match "\\([0-9]\\{14\\}\\)" old-filename)
                          (match-string 1 old-filename)))
             (new-filename (expand-file-name
                            (concat (file-name-directory old-filename)
                                    timestamp "-" slug ".org"))))
        (unless (string-equal old-filename new-filename)
          (rename-file old-filename new-filename)
          (set-visited-file-name new-filename)
          (save-buffer)
          (dd/org-roam-refresh-agenda-list))))))


(use-package org-roam
  :ensure t
  :defer t
  :custom
  (org-roam-directory "~/Documents/roam")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-dailies-directory "journal/")
  (org-roam-dailies-capture-templates
   '(("d" "Default" entry "* %?\n/Entered on/ %U" :target
      (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+FILETAGS: :journal:\n\n"))))
  (org-roam-capture-templates
   '(("d" "Default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n\n")
      :unnarrowed t)
     ("t" "Topic" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :topic:\n\n"))
     ("c" "Person" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :person:\n\n"))
     ("d" "Team" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: :team:\n\n"))))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n o" . org-roam-node-open)
         ("C-c n p" . dd/org-roam-find-project)
         ("C-c n c" . dd/org-roam-capture-inbox)
         ("C-c n t" . dd/org-roam-capture-task)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :hook
  (org-roam-find-file-hook . (lambda () (add-hook 'after-save-hook #'dd/org-roam-update-filename-on-title-change nil t)))
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-enable)
  (dd/org-roam-refresh-agenda-list))

(use-package ht
  :ensure t)

(use-package org-roam-dblocks
  :after org
  :load-path "contrib/chrisbarrett-nursery/lisp/"
  :hook (org-mode . org-roam-dblocks-autoupdate-mode))

(use-package org-drill
  :ensure t
  :defer t
  :after org)

(use-package ts
  :ensure t
  :defer t)

(use-package org-roam-review
  :after org
  :load-path "contrib/chrisbarrett-nursery/lisp/"
  :defer t
  :hook (org-roam-capture-new-node . org-roam-review-set-seedling)
  :commands (org-roam-review
             org-roam-review-list-by-maturity
             org-roam-review-list-recently-added
             org-roam-review-set-seedling
             org-roam-review-set-evergreen))

(use-package consult-org-roam
  :ensure t
  :after org-roam
  :init
  (require 'consult-org-roam)
  ;; Activate the minor mode
  (consult-org-roam-mode 1)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Eventually suppress previewing for certain functions
  (consult-customize
   consult-org-roam-forward-links
   :preview-key "M-.")
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c n e" . consult-org-roam-file-find)
  ("C-c n b" . consult-org-roam-backlinks)
  ("C-c n B" . consult-org-roam-backlinks-recursive)
  ("C-c n l" . consult-org-roam-forward-links)
  ("C-c n r" . consult-org-roam-search))

;;; Denote

;; Setup ~denote~ for note taking
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :config
  (require 'denote-journal-extras)
  (setq denote-journal-extras-title-format 'day-date-month-year))

(use-package consult-denote
  :ensure t)

(use-package denote-menu
  :ensure t
  :defer t
  :bind (("C-c z" . denote-menu-list-notes)
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

  ;; Agenda
  (org-agenda-prefix-format '((agenda . " %i %-12t%-35c")
                              (todo   . " %i %-12:c %-4e")
                              (tags   . " %i %-12:c")
                              (search . " %i %-12:c")))
  (org-agenda-window-setup 'current-window)

  (org-agenda-custom-commands
   '(("g" "Get Things Done (GTD)"
      ((agenda ""
               ((org-agenda-span 1)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'deadline))
                (org-deadline-warning-days 0)))
       (tags-todo "inbox"
                  ((org-agenda-prefix-format "  %?-35t% s")
                   (org-agenda-overriding-header "\nInbox\n")))
       (todo "NEXT"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format "  %i %-35:c [%e] ")
              (org-agenda-overriding-header "\nNext tasks\n")))
       (todo "HOLD"
             ((org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline))
              (org-agenda-prefix-format "  %i %-35:c [%e] ")
              (org-agenda-overriding-header "\nTasks on hold\n")))
       (agenda nil
               ((org-agenda-entry-types '(:deadline))
                ;; (org-agenda-format-date "")
                (org-agenda-prefix-format "  %i %-35:c [%e] ")
                (org-deadline-warning-days 7)
                (org-agenda-skip-function
                 '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                (org-agenda-overriding-header "\nDeadlines")))
       (tags "CLOSED>=\"<today>\""
             ((org-agenda-prefix-format "  %i %-35:c [%e] ")
              (org-agenda-overriding-header "\nCompleted today\n")))
       (agenda "CLOSED>=\"<-1w>\""
             ((org-agenda-entry-types '(:closed))
              (org-agenda-overriding-header "\nCompleted this week\n")))))))

  ;; Hide blocks and drawers by default
  (org-hide-block-startup t)
  (org-hide-drawer-startup t)

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

  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "DELEGATED(f)" "CANCELED(c)")))

  (org-startup-folded 'content)

  :bind
  ("C-c a" . org-agenda)
  ("C-x n t" . org-toggle-narrow-to-subtree)

  :hook
  (org-mode . org-indent-mode)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (mermaid . t))))

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

;; Capture links to resources in other apps, such as Mail, Firefox, etc.
(use-package org-mac-link
  :load-path "contrib/"
  :defer t
  :after org
  :bind ("C-c g" . org-mac-link-get-link))

;; Handy utility to capture attachments from clipboard
(use-package org-download
  :ensure t
  :defer t
  :after org
  :custom (org-download-method 'attach))


;;; Mail

(setq user-full-name "Dmitry Dolzhenko"
      user-mail-address "mailbox@dolzhenko.me")

;;; Text

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

(use-package gptel
  :ensure t
  :defer t
  :preface
  (defun dd/gptel-api-key (host)
    (let ((auth (car (auth-source-search :host host))))
      (when auth
        (plist-get auth :secret))))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-expert-commands t)
  :bind
  (("C-c b s" . gptel-send)
   ("C-c b m" . gptel-menu)
   ("C-c b r" . gptel-rewrite-menu)
   ("C-c b a" . gptel-context-add))
  :config
  (gptel-make-anthropic "Claude"
    :stream t
    :key (dd/gptel-api-key "api.anthropic.com")))

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
  :defer t)

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

;;; Programming

(use-package gtags-mode
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t
  :custom (markdown-command "pandoc"))

(use-package sh-script
  :ensure t
  :defer t
  :hook (sh-mode . flymake-mode))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.njk\\'" . web-mode)
         ("\\.liquid\\'" . web-mode)))

(use-package go-mode
  :ensure t
  :defer t)

(use-package rust-mode
  :ensure t
  :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package jinja2-mode
  :ensure t
  :defer t)

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

;;; Snippets

(use-package yasnippet
  :ensure t
  :defer t
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer t
  :after yasnippet)


;;; init.el ends here
