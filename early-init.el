;;; early-init.el -*- lexical-binding: t; -*-

;;; Garbage collection

;; Garbage collection significantly affects startup times. This setting delays
;; garbage collection during startup but will be reset later.
(setq gc-cons-threshold most-positive-fixnum)

;; Set garbage collection memory threshold to some large number.
;; Too small value will result to more frequent GC pauses,
;; too large value will result to longer GC pauses.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 50 1024 1024))))

;;; Performance

;; Prefer loading newer compiled files
(setq load-prefer-newer t)

;; Font compacting can be very resource-intensive, especially when rendering
;; icon fonts on Windows. This will increase memory usage.
(setq inhibit-compacting-font-caches t)

(unless (or (daemonp) noninteractive)
  ;; Suppress redisplay and redraw during startup to avoid delays and
  ;; prevent flashing an unstyled Emacs frame.
  (setq-default inhibit-redisplay t) ; Can cause artifacts
  (setq-default inhibit-message t)

  ;; Reset the above variables to prevent Emacs from appearing frozen or
  ;; visually corrupted after startup or if a startup error occurs.
  (defun dd/reset-inhibited-vars-h ()
    (setq-default inhibit-redisplay nil) ; Can cause artifacts
    (setq-default inhibit-message nil)
    (remove-hook 'post-command-hook #'dd/reset-inhibited-vars-h))

  (add-hook 'post-command-hook
            #'dd/reset-inhibited-vars-h -100)

  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (setq mode-line-format nil)))

  (put 'mode-line-format 'initial-value
       (default-toplevel-value 'mode-line-format))
  (setq-default mode-line-format nil)

  (defun dd/startup--load-user-init-file (fn &rest args)
    "Advice for startup--load-user-init-file to reset mode-line-format."
    (unwind-protect
        (progn
          ;; Start up as normal
          (apply fn args))
      ;; If we don't undo inhibit-{message, redisplay} and there's an
      ;; error, we'll see nothing but a blank Emacs frame.
      (setq-default inhibit-message nil)
      (unless (default-toplevel-value 'mode-line-format)
        (setq-default mode-line-format
                      (get 'mode-line-format 'initial-value)))))

  (advice-add 'startup--load-user-init-file :around
              #'dd/startup--load-user-init-file)

  ;; Without this, Emacs will try to resize itself to a specific column size
  (setq frame-inhibit-implied-resize t)

  ;; A second, case-insensitive pass over `auto-mode-alist' is time wasted.
  ;; No second pass of case-insensitive search over auto-mode-alist.
  (setq auto-mode-case-fold nil)

  ;; Reduce *Message* noise at startup. An empty scratch buffer (or the
  ;; dashboard) is more than enough, and faster to display.
  (setq inhibit-startup-screen t
        inhibit-startup-echo-area-message user-login-name)
  (setq initial-buffer-choice nil
        inhibit-startup-buffer-menu t
        inhibit-x-resources t)

  ;; Disable bidirectional text scanning for a modest performance boost.
  (setq-default bidi-display-reordering 'left-to-right
                bidi-paragraph-direction 'left-to-right)

  ;; Give up some bidirectional functionality for slightly faster re-display.
  (setq bidi-inhibit-bpa t)

  ;; Remove "For information about GNU Emacs..." message at startup
  (advice-add #'display-startup-echo-area-message :override #'ignore)

  ;; Suppress the vanilla startup screen completely. We've disabled it with
  ;; `inhibit-startup-screen', but it would still initialize anyway.
  (advice-add #'display-startup-screen :override #'ignore)

  ;; Shave seconds off startup time by starting the scratch buffer in
  ;; `fundamental-mode'
  (setq initial-major-mode 'fundamental-mode
        initial-scratch-message nil)

  ;; Unset command line options irrelevant to the current OS. These options
  ;; are still processed by `command-line-1` but have no effect.
  (unless (eq system-type 'darwin)
    (setq command-line-ns-option-alist nil))
  (unless (memq initial-window-system '(x pgtk))
    (setq command-line-x-option-alist nil)))

;; Some other dubious performance optimisations.
(setq fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      inhibit-compacting-font-caches t)

;;; Native compilation

(if (and (featurep 'native-compile)
         (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    ;; Activate `native-compile'
    (setq ;; Async compilcation of .elc’s
     native-comp-jit-compilation t
     ;; Ask before killing compilation process on exit
     native-comp-async-query-on-exit t
     ;; Compile installed packages
     package-native-compile t
     ;; Suppress compiler warnings and don't inundate users with their popups.
     native-comp-async-report-warnings-errors 'silent))

;;; UI elements

;; Hide frame title and frame icon
(when (memq initial-window-system '(mac ns))
  (setq frame-title-format nil)
  (setq ns-use-proxy-icon nil))

;; Make the title bar transparent on MacOS
(when (memq initial-window-system '(mac ns))
  (add-to-list 'default-frame-alist '(ns-appearance . nil)) ; nil for dark text
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; Disable startup screens and messages
(setq inhibit-splash-screen t)

;; I intentionally avoid calling `menu-bar-mode', `tool-bar-mode', and
;; `scroll-bar-mode' because manipulating frame parameters can trigger or queue
;; a superfluous and potentially expensive frame redraw at startup, depending
;; on the window system. The variables must also be set to `nil' so users don't
;; have to call the functions twice to re-enable them.
(push '(menu-bar-lines . 0) default-frame-alist)
  (unless (memq window-system '(mac ns))
    (setq menu-bar-mode nil))

(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

;; Disable GUIs because they are inconsistent across systems, desktop
;; environments, and themes, and they don't match the look of Emacs.
(setq use-file-dialog nil)
(setq use-dialog-box nil)


;;; Misc

(set-language-environment "UTF-8")

;; Set-language-environment sets default-input-method, which is unwanted.
(setq default-input-method nil)

;;; package.el

(setq package-enable-at-startup nil)
(setq package-quickstart nil)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(customize-set-variable 'package-archive-priorities '(("gnu" . 4)
                                                      ("nongnu" . 3)
                                                      ("melpa" . 2)
                                                      ("melpa-stable" . 1)))
