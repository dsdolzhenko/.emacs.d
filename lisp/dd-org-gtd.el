;;; dd-org-gtd.el --- Helpers for a Projects/Areas/People GTD file -*- lexical-binding: t; -*-

;;; Commentary:

;; Not intended for distribution/publishing — a private, local module
;; extracted out of init.el for readability and testability.  Home for
;; GTD-related helpers in general; currently holds the org-capture
;; target-selection helpers, more may move in here later.
;;
;; Provides `:function' targets for org-capture that prompt (via
;; `completing-read') for a non-archived Project/Area (tasks) or
;; Project/Area/Person (meetings) from `dd/org-gtd-file', then file
;; the new entry under that heading's Tasks/Meetings/1on1s
;; subheading, creating the subheading if it doesn't exist yet.

;;; Code:

(require 'org)

(defvar dd/org-gtd-file "~/Documents/org/gtd.org"
  "Path to the GTD org file used by capture helpers below.")

(defun dd/org-gtd--non-archived-level2-targets (section-child-alist)
  "Collect capturable headings from `dd/org-gtd-file'.

SECTION-CHILD-ALIST maps a top-level heading name (e.g. \"Projects\")
to the name of the child subheading new entries should be filed under
(e.g. \"Tasks\"). Returns an alist of (DISPLAY . (MARKER CHILD-NAME
SECTION)) for every non-archived level-2 heading found under one of
those top-level sections, where DISPLAY has org link markup stripped
and SECTION is the matched top-level heading name."
  (let (candidates)
    (with-current-buffer (find-file-noselect (expand-file-name dd/org-gtd-file))
      (org-with-wide-buffer
       (org-map-entries
        (lambda ()
          (when (= (org-outline-level) 2)
            (let* ((parent (save-excursion
                              (org-up-heading-safe)
                              (org-trim (org-get-heading t t t t))))
                   (child-name (cdr (assoc parent section-child-alist))))
              (when (and child-name
                         (not (member org-archive-tag (org-get-tags))))
                (push (cons (org-link-display-format
                             (org-trim (org-get-heading t t t t)))
                            (list (point-marker) child-name parent))
                      candidates)))))
        nil 'file)))
    (nreverse candidates)))

(defun dd/org-gtd--group-completion-table (candidates)
  "Completion table over CANDIDATES, an alist of DISPLAY . (MARKER
CHILD-NAME SECTION) as returned by
`dd/org-gtd--non-archived-level2-targets'.

Groups candidates by SECTION via the standard `group-function'
completion metadata, which completion UIs such as Vertico use to
render titled groups; UIs that don't support it just show a flat
list, so this never changes matching/selection behavior."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata
          (category . dd-org-gtd-target)
          (group-function
           . ,(lambda (cand transform)
                (if transform
                    cand
                  (nth 2 (cdr (assoc cand candidates)))))))
      (complete-with-action action candidates string pred))))

(defun dd/org-gtd--select-target (section-child-alist prompt)
  "Prompt via `completing-read' for one heading from
SECTION-CHILD-ALIST (see `dd/org-gtd--non-archived-level2-targets'),
grouped by section where the completion UI supports it.
Return (MARKER CHILD-NAME SECTION) for the selection."
  (let* ((candidates (dd/org-gtd--non-archived-level2-targets section-child-alist))
         (choice (completing-read prompt
                                   (dd/org-gtd--group-completion-table candidates)
                                   nil t))
         (target (assoc choice candidates)))
    (unless target
      (user-error "No matching GTD heading selected"))
    (cdr target)))

(defun dd/org-gtd--find-child-heading (name)
  "With point on a parent heading, return the position of its direct
child heading titled NAME, or nil if none exists."
  (let* ((child-level (1+ (org-outline-level)))
         (subtree-end (save-excursion (org-end-of-subtree t t) (point))))
    (save-excursion
      (forward-line 1)
      (catch 'found
        (while (re-search-forward org-heading-regexp subtree-end t)
          (goto-char (match-beginning 0))
          (if (and (= (org-outline-level) child-level)
                   (equal (org-trim (org-get-heading t t t t)) name))
              (throw 'found (point))
            (goto-char (match-end 0))))
        nil))))

(defun dd/org-gtd--goto-or-create-child (name)
  "With point on a parent heading, move point onto its direct child
heading titled NAME, creating it as the last child at the correct
level if it doesn't already exist."
  (org-back-to-heading t)
  (let ((pos (dd/org-gtd--find-child-heading name)))
    (if pos
        (goto-char pos)
      (let* ((child-level (1+ (org-outline-level)))
             (subtree-end (save-excursion (org-end-of-subtree t t) (point))))
        (goto-char subtree-end)
        (unless (bolp) (insert "\n"))
        (let ((heading-start (point)))
          (insert (make-string child-level ?*) " " name "\n")
          (goto-char heading-start))))))

(defun dd/org-capture-task-target ()
  "org-capture `:function' target: prompt for a non-archived Project or
Area and move point to its Tasks subheading (creating it if needed)."
  (pcase-let ((`(,marker ,child ,_section)
               (dd/org-gtd--select-target
                '(("Projects" . "Tasks") ("Areas" . "Tasks"))
                "Capture task to Project/Area: ")))
    (set-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (dd/org-gtd--goto-or-create-child child)))

(defun dd/org-capture-meeting-target ()
  "org-capture `:function' target: prompt for a non-archived Project,
Area, or Person and move point to its Meetings/1on1s subheading
(creating it if needed)."
  (pcase-let ((`(,marker ,child ,_section)
               (dd/org-gtd--select-target
                '(("Projects" . "Meetings")
                  ("Areas" . "Meetings")
                  ("People" . "Meetings"))
                "Capture meeting for: ")))
    (set-buffer (marker-buffer marker))
    (widen)
    (goto-char marker)
    (dd/org-gtd--goto-or-create-child child)))

(provide 'dd-org-gtd)
;;; dd-org-gtd.el ends here
