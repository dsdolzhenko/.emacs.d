;;; dd-org-gtd-test.el --- Tests for dd-org-gtd.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with:
;;   emacs -Q --batch -L . -l ert -l dd-org-gtd.el -l dd-org-gtd-test.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'dd-org-gtd)

(defconst dd/org-gtd-test--fixture "\
* Projects
** [[https://example.com/ABC-1][ABC-1]]: Demo project
*** Tasks
**** TODO existing task
** Archive :ARCHIVE:
*** Old project
* Areas
** Demo area
*** Notes
- existing area note
* People
** Demo person
*** 1on1s
**** [2024-01-01 Mon] existing 1on1
")

(defmacro dd/org-gtd-test--with-fixture (&rest body)
  "Run BODY with `dd/org-gtd-file' bound to a temp file holding the fixture."
  (declare (indent 0))
  `(let* ((dd/org-gtd-file (make-temp-file "dd-org-gtd-test" nil ".org")))
     (unwind-protect
         (progn
           (with-temp-file dd/org-gtd-file
             (insert dd/org-gtd-test--fixture))
           ,@body)
       (let ((buf (find-buffer-visiting dd/org-gtd-file)))
         (when buf (kill-buffer buf)))
       (delete-file dd/org-gtd-file))))

(ert-deftest dd-org-gtd-test/targets-for-tasks ()
  (dd/org-gtd-test--with-fixture
    (let ((candidates (dd/org-gtd--non-archived-level2-targets
                        '(("Projects" . "Tasks") ("Areas" . "Tasks")))))
      (should (equal (mapcar #'car candidates)
                     '("ABC-1: Demo project" "Demo area")))
      (should (equal (nth 2 (cdr (assoc "ABC-1: Demo project" candidates))) "Projects"))
      (should (equal (nth 2 (cdr (assoc "Demo area" candidates))) "Areas")))))

(ert-deftest dd-org-gtd-test/targets-for-meetings-include-people ()
  (dd/org-gtd-test--with-fixture
    (let ((candidates (dd/org-gtd--non-archived-level2-targets
                        '(("Projects" . "Meetings")
                          ("Areas" . "Meetings")
                          ("People" . "1on1s")))))
      (should (equal (mapcar #'car candidates)
                     '("ABC-1: Demo project" "Demo area" "Demo person")))
      (should (equal (nth 2 (cdr (assoc "Demo person" candidates))) "People")))))

(ert-deftest dd-org-gtd-test/group-completion-table-groups-by-section ()
  (dd/org-gtd-test--with-fixture
    (let* ((candidates (dd/org-gtd--non-archived-level2-targets
                         '(("Projects" . "Meetings")
                           ("Areas" . "Meetings")
                           ("People" . "1on1s"))))
           (table (dd/org-gtd--group-completion-table candidates))
           (group-fn (alist-get 'group-function (cdr (funcall table "" nil 'metadata)))))
      (should (equal (funcall group-fn "ABC-1: Demo project" nil) "Projects"))
      (should (equal (funcall group-fn "Demo area" nil) "Areas"))
      (should (equal (funcall group-fn "Demo person" nil) "People"))
      ;; TRANSFORM non-nil must return the candidate unchanged.
      (should (equal (funcall group-fn "Demo area" t) "Demo area")))))

(ert-deftest dd-org-gtd-test/find-child-heading ()
  (dd/org-gtd-test--with-fixture
    (with-current-buffer (find-file-noselect dd/org-gtd-file)
      (goto-char (point-min))
      (search-forward "Demo project")
      (org-back-to-heading t)
      (should (dd/org-gtd--find-child-heading "Tasks"))
      (should-not (dd/org-gtd--find-child-heading "Meetings")))))

(ert-deftest dd-org-gtd-test/goto-or-create-child-reuses-existing ()
  (dd/org-gtd-test--with-fixture
    (with-current-buffer (find-file-noselect dd/org-gtd-file)
      (goto-char (point-min))
      (search-forward "Demo project")
      (org-back-to-heading t)
      (dd/org-gtd--goto-or-create-child "Tasks")
      (should (equal (org-get-heading t t t t) "Tasks"))
      (should (= (org-outline-level) 3))
      ;; Still exactly one "Tasks" heading under the project.
      (goto-char (point-min))
      (should (= 1 (how-many "^\\*\\*\\* Tasks$"))))))

(ert-deftest dd-org-gtd-test/goto-or-create-child-creates-missing ()
  (dd/org-gtd-test--with-fixture
    (with-current-buffer (find-file-noselect dd/org-gtd-file)
      (goto-char (point-min))
      (search-forward "Demo project")
      (org-back-to-heading t)
      (dd/org-gtd--goto-or-create-child "Meetings")
      (should (equal (org-get-heading t t t t) "Meetings"))
      (should (= (org-outline-level) 3)))))

(ert-deftest dd-org-gtd-test/capture-task-target-lands-on-tasks ()
  (dd/org-gtd-test--with-fixture
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Demo area")))
      (dd/org-capture-task-target)
      (should (equal (org-get-heading t t t t) "Tasks"))
      (save-excursion
        (org-up-heading-safe)
        (should (equal (org-get-heading t t t t) "Demo area"))))))

(ert-deftest dd-org-gtd-test/capture-meeting-target-lands-on-meetings-for-person ()
  (dd/org-gtd-test--with-fixture
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Demo person")))
      (dd/org-capture-meeting-target)
      (should (equal (org-get-heading t t t t) "Meetings"))
      (save-excursion
        (org-up-heading-safe)
        (should (equal (org-get-heading t t t t) "Demo person"))))))

(ert-deftest dd-org-gtd-test/capture-note-target-reuses-existing-notes ()
  (dd/org-gtd-test--with-fixture
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Demo area")))
      (dd/org-capture-note-target)
      (should (equal (org-get-heading t t t t) "Notes"))
      (save-excursion
        (org-up-heading-safe)
        (should (equal (org-get-heading t t t t) "Demo area")))
      ;; Still exactly one "Notes" heading under the area.
      (goto-char (point-min))
      (should (= 1 (how-many "^\\*\\*\\* Notes$"))))))

(ert-deftest dd-org-gtd-test/capture-note-target-creates-notes-for-person ()
  (dd/org-gtd-test--with-fixture
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Demo person")))
      (dd/org-capture-note-target)
      (should (equal (org-get-heading t t t t) "Notes"))
      (save-excursion
        (org-up-heading-safe)
        (should (equal (org-get-heading t t t t) "Demo person"))))))

(provide 'dd-org-gtd-test)
;;; dd-org-gtd-test.el ends here
