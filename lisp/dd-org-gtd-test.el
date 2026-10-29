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
    (let ((displays (mapcar #'car
                             (dd/org-gtd--non-archived-level2-targets
                              '(("Projects" . "Tasks") ("Areas" . "Tasks"))))))
      (should (equal displays '("ABC-1: Demo project" "Demo area"))))))

(ert-deftest dd-org-gtd-test/targets-for-meetings-include-people ()
  (dd/org-gtd-test--with-fixture
    (let ((displays (mapcar #'car
                             (dd/org-gtd--non-archived-level2-targets
                              '(("Projects" . "Meetings")
                                ("Areas" . "Meetings")
                                ("People" . "1on1s"))))))
      (should (equal displays '("ABC-1: Demo project" "Demo area" "Demo person"))))))

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

(ert-deftest dd-org-gtd-test/capture-meeting-target-lands-on-1on1s-for-person ()
  (dd/org-gtd-test--with-fixture
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) "Demo person")))
      (dd/org-capture-meeting-target)
      (should (equal (org-get-heading t t t t) "1on1s"))
      (save-excursion
        (org-up-heading-safe)
        (should (equal (org-get-heading t t t t) "Demo person"))))))

(provide 'dd-org-gtd-test)
;;; dd-org-gtd-test.el ends here
