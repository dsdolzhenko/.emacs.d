# lisp/

Private, local Elisp modules for this Emacs config — not packaged, not
published, no `Package-Requires` headers. Loaded via a `load-path` entry
added in `init.el`.

## dd-org-gtd.el

Helpers for the GTD file at `dd/org-gtd-file` (default
`~/Documents/org/gtd.org`), which is organized as `* Projects` / `* Areas` /
`* People` with each entry a level-2 heading.

Currently provides the org-capture `:function` targets wired up in
`init.el`'s `org-capture-templates`:

- `dd/org-capture-task-target` — prompt for a non-archived Project or Area,
  file the new entry under its `Tasks` subheading (created if missing).
- `dd/org-capture-meeting-target` — prompt for a non-archived Project, Area,
  or Person, file the new entry under its `Meetings` subheading (created if
  missing).
- `dd/org-capture-note-target` — prompt for a non-archived Project, Area, or
  Person, file the new entry under its `Notes` subheading (created if
  missing).

Candidates in the prompt are grouped by section (Projects/Areas/People) via
the standard `group-function` completion metadata, which completion UIs that
support it (e.g. Vertico) render as titled groups — no extra dependency;
UIs without support just show a flat list.

More GTD-related helpers may move into this module over time.

### Loading

```elisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'dd-org-gtd)
```

### Tests

`dd-org-gtd-test.el` has basic ERT coverage against a throwaway fixture file
(never touches the real `gtd.org`). Run from this directory:

```sh
emacs -Q --batch -L . -l ert -l dd-org-gtd.el -l dd-org-gtd-test.el \
  -f ert-run-tests-batch-and-exit
```
