;;; Mixin: Org-mode starter config

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
					;
; Default directory where Org-mode files are stored
(setq org-directory "/Users/alexandre/Library/Mobile\ Documents/com~apple~CloudDocs/Documents/Cerveau\ Digital/1\ -\ Pilotage")

 ; Specifies exactly the list of files or directories to be searched for agenda entries
(setq org-agenda-files '("inbox.org" "work.org"))

(use-package org
  :ensure t
  :defer t ; defers the loading of Org mode until it's actually needed.
  :config
  (setq variable-pitch-mode 1) ; changes the font to a variable-width (proportional) font,
  (setq auto-fill-mode 0) ; disables the auto-fill mode, which automatically wraps text at the fill-column
  (setq visual-line-mode 1) ;  displays long lines "virtually" wrapped, making them easier to read without actually changing the underlying text.
  (setq
      org-startup-folded t ; At opening, show only top headings, everything collapsed, for easy outlining of a file content
      org-startup-indented t ; Org mode will use an indented display for headings and list items
      org-enforce-todo-dependencies nil ; Org mode will not enforce TODO dependencies
      org-pretty-entities t ; Org mode will display special Unicode characters for common entities like arrows and fractions instead of using plain text representations
      org-blank-before-new-entry nil ; prevent Emacs from inserting blank lines before new entries
      org-use-sub-superscripts "{}"  ; Org mode will use curly braces {} to represent subscripts and superscripts.
      org-hide-emphasis-markers t ; Org mode will hide the markers used for emphasis (bold, italic, etc.) in the buffer and only display the emphasized text
      org-hide-leading-stars t ; Org mode will hide leading stars (asterisks) that indicate heading levels. This gives a cleaner look to the buffer.
      org-fontify-done-headline t ; Org mode will fontify DONE headlines differently to visually distinguish them.
      org-fontify-quote-and-verse-blocks t ; Org mode will fontify quote and verse blocks with a different face to distinguish them from regular text.
      org-startup-with-inline-images t ; Org mode will automatically display inline images (images that are directly included in the text) when opening an Org file
      ;org-image-actual-width '(600) ; sets the actual width of inline images to 600 pixels.
      org-return-follows-link t ; pressing the RET (Enter) key on a link will follow the link instead of inserting a new line
      org-agenda-block-separator "" ; Org mode will not display separators between different agenda blocks in the agenda view.
      org-agenda-skip-deadline-if-done t ; skip displaying tasks in the agenda view that have deadlines and are marked as "DONE"
      org-agenda-skip-scheduled-if-done t ; skip displaying tasks in the agenda view that are scheduled  and are marked as "DONE"
      org-deadline-warning-days 4 ; number of days before the deadline when Org mode should issue a warning for upcoming deadlines.
      org-agenda-todo-ignore-scheduled 'all ; ignore all scheduled tasks when generating the todo list in the agenda view
      org-agenda-todo-ignore-with-date 'all ; ignore all tasks that have both a scheduled date and a deadline when generating the todo list in the agenda view
      org-agenda-tags-todo-honor-ignore-options t
      org-log-into-drawer t ; log state changes (e.g., when a task is marked as DONE) into a drawer named "LOGBOOK" in the task's properties.
      org-log-done t ; add a timestamp and a note in the "LOGBOOK" drawer whenever a task is marked as DONE.
      org-export-with-smart-quotes t ; Make exporting quotes better
      )
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and All Todos"
             ((agenda)
              (todo)))
            ("w" "Work" agenda ""
             ((org-agenda-files '("work.org"))))))
  (add-to-list 'org-modules 'org-habit)
  (customize-set-variable 'org-blank-before-new-entry
                        '((heading . nil)
                          (plain-list-item . nil)))
  (setq org-cycle-separator-lines 1)
 )

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package org-superstar
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org-modern
  :ensure t)
(with-eval-after-load 'org (global-org-modern-mode))
