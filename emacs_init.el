;; Take up all screen space on macOS when starting
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ns-use-native-fullscreen nil)

;, Enable Native Smooth Scrolling (Emacs 29)
(pixel-scroll-precision-mode 1)

;; Turn off some unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(setq inhibit-startup-message t) ; Don't show the splash screen

;; start with an empty scratch buffer in Org mode.
(setq initial-major-mode 'org-mode)

;; Wraps the lines visually for display purposes (do not split words...)
(global-visual-line-mode t)

;; Blink cursor
(blink-cursor-mode 1)

;; Add the colum position in the mod line
(column-number-mode)

;; Make sure it is all utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Enable line numbering in prog mode
;;(global-display-line-numbers-mode 1) to display lines everywhere
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Set the  margins width (too small by default) in org and markdown modes
(defun my/set-margins-for-org-and-markdown ()
  (when (or (derived-mode-p 'org-mode)
            (derived-mode-p 'markdown-mode))
     (setq-local left-margin-width 3)
    (setq-local right-margin-width 3)
    (set-window-buffer nil (current-buffer))))
(add-hook 'find-file-hook 'my/set-margins-for-org-and-markdown)

;; Remember recently edited files
(recentf-mode 1)
(setq recentf-max-items 50)

;; Remember minibuffer prompt history
(setq history-length 25) ; store max 25 history entries
(setq savehist-autosave-interval 600) ; save writes on SSD
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Automatic insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

;; Confirm before quitting Emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;; Changing the location of the custom file to keep the config clean
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't pop UI dialogs when prompting
(setq use-dialog-box nil)

;; move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; make searches case insensitive
(setq case-fold-search t)

;; delete trailing whitespace when file saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Set TRAMP default connection mode to SSH
(setq tramp-default-method "ssh")

;; Change default working directory
(setq default-directory "/Users/alexandre/Documents")

;; ========== Place Backup Files in Specific Directory ==========
;; Enable backup files.
(setq make-backup-files t)
(setq backup-by-copying t)
;; Enable versioning
(setq delete-old-versions t
  kept-new-versions 2
  kept-old-versions 2
  version-control t)
;; Save all backup file in this directory.
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups"))))

;; ========== Use-Package ==========;
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ========== Which-Key ==========;
(use-package which-key
  :ensure t)
(which-key-mode)

;; ========== rg.el (ripgrep) ==========;
(use-package rg
  :ensure t)
(rg-enable-default-bindings)

;; ========== Helm ==========;
(use-package helm
  :ensure t)

;; ========== Company ==========;
(use-package company
  :ensure t
  :config)
(add-hook 'after-init-hook 'global-company-mode) ; use company-mode in all buffers

;; ========== Markdown mode ===========
(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-header-scaling t)
  (setq markdown-fontify-code-blocks-natively t)
  (setq org-src-fontify-natively t)
  (setq markdown-header-scaling t)
  (setq markdown-enable-wiki-links t)
)

;; ========== Org mode ==========
(use-package org
  :ensure t
  :defer t ; defers the loading of Org mode until it's actually needed.
  :config
  (setq variable-pitch-mode 1) ; changes the font to a variable-width (proportional) font,
  (setq auto-fill-mode 0) ; disables the auto-fill mode, which automatically wraps text at the fill-column
  (setq visual-line-mode 1) ;  displays long lines "virtually" wrapped, making them easier to read without actually changing the underlying text.
  (setq
      org-startup-indented t ; Org mode will use an indented display for headings and list items
      org-enforce-todo-dependencies nil ; Org mode will not enforce TODO dependencies
      org-pretty-entities t ; Org mode will display special Unicode characters for common entities like arrows and fractions instead of using plain text representations
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
      )
  (add-to-list 'org-modules 'org-habit)
  )

(use-package org-superstar
  :ensure t)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package org-modern
  :ensure t)
(with-eval-after-load 'org (global-org-modern-mode))

;; ========== Theming ==========;
;; Increase font size (too small for me by default)
(set-face-attribute 'default nil :font "Inter-18")

;; Customize Org Mode heading sizes in Leuven theme
(load-theme 'leuven)
(with-eval-after-load 'org-faces
  (dolist (face '((org-document-title . 1.5)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)))
    (set-face-attribute (car face) nil :height (cdr face))))
