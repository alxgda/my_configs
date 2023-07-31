;; Take up all screen space on macOS when starting
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq ns-use-native-fullscreen nil)

;; Turn off some unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Don't show the splash screen
(setq inhibit-startup-message t)

;; Increase font size (too small for me by default)
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 155)

;; Wraps the lines visually for display purposes (do not split words...)
(global-visual-line-mode t)

;; Don't blink cursor
(blink-cursor-mode 0)

;; Set up the visible bell
(setq visible-bell t)

;; Add the colum position in the mod line
(column-number-mode)

;; Indent
(setq-default tab-width 2
      indent-tabs-mode  t
      tab-always-indent t)

;; mac switch meta key
(defun mac-switch-meta nil
  "switch meta between Option and Command"
  (interactive)
  (if (eq mac-option-modifier nil)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	)
    (progn
      (setq mac-option-modifier nil)
      (setq mac-command-modifier 'meta)
      )
    )
  )

;; Make sure it is all utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Enable line numbering in prog mode
;;(global-display-line-numbers-mode 1) to display lines everywhere
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Set the  margins width (too small by default)
(setq-default left-margin-width 1 right-margin-width 3)
(set-window-buffer nil (current-buffer))

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

;; Setup package archives
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; ========== Use-Package ==========;
(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ========== Which-Key ==========;
;; Install and configure which-key package
(use-package which-key
  :ensure t)
(which-key-mode)

;; ========== Markdown mode ==========;
(use-package markdown-mode
  :ensure t
  :defer t
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-hide-markup t)
  (setq markdown-header-scaling t)
  (setq markdown-enable-wiki-links t))

;; ========== Org mode ==========
(use-package org
  :straight (:type built-in) ; tells use-package that Org mode is a built-in package and does not need to be installed separately.
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
      org-agenda-block-separator "" ; Org mode will not display separators between different agenda blocks in the agenda view.
      org-fontify-whole-heading-line t ; Org mode will apply fontification (e.g., colors, styles) to the entire heading line
      org-fontify-done-headline t ; Org mode will fontify DONE headlines differently to visually distinguish them.
      org-fontify-quote-and-verse-blocks t ; Org mode will fontify quote and verse blocks with a different face to distinguish them from regular text.
      org-startup-with-inline-images t ; Org mode will automatically display inline images (images that are directly included in the text) when opening an Org file
      org-image-actual-width '(600) ; sets the actual width of inline images to 600 pixels.
      org-return-follows-link t ; pressing the RET (Enter) key on a link will follow the link instead of inserting a new line
      )


;; ========== Theme ==========;
(use-package ef-themes
  :ensure t)
(require 'ef-themes)
(load-theme 'ef-light :no-confirm)
