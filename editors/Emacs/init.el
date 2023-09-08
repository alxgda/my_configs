
;; Inspired by:
;; - https://github.com/ashton314/emacs-bedrock


;; Contents:
;;
;;  - Frame enhancements
;;  - Interface enhancements
;; - Files management settings
;;  - Packages
;;  - Mixins
;;  - Theme

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Frame enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Take up all screen space on macOS when starting
(setq frame-resize-pixelwise t)
(setq default-frame-alist '((fullscreen . maximized)
                            (ns-transparent-titlebar . t))) ; use a transparent titlebar, makes an integrated and modern UI

;; Silence useless startup messages and screens
(setq inhibit-startup-echo-area-message (user-login-name)) ; Don't show the welcome message
(setq inhibit-startup-message t) ; Don't show the splash screen
(setq display-time-default-load-average nil) ;  Don't show the load time

;; Turn off some unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;, Enable Native Smooth Scrolling (Emacs 29)
(pixel-scroll-precision-mode 1)

;; Start with an empty scratch buffer in Org mode.
(setq initial-major-mode 'org-mode)

;; Confirm before quitting Emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Wraps the lines visually for display purposes (do not split words...)
(global-visual-line-mode t)

;; Blink cursor
(blink-cursor-mode 1)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Move through windows with Ctrl-<arrow keys>
(windmove-default-keybindings 'control) ; You can use other modifiers here

;; Make sure it is all utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Enable line numbering in prog mode only
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Set the  margins width (too small by default) in org and markdown modes
(defun my/set-margins-for-org-and-markdown ()
  (when (or (derived-mode-p 'org-mode)
            (derived-mode-p 'markdown-mode))
     (setq-local left-margin-width 3)
    (setq-local right-margin-width 3)
    (set-window-buffer nil (current-buffer))))
(add-hook 'find-file-hook 'my/set-margins-for-org-and-markdown)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Automatic insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

;; Don't pop UI dialogs when prompting
(setq use-dialog-box nil)

;; make searches case insensitive
(setq case-fold-search t)

;; delete trailing whitespace when file saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  File management settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change default working directory
(setq default-directory "/Users/alexandre/Documents")

;; Remember recently edited files
(recentf-mode 1)
(setq recentf-max-items 50)

;; Remember minibuffer prompt history
(setq history-length 25) ; store max 25 history entries
(setq savehist-autosave-interval 600) ; save writes on SSD
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

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

;; Changing the location of the custom file to keep the config clean
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Set TRAMP default connection mode to SSH
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ========== Use-Package ==========;
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

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

;; ========== YAML mode ===========
(use-package yaml-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Mixins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
;; These ones are *strongly* recommended!
;;(load-file (expand-file-name "mixins/base.el" user-emacs-directory))

;; Software development
;;(load-file (expand-file-name "mixins/dev.el" user-emacs-directory))

;; Org-mode configuration
(load-file (expand-file-name "mixins/org-mode.el" user-emacs-directory))

;; Tools for knowledge management

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Increase font size (too small for me by default)
(set-face-attribute 'default nil :font "Inter-19")

;; Use Leuven theme
(load-theme 'leuven)

;; Customize Org Mode heading sizes
(with-eval-after-load 'org-faces
  (dolist (face '((org-document-title . 1.5)
                  (org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)))
    (set-face-attribute (car face) nil :height (cdr face))))
