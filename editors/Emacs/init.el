;; Inspired by:
;; - https://github.com/ashton314/emacs-bedrock
;; - https://emacs-config-generator.fly.dev/

;; Contents:
;;
;;  - Frame enhancements
;;  - Interface enhancements
;;  - Files management settings
;;  - Packages
;;  - Mixins
;;  - Theme

(when (< emacs-major-version 29)
  (error (format "This configuration requires Emacs 29 or newer; you have version %s" emacs-version)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Frame enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Startup speed, annoyance suppression
(setopt gc-cons-threshold 100000000) ; increasing the garbage collection threshold from 800kb to 100 mb
(setopt read-process-output-max (* 3 1024 1024)) ;  increasing the maximum read size from 4kb to 3mb
(setopt comp-deferred-compilation t) ; postpones some of the compilation workload when Emacs is idle

(setopt warning-suppress-log-types '((comp) (bytecomp)))
(setopt native-comp-async-report-warnings-errors 'silent)

;; Take up all screen space on macOS when starting
(setopt frame-resize-pixelwise t)
(setopt default-frame-alist '((fullscreen . maximized)
                            (ns-transparent-titlebar . t))) ; use a transparent titlebar, makes an integrated and modern UI

;; Silence useless startup messages and screens
(setopt inhibit-startup-echo-area-message (user-login-name)) ; Don't show the welcome message
(setopt inhibit-startup-message t) ; Don't show the splash screen
(setopt display-time-default-load-average nil) ;  Don't show the load time

;; Turn off some unneeded UI elements
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode 1)
(tab-bar-mode 1)
(setq tab-bar-new-tab-choice "*scratch*")
(add-to-list 'default-frame-alist '(undecorated-round . t)) ; hide the titlebar, make the frame have round corner

;; Start with an empty scratch buffer in Org mode.
(setopt initial-major-mode 'org-mode)

;, Enable Native Smooth Scrolling (Emacs 29)
(pixel-scroll-precision-mode 1)

;; Confirm before quitting Emacs
(setopt confirm-kill-emacs #'yes-or-no-p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use common keystrokes by default
(cua-mode)

;; Mode line information
(setopt line-number-mode t)                        ; Show current line in modeline
(setopt column-number-mode t)                      ; Show column as well

(setopt x-underline-at-descent-line nil)           ; Prettier underlines
(setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
;(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Wraps the lines visually for display purposes (do not split words...)
(global-visual-line-mode t)

;; Blink cursor
(blink-cursor-mode 1)

;; Prefer spaces to tabs
(setq-default indent-tabs-mode nil)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

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
(setopt default-buffer-file-coding-system 'utf-8)

;; Enable line numbering in prog mode only
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setopt display-line-numbers-width 3) ; Set a minimum width

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;; Automatic insert closing parens
(electric-pair-mode t)

;; Visualize matching parens
(show-paren-mode 1)

;; Don't pop UI dialogs when prompting
(setopt use-dialog-box nil)

;; make searches case insensitive
(setopt case-fold-search t)

;; delete trailing whitespace when file saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  File management settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change default working directory
(setopt default-directory "/Users/alexandre/Documents")

;; Add unique buffer names in the minibuffer where there are many
;; identical files. This is super useful if you rely on folders for
;; organization and have lots of files with the same name,
;; e.g. foo/index.ts and bar/index.ts.
(require 'uniquify)

;; Remember recently edited files
(recentf-mode 1)
(setopt recentf-max-items 50)

;; Remember minibuffer prompt history
(setopt history-length 25) ; store max 25 history entries
(setopt savehist-autosave-interval 600) ; save writes on SSD
(savehist-mode 1)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Automatically reread from disk if the underlying file changes
(setopt auto-revert-avoid-polling t)
;; Some systems don't do file notifications well; see
;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
(setopt auto-revert-interval 5)
(setopt auto-revert-check-vc-info t)
(global-auto-revert-mode)

(setq load-prefer-newer t)

;; Enable backup files.
(setopt make-backup-files t)
(setopt backup-by-copying t)

;; Enable versioning
(setopt delete-old-versions t
  kept-new-versions 2
  kept-old-versions 2
  version-control t)

;; Save all backup file in this directory.
(setopt backup-directory-alist (quote ((".*" . "~/.emacs_backups"))))

;; Changing the location of the custom file to keep the config clean
(setopt custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Move files to trash when deleting
(setopt delete-by-moving-to-trash t)

;; Set TRAMP default connection mode to SSH
(setopt tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq debug-on-error t)
(setq use-package-verbose t)  ; get more detailed messages from use-package
(setq gnutls-verify-error t)
(setq tls-checktrust gnutls-verify-error)
(setq gnutls-min-prime-bits 2048)

;; ========== Use-Package ==========;
(setopt package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ))

(package-initialize)
(unless package-archive-contents
	(package-refresh-contents))

(require 'use-package)
(setopt use-package-always-ensure t)

;; ========== Which-Key ==========;
(use-package which-key
  :ensure t)
(which-key-mode)

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
  (setopt markdown-command "multimarkdown")
  (setopt markdown-header-scaling t)
  (setopt markdown-fontify-code-blocks-natively t)
  (setopt org-src-fontify-natively t)
  (setopt markdown-header-scaling t)
  (setopt markdown-enable-wiki-links t)
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
(load-file (expand-file-name "mixins/buffers.el" user-emacs-directory))


;; Org-mode configuration
(load-file (expand-file-name "mixins/org-mode.el" user-emacs-directory))

;; Tools for knowledge management

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package spacemacs-theme
  :ensure t
  :config
  (load-theme 'spacemacs-dark t))

(custom-set-faces
  '(org-level-1 ((t (:height 1.3 :weight bold))))
  '(org-level-2 ((t (:height 1.2 :weight bold))))
  '(org-level-3 ((t (:height 1.1 :weight bold)))))

;; Set the  margins width (too small by default) in org and markdown modes
(defun my/set-margins-for-org-and-markdown ()
  (when (or (derived-mode-p 'org-mode)
            (derived-mode-p 'markdown-mode))
     (setopt-local left-margin-width 4)
    (setopt-local right-margin-width 4)
    (set-window-buffer nil (current-buffer))))
(add-hook 'find-file-hook 'my/set-margins-for-org-and-markdown)

;; Increase font size (too small for me by default)
;(set-face-attribute 'default nil :font "Inter-19")
(set-face-attribute 'default nil
                    :height 160)
