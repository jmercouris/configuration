;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add melpa to packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
;; disable splash screen
(setq inhibit-splash-screen t)
;; disable menu bar
(menu-bar-mode -1)
;; disable tool bar
(tool-bar-mode -1)
;; set initial scratch bar message
(setq initial-scratch-message nil)
;; show parenthesis pairing
(show-paren-mode 1)
;; make buffer switch command auto suggestions, also for find-file command
(ido-mode 1)
;; make ido display choices vertically
(setq ido-separator "\n")
;; display any item that contains the chars you typed
(setq ido-enable-flex-matching t)
;; highlight current line
(global-hl-line-mode 1)
(set-face-background 'hl-line "#D3D3D3")
;; truncate lines by default
(set-default 'truncate-lines t)
;; centered cursor mode
(and
 (require 'centered-cursor-mode)
 (global-centered-cursor-mode +1))
;; all back up files into same systemwide temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; enable elpy for python development
(package-initialize)
(elpy-enable)
;; zpt files load html-mode
(add-to-list 'auto-mode-alist '("\\.pt$" . web-mode))
;; Artist Mode Hooks
(add-hook 'artist-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<f1>") 'org-mode)
	    (local-set-key (kbd "<f2>") 'artist-select-op-pen-line) ; f2 = pen mode
            (local-set-key (kbd "<f3>") 'artist-select-op-line)     ; f3 = line
	    (local-set-key (kbd "<f4>") 'artist-select-op-square)   ; f4 = rectangle
	    (local-set-key (kbd "<f5>") 'artist-select-op-ellipse)  ; f5 = ellipse
	    (local-set-key (kbd "C-z") 'undo)
	    ))
;; Set Eshell Prompt
(setq eshell-prompt-function
      (lambda nil "> "))
