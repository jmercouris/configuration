(when window-system (set-frame-size (selected-frame) 120 60))
;; disable splash screen
(setq inhibit-splash-screen t)
;; disable menu bar
(menu-bar-mode -1)
(when (display-graphic-p)
  ;; disable scroll bar
  (scroll-bar-mode -1))
;; disable tool bar
(when window-system
    (tool-bar-mode -1))
;; set initial scratch bar message
(setq initial-scratch-message nil)
;; set initial major mode to text mode
(setq initial-major-mode 'text-mode)
;; disable tool tip mode
(tooltip-mode 0)
;; make mode-line appear flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
;; highlight current line
(global-hl-line-mode 1)
;; only current line in current window
(setq h1-line-sticky-flag nil)
;; disable highlighting in terminal
(add-hook 'term-mode-hook (lambda ()
			    (setq-local global-hl-line-mode
					nil)))
(set-face-background 'hl-line "#F5F5F5")
;; change Highlighted Text Color
(set-face-attribute 'region nil :background "#00ed00")
;; set cursor color
(add-to-list 'default-frame-alist '(cursor-color . "#00ee00"))
;; disable Cursor Blink
(blink-cursor-mode 0)
;; font Size
(set-face-attribute 'default nil :height 144)
;; remove title from window
(setq frame-title-format nil)
