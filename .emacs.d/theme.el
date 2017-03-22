;; disable splash screen
(setq inhibit-splash-screen t)
;; disable menu bar
(menu-bar-mode -1)
(when (display-graphic-p)
  ;; disable scroll bar
  (scroll-bar-mode -1)
  ;; set fringe mode to disable by default
  (set-fringe-mode 0))
;; disable tool bar
(if window-system
    (tool-bar-mode -1))
;; set initial scratch bar message
(setq initial-scratch-message nil)
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
(set-face-background 'hl-line "#DEDEDE")
;; change Highlighted Text Color
(set-face-attribute 'region nil :background "#00ed00")
;; set cursor color
(set-cursor-color "#00f900")
;; disable Cursor Blink
(blink-cursor-mode 0)
;; font Size
(set-face-attribute 'default nil :height 144)
