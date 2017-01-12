;; Emacs Configuration
;; add melpa to packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))
;; update system path on OSX
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))
;; set default shell to bash for rgrep
(setq shell-file-name "/bin/sh")
;; disable native osx full screen
(setq ns-use-native-fullscreen nil)
;; disable splash screen
(setq inhibit-splash-screen t)
;; disable menu bar
(menu-bar-mode -1)
;; disable scroll bar
(when (display-graphic-p)
  (scroll-bar-mode -1))
;; disable tool bar
(if window-system
    (tool-bar-mode -1))
;; set initial scratch bar message
(setq initial-scratch-message nil)
;; show parenthesis pairing
(show-paren-mode 1)
;; make buffer switch command auto suggestions, also for find-file command
(ido-mode 1)
;; undo tree mode
(undo-tree-mode 1)
;; rkfebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
;; make ido display choices vertically
(setq ido-separator "\n")
;; display any item that contains the chars you typed
(setq ido-enable-flex-matching t)
;; highlight current line
(global-hl-line-mode 1)
;; disable highlighting in terminal
(add-hook 'term-mode-hook (lambda ()
			    (setq-local global-hl-line-mode
					nil)))
;; only current line in current window
(setq h1-line-sticky-flag nil)
(set-face-background 'hl-line "#D3D3D3")
;; change Highlighted Text Color
(set-face-attribute 'region nil :background "#00ed00")
;; set cursor color
(set-cursor-color "#00ed00")
;; truncate lines by default
(set-default 'truncate-lines t)
;; golden ratio mode
(golden-ratio-mode 1)
;; autoscale (for wide screen)
(setq golden-ratio-adjust-factor .85)
;; ignore certain buffers
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
;; all back up files into same systemwide temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; enable elpy for python development
(package-initialize)
(elpy-enable)
;; sphinx-doc mode in python
(add-hook 'python-mode-hook (lambda ()
			      (require 'sphinx-doc)
			      (sphinx-doc-mode t)))
;; zpt files load html-mode
(add-to-list 'auto-mode-alist '("\\.pt$" . web-mode))
;; neotree window position
(setq neo-window-position 'right)
;; neotree ignore specific folders
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "__pycache__"))
;; neotree toggle with ctrl-t
(global-set-key (kbd "C-t") 'neotree-toggle)
;; neotree refresh show file alt-r
(global-set-key (kbd "M-r") 'neotree-find)
;; artist Mode Hooks
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
;; Paste from OSX
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))
;; Paste to OSX
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
;; set copy/paste functions
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
(put 'upcase-region 'disabled nil)
;; add Macports Path
(setq exec-path (append exec-path '("/opt/local/bin")))
;; column
(setq column-number-mode t)
;; neotree use ascii instead of folder icons
(setq neo-theme 'ascii)
;; C-n/p is more intuitive in vertical layout
(defun ido-define-keys ()
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)
;; disable Cursor Blink
(blink-cursor-mode 0)
;; font Size
(set-face-attribute 'default nil :height 144)
;; load Additional Files
(load "~/.emacs.d/irc")
(load "~/.emacs.d/hydra")
(load "~/.emacs.d/center")

;; layout restore
;; (load "~/.emacs.d/layout-restore")
;; (require 'layout-restore)
;; (global-set-key [?\C-c ?l] 'layout-save-current)
;; (global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
;; (global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)

;; enable centered-point-mode in python
(add-hook 'prog-mode-hook 'centered-cursor-mode)
;; disable in terminal modes
(define-global-minor-mode global-centered-point-mode centered-cursor-mode
  (lambda ()
    (when (not (memq major-mode
                     (list 'Info-mode 'term-mode 'eshell-mode 'shell-mode 'erc-mode)))
      (centered-cursor-mode))))
;; word count alias
(defalias 'word-count 'count-words)
;; window register save and recal
(global-unset-key (kbd "s-r"))
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-r") 'window-configuration-to-register)
(global-set-key (kbd "s-o") 'jump-to-register)
;; multi-term configuration
(global-unset-key (kbd "s-t"))
(when (require 'multi-term nil t)
  (global-set-key (kbd "s-t") 'multi-term)
  (global-set-key (kbd "s-}") 'multi-term-next)
  (global-set-key (kbd "s-{") 'multi-term-prev))
;; find file in repository
(global-set-key (kbd "C-x C-g") 'find-file-in-repository)
;; enable buffer erasing
(put 'erase-buffer 'disabled nil)
;; switch window behavior uses switch-window package
(global-set-key (kbd "C-x o") 'switch-window)
;; golden-ratio should be called when switch-window is called
(add-to-list 'golden-ratio-extra-commands 'switch-window)
;; magit-status
(global-set-key (kbd "C-x g") 'magit-status)
;; python shell prompt warning
(setq python-shell-prompt-detect-failure-warning nil)
;; avy movement commands
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-g") 'avy-goto-line)
;; set smex to super + d
(global-set-key (kbd "s-d") 'smex)
(global-set-key (kbd "s-D") 'smex-major-mode-commands)
;; make mode-line appear flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
;; disable frame creation hotkey
(global-unset-key (kbd "s-n"))
;; disable printing hotkey
(global-unset-key (kbd "s-p"))
;; previous and Next Buffer
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-p") 'previous-buffer)
;; windmove
(windmove-default-keybindings)
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-l") 'windmove-up)
(global-set-key (kbd "s-;") 'windmove-right)
;; .http files load rest-client mode
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
;; start hi-win mode
(hiwin-activate)
;; highlight symbol at point
(global-unset-key (kbd "s-h"))
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)

;; ediff don't open new frame, split horiziontally
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (setq ediff-split-window-function 'split-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hiwin-mode t)
 '(package-selected-packages
   (quote
    (exec-path-from-shell elpy hiwin smex avy rainbow-delimiters switch-window restclient find-file-in-repository multi-term web-mode undo-tree sphinx-doc perspective persp-mode neotree markdown-mode magit latex-preview-pane kivy-mode key-chord jinja2-mode hydra golden-ratio circe centered-cursor-mode auctex)))
 '(switch-window-qwerty-shortcuts (quote ("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))
 '(switch-window-shortcut-style (quote qwerty)))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "nil" :family "Menlo"))))
 '(fringe ((t nil)))
 '(highlight-indentation-face ((t (:background "gray95"))))
 '(hiwin-face ((t (:background "gray95"))))
 '(mode-line ((t (:background "gray55" :foreground "White" :box nil))))
 '(term-color-white ((t (:background "white" :foreground "light green")))))
