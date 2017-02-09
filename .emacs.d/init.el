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
;; make ido display choices vertically
(setq ido-separator "\n")
;; undo tree mode
(undo-tree-mode 1)
;; rebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
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
(set-cursor-color "#00f900")
;; truncate lines by default
(set-default 'truncate-lines t)
;; golden ratio mode
(golden-ratio-mode 1)
;; autoscale (for wide screen)
(setq golden-ratio-adjust-factor .75)
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
;; set eshell prompt
(setq eshell-prompt-function
      (lambda nil "> "))
;; clear eshell buffer
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
          (local-set-key (kbd "C-l") 'eshell-clear-buffer)))
;; Paste from OSX
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))
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
(load "~/.emacs.d/vkill")

;; layout restore
;; (load "~/.emacs.d/layout-restore")
;; (require 'layout-restore)
;; (global-set-key [?\C-c ?l] 'layout-save-current)
;; (global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
;; (global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)

;; enable smooth scrolling mode
(smooth-scrolling-mode 1)
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
;; switch window kill window
(global-set-key (kbd "C-x w") 'switch-window-then-delete)
;; golden-ratio should be called when switch-window is called
(add-to-list 'golden-ratio-extra-commands 'switch-window)
(add-to-list 'golden-ratio-extra-commands 'elpy-occur-definitions)
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
(hiwin-draw-ol)
;; highlight symbol at point
(global-unset-key (kbd "s-h"))
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)
;; minor mode lighter sets to diminish in mode-line
(defun modeline-set-lighter (minor-mode lighter)
  (when (assq minor-mode minor-mode-alist)
    (setcar (cdr (assq minor-mode minor-mode-alist)) lighter)))
(defun modeline-remove-lighter (minor-mode)
  (modeline-set-lighter minor-mode ""))
;; diminish lighters for following modes
(modeline-remove-lighter 'hiwin-mode)
(modeline-remove-lighter 'auto-revert-mode)
(modeline-remove-lighter 'golden-ratio-mode)
(modeline-remove-lighter 'sphinx-doc-mode)
(modeline-remove-lighter 'which-key-mode)
(modeline-remove-lighter 'highlight-indentation-mode)
;; example (modeline-set-lighter 'abbrev-mode " Abbr")
;; fill comment to width
(defun fill-comment ()
  "Fill text to column width for comments"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (while (< (current-column) fill-column) (insert ?#))))
(global-set-key (kbd "s-/") 'fill-comment)
;;xml formatting
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))
;; Which key prompts on C-x etc
(which-key-mode)
(which-key-setup-minibuffer)
;; shorten mode line for git
(defun my-shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat "G" (substring string 3)))
   (t
    string)))
(advice-add 'vc-git-mode-line-string :filter-return 'my-shorten-vc-mode-line)
;; use ibuffer instead of regular buffer list
(defalias 'list-buffers 'ibuffer)
;; imenu anywhere binding
(global-set-key (kbd "C-.") 'ido-imenu-anywhere)
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
    (imenu-anywhere py-isort which-key json-mode smooth-scrolling realgud exec-path-from-shell elpy hiwin smex avy rainbow-delimiters switch-window restclient find-file-in-repository multi-term web-mode undo-tree sphinx-doc perspective persp-mode neotree markdown-mode magit latex-preview-pane kivy-mode key-chord jinja2-mode hydra golden-ratio circe auctex)))
 '(realgud:pdb-command-name "python -m pdb")
 '(smooth-scroll-margin 15)
 '(smooth-scroll-strict-margins nil)
 '(switch-window-qwerty-shortcuts (quote ("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))
 '(switch-window-shortcut-style (quote qwerty)))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "nil" :family "Menlo"))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(font-lock-doc-face ((t (:foreground "gray40"))))
 '(font-lock-function-name-face ((t (:foreground "green4"))))
 '(font-lock-keyword-face ((t (:foreground "royal blue"))))
 '(font-lock-string-face ((t (:foreground "dark red"))))
 '(font-lock-variable-name-face ((t (:foreground "lime green"))))
 '(fringe ((t nil)))
 '(highlight-indentation-face ((t (:background "gray95"))))
 '(hiwin-face ((t (:background "gray95"))))
 '(mode-line ((t (:background "gray55" :foreground "White" :box nil))))
 '(neo-header-face ((t (:foreground "green3"))))
 '(neo-root-dir-face ((t (:foreground "green3"))))
 '(term-color-white ((t (:background "white" :foreground "light green")))))
