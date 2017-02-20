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
;; make mode-line appear flat
(set-face-attribute 'mode-line nil :box nil)
(set-face-attribute 'mode-line-inactive nil :box nil)
;; disable frame creation hotkey
(global-unset-key (kbd "s-n"))
;; disable printing hotkey
(global-unset-key (kbd "s-p"))
; disable auto save
(setq auto-save-default nil)
;; ivy configuration
(ivy-mode 1)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "s-d") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; flex search for everything but swiper
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus) (t . ivy--regex-fuzzy)))
;; ivy kill switch buffer
(define-key
    ivy-switch-buffer-map
    (kbd "C-k")
  (lambda ()
    (interactive)
    (ivy-set-action 'kill-buffer)
    (ivy-done)))
;; show parenthesis pairing
(show-paren-mode 1)
;; undo tree mode
(undo-tree-mode 1)
;; rebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
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
	    (local-set-key (kbd "<f1>") 'artist-select-op-pen-line) ; f2 = pen mode
            (local-set-key (kbd "<f2>") 'artist-select-op-line)     ; f3 = line
	    (local-set-key (kbd "<f3>") 'artist-select-op-square)   ; f4 = rectangle
	    (local-set-key (kbd "<f4>") 'artist-select-op-ellipse)  ; f5 = ellipse
	    (local-set-key (kbd "C-z") 'undo)
	    ))
;; set eshell prompt
(setq eshell-prompt-function
      (lambda nil "> "))
;; clear buffer
(global-unset-key (kbd "s-c"))
(global-set-key (kbd "s-c") 'erase-buffer)
;; clear eshell buffer
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
	 (local-set-key (kbd "s-c") 'eshell-clear-buffer)))
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
;; disable Cursor Blink
(blink-cursor-mode 0)
;; font Size
(set-face-attribute 'default nil :height 144)
;; load Additional Files
(load "~/.emacs.d/irc")
(load "~/.emacs.d/hydra")
(load "~/.emacs.d/center")
(load "~/.emacs.d/vkill")
;; enable smooth scrolling mode
(smooth-scrolling-mode 1)
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
;; magit-status
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-completing-read-function 'ivy-completing-read)
;; python shell prompt warning
(setq python-shell-prompt-detect-failure-warning nil)
;; avy movement commands
(global-set-key (kbd "s-.") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-g") 'avy-goto-line)
;; previous and Next Buffer
(global-set-key (kbd "s-n") 'next-buffer)
(global-set-key (kbd "s-p") 'previous-buffer)
;; windmove
(windmove-default-keybindings)
(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)
;; .http files load rest-client mode
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
;; start hi-win mode
(hiwin-activate)
;; highlight symbol at point
(global-unset-key (kbd "s-h"))
(global-set-key (kbd "s-h") 'highlight-symbol-at-point)
;; minor mode lighter sets to diminish in mode-line
(eval-after-load "hiwin" '(diminish 'hiwin-mode))
(eval-after-load "golden-ratio" '(diminish 'golden-ratio-mode))
(eval-after-load "sphinx-doc" '(diminish 'sphinx-doc-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "elpy" '(diminish 'elpy-mode))
(eval-after-load "magit" '(diminish 'auto-revert-mode))
(diminish 'highlight-indentation-mode)
(diminish 'ivy-mode)
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
(global-set-key (kbd "C-.") 'imenu-anywhere)
;; imenu binding
(global-set-key (kbd "C->") 'imenu)
;; use browse-kill ring as the default for M-y
(browse-kill-ring-default-keybindings)
;; open shell on remote machine
(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/scp:" host ":"))
      (shell (concat "*" host "*")))))
;; example of setting up custom shells
;; (defun myserver-shell () (interactive) (remote-shell "myserver"))
(setq org-log-done t)
(setq org-agenda-files (list "~/.root.org"
                             "~/Documents/Academic/.academic.org"
			     "~/Projects/.projects.org"
			     "~/Work/.work.org"))
;; ediff don't open new frame, split horiziontally
;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; (setq ediff-split-window-function 'split-window-horizontally)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(circe-default-part-message "Exit.")
 '(circe-reduce-lurker-spam t t)
 '(hiwin-mode t)
 '(ivy-completing-read-handlers-alist
   (quote
    ((tmm-menubar . completing-read-default)
     (tmm-shortcut . completing-read-default)
     (elpy-doc . completing-read-default))))
 '(package-selected-packages
   (quote
    (flx counsel flyspell-correct-ivy browse-kill-ring imenu-anywhere py-isort which-key json-mode smooth-scrolling realgud exec-path-from-shell elpy hiwin smex avy rainbow-delimiters switch-window restclient find-file-in-repository multi-term web-mode undo-tree sphinx-doc perspective persp-mode neotree markdown-mode magit latex-preview-pane kivy-mode key-chord jinja2-mode hydra golden-ratio circe auctex)))
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
 '(circe-prompt-face ((t (:background "textBackgroundColor" :foreground "Black" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(font-lock-doc-face ((t (:foreground "gray40"))))
 '(font-lock-function-name-face ((t (:foreground "forest green"))))
 '(font-lock-keyword-face ((t (:foreground "royal blue"))))
 '(font-lock-string-face ((t (:foreground "dark red"))))
 '(font-lock-variable-name-face ((t (:foreground "green3"))))
 '(fringe ((t nil)))
 '(highlight-indentation-face ((t (:background "gray95"))))
 '(hiwin-face ((t (:background "gray95"))))
 '(mode-line ((t (:background "gray55" :foreground "White" :box nil))))
 '(neo-header-face ((t (:foreground "green3"))))
 '(neo-root-dir-face ((t (:foreground "green3"))))
 '(term-color-white ((t (:background "white" :foreground "light green")))))
