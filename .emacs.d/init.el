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
;; show parenthesis pairing
(show-paren-mode 1)
;; undo tree mode
(undo-tree-mode 1)
;; rebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
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
;; truncate lines by default
(set-default 'truncate-lines t)
;; all back up files into same systemwide temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; setup yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; enable elpy for python development
(package-initialize)
(elpy-enable)
;; temporary python shell fix until Emacs rc 25.2
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))
;; peep-dired kill buffers on disabling of minor mode
(setq peep-dired-cleanup-on-disable t)
;; neotree window position
(setq neo-window-position 'right)
;; neotree ignore specific folders
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "__pycache__"))
;; neotree toggle with ctrl-t
(global-set-key (kbd "C-t") 'neotree-toggle)
;; neotree refresh show file alt-r
(global-set-key (kbd "M-r") 'neotree-find)
;; neotree use ascii instead of folder icons
(setq neo-theme 'ascii)
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
;; add Macports Path
(setq exec-path (append exec-path '("/opt/local/bin")))
;; column
(setq column-number-mode t)
;; disable Cursor Blink
(blink-cursor-mode 0)
;; font Size
(set-face-attribute 'default nil :height 144)
;; scroll behavior
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
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
;; magit setup
(setq magit-completing-read-function 'ivy-completing-read)
;; python shell prompt warning
(setq python-shell-prompt-detect-failure-warning nil)
;; previous and Next Buffer
(global-unset-key (kbd "s-n"))
(global-set-key (kbd "s-n") 'next-buffer)
(global-unset-key (kbd "s-p"))
(global-set-key (kbd "s-p") 'previous-buffer)
;; windmove
(windmove-default-keybindings)
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-l") 'windmove-up)
(global-set-key (kbd "s-;") 'windmove-right)
;; .http files load rest-client mode
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
;; auto dim other buffers
(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))
;; highlight symbol at point
(global-unset-key (kbd "s-s"))
(global-set-key (kbd "s-s") 'highlight-symbol-at-point)
;; minor mode lighter sets to diminish in mode-line
(eval-after-load "hiwin" '(diminish 'hiwin-mode))
(eval-after-load "golden-ratio" '(diminish 'golden-ratio-mode))
(eval-after-load "sphinx-doc" '(diminish 'sphinx-doc-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "elpy" '(diminish 'elpy-mode))
(eval-after-load "magit" '(diminish 'auto-revert-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "auto-dim-other-buffers" '(diminish 'auto-dim-other-buffers-mode))
(diminish 'highlight-indentation-mode)

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
(setq ibuffer-expert t)
;; imenu anywhere binding
(global-set-key (kbd "C-.") 'imenu-anywhere)
;; imenu binding
(global-set-key (kbd "C->") 'imenu)
;; use browse-kill ring as the default for M-y
(browse-kill-ring-default-keybindings)
;; org configuration
(setq org-log-done t)
(setq org-agenda-files (list "~/.root.org"
                             "~/Documents/Academic/.academic.org"
			     "~/Projects/.projects.org"
			     "~/Work/.work.org"))
;; Projectile
 (projectile-global-mode)
(setq projectile-enable-caching t)
(counsel-projectile-on)

;; load Additional Files
(load "~/.emacs.d/irc")
(load "~/.emacs.d/ivy")
(load "~/.emacs.d/hydra")
(load "~/.emacs.d/center")
(load "~/.emacs.d/functions")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(circe-default-part-message "Exit.")
 '(circe-reduce-lurker-spam t t)
 '(display-time-world-list
   (quote
    (("America/Chicago" "Chicago")
     ("Europe/Berlin" "Berlin")
     ("Europe/Athens" "Athens")
     ("Europe/London" "London")
     ("America/Los_Angeles" "San Francisco")
     ("America/Argentina/Buenos_Aires" "Buenos Aires"))))
 '(hiwin-mode t)
 '(ivy-completing-read-handlers-alist
   (quote
    ((tmm-menubar . completing-read-default)
     (tmm-shortcut . completing-read-default)
     (elpy-doc . completing-read-default))))
 '(org-return-follows-link t)
 '(package-selected-packages
   (quote
    (auto-dim-other-buffers counsel-projectile projectile peep-dired flx counsel flyspell-correct-ivy browse-kill-ring imenu-anywhere py-isort which-key json-mode realgud exec-path-from-shell elpy smex avy switch-window restclient find-file-in-repository multi-term web-mode undo-tree sphinx-doc perspective persp-mode neotree markdown-mode magit latex-preview-pane kivy-mode jinja2-mode hydra golden-ratio circe auctex)))
 '(realgud:pdb-command-name "python -m pdb")
 '(switch-window-qwerty-shortcuts (quote ("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))
 '(switch-window-shortcut-style (quote qwerty)))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "nil" :family "Menlo"))))
 '(auto-dim-other-buffers-face ((t (:background "gray95"))))
 '(circe-prompt-face ((t (:background "textBackgroundColor" :foreground "Black" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(font-lock-doc-face ((t (:foreground "gray40"))))
 '(font-lock-function-name-face ((t (:foreground "green4"))))
 '(font-lock-keyword-face ((t (:foreground "royal blue"))))
 '(font-lock-string-face ((t (:foreground "dark red"))))
 '(font-lock-variable-name-face ((t (:foreground "green3"))))
 '(fringe ((t nil)))
 '(highlight-indentation-face ((t (:background "gray95"))))
 '(hiwin-face ((t (:background "gray95"))))
 '(mode-line ((t (:background "gray55" :foreground "White" :box nil))))
 '(neo-header-face ((t (:foreground "green3"))))
 '(neo-root-dir-face ((t (:foreground "green3"))))
 '(show-paren-match ((t (:background "SeaGreen1"))))
 '(term-color-white ((t (:background "white" :foreground "light green")))))
