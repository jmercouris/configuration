(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load MacOS specific configuration
(when (memq window-system '(mac ns))
  (load "~/.emacs.d/osx.el"))
;; set default shell to bash for rgrep
(setq shell-file-name "/bin/sh")
;; allow upcase region
(put 'upcase-region 'disabled nil)
;; undo tree mode
(undo-tree-mode 1)
;; when yanking over a marked region, overwrite
(delete-selection-mode)
;; disable tabs for indenting
(setq-default indent-tabs-mode nil)
;; enable narrow-to-region mode (C-x-n-n)
(put 'narrow-to-region 'disabled nil)
;; hs mode
(load-library "hideshow")
(global-set-key (kbd "C-=") 'hs-toggle-hiding)
;; rebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'comment-dwim)
;; truncate lines by default
(set-default 'truncate-lines t)
;; all back up files into same systemwide temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; set default browser
(setq browse-url-browser-function 'eww-browse-url)
;; show column number
(setq column-number-mode t)
;; scroll behavior
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; window register save and recall
(global-unset-key (kbd "s-r"))
(global-unset-key (kbd "s-o"))
(global-set-key (kbd "s-r") 'window-configuration-to-register)
(global-set-key (kbd "s-o") 'jump-to-register)
;; comment box
(global-set-key (kbd "s-/") 'comment-box)
;; multi-term configuration
(global-unset-key (kbd "s-t"))
(when (require 'multi-term nil t)
  (global-set-key (kbd "s-t") 'multi-term)
  (global-set-key (kbd "s-}") 'multi-term-next)
  (global-set-key (kbd "s-{") 'multi-term-prev))
;; .http files load rest-client mode
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
;; yes or no to y or n
(defalias 'yes-or-no-p 'y-or-n-p)
;; occur mode n-p
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)
;; previous and Next Buffer
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-d") 'kill-this-buffer)
;; don't open new windows for these buffers
(add-to-list 'same-window-buffer-names "*wclock*")
;; down case region
(put 'downcase-region 'disabled nil)
;; set world time list
(setq display-time-world-list
      (quote
       (("America/Chicago" "Chicago")
	("Europe/Berlin" "Berlin")
	("Europe/Athens" "Athens")
	("Europe/London" "London")
	("America/Los_Angeles" "San Francisco")
	("America/Argentina/Buenos_Aires" "Buenos Aires"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup mouse disable
(use-package disable-mouse
  :diminish global-disable-mouse-mode
  :config
  (global-disable-mouse-mode))
;; setup yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))
(use-package peep-dired
  :config
  (setq peep-dired-cleanup-on-disable t)
  (setq peep-dired-ignored-extensions '("pyc")))
;; setup neotree
(use-package neotree
  :config
  (setq neo-window-position 'right)
  (setq neo-theme 'ascii)
  (setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "__pycache__")))
;; switch window behavior uses switch-window package
(use-package switch-window
  :config
  (global-set-key (kbd "C-x o") 'switch-window)
  (setq switch-window-qwerty-shortcuts (quote ("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))
  (setq switch-window-shortcut-style (quote qwerty))
  (global-set-key (kbd "C-x w") 'switch-window-then-delete))
;; magit setup
(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
;; windmove
(use-package windmove
  :config
  (global-set-key (kbd "s-j") 'windmove-left)
  (global-set-key (kbd "s-k") 'windmove-down)
  (global-set-key (kbd "s-l") 'windmove-up)
  (global-set-key (kbd "s-;") 'windmove-right))
;; framemove
(use-package framemove
  :config
  (setq framemove-hook-into-windmove t))

(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "anaconda-mode" '(diminish 'anaconda-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "magit" '(diminish 'auto-revert-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "back-button" '(diminish 'back-button-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "beacon" '(diminish 'beacon-mode))

;; which key prompts on C-x etc
(use-package which-key
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))
;; use ibuffer instead of regular buffer list
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer)
  (setq ibuffer-expert t))
;; imenu anywhere binding
(use-package imenu-anywhere
  :config
  (global-set-key (kbd "C-.") 'ivy-imenu-anywhere)
  (global-set-key (kbd "C->") 'imenu-reposition))
;; browse-kill ring
(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings))
;; org configuration
(use-package org
  :config 
  (setq org-startup-indented t)
  (setq org-log-done t)
  (setq org-return-follows-link t)
  (setq org-todo-keywords
        '((sequence "TASK" "DONE")))
  (setq org-todo-keyword-faces
        '(("WAIT" . "gray")))
  (setq org-capture-templates
        '(("t" "TODO" entry (file "/Users/jmercouris/Documents/Organization/tasks.org")
           "* TODO %? %^G \n  %U")
          ("n" "Note" entry (file "/Users/jmercouris/Documents/Organization/notes.org")
           "* %? %^G\n%U")
          ("j" "Journal" entry
           (file "/Users/jmercouris/Documents/Personal/Journal/journal.org")
           "* %U %? %^G\n")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)))
  (setq org-src-fontify-natively t)
  ;; update the table of contents on save
  (if (require 'toc-org nil t)
      (add-hook 'org-mode-hook 'toc-org-enable)
    (warn "toc-org not found"))
  ;; org mode should auto-fill
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-src-tab-acts-natively t))

;; projectile
(use-package projectile
  :ensure t
  :config (projectile-global-mode))
(use-package counsel-projectile
  :ensure t)
;; back button mode
(use-package back-button
  :ensure t
  :config (back-button-mode 1))
;; smart parens mode
(use-package smartparens
  :ensure t
  :config (show-smartparens-global-mode +1))
;;popwin mode
(use-package popwin
  :ensure t
  :config (popwin-mode 1))
(use-package beacon
  :ensure t
  :config (beacon-mode 1))
;; docview mode continuous
(setq doc-view-continuous t)
;; auto-rename new eww buffers
(defun rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'rename-eww-hook)
;; Ctrl + tab suggests completion based on git
(global-set-key (kbd "<C-tab>") 'git-complete)
;; set custom file
(setq custom-file "~/.emacs.d/custom.el")
;; setup elfeed
(setq elfeed-feeds
      '(("https://www.reddit.com/r/programming.rss" r/programming)
        ("https://www.reddit.com/r/emacs.rss" r/emacs)
        ("https://news.ycombinator.com/rss" yc/news)))
;; Use ace jump zap to char instead of normal zap to char
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load additional files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/theme")
(load "~/.emacs.d/custom")
(load "~/.emacs.d/private")
(load "~/.emacs.d/eshell")
(load "~/.emacs.d/irc")
(load "~/.emacs.d/ivy")
(load "~/.emacs.d/hydra")
(load "~/.emacs.d/functions")
;; load external packages
(load "~/.emacs.d/packages/git-complete/git-complete")
(load "~/.emacs.d/packages/counsel-css/counsel-css")
;; load develop files
(load "~/.emacs.d/develop/_css")
(load "~/.emacs.d/develop/_python")
(load "~/.emacs.d/develop/_c")
(load "~/.emacs.d/develop/_lisp")
(load "~/.emacs.d/develop/_yaml")
(load "~/.emacs.d/develop/_tex")
(load "~/.emacs.d/develop/_qt")
(load "~/.emacs.d/develop/_php")
