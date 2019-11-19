(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/org/lisp")
(add-to-list 'load-path "~/.emacs.d/org/contrib/lisp" t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;; load MacOS specific configuration
(when (memq window-system '(mac ns))
  (load "~/.emacs.d/darwin.el"))
;; set default shell to bash for rgrep/git grep
(setq shell-file-name "/bin/sh")
;; allow upcase region
(put 'upcase-region 'disabled nil)
;; when yanking over a marked region, overwrite
(delete-selection-mode)
;; disable tabs for indenting
(setq-default indent-tabs-mode nil)
;; enable narrow-to-region mode (C-x-n-n)
(put 'narrow-to-region 'disabled nil)
;; down case region
(put 'downcase-region 'disabled nil)
;; rebind comment region
(global-set-key (kbd "C-/") 'comment-dwim)
(global-set-key (kbd "M-/") 'comment-box)
;; dabbrev expand
(global-set-key (kbd "s-/") 'dabbrev-expand)
;; truncate lines by default
(set-default 'truncate-lines t)
;; all back up files into same systemwide temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; show column number
(setq column-number-mode t)
;; scroll behavior
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)
;; allow dired to copy to other dired buffers
(setq dired-dwim-target t)
;; multi-term configuration
(global-unset-key (kbd "s-t"))
(when (require 'multi-term nil t)
  (global-set-key (kbd "s-t") 'multi-term)
  (global-set-key (kbd "s-}") 'multi-term-next)
  (global-set-key (kbd "s-{") 'multi-term-prev))
;; yes or no to y or n
(defalias 'yes-or-no-p 'y-or-n-p)
;; previous and Next Buffer
(global-set-key (kbd "s-]") 'next-buffer)
(global-set-key (kbd "s-[") 'previous-buffer)
(global-set-key (kbd "s-d") 'kill-this-buffer)
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
;; setup log file
(use-package auto-revert-tail-mode
  :mode "\\.log\\'")
;; use forge
(use-package forge
  :after magit)
;; setup restclient mode
(use-package restclient-mode
  :mode "\\.http$")
;; setup mouse disable
(use-package disable-mouse
  :diminish disable-mouse-global-mode
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
  (setq magit-completing-read-function 'ivy-completing-read))
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
;; which key prompts on C-x etc
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))
;; use ibuffer instead of regular buffer list
(use-package ibuffer
  :config
  (defalias 'list-buffers 'ibuffer)
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
      (quote (("default"
               ("lisp" (mode . lisp-mode))
               ("python" (mode . python-mode))
               ("web" (or
                       (mode . css-mode)
                       (mode . html-mode)
                       (mode . mhtml-mode)))
               ("sldb" (mode . sldb-mode))
               ("org" (mode . org-mode))
               ("dired" (mode . dired-mode))
               ("eww" (mode . eww-mode))
               ("irc" (or
                       (mode . circe-channel-mode)
                       (mode . circe-servper-mode)))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)))))))
  (add-hook 'ibuffer-mode-hook
            (lambda () (ibuffer-switch-to-saved-filter-groups "default"))))
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
  (require 'org-contacts)
  (setq org-startup-indented t)
  (setq org-log-done t)
  (setq org-return-follows-link t)
  (setq org-todo-keywords
        '((sequence "TASK" "DONE")))
  (setq org-todo-keyword-faces
        '(("WAIT" . "gray")))
  (setq org-capture-templates
        '(("t" "Personal Task" entry (file "/Users/jmercouris/Documents/Organization/tasks.org")
           "* TASK %? %^G \n  %U")
          ("n" "Note" entry (file "/Users/jmercouris/Documents/Organization/notes.org")
           "* %? %^G\n%U")
          ("j" "Journal" entry (file "/Users/jmercouris/Documents/Personal/Journal/journal.org")
           "* %U %? %^G\n")
          ("w" "Work Task" entry (file "/Users/jmercouris/Work/Atlas/Document/Atlas/tasks.org")
           "* TASK %? %^G \n  %U")
          ("c" "Work Contact" entry (file "/Users/jmercouris/Work/Atlas/Atlas/contacts.org")
           "* %(org-contacts-template-name)
              %(org-contacts-template-email)")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((lisp . t)))
  (setq org-src-fontify-natively t)
  ;; org mode should auto-fill
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (setq org-src-tab-acts-natively t))
;; projectile
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-global-mode))
(use-package counsel-projectile
  :ensure t)
;;popwin mode
(use-package popwin
  :ensure t
  :config (popwin-mode 1))
(use-package beacon
  :ensure t
  :config (beacon-mode 1))
(use-package goto-last-change
  :ensure t
  :config
  (global-unset-key (kbd "s--"))
  (global-set-key (kbd "s--") 'goto-last-change))
(use-package webpaste
  :ensure t
  :config
  (progn
    (setq webpaste-provider-priority '("dpaste.com" "ix.io"))))
;; diminish modes
(eval-after-load "anaconda-mode" '(diminish 'anaconda-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "magit" '(diminish 'auto-revert-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))
(eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "beacon" '(diminish 'beacon-mode))
(with-eval-after-load 'magit
 (setq magit-repository-directories '(("~/Work/Atlas" . 1)
                                       ("~/Projects" . 1)
                                       ("~/Source" . 1))))
(with-eval-after-load 'orgit
 (setq orgit-store-repository-id t))
;; docview mode continuous
(setq doc-view-continuous t)
;; Ctrl + tab suggests completion based on git
(global-set-key (kbd "<C-tab>") 'git-complete)
;; set custom file
(setq custom-file "~/.emacs.d/custom.el")
;; setup elfeed
(setq elfeed-feeds
      '(("https://www.reddit.com/r/Common_Lisp.rss" common/lisp)
        ("https://www.reddit.com/r/lisp.rss" lisp)
        ("https://www.reddit.com/r/emacs.rss" emacs)
        ("https://news.ycombinator.com/rss" yc/news)
        ("https://tim.blog/feed/" tim-ferris/blog)
        ("https://signalvnoise.com/posts.rss" signal/noise)))
;; Use ace jump zap to char instead of normal zap to char
(global-unset-key (kbd "M-z"))
(global-set-key (kbd "M-z") 'ace-jump-zap-to-char)
;; move-text moves lines of text up or down
(move-text-default-bindings)
;; add hook to highlight parenthesis
(add-hook 'prog-mode-hook 'highlight-parentheses-mode)
;; change glyph showing lines are truncated
(set-display-table-slot standard-display-table 0 ?\|)
;; change how URLS are browsed
(setq browse-url-browser-function '(("http://www.lispworks.com/reference/HyperSpec/.*" . eww-browse-url)
                                    ("file:///.*HyperSpec.*" . eww-browse-url)
                                    ("." . browse-url-default-browser)))
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
;; load develop files
(load "~/.emacs.d/develop/_lisp")
(load "~/.emacs.d/develop/_python")
(load "~/.emacs.d/develop/_c")
(load "~/.emacs.d/develop/_yaml")
(load "~/.emacs.d/develop/_tex")
(load "~/.emacs.d/develop/_php")
(load "~/.emacs.d/develop/_sql")
