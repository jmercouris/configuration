(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)
;; use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;; load osx specific things
(when (memq window-system '(mac ns))
  (load "~/.emacs.d/osx.el"))
;; set default shell to bash for rgrep
(setq shell-file-name "/bin/sh")
;; allow upcase region
(put 'upcase-region 'disabled nil)
;; undo tree mode
(undo-tree-mode 1)
;; enable narrow-to-region mode (C-x-n-n)
(put 'narrow-to-region 'disabled nil)
;; hs mode
(load-library "hideshow")
(global-set-key (kbd "C-=") 'hs-toggle-hiding)
;; rebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
;; truncate lines by default
(set-default 'truncate-lines t)
;; disable mouse
(global-disable-mouse-mode)
;; disable tool tip mode
(tooltip-mode 0)
;; all back up files into same systemwide temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; setup yasnippet
(require 'yasnippet)
(yas-global-mode 1)
;; peep-dired kill buffers on disabling of minor mode
(setq peep-dired-cleanup-on-disable t)
(setq peep-dired-ignored-extensions '("pyc"))
;; neotree window position
(setq neo-window-position 'right)
;; neotree use ascii instead of folder icons
(setq neo-theme 'ascii)
;; neotree ignore specific folders
(setq neo-hidden-regexp-list '("^\\." "\\.cs\\.meta$" "\\.pyc$" "~$" "^#.*#$" "__pycache__"))
;; set eshell prompt
(setq eshell-prompt-function
      (lambda nil "> "))
;; column
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
;; multi-term configuration
(global-unset-key (kbd "s-t"))
(when (require 'multi-term nil t)
  (global-set-key (kbd "s-t") 'multi-term)
  (global-set-key (kbd "s-}") 'multi-term-next)
  (global-set-key (kbd "s-{") 'multi-term-prev))
;; switch window behavior uses switch-window package
(global-set-key (kbd "C-x o") 'switch-window)
(setq switch-window-qwerty-shortcuts (quote ("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))
(setq switch-window-shortcut-style (quote qwerty))
;; switch window kill window
(global-set-key (kbd "C-x w") 'switch-window-then-delete)
;; magit setup
(setq magit-completing-read-function 'ivy-completing-read)
;; previous and Next Buffer
(global-unset-key (kbd "s-n"))
(global-set-key (kbd "s-n") 'next-buffer)
(global-unset-key (kbd "s-p"))
(global-set-key (kbd "s-p") 'previous-buffer)
;; kill current buffer
(global-set-key (kbd "s-d") 'kill-this-buffer)
;; windmove
(global-set-key (kbd "s-j") 'windmove-left)
(global-set-key (kbd "s-k") 'windmove-down)
(global-set-key (kbd "s-l") 'windmove-up)
(global-set-key (kbd "s-;") 'windmove-right)
;; framemove
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
;; .http files load rest-client mode
(add-to-list 'auto-mode-alist '("\\.http$" . restclient-mode))
;; auto dim other buffers
(auto-dim-other-buffers-mode t)
;; minor mode lighter sets to diminish in mode-line
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "anaconda-mode" '(diminish 'anaconda-mode))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "magit" '(diminish 'auto-revert-mode))
(eval-after-load "ivy" '(diminish 'ivy-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "back-button" '(diminish 'back-button-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "auto-dim-other-buffers" '(diminish 'auto-dim-other-buffers-mode))
(eval-after-load "highlight-indentation" '(diminish 'highlight-indentation-mode))
(eval-after-load "disable-mouse" '(diminish 'global-disable-mouse-mode))
(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
;; don't open new windows for these buffers
(add-to-list 'same-window-buffer-names "*wclock*")
;; which key prompts on C-x etc
(which-key-mode)
(which-key-setup-minibuffer)
;; use ibuffer instead of regular buffer list
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-expert t)
;; imenu anywhere binding
(global-set-key (kbd "C-.") 'ivy-imenu-anywhere)
;; imenu binding
(global-set-key (kbd "C->") 'imenu-reposition)
;; use browse-kill ring as the default for M-y
(browse-kill-ring-default-keybindings)
;; down case region
(put 'downcase-region 'disabled nil)
;; org configuration
(setq org-startup-indented t)
(setq org-log-done t)
(setq org-return-follows-link t)
(setq org-todo-keywords
      '((sequence "TODO" "EXEC" "WAIT" "APRV" "DONE")))
(setq org-todo-keyword-faces
      '(("WAIT" . "gray") ("APRV" . "green")))
(setq org-agenda-files (list "~/.root.org"
                             "~/Documents/Academic/.academic.org"
			     "~/Projects/.projects.org"
			     "~/Work/.work.org"))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))
(setq org-src-fontify-natively t)
;; projectile
(projectile-global-mode)
(setq projectile-enable-caching t)
(counsel-projectile-on)
;; set world time list
(setq display-time-world-list
      (quote
       (("America/Chicago" "Chicago")
	("Europe/Berlin" "Berlin")
	("Europe/Athens" "Athens")
	("Europe/London" "London")
	("America/Los_Angeles" "San Francisco")
	("America/Argentina/Buenos_Aires" "Buenos Aires"))))
;; yes or no to y or n
(defalias 'yes-or-no-p 'y-or-n-p)
;; occur mode n-p
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)
;; back button mode
(back-button-mode 1)
;; smart parens mode
(show-smartparens-global-mode +1)
;; docview mode continuous
(setq doc-view-continuous t)
;; auto-rename new eww buffers
(defun rename-eww-hook ()
  "Rename eww browser's buffer so sites open in new page."
  (rename-buffer "eww" t))
(add-hook 'eww-mode-hook #'rename-eww-hook)
;; set custom file
(setq custom-file "~/.emacs.d/custom.el")
;; load Additional Files
(load "~/.emacs.d/theme")
(load "~/.emacs.d/custom")
(load "~/.emacs.d/private")
(load "~/.emacs.d/irc")
(load "~/.emacs.d/ivy")
(load "~/.emacs.d/hydra")
(load "~/.emacs.d/multi-smtp")
(load "~/.emacs.d/functions")
;; load Develop Files
(load "~/.emacs.d/develop/_python")
(load "~/.emacs.d/develop/_c")
(load "~/.emacs.d/develop/_lisp")
(load "~/.emacs.d/develop/_yaml")
(load "~/.emacs.d/develop/_tex")
(load "~/.emacs.d/develop/_json")
