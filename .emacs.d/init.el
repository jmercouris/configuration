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
;; show parenthesis pairing
(show-paren-mode 1)
;; undo tree mode
(undo-tree-mode 1)
;; rebind comment region
(global-set-key (kbd "M-/") 'comment-or-uncomment-region)
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
;; python configuration
(package-initialize)
(elpy-enable)
(setq realgud:pdb-command-name "python -m pdb")
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
;; neotree use ascii instead of folder icons
(setq neo-theme 'ascii)
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
;; add Macports Path
(setq exec-path (append exec-path '("/opt/local/bin")))
;; column
(setq column-number-mode t)
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
(setq switch-window-qwerty-shortcuts (quote ("a" "s" "d" "f" "j" "k" "l" ";" "w" "e" "i" "o")))
(setq switch-window-shortcut-style (quote qwerty))
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
;; which key prompts on C-x etc
(which-key-mode)
(which-key-setup-minibuffer)
;; use ibuffer instead of regular buffer list
(defalias 'list-buffers 'ibuffer)
(setq ibuffer-expert t)
;; imenu anywhere binding
(global-set-key (kbd "C-.") 'imenu-anywhere)
;; imenu binding
(global-set-key (kbd "C->") 'imenu)
;; use browse-kill ring as the default for M-y
(browse-kill-ring-default-keybindings)
;; down case region
(put 'downcase-region 'disabled nil)
;; org configuration
(setq org-log-done t)
(setq org-return-follows-link t)
(setq org-agenda-files (list "~/.root.org"
                             "~/Documents/Academic/.academic.org"
			     "~/Projects/.projects.org"
			     "~/Work/.work.org"))
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
