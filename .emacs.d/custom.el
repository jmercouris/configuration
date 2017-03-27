(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(ivy-completing-read-handlers-alist
   (quote
    ((tmm-menubar . completing-read-default)
     (tmm-shortcut . completing-read-default)
     (elpy-doc . completing-read-default))))
 '(org-agenda-files
   (quote
    ("~/Work/.work.org" "~/.root.org" "~/Documents/Academic/.academic.org" "~/Projects/.projects.org")))
 '(package-selected-packages
   (quote
    (multiple-cursors auto-dim-other-buffers counsel-projectile projectile peep-dired flx counsel flyspell-correct-ivy browse-kill-ring imenu-anywhere py-isort which-key json-mode realgud exec-path-from-shell elpy avy switch-window restclient find-file-in-repository multi-term web-mode undo-tree neotree markdown-mode magit latex-preview-pane kivy-mode jinja2-mode hydra circe auctex))))
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
