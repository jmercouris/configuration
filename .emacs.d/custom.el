(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-mode t)
 '(frame-resize-pixelwise t)
 '(imenu-list-size 25)
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position smartrep-mode-line-string "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(org-agenda-files
   (quote
    ("~/Work/.work.org" "~/.root.org" "~/Documents/Academic/.academic.org" "~/Projects/.projects.org")))
 '(package-selected-packages
   (quote
    (popwin magit-gh-pulls slime-company gist qt-pro-mode imenu-list toc-org soap-client popup json-navigator paredit use-package highlight-indentation dired+ framemove yaml-mode list-processes+ anaconda-mode company-anaconda highlight-indent-guides highlight-cl chronos diminish expand-region flycheck disable-mouse smartparens yasnippet back-button multiple-cursors auto-dim-other-buffers counsel-projectile projectile peep-dired flx counsel flyspell-correct-ivy browse-kill-ring imenu-anywhere py-isort which-key json-mode realgud exec-path-from-shell avy switch-window restclient find-file-in-repository multi-term web-mode undo-tree neotree markdown-mode magit latex-preview-pane kivy-mode hydra circe auctex)))
 '(safe-local-variable-values (quote ((Base . 10) (Syntax . ANSI-Common-Lisp))))
 '(send-mail-function (quote smtpmail-send-it))
 '(window-resize-pixelwise t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "nil" :family "Menlo"))))
 '(auto-dim-other-buffers-face ((t (:background "gray85"))))
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
 '(org-level-2 ((t (:foreground "royal blue"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "gray40"))))
 '(show-paren-match ((t (:background "SeaGreen1"))))
 '(term-color-white ((t (:background "white" :foreground "light green")))))
