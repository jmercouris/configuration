(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(beacon-blink-delay 0.0)
 '(beacon-blink-duration 0.2)
 '(beacon-blink-when-window-scrolls nil)
 '(beacon-mode t)
 '(company-idle-delay 0.25)
 '(dired-sidebar-display-alist (quote ((side . right) (slot . -1))))
 '(disable-mouse-mode-global-lighter "")
 '(frame-resize-pixelwise t)
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(gnutls-trustfiles
   (quote
    ("/etc/ssl/certs/ca-certificates.crt" "/etc/pki/tls/certs/ca-bundle.crt" "/etc/ssl/ca-bundle.pem" "/usr/ssl/certs/ca-bundle.crt" "/usr/local/share/certs/ca-root-nss.crt" "/private/etc/ssl/cert.pem")))
 '(ibuffer-sidebar-display-alist (quote ((side . right) (slot . 1))))
 '(imenu-list-size 25)
 '(mode-line-format
   (quote
    ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position smartrep-mode-line-string "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)))
 '(org-directory "/Users/jmercouris/Documents/Organization/")
 '(org-projectile-capture-template "* TASK %?
")
 '(org-projectile-per-project-filepath "README.org")
 '(org-projectile-projects-file "/Users/jmercouris/Documents/Organization/projects.org")
 '(package-selected-packages
   (quote
    (htmlize fish-mode org-projectile nginx-mode highlight-parentheses tldr makefile-executor edbi-database-url ibuffer-sidebar dired-sidebar w3m move-text vlf ace-jump-zap slime-company elfeed edbi edbi-sqlite jinja2-mode beacon sphinx-doc php-mode date-at-point pyvenv hungry-delete popwin magit-gh-pulls gist imenu-list soap-client popup json-navigator paredit use-package highlight-indentation dired+ framemove yaml-mode list-processes+ anaconda-mode company-anaconda diminish expand-region flycheck disable-mouse smartparens yasnippet multiple-cursors counsel-projectile projectile peep-dired flx counsel flyspell-correct-ivy browse-kill-ring imenu-anywhere py-isort which-key json-mode realgud exec-path-from-shell avy switch-window restclient multi-term web-mode undo-tree markdown-mode magit latex-preview-pane hydra circe auctex)))
 '(popwin-mode t)
 '(popwin:popup-window-height 30)
 '(safe-local-variable-values
   (quote
    ((Package . CL-FAD)
     (Package . FLEXI-STREAMS)
     (Package . CL-PPCRE)
     (Package . CL-INTERPOL)
     (Package . CL-UNICODE)
     (Package . CL-USER)
     (Syntax . COMMON-LISP)
     (Base . 10)
     (Syntax . ANSI-Common-Lisp))))
 '(send-mail-function (quote smtpmail-send-it))
 '(sml-modeline-borders nil)
 '(sml-modeline-mode t)
 '(sml-modeline-numbers (quote percentage))
 '(window-resize-pixelwise t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 125 :width normal :foundry "nil" :family "Menlo"))))
 '(circe-prompt-face ((t (:background "textBackgroundColor" :foreground "Black" :weight bold))))
 '(font-lock-comment-face ((t (:foreground "gray55"))))
 '(font-lock-doc-face ((t (:foreground "gray40"))))
 '(font-lock-function-name-face ((t (:foreground "green4"))))
 '(font-lock-keyword-face ((t (:foreground "royal blue"))))
 '(font-lock-string-face ((t (:foreground "dark red"))))
 '(font-lock-variable-name-face ((t (:foreground "green3"))))
 '(highlight-indentation-face ((t (:background "gray95"))))
 '(hiwin-face ((t (:background "gray95"))))
 '(mode-line ((t (:background "gray55" :foreground "White" :box nil))))
 '(neo-header-face ((t (:foreground "green3"))))
 '(neo-root-dir-face ((t (:foreground "green3"))))
 '(org-level-2 ((t (:foreground "royal blue"))))
 '(org-level-3 ((t (:inherit outline-3 :foreground "cadet blue"))))
 '(org-todo ((t (:foreground "RoyalBlue2"))))
 '(show-paren-match ((t (:background "SeaGreen1"))))
 '(sml-modeline-end-face ((t (:background "gray70"))))
 '(sml-modeline-vis-face ((t (:background "gray55"))))
 '(term-color-white ((t (:background "white" :foreground "light green")))))
