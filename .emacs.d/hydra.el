;; Mode specific hydras are activated always by s-h (cmd + h)
(global-unset-key (kbd "s-h"))

;; desktop
(defhydra hydra-desktop (:color blue :columns 1)
  "Desktop"
  ("c" desktop-clear "clear")
  ("s" desktop-save "save")
  ("r" desktop-revert "revert")
  ("o" desktop-change-dir "open")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-z"))
(global-set-key (kbd "s-z") 'hydra-desktop/body)

;; zoom
(defhydra hydra-zoom (:columns 2)
  "Zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))
;; Assign Hydra to hotkey
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'hydra-zoom/body)

;; vc
(defhydra hydra-vc (:color blue :columns 2)
  "Version Control"
  ("s" magit-status "status")
  ("h" vc-print-log "history")
  ("b" magit-blame "blame")
  ("c" magit-checkout "checkout")
  ("m" magit-merge "merge")
  ("l" magit-log "magit log")
  ("d" vc-diff "diff")
  ("!" magit-git-command "command")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-m"))
(global-set-key (kbd "s-m") 'hydra-vc/body)

;; org
(defhydra hydra-org (:color red :hint nil)
  "
Navigation^                 Operations
---------------------------------------------------------
_j_ next heading             ^_s_ort
_k_ prev heading
_h_ next heading (=level)
_l_ prev heading (=level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("s" org-sort)
  ("g" org-goto :exit t)
  ("q" nil))

;; Assign hydra to hotkey when in org mode
(eval-after-load "org"
  '(progn
  (define-key org-mode-map (kbd "s-h") 'hydra-org/body)))

;; window movement / management
(defhydra hydra-window (:hint nil)
   "
Movement^^       ^Split^           ^Switch^        ^Resize^
----------------------------------------------------------------
_j_ ←           _v_ertical         _b_uffer        _u_ X←
_k_ ↓           _x_ horizontal     _f_ind files    _i_ X↓
_l_ ↑           _1_only this       _s_wap          _o_ X↑
_;_ →           _d_elete                         _p_ X→
_F_ollow        _e_qualize           
_q_ quit                    
"
   ("j" windmove-left )
   ("k" windmove-down )
   ("l" windmove-up )
   (";" windmove-right )
   ("u" hydra-move-splitter-left)
   ("i" hydra-move-splitter-down)
   ("o" hydra-move-splitter-up)
   ("p" hydra-move-splitter-right)
   ("b" ivy-switch-buffer)
   ("f" counsel-find-file)
   ("F" follow-mode)
   ("s" switch-window-then-swap-buffer) 
       
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("d" delete-window)
   ("1" delete-other-windows)
   ("e" balance-windows)
   ("q" nil)
   )
(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))
(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))
(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))
(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-w"))
(global-set-key (kbd "s-w") 'hydra-window/body)

;; iBuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _p_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _n_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("n" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("p" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))
;; Assign hydra to hotkey when in ibuffer mode
(eval-after-load "ibuffer"
  '(progn
  (define-key ibuffer-mode-map (kbd "s-h") 'hydra-ibuffer-main/body)))

;; avy
(defhydra hydra-avy (:color blue)
  "Avy-Goto"
  ("c" avy-goto-char "char")
  ("C" avy-goto-char-2 "char-2")
  ("w" avy-goto-word-1 "word")
  ("s" avy-goto-subword-1 "subword")
  ("l" avy-goto-line "line")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-."))
(global-set-key (kbd "s-.") 'hydra-avy/body)

;; Restclient
(defhydra hydra-restclient (:columns 2)
  "Restclient"
  ("n" restclient-jump-next "next")
  ("p" restclient-jump-prev "previous")
  ("RET" restclient-http-send-current-stay-in-window "execute")
  ("x" restclient-http-send-current "execute response focus")
  ("q" nil "quit"))
;; Assign hydra to hotkey when in restclient mode
(eval-after-load "restclient"
  '(progn
  (define-key restclient-mode-map (kbd "s-h") 'hydra-restclient/body)))

;; Python
(defhydra hydra-python (:columns 4)
  "Python"
  ("s" py-isort-region "sort imports")
  ("f" elpy-format-code "format code")
  ("i" indent-for-tab-command "indent")
  ("j" elpy-goto-definition "push definition")
  ("k" pop-tag-mark "pop definition")
  ("d" elpy-doc "doc")
  ("o" elpy-occur-definitions "outline")
  ("l" elpy-shell-switch-to-shell "shell")
  ("n" end-of-defun "next func")
  ("p" beginning-of-defun "prev func")
  ("q" nil "quit"))
;; Assign hydra to hotkey when in python mode
(eval-after-load "python"
  '(progn
  (define-key python-mode-map (kbd "s-h") 'hydra-python/body)))

;; projectile
(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   counsel-projectile-ag)
  ("b"   counsel-projectile-switch-to-buffer)
  ("c"   counsel-projectile-invalidate-cache)
  ("d"   counsel-projectile-find-dir)
  ("s-f" counsel-projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   counsel-projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("o"   projectile-multi-occur)
  ("s-p" counsel-projectile-switch-project "switch project")
  ("p"   counsel-projectile-switch-project)
  ("s"   counsel-projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   counsel-projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-f"))
(global-set-key (kbd "s-f") 'hydra-projectile/body)

;; dired
(defhydra hydra-dired (:color blue :columns 1)
  "Dired"
  ("v" peep-dired "peep")
  ("n" dired-next-line "next line")
  ("p" dired-previous-line "previous line")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "s-h") 'hydra-dired/body) ))

;; gnus
(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-gnus-group (:color blue :columns 1)
       "Do?"
       ("r" gnus-group-list-active "REMOTE groups A A")
       ("l" gnus-group-list-all-groups "LOCAL groups L")
       ("c" gnus-topic-catchup-articles "Read all c")
       ("G" gnus-group-make-nnir-group "Search server G G")
       ("g" gnus-group-get-new-news "Refresh g")
       ("s" gnus-group-enter-server-mode "Servers")
       ("m" gnus-group-new-mail "Compose m OR C-x m")
       ("#" gnus-topic-mark-topic "mark #")
       ("a" hydra-switch-account/body "Switch Sending Account")
       ("q" nil "cancel"))

     (define-key gnus-group-mode-map (kbd "s-h") 'hydra-gnus-group/body)))

;; gnus-summary-mode
(eval-after-load 'gnus-sum
  '(progn
     (defhydra hydra-gnus-summary (:color blue :columns 1)
       "Do?"
       ("s" gnus-summary-show-thread "Show thread")
       ("h" gnus-summary-hide-thread "Hide thread")
       ("n" gnus-summary-insert-new-articles "Refresh / N")
       ("f" gnus-summary-mail-forward "Forward C-c C-f")
       ("!" gnus-summary-tick-article-forward "Mail -> disk !")
       ("p" gnus-summary-put-mark-as-read "Mail <- disk")
       ("c" gnus-summary-catchup-and-exit "Read all c")
       ("e" gnus-summary-resend-message-edit "Resend S D e")
       ("R" gnus-summary-reply-with-original "Reply with original R")
       ("r" gnus-summary-reply "Reply r")
       ("W" gnus-summary-wide-reply-with-original "Reply all with original S W")
       ("w" gnus-summary-wide-reply "Reply all S w")
       ("#" gnus-topic-mark-topic "mark #")
       ("a" hydra-switch-account/body "Switch Sending Account")
       ("q" nil "cancel"))

     (define-key gnus-summary-mode-map (kbd "s-h") 'hydra-gnus-summary/body)))

;; gnus-article-mode
(eval-after-load 'gnus-art
  '(progn
     (defhydra hydra-gnus-article (:color blue :columns 1)
       "Do?"
       ("f" gnus-summary-mail-forward "Forward")
       ("R" gnus-article-reply-with-original "Reply with original R")
       ("r" gnus-article-reply "Reply r")
       ("W" gnus-article-wide-reply-with-original "Reply all with original S W")
       ("o" gnus-mime-save-part "Save attachment at point o")
       ("w" gnus-article-wide-reply "Reply all S w")
       ("a" hydra-switch-account/body "Switch Sending Account")
       ("q" nil "cancel"))

     (define-key gnus-article-mode-map (kbd "s-h") 'hydra-gnus-article/body)))

;; message mode
(eval-after-load 'message
  '(progn
     (defhydra hydra-message (:color blue :columns 1)
       "Do?"
       ("ca" mml-attach-file "Attach C-c C-a")
       ("cc" message-send-and-exit "Send C-c C-c")
       ("q" nil "cancel"))
     (define-key message-mode-map (kbd "s-h") 'hydra-message/body)))

;; switch account hydra
(eval-after-load 'gnus-group
  '(progn
     (defhydra hydra-switch-account (:color blue :columns 1)
     "Switch Account"
     ("h" set-email-home "Set Email Home")
     ("w" set-email-work "Set Email Work"))))

;; hydra rectangle
(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_p_^       _w_ copy      _o_pen       _N_umber-lines
_b_   _f_     _y_ank        _t_ype       _e_xchange-point
  ^_n_^       _d_ kill      _c_lear      _r_eset-region-mark
^^^^          _u_ndo        _q_ quit
"
  ("p" rectangle-previous-line)
  ("n" rectangle-next-line)
  ("b" rectangle-backward-char)
  ("f" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("q" nil))
;; assign hydra to hotkey
(global-unset-key (kbd "s-s"))
(global-set-key (kbd "s-s") 'hydra-rectangle/body)

;; hydra browse
(defhydra hydra-browse (:color blue :columns 2)
  "Browse"
  ("n" narrow-to-defun "narrow")
  ("w" widen "widen")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-b"))
(global-set-key (kbd "s-b") 'hydra-browse/body)
