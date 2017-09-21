;; Mode specific hydras are activated always by s-h (cmd + h)
(global-unset-key (kbd "s-h"))

;; desktop
(defhydra hydra-desktop (:color blue :columns 1)
  "Desktop"
  ("c" desktop-clear "clear")
  ("S" desktop-save "save")
  ("s" desktop-save-current-dir "save current directory")
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

;; timeclock
(defhydra hydra-timeclock (:color blue :hint nil)
  "
Clock^^         Report
---------------------------------------------------------
log_i_n         generate _r_eport
log_o_ut        visit _l_og file
^              _w_orkday remaining
"
  ("i" timeclock-in)
  ("o" timeclock-out)
  ("r" timeclock-generate-report)
  ("l" timeclock-visit-timelog)
  ("w" timeclock-workday-remaining-string)
  ("q" nil))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-a"))
(global-set-key (kbd "s-a") 'hydra-timeclock/body)

;; org
(defhydra hydra-org (:color red :hint nil)
  "
Navigation^                 Operations      Configuration
---------------------------------------------------------
_f_ next heading             _s_ort            in_d_ent mode
_b_ prev heading             _i_nsert url
_n_ next heading (=level)    _a_rchive
_p_ prev heading (=level)    _c_opy url
_u_p higher heading          e_x_ecute src
_g_o to
"
  ("f" outline-next-visible-heading)
  ("b" outline-previous-visible-heading)
  ("n" org-forward-heading-same-level)
  ("p" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("s" org-sort)
  ("i" org-insert-link)
  ("g" org-goto :exit t)
  ("d" org-indent-mode)
  ("a" org-archive-subtree-default)
  ("c" org-retrieve-url-from-point)
  ("x" org-babel-execute-src-block)
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
_j_ ←           _v_ertical         _b_uffer        _u_ ←
_k_ ↓           _h_orizontal       _f_ind files    _i_ ↓
_l_ ↑           _1_only this       _P_rojectile    _o_ ↑
_;_ →           _d_elete           _s_wap          _p_ →
_F_ollow        _e_qualize         ^ ^             _8_0 columns
_q_uit                    
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
   ("P" counsel-projectile-find-file)
   ("F" follow-mode)
   ("s" switch-window-then-swap-buffer)
   ("8" set-80-columns)
   ("v" split-window-right)
   ("h" split-window-below)
   ("3" split-window-right)
   ("2" split-window-below)
   ("d" delete-window)
   ("1" delete-other-windows)
   ("e" balance-windows)
   ("q" nil))

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
  ("q" hydra-ibuffer-main/body "back" :color blue))

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
  ("q" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("q" hydra-ibuffer-main/body "back" :color blue))

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
  ("q" hydra-ibuffer-main/body "back" :color blue))
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
  ("." avy-goto-word-1)
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
(defhydra hydra-python (:color blue :hint nil)
  "
    Navigation          Formatting         Shell
---------------------------------------------------------------------
   Documentation_?_       _s_ort imports       shel_l_
   Definitions_._         _i_ndent             send regio_n_
   _a_ssignments          _f_lycheck error     _t_est
   _r_eferences           _r_emove unused
   _o_utline
   _c_lass outline

"
  ("s" py-isort-region)
  ("?" anaconda-mode-show-doc)
  ("." anaconda-mode-find-definitions :color red)
  ("a" anaconda-mode-find-assignments)
  ("r" anaconda-mode-find-references)
  ("i" indent-for-tab-command)
  ("l" python-shell)
  ("n" python-shell-send-region)
  ("o" python-outline)
  ("c" python-class-outline)
  ("f" flycheck-next-error :color red)
  ("t" python-django-test)
  ("r" pyrm)
  ("q" nil "quit"))
;; Assign hydra to hotkey when in python mode
(eval-after-load "python"
  '(progn
  (define-key python-mode-map (kbd "s-h") 'hydra-python/body)))

;; projectile
(defhydra hydra-projectile (:color blue
                            :hint nil)
  "
    Projectile: %(projectile-project-root)

    Find File          Search/Tags         Cache
---------------------------------------------------------------------
_s-f_: file            _g_: git grep        _c_: cache clear
 _fd_: file curr dir   _o_: multi-occur     _x_: remove known project
  _d_: dir                                ^^_X_: cleanup non-existing
                                        ^^_z_: cache current
                                        ^^_k_: uncache current

"
  ("c"   counsel-projectile-invalidate-cache)
  ("d"   counsel-projectile-find-dir)
  ("s-f" counsel-projectile-find-file)
  ("fd"  projectile-find-file-in-directory)
  ("g"   counsel-git-grep)
  ("s-g" ggtags-update-tags)
  ("K"   counsel-projectile-kill-buffers)
  ("o"   projectile-multi-occur)
  ("s-p" counsel-projectile-switch-project "switch project")
  ("p"   counsel-projectile-switch-project)
  ("s"   counsel-projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   counsel-projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("k"   projectile-purge-file-from-cache)
  ("q"   nil "cancel" :color blue))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-f"))
(global-set-key (kbd "s-f") 'hydra-projectile/body)

;; dired
(defhydra hydra-dired (:color blue :columns 1)
  "Dired"
  ("s" peep-dired "show preview")
  ("n" dired-next-line "next line" :color red)
  ("p" dired-previous-line "previous line" :color red)
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
       ("d" gnus-summary-delete-article "Delete Article")
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
  ^_n_^       _k_ill        _c_lear      _r_eset-region-mark
^^^^          _u_ndo        _q_ quit
"
  ("p" rectangle-previous-line)
  ("n" rectangle-next-line)
  ("b" rectangle-backward-char)
  ("f" rectangle-forward-char)
  ("k" kill-rectangle)                    ;; C-x r k
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

;; hydra browse
(defhydra hydra-browse (:color red :hint nil)
  "
Ring^^^                  Hiding^^          Navigation
---------------------------------------------------------
_f_orward  local        _nd_efun          _t_oggle tree
_b_ackward local        _nr_egion         _r_efresh point
_gf_global forward      _w_iden           _d_ired
_gb_global backward     _h_ide mode       _i_menu list
^^                      _s_how toggle
^^                      show _a_ll
_q_uit
"
  ("f" back-button-local-forward)
  ("b" back-button-local-backward)
  ("gf" back-button-global-forward)
  ("gb" back-button-global-backward)

  ("nd" narrow-to-defun)
  ("nr" narrow-to-region)
  ("w" widen)
  ("h" hs-minor-mode)
  ("s" hs-toggle-hiding)
  ("a" hs-show-all)

  ("t" neotree-toggle)
  ("r" neotree-find)
  ("d" dired)
  ("i" imenu-list-minor-mode)
  ("q" nil))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-b"))
(global-set-key (kbd "s-b") 'hydra-browse/body)

;; hydra artist-mode
(defhydra hydra-artist-mode (:color blue :columns 2)
  "Draw"
  ("p" artist-select-op-pen-line "pen")
  ("l" artist-select-op-line "line")
  ("s" artist-select-op-square "square")
  ("e" artist-select-op-ellipse "ellipse")
  ("z" undo "undo")
  ("q" nil "quit"))
;; Assign hydra to hotkey when in artist mode
(eval-after-load "artist"
  '(progn
  (define-key artist-mode-map (kbd "s-h") 'hydra-artist-mode/body)))

;; hydra eww-mode
(defhydra hydra-eww (:color blue :columns 2)
  "Browse"
  ("r" eww-readable "readable")
  ("d" eww-download "download")
  ("l" eww-list-buffers "list buffers")
  ("g" eww-reload "reload")
  ("x" eww-browse-with-external-browser "external browse")
  ("b" eww-back-url "backward")
  ("f" eww-forward-url "forward")
  ("q" nil "quit"))
;; Assign hydra to hotkey when in artist mode
(eval-after-load "eww"
  '(progn
  (define-key eww-mode-map (kbd "s-h") 'hydra-eww/body)))

;; hydra info-mode
(defhydra hydra-info (:color blue :columns 2)
  "Buffer Info"
  ("f" copy-buffer-file-name-as-kill  "buffer file name")
  ("w" count-words "count words/lines")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-i"))
(global-set-key (kbd "s-i") 'hydra-info/body)

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_d_] Mark all defun
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("d" mc/mark-all-like-this-in-defun :exit t)
  ("q" nil))

;; hydra edit mode
(defhydra hydra-edit (:color blue :columns 2)
  "Edit"
  ("r" hydra-rectangle/body "rectangle")
  ("m" hydra-multiple-cursors/body "multiple cursors")
  ("e" er/expand-region "expand region")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-e"))
(global-set-key (kbd "s-e") 'hydra-edit/body)

;; hydra circe mode
(defhydra hydra-irc (:color red :columns 2)
  "IRC"
  ("r" circe-reconnect "reconnect")
  ("n" tracking-next-buffer "next unread buffer")
  ("q" nil "quit"))
;; Assign hydra to hotkey when in circe mode
(eval-after-load "circe"
  '(progn
  (define-key circe-mode-map (kbd "s-h") 'hydra-irc/body)))

;; hydra term mode
(defhydra hydra-term (:color blue :columns 2)
  "Term"
  ("l" term-line-mode "term-line mode")
  ("c" term-char-mode "term-char mode")
  ("q" nil "quit"))
;; Assign hydra to hotkey when in term mode
(eval-after-load "multi-term"
  '(progn
     (define-key term-raw-map (kbd "s-h") 'hydra-term/body)
     (define-key term-mode-map (kbd "s-h") 'hydra-term/body)))

;; hydra json mode
(defhydra hydra-json (:color blue :columns 2)
  "Json"
  ("f" json-mode-beautify "format")
  ("p" json-mode-show-path "show path")
  ("n" json-navigator-navigate-region "navigate region")
  ("q" nil "quit"))
(eval-after-load "json-mode"
  '(progn
  (define-key json-mode-map (kbd "s-h") 'hydra-json/body)))

;; Lisp
(defhydra hydra-lisp (:color blue :hint nil)
  "
    Navigation     Formatting      Shell
---------------------------------------------------------------------
   _d_ocumentation   check _p_arens    shel_l_
                                   ^^^^_e_val region
                                   ^^^^load _f_ile
"
  ("d" slime-documentation-lookup)
  ("l" slime-repl)
  ("e" slime-eval-region)
  ("f" slime-load-file)
  ("p" check-parens)
  ("q" nil "quit"))
;; Assign hydra to hotkey when in python mode
(eval-after-load "lisp-mode"
  '(progn
  (define-key lisp-mode-map (kbd "s-h") 'hydra-lisp/body)))
