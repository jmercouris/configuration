;; Mode specific hydras are activated always by s-h (cmd + h)
(global-unset-key (kbd "s-h"))

;; Hydra Desktop
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

;; Hydra Zoom
(defhydra hydra-zoom (:columns 2)
  "Zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out")
  ("0" (text-scale-adjust 0) "reset")
  ("q" nil "quit" :color blue))
;; Assign Hydra to hotkey
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'hydra-zoom/body)

;; Hydra Magit
(defhydra hydra-magit (:color blue :columns 2)
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
(global-set-key (kbd "s-m") 'hydra-magit/body)

;; Hydra Org
(defhydra hydra-org (:color red :hint nil)
  "
Navigation^
---------------------------------------------------------
_j_ next heading
_k_ prev heading
_h_ next heading (same level)
_l_ prev heading (same level)
_u_p higher heading
_g_o to
"
  ("j" outline-next-visible-heading)
  ("k" outline-previous-visible-heading)
  ("h" org-forward-heading-same-level)
  ("l" org-backward-heading-same-level)
  ("u" outline-up-heading)
  ("g" org-goto :exit t))

(defhydra hydra-window ()
   "
Movement^^       ^Split^           ^Switch^        ^Resize^
----------------------------------------------------------------
_j_ ←           _v_ertical         _b_uffer        _u_ X←
_k_ ↓           _x_ horizontal     _f_ind files    _i_ X↓
_l_ ↑           _1_only this       _s_wap          _o_ X↑
_;_ →           _d_elete                         _p_ X→
_F_ollow                   
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

;; Hydra iBuffer
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

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

;; Hydra Avy
(defhydra hydra-avy (:color blue)
  "Avy-Goto"
  ("c" avy-goto-char "char")
  ("C" avy-goto-char-2 "char-2")
  ("w" avy-goto-word-1 "word")
  ("s" avy-goto-subword-1 "subword")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-."))
(global-set-key (kbd "s-.") 'hydra-avy/body)

;; Hydra Restclient
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

;; Hydra Python
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
