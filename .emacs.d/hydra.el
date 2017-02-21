(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

(defhydra hydra-magit (:color blue :columns 1)
  "Magit"
  ("s" magit-status "status")
  ("c" magit-checkout "checkout")
  ("m" magit-merge "merge")
  ("l" magit-log "log")
  ("!" magit-git-command "command")
  ("$" magit-process "process")
  ("q" nil "quit"))

;; Assign Hydra to hotkey
(global-unset-key (kbd "s-m"))
(global-set-key (kbd "s-m") 'hydra-magit/body)

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
