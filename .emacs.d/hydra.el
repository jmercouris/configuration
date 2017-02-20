(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("=" text-scale-increase "in")
  ("-" text-scale-decrease "out"))

(defhydra hydra-magit (:color blue :columns 8)
  "Magit"
  ("c" magit-status "status")
  ("C" magit-checkout "checkout")
  ("v" magit-branch-manager "branch manager")
  ("m" magit-merge "merge")
  ("l" magit-log "log")
  ("!" magit-git-command "command")
  ("$" magit-process "process"))

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
_h_ ←           _v_ertical         _b_uffer        _y_ X←
_j_ ↓           _x_ horizontal     _f_ind files    _u_ X↓
_k_ ↑           _1_only this       _s_wap          _i_ X↑
_l_ →           _d_elete                         _o_ X→
_F_ollow                   
_q_ quit                    
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("y" hydra-move-splitter-left)
   ("u" hydra-move-splitter-down)
   ("i" hydra-move-splitter-up)
   ("o" hydra-move-splitter-right)
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
