(define-minor-mode centered-point-mode
  "Alaways center the cursor in the middle of the screen."
  :lighter "..."
  (cond (centered-point-mode (add-hook 'post-command-hook 'line-change))
	(t (remove-hook 'post-command-hook 'line-change)))
  )
 
(defun line-change ()
  (recenter)
  )
