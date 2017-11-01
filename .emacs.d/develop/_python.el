(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'smartparens-mode)


(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))

(setq realgud:pdb-command-name "python -m pdb")

(setq python-shell-prompt-detect-failure-warning nil)

(defun python-shell ()
    (interactive)
  (run-python)
  (python-shell-switch-to-shell))

(defun python-outline ()
  (interactive)
  (occur "class \\|def "))

(defun python-class-outline ()
  (interactive)
  (occur "class "))

(defun pyrm ()
  (interactive)
  (setq command (concatenate 'string "autoflake --in-place --remove-unused-variables " buffer-file-name))
  (shell-command command)
  ;; Reload the modified file
  (revert-buffer t t))

(defun my/calculate-stops ()
  (save-excursion
    (let ((start
           (condition-case e
               (while t (backward-sexp))
             (error (point))))
          stops)
      (push start stops)
      (condition-case e
          (while t
            (forward-sexp)
            (when (looking-at "\\s-*,")
              (push (point) stops)))
        (error (push (point) stops)))
      (nreverse stops))))

(defun python-transpose-args ()
  (interactive)
  (when (looking-at "\\s-") (backward-sexp))
  (cl-loop with p = (point)
           with previous = nil
           for stop on (my/calculate-stops)
           for i upfrom 0
           when (<= p (car stop)) do
           (when previous
             (let* ((end (cadr stop))
                    (whole (buffer-substring previous end))
                    middle last)
               (delete-region previous end)
               (goto-char previous)
               (setf middle (if (> i 1) (- (car stop) previous)
                              (string-match "[^, \\t]" whole 
                                            (- (car stop) previous)))
                     last (if (> i 1) (substring whole 0 middle)
                            (concat (substring whole (- (car stop) previous) middle)
                                    (substring whole 0 (- (car stop) previous)))))
               (insert (substring whole middle) last)))
           (cl-return)
           end do (setf previous (car stop))))

(eval-after-load "python"
  '(progn
  (define-key python-mode-map (kbd "C-M-t") 'python-transpose-args)))
