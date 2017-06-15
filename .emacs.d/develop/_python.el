(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)

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

(defun python-django-test ()
  (interactive)
  (kill-new
   (concatenate 'string "./manage.py test "
		(replace-in-string (projectile-project-root) "" (buffer-file-name)))))
