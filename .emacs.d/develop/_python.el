(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'company-mode)

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))

(setq realgud:pdb-command-name "python -m pdb")

;; temporary python shell fix until Emacs rc 25.2
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(defun python-shell ()
    (interactive)
  (run-python)
  (python-shell-switch-to-shell)
  )

(defun python-outline ()
  (interactive)
  (occur "class \\|def ")
  )

(defun python-class-outline ()
  (interactive)
  (occur "class ")
  )
