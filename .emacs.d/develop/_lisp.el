(setq inferior-lisp-program "ecl")
(slime-setup '(slime-fancy slime-company))

(add-hook 'lisp-mode-hook 'highlight-indentation-mode)
