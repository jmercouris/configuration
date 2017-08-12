(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(load "~/.quicklisp/clhs-use-local.el" t)

(setq inferior-lisp-program "ecl")
(slime-setup '(slime-fancy slime-company))

(add-hook 'lisp-mode-hook 'highlight-indentation-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
