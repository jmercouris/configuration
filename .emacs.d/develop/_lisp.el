(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(load "~/.quicklisp/clhs-use-local.el" t)

(setq inferior-lisp-program "ecl")
(slime-setup '(slime-fancy slime-company))

(add-hook 'lisp-mode-hook 'highlight-indent-guides-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'smartparens-mode)
(add-hook 'lisp-mode-hook 'company-mode)

(add-hook 'slime-mode-hook 'company-mode)
(add-hook 'slime-mode-hook 'smartparens-mode)
