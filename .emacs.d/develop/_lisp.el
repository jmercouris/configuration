(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(load "~/.quicklisp/clhs-use-local.el" t)

(setq inferior-lisp-program "/usr/local/src/ccl/dx86cl64")
(slime-setup '(slime-fancy slime-company slime-asdf slime-indentation))

(defun lisp-outline ()
  (interactive)
  (occur "defun \\|defclass\\|defmethod "))

(defun lisp-compile ()
  (interactive)
  (compile "ccl --no-init --load make.lisp"))

(add-hook 'lisp-mode-hook 'highlight-indent-guides-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'smartparens-mode)
(add-hook 'lisp-mode-hook 'company-mode)

(add-hook 'slime-mode-hook 'company-mode)
(add-hook 'slime-mode-hook 'smartparens-mode)

(eval-after-load "lisp"
  '(progn
     (define-key lisp-mode-map (kbd "<f5>") 'lisp-compile)))
