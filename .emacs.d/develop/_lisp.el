(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(load "~/.quicklisp/clhs-use-local.el" t)

(setq inferior-lisp-program "/opt/local/bin/sbcl")
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

;; Lisp
(defhydra hydra-lisp (:color blue :hint nil)
  "
   Navigation      ^^Formatting      ^^REPL
---------------------------------------------------------------------
   _d_ocumentation   check _p_arens    _O_pen
   _o_utline         re_i_ndent        _e_val region
   _w_ho calls                       ^^eval de_f_un
   definition_._                     ^^_l_oad file
   pop definition_,_                 ^^_r_estart
                                   ^^^^load _s_ystem
                                   ^^^^_c_lear
"
  ("d" slime-documentation-lookup)
  ("O" slime)
  ("o" lisp-outline)
  ("f" slime-eval-defun)
  ("e" slime-eval-region)
  ("l" slime-load-file)
  ("p" check-parens)
  ("r" slime-restart-inferior-lisp)
  ("s" slime-load-system)
  ("c" slime-repl-clear-buffer)
  ("i" paredit-reindent-defun)
  ("w" slime-who-calls)
  ("." slime-edit-definition)
  ("," slime-pop-find-definition-stack :color red)
  ("q" nil "quit"))
;; Assign hydra to hotkey when in lisp mode
(eval-after-load "lisp-mode"
  '(progn
  (define-key lisp-mode-map (kbd "s-h") 'hydra-lisp/body)))
