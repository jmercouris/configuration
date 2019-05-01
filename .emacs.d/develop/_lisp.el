(load "~/.quicklisp/clhs-use-local.el" t)


(slime-setup '(slime-fancy slime-company slime-asdf slime-indentation slime-sbcl-exts))

(setq slime-lisp-implementations
      '((sbcl ("/opt/local/bin/sbcl" ""))
        (sbcl-mem ("/opt/local/bin/sbcl" "--dynamic-space-size" "2560"))))

(defun lisp-outline ()
  (interactive)
  (occur "defun \\|defclass\\|defmethod "))

(add-hook 'lisp-mode-hook 'highlight-indentation-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'company-mode)
(add-hook 'lisp-mode-hook 'swap-brackets-parens)
(add-hook 'lisp-mode-hook 'turn-off-indent-tabs)
(add-hook 'slime-mode-hook 'smartparens-mode)

;; Lisp
(defhydra hydra-lisp (:color blue :hint nil)
  "
   Navigation      ^^Formatting      ^^REPL          ^^Testing
---------------------------------------------------------------------
   _d_ocumentation   check _p_arens    _O_pen          _t_est system
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
  ("t" slime-repl-test-system)
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
