(add-hook 'LaTeX-mode-hook 'company-mode)

(eval-after-load "company"
  '(add-to-list 'company-backends 'company-yasnippet))

(defhydra hydra-latex (:columns 4)
  "Hydra Latex"
  ("c" TeX-command-master "Compile")
  ("q" nil "quit" :color blue))

;; Assign hydra to hotkey when in latex mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (define-key LaTeX-mode-map (kbd "s-h") 'hydra-latex/body)))

