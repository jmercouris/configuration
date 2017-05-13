(add-hook 'LaTeX-mode-hook 'company-mode)

(eval-after-load "company"
 '(add-to-list 'company-backends 'company-yasnippet))
