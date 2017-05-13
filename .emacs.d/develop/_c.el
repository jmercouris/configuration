;; offset switch statements nicely
(c-set-offset 'case-label '+)
;; default spacing = 4
(setq-default c-basic-offset 4)
(setq c-default-style "bsd")
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'semantic-mode)
(add-hook 'c-mode-hook 'company-mode)
