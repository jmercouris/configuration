;; ivy configuration
(ivy-mode 1)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "s-SPC") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;; flex search for everything but swiper
(setq ivy-re-builders-alist
      '((swiper . ivy--regex-plus)
	(counsel-git-grep . ivy--regex-plus)
	(t . ivy--regex-fuzzy)))
;; ivy kill switch buffer
(define-key
    ivy-switch-buffer-map
    (kbd "C-k")
  (lambda ()
    (interactive)
    (ivy-set-action 'kill-buffer)
    (ivy-done)))
;; bind to C-c j
(global-set-key (kbd "C-c j") 'counsel-git-grep)
