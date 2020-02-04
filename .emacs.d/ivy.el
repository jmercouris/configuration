;; ivy configuration
(ivy-mode 1)
(global-set-key "\C-s" 'swiper)
(global-set-key "\M-s" 'swiper-all)
(global-set-key (kbd "s-SPC") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-j") 'counsel-file-jump)
;; ivy kill switch buffer
(define-key
    ivy-switch-buffer-map
    (kbd "C-k")
  (lambda ()
    (interactive)
    (ivy-set-action 'kill-buffer)
    (ivy-done)))
