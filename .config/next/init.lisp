(in-package :next)

(setf *start-page-url* "about:blank")

(define-command show-minibuffer ()
  (minibuffer-set-height *interface* (window-active *interface*) 100))

(define-command hide-minibuffer ()
  (minibuffer-set-height *interface* (window-active *interface*) 10))

(define-key *global-map* (kbd "C-q") 'show-minibuffer)
(define-key *global-map* (kbd "M-q") 'hide-minibuffer)

