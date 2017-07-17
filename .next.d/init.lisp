(in-package :next)

(defun print-text ()
  (print "hey"))

(define-key global-map (kbd "C-a") #'print-text)
