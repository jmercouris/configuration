(in-package :next)

;; swap command and control keys on OSX
;; so that control ACTUALLY represents control
(let ((original_control *control-key*)
      (original_command *meta-key*))
  (setf *control-key* original_command)
  (setf *meta-key* original_control))

(defun print-text ()
  (print "change window"))

(define-key global-map (kbd "C-x o") #'print-text)
