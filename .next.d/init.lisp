(in-package :next)

;; swap keys for consistency with other non-qt programs
(let ((original_control *control-key*)
      (original_meta *meta-key*)
      (original_alt *alt-key*))
  (setf *control-key* original_meta)
  (setf *meta-key* original_alt)
  (setf *super-key* original_control))
