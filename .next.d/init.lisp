(in-package :next)

(setf *start-page-url* "about:blank")

(defvar *shell-mode-map* (make-hash-table :test 'equalp))

(defclass shell-mode (mode) ())

(defun shell-mode ()
  (make-instance 'application-mode
		 :name "shell-mode"
		 :keymap *shell-mode-map*))

(defparenstatic clear-shell-output
    (setf (ps:chain document body inner-h-t-m-l) ""))

(defparen append-output (output)
  (setf (ps:chain document body inner-h-t-m-l)
        (ps:chain document body inner-h-t-m-l
                  (concat (ps:lisp
                           (concatenate 'string output "<br />"))))))

(defun run-shell-command (input)
  (interface:web-view-execute
   (view *active-buffer*)
   (append-output
    (uiop:run-program input :force-shell t :output :string))))

(defun open-new-shell ()
  (set-visible-active-buffer
   (generate-new-buffer "*shell*" (shell-mode))))

(define-key *shell-mode-map* (kbd "c")
  (:input *minibuffer* run-shell-command))

(define-key *shell-mode-map* (kbd "k")
  #'clear-shell-output)

(define-key *global-map* (kbd "C-x s")
  #'open-new-shell)
