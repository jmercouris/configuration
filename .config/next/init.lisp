(setf *socket-path* nil)

(define-configuration browser
  ((session-restore-prompt :always-ask)
   (autofills (list (next::make-autofill :key "Name" :fill "John Mercouris")))))

(define-configuration buffer
  ((keymap-scheme-name scheme:emacs)
   (default-new-buffer-url "about:blank")
   (override-map (let ((map (make-keymap "my-override-map")))
                             (define-key map
                               "M-x" 'execute-command
                               "C-q" 'quit)
                   map))))

(defvar +dev-data-profile+ (make-instance 'data-profile :name "dev")
  "Development profile.")

(defmethod next:expand-data-path ((profile (eql +dev-data-profile+)) (path data-path))
  "Persist data to /tmp/next/."
  (expand-default-path (make-instance (class-name (class-of path))
                                      :basename (basename path)
                                      :dirname "/tmp/next/")))

#+darwin
(define-command cpu-sleep ()
  (uiop:run-program "/Users/jmercouris/User/cpu_sleep"))

#+darwin
(define-command cpu-battery ()
  (echo (uiop:run-program "/Users/jmercouris/User/cpu_battery" :output :string)))
