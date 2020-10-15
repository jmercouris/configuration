(setf *socket-path* nil)

(define-configuration browser
  ((session-restore-prompt :never-restore)
   (external-editor-program "/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs")))

(define-configuration web-buffer
  ((default-modes (append '(emacs-mode) %slot-default))
   (default-new-buffer-url "about:blank")
   (override-map (let ((map (make-keymap "my-override-map")))
                             (define-key map
                               "C-q" 'quit
                               "C-1" 'make-window
                               "C-2" 'delete-current-window)
                   map))))

(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default))
   (default-new-buffer-url "about:blank")
   (override-map (let ((map (make-keymap "my-override-map")))
                             (define-key map
                               "C-q" 'quit
                               "C-1" 'make-window
                               "C-2" 'delete-current-window)
                   map))))

(defvar +dev-data-profile+ (make-instance 'data-profile :name "dev")
  "Development profile.")

(defmethod nyxt:expand-data-path ((profile (eql +dev-data-profile+)) (path data-path))
  "Persist data to /tmp/nyxt/."
  (expand-default-path (make-instance (class-name (class-of path))
                                      :basename (basename path)
                                      :dirname "/tmp/nyxt/")))

(define-url-group nyxt (match-bookmarks "+nyxt"))

#+darwin
(define-command cpu-sleep ()
  "Put the computer to sleep."
  (uiop:run-program "/Users/jmercouris/User/cpu_sleep"))

#+darwin
(define-command cpu-battery ()
  "Show the battery status."
  (echo (uiop:run-program "/Users/jmercouris/User/cpu_battery" :output :string)))

#+darwin
(define-command open-in-safari ()
  "Open the current URL in Safari"
  (uiop:run-program (list "open" "-a" "Safari" (object-string (url (current-buffer))))))

;; (defun nyxt::command-display (command)
;;   (format nil "~a" (str:downcase (nyxt::sym command))))

;; (ql:quickload :nx-reader)
;; (setf nx-reader:rss-urls (list "https://news.ycombinator.com/rss"
;;                                "https://tim.blog/feed/"
;;                                "https://lobste.rs/rss"))
