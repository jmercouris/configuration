(define-configuration browser
  ((session-restore-prompt :always-restore)
   (autofills (list (make-autofill :key "Current time"
                                   :fill (lambda () (format nil "~a" (local-time:now))))))))

(define-configuration nyxt/auto-mode:auto-mode
  ((nyxt/auto-mode:prompt-on-mode-toggle t)))

(define-configuration buffer
  ((default-modes (append '(emacs-mode) %slot-default%))
   (override-map (let ((map (make-keymap "my-override-map")))
                   (define-key map
                     "C-o" 'execute-command
                     "C-q" 'quit
                     "C-1" 'make-window
                     "C-2" 'delete-current-window
                     "keypadleft" 'nyxt/web-mode:history-backwards
                     "keypadright" 'nyxt/web-mode:history-forwards)
                   map))))

(define-configuration web-buffer
  ((default-modes (append '(emacs-mode blocker-mode) %slot-default%))
   (password-interface (make-instance 'password:password-store-interface))))

(define-configuration (prompt-buffer)
    ((default-modes (append '(emacs-mode) %slot-default%))
     (hide-suggestion-count-p t)))

(define-configuration status-buffer
    ((style (str:concat 
             %slot-default%
             (cl-css:css
              '(("#container"
                 ;; Columns: controls, url
                 :grid-template-columns "120px 2fr")))))))

(defun my-format-status (window)
  (let ((buffer (current-buffer window)))
    (markup:markup
     (:div :id "container"
           (:div :id "controls" :class "arrow-right"
                 (markup:raw (format-status-buttons)))
           (:div :id "url"
                 (markup:raw
                  (format-status-load-status buffer)
                  (format-status-url buffer)))))))

(define-configuration window
  ((status-formatter #'my-format-status)))

(define-configuration nyxt/web-mode::web-mode
  ((nyxt/web-mode::hints-alphabet "asdfghjklqwertyuiop")))

(define-command-global current-time ()
  "Show the current time."
  (echo "~a" (local-time:now)))

(defmethod nyxt::startup ((browser browser) urls)
  "Make a blank buffer."
  (window-make browser)
  (let ((window (current-window))
        (buffer (make-buffer :url (quri:uri "about:blank"))))
    (window-set-buffer window buffer)
    (toggle-fullscreen window)))

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

(define-command-global open-in-firefox ()
  "Open the current URL in Firefox"
  (uiop:run-program (list "firefox" (render-url (url (current-buffer))))))
