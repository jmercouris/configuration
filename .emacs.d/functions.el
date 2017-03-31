;; show file name 
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))
;; copy buffer file name to kill ring
(defun copy-buffer-file-name-as-kill (choice)
  "Copyies the buffer {name/mode}, file {name/full path/directory} to the kill-ring."
  (interactive "cCopy (b) buffer name, (m) buffer major mode, (f) full buffer-file path, (d) buffer-file directory, (n) buffer-file basename")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          ((eq choice ?b)
           (setq new-kill-string (buffer-name)))
          ((eq choice ?m)
           (setq new-kill-string (format "%s" major-mode)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))

;; define a function to scroll with the cursor in place, moving the page instead
;; Navigation Functions
(defun scroll-down-in-place (n)
  (interactive "p")
  (previous-line n)
  (unless (eq (window-start) (point-min))
    (scroll-down n)))
(defun scroll-up-in-place (n)
  (interactive "p")
  (next-line n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))
(global-set-key "\M-n" 'scroll-up-in-place)
(global-set-key "\M-p" 'scroll-down-in-place)

;; open shell on remote machine
(defun remote-shell (&optional host)
  "Open a remote shell to a host."
  (interactive)
  (with-temp-buffer
    (let ((host (if host host (read-string "Host: "))))
      (cd (concat "/scp:" host ":"))
      (shell (concat "*" host "*")))))

;; example of setting up custom shells
;; (defun myserver-shell () (interactive) (remote-shell "myserver"))

;;xml formatting
(defun pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;; fill comment to width
(defun fill-comment ()
  "Fill text to column width for comments"
  (interactive)
  (save-excursion
    (move-end-of-line 1)
    (while (< (current-column) fill-column) (insert ?#))))
(global-set-key (kbd "s-/") 'fill-comment)

;; clear eshell buffer
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))
(add-hook 'eshell-mode-hook
      '(lambda()
	 (local-set-key (kbd "s-c") 'eshell-clear-buffer)))

;; paste from OSX
(defun copy-from-osx ()
  "Handle copy/paste intelligently on osx."
  (let ((pbpaste (purecopy "/usr/bin/pbpaste")))
    (if (and (eq system-type 'darwin)
             (file-exists-p pbpaste))
        (let ((tramp-mode nil)
              (default-directory "~"))
          (shell-command-to-string pbpaste)))))

;; paste to OSX
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))
;; set copy/paste functions
(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)
(put 'upcase-region 'disabled nil)

(defun set-window-width (n)
  "Set the selected window's width."
  (if (> n (window-width))
      (enlarge-window-horizontally (- n (window-width)))
      (shrink-window-horizontally (- (window-width) n))))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(defun org-retrieve-url-from-point ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
      (kill-new text))))
