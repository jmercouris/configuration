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
(global-unset-key (kbd "s-n"))
(global-unset-key (kbd "s-p"))
(bind-key* "s-p" 'scroll-down-in-place)
(bind-key* "s-n" 'scroll-up-in-place)

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

(defun set-window-width (n)
  "Set the selected window's width to N columns wide."
  (if (> n (window-width))
      (enlarge-window-horizontally (- n (window-width)))
      (shrink-window-horizontally (- (window-width) n))))

(defun set-80-columns ()
  "Set the selected window to 81 columns."
  (interactive)
  (set-window-width 81))

(defun org-retrieve-url-from-point ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 ;; org-context seems to return nil if the current element
                 ;; starts at buffer-start or ends at buffer-end
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (setq substring text)
    (kill-new text)))

(defun desktop-save-current-dir ()
  (interactive)
  (desktop-save (file-name-directory
			  (if (eq major-mode 'dired-mode)
			      (dired-get-filename)
			    (or (buffer-file-name) "")))))

(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

(defun toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(defun company-to-yasnippet ()
  "Company suggestions for yasnippet"
  (interactive)
  (company-abort)
  (call-interactively 'company-yasnippet))
(global-unset-key (kbd "<backtab>"))
(bind-key* "<backtab>" 'company-to-yasnippet)

(defun imenu-reposition ()
  "Imenu that will scroll the function as high up as necessary to be readable"
  (interactive)
  (call-interactively 'imenu)
  (reposition-window))

(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a path."
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
	(widen)
	(while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
		    (condition-case nil
			(progn
			  (nxml-backward-up-element) ; always returns nil
			  t)
		      (error nil)))
	  (setq path (cons (xmltok-start-tag-local-name) path)))
	(if (called-interactively-p t)
	    (message "/%s" (mapconcat 'identity path "/"))
	  (format "/%s" (mapconcat 'identity path "/")))))))

(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (filename)
            (find-file filename)
            (call-interactively command))
          (dired-get-marked-files))))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun org-open-default-external (&optional arg)
  (interactive "P")
  (let ((browse-url-browser-function #'browse-url-default-macosx-browser))
    (org-open-at-point)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (remove-if-not 'buffer-file-name (buffer-list)))))
(global-set-key (kbd "C-c k") 'kill-other-buffers)

(defun sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-sidebar)
  (ibuffer-sidebar-toggle-sidebar))

(defun slime-lisp-select ()
  "Allow selection of slime from slime-lisp-implementations"
  (interactive)
  (let ((current-prefix-arg '-)) (call-interactively 'slime)))

(defun turn-off-indent-tabs ()
  "Unconditionally turn off tab indentation."
  (setq indent-tabs-mode nil))

(defun turn-on-indent-tabs ()
  "Unconditionally turn on tab indentation."
  (setq indent-tabs-mode t))

(defun fill-comment-box (b e)
"Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."
(interactive "r")
(let ((e (copy-marker e t)))
  (goto-char b)
  (end-of-line)
  (insert-char ?  (- fill-column (current-column)))
  (comment-box b e 1)
  (goto-char e)
  (set-marker e nil)))
(global-set-key (kbd "s-/") 'fill-comment-box)

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun zoom-in ()
  (interactive)
  (let ((x (+ 10 (face-attribute 'default :height))))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height) 10)))
    (set-face-attribute 'default nil :height x)))
