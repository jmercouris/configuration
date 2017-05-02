;; set shell path
(exec-path-from-shell-initialize)

;; disable quit by command + q
(global-unset-key (kbd "s-q"))

;; add Macports Path
(setq exec-path (append exec-path '("/opt/local/bin")))

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

;; desktop
(defhydra hydra-system (:color blue :columns 1)
  "System"
  ("b" battery "battery status")
  ("s" sleep "sleep")
  ("q" nil "quit"))
;; Assign Hydra to hotkey
(global-unset-key (kbd "s-s"))
(global-set-key (kbd "s-s") 'hydra-system/body)

(defun battery ()
  (interactive)
  (display-message-or-buffer
   (shell-command-to-string "battery")))

(defun sleep ()
  (interactive)
  (shell-command "cpu_sleep"))

(defun screen-saver ()
  (interactive)
  (shell-command "screen_saver" 0))
