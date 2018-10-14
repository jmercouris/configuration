;; set main method to nil
(setq gnus-select-method '(nnml ""))

(setq gnus-secondary-select-methods
      '(
        (nnimap "personal"
                (nnimap-address "mail.gandi.net")
                (nnimap-authinfo-file "~/.authinfo.gpg")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnimap "work"
                (nnimap-address "mail.gandi.net")
                (nnimap-authinfo-file "~/.authinfo.gpg")
                (nnimap-server-port 993)
                (nnimap-stream ssl))
        (nnfolder "archive"
                  (nnfolder-directory   "~/Mail/archive")
                  (nnfolder-active-file "~/Mail/archive/active")
                  (nnfolder-get-new-mail nil)
                  (nnfolder-inhibit-expiry t))))

;; personal Information
(setq user-full-name "John Mercouris")
(setq mml2015-signers '("A1AB09DB505BC4B8")
      mml2015-encrypt-to-self t)

;; set-up smtp
(setq smtpmail-smtp-server "mail.gandi.net"
      smtpmail-smtp-service 587
      message-send-mail-function 'smtpmail-send-it)

;; sort threads by date
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

;; show threads
(setq gnus-show-threads t)

;; try to encrypt emails we'll send
(add-hook 'message-setup-hook 'mml-secure-message-encrypt)

;; threads
(setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; read HTML mail
(setq mm-text-html-renderer 'shr)

;; always use topic mode
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;To set personal email
(defun setPersonal ()
  (interactive)
  (message "from personal")
  (setq user-mail-address "john@mercouris.email"))

;;To set work email
(defun setWork ()
  (interactive)
  (message "from work")
  (setq user-mail-address "john@atlas.engineer"))

;;Select automatically while replying 
(add-hook 'message-mode-hook
          '(lambda ()
             (cond ((string-match "personal" gnus-newsgroup-name) (setPersonal))
                   ((string-match "work" gnus-newsgroup-name) (setWork)))))

