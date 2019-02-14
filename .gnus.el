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

(setq gnus-parameters
      '(("personal"
         (posting-style
          (address "john@mercouris.email")))
        ("work"
         (posting-style
          (address "john@atlas.engineer")))))

(defun gnus-summary-archive-article ()
  (interactive)
  (gnus-summary-move-article :to-newsgroup "nnfolder+archive:Archive"))

;;Select automatically while replying
(add-hook 'message-mode-hook
          '(lambda ()
             (cond ((string-match "personal" gnus-newsgroup-name) (setPersonal))
                   ((string-match "work" gnus-newsgroup-name) (setWork)))))


;;Three pane configuration
(gnus-add-configuration
     '(article
       (horizontal 1.0
                   (vertical 30
                             (group 1.0))
                   (vertical 1.0
                             (summary 0.25 point)
                             (article 1.0)))))
    (gnus-add-configuration
     '(summary
       (horizontal 1.0
                   (vertical 30
                             (group 1.0))
                   (vertical 1.0
                             (summary 1.0 point)))))


;; Summary View
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))
(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))
(setq gnus-summary-display-arrow t)
