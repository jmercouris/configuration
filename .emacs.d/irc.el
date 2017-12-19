(setq circe-network-options
      `(("freenode"
	 :host "irc.freenode.net"
         :tls t
         :nick ,freenode-nick
         :sasl-username ,freenode-username
         :sasl-password ,freenode-password
	 :channels ("#lisp" "##nEXT-Browser" "#ccl")
	 :port "6697")))

(setq circe-default-part-message "Exit.")
(setq circe-reduce-lurker-spam t)
