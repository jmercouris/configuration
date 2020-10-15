(setq circe-network-options
      `(("irccloud"
	 :host "bnc.irccloud.com"
         :tls t
         :nick ,freenode-nick
         :pass ,irc-cloud-password
	 :channels ("#lisp" "#ccl" "#next-browser" "#nyxt" "#startups" "#mezzano")
	 :port "6697")
        ("freenode"
	 :host "irc.freenode.net"
         :tls t
         :nick ,freenode-nick
         :sasl-username ,freenode-username
         :sasl-password ,freenode-password
	 :channels ("#lisp" "#next-browser" "#startups")
	 :port "6697")))
        
(setq circe-default-part-message "Exit.")
(setq circe-reduce-lurker-spam t)
(require 'circe-color-nicks)
(enable-circe-color-nicks)
