(setq circe-network-options
      `(("freenode"
	 :host "irc.freenode.net"
         :tls t
         :nick ,freenode-nick
         :sasl-username ,freenode-username
         :sasl-password ,freenode-password
	 :channels ("#lisp" "#ccl" "#next-browser" "#startups" "#mezzano")
	 :port "6697")
        ("ircnet"
         :host "irc.ircnet.com"
         :tls t
         :nick ,freenode-nick
         :channels ("#worldchat")
         :port "6697")
        ("gnome"
         :host "irc.gnome.org"
         :tls t
         :nick ,freenode-nick
         :port "6697")))
        
(setq circe-default-part-message "Exit.")
(setq circe-reduce-lurker-spam t)
(require 'circe-color-nicks)
(enable-circe-color-nicks)
