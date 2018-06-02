# Export Path Variables
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin $PATH
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/3.4/bin $PATH
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/Current/bin $PATH
set --export PATH /opt/local/bin /opt/local/sbin $PATH
set --export PATH /Users/jmercouris/User $PATH
set --export VIRTUAL_ENV_DISABLE_PROMPT 1
set --export PKG_CONFIG_PATH /usr/local/lib/pkgconfig /usr/local/lib
set --export CLICOLOR "1"
set --export VISUAL "emacs -nw"
set --export EDITOR "$VISUAL"
set --export TESSDATA_PREFIX /opt/local/share/tessdata
# Colorize less things such as man pages
set -x LESS_TERMCAP_mb (printf "\033[01;31m")
set -x LESS_TERMCAP_md (printf "\033[01;31m")
set -x LESS_TERMCAP_me (printf "\033[0m")
set -x LESS_TERMCAP_se (printf "\033[0m")
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\033[0m")
set -x LESS_TERMCAP_us (printf "\033[01;32m")

# Open Emacs in a terminal window
alias e="emacs -nw"
# Open Emacs in a graphical window
alias ex="open -n -a /Applications/Emacs.app"
# Allow alias usage while sudo
alias sudo='sudo '
# Useful ls alias
alias l='ls -la'
# Get router IP
alias router='netstat -rn |grep default'
# Quit Terminal
alias q='exit'
