# Export Path Variables
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/Current/bin $PATH
set --export PATH /opt/local/bin /opt/local/sbin $PATH
set --export PATH $PATH /opt/local/libexec/qt5/bin
set --export PATH /Users/jmercouris/User $PATH
# Python Specific Environment Variables
set --export VIRTUAL_ENV_DISABLE_PROMPT 1
set --export PYTHONPATH /Users/jmercouris/Projects/scikit-learn

set --export CLICOLOR "1"
set --export VISUAL "emacs -nw"
set --export EDITOR "$VISUAL"

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
# Alpine Custom Configuration File
alias alpine='alpine -P ~/.pine.conf'
#Folder Shortcut Alias
alias downloads='cd /Users/jmercouris/Downloads'
alias academic='cd /Users/jmercouris/Documents/Academic'
alias documents='cd /Users/jmercouris/Documents'
alias projects='cd /Users/jmercouris/Projects'
alias desktop='cd /Users/jmercouris/Desktop'