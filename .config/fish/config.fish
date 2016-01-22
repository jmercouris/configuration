# Export Path Variables
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/Current/bin $PATH
set --export PATH /opt/local/bin /opt/local/sbin $PATH
set --export PATH /Users/jmercouris/User $PATH
set --export PATH $PATH /Library/TeX/texbin/
set --export PATH $PATH /Applications/Postgres.app/Contents/Versions/9.4/bin
set --export PATH /Users/jmercouris/.rvm/bin $PATH
set --export JAVA_HOME "/Library/Java/JavaVirtualMachines/jdk1.8.0_25.jdk/Contents/Home/"
set --export CLICOLOR "1"
set --export LSCOLORS "GxFxCxDxBxegedabagaced"
set --export ATLAS_TOKEN "1Dms5dCXOXmzpw.atlasv1.PuFJToBzrGtyI1YO9Shl8TWoFQLgwg3YojUPOPrNHC99a4z8teZrmxZ9v6PiWbVFDoM"
set --export VISUAL "emacs -nw"
set --export EDITOR "$VISUAL"

# Easy Install alias
alias easy_install='/opt/local/bin/easy_install-2.7'
# Get Battery Status
alias battery='pmset -g batt'
# Open Emacs in a a terminal window
alias e="emacs -nw"
# Allow alias usage while sudo
alias sudo='sudo '
# Useful ls alias
alias l='ls -la'
# Get router IP
alias router='netstat -rn |grep default'
# Get CPU Temperature
alias temperature='osx-cpu-temp'
# Quit Terminal
alias q='exit'
# Spotlight functionality in Terminal
alias spotlight='mdfind'

#Folder Shortcut Alias
alias downloads='cd /Users/jmercouris/Downloads'
alias documents='cd /Users/jmercouris/Documents'
alias projects='cd /Users/jmercouris/Projects'
alias desktop='cd /Users/jmercouris/Desktop'
alias tub='cd /Users/jmercouris/Documents/TUB'
alias iit='cd /Users/jmercouris/Documents/IIT'
