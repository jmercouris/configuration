# Export Path Variables
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/Current/bin $PATH
set --export PATH /opt/local/bin /opt/local/sbin $PATH
set --export PATH /Users/jmercouris/User $PATH
set --export PATH $PATH /Library/TeX/texbin/
set --export PATH $PATH /Applications/Postgres.app/Contents/Versions/9.5/bin
set --export CLICOLOR "1"
set --export ATLAS_TOKEN "1Dms5dCXOXmzpw.atlasv1.PuFJToBzrGtyI1YO9Shl8TWoFQLgwg3YojUPOPrNHC99a4z8teZrmxZ9v6PiWbVFDoM"
set --export VISUAL "emacs -nw"
set --export EDITOR "$VISUAL"
set --export VIRTUAL_ENV_DISABLE_PROMPT 1

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
# Weather Chicago
alias weather_chicago='curl wttr.in/chicago'
alias weather_berlin='curl wttr.in/berlin'
# Screen Saver
alias screen_saver='/System/Library/Frameworks/ScreenSaver.framework/Resources/ScreenSaverEngine.app/Contents/MacOS/ScreenSaverEngine'
# Sleep
alias sleep='pmset sleepnow'

# Alias Postgres
alias postgres_start='sudo /opt/local/etc/LaunchDaemons/org.macports.postgresql95-server/postgresql95-server.wrapper start'
alias postgres_stop='sudo /opt/local/etc/LaunchDaemons/org.macports.postgresql95-server/postgresql95-server.wrapper stop'
alias postgres_restart='sudo /opt/local/etc/LaunchDaemons/org.macports.postgresql95-server/postgresql95-server.wrapper restart'

#Folder Shortcut Alias
alias downloads='cd /Users/jmercouris/Downloads'
alias documents='cd /Users/jmercouris/Documents'
alias projects='cd /Users/jmercouris/Projects'
alias desktop='cd /Users/jmercouris/Desktop'
alias tub='cd /Users/jmercouris/Documents/TUB'
alias iit='cd /Users/jmercouris/Documents/IIT'
