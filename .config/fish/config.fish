# Export Path Variables
set --export PATH /opt/local/Library/Frameworks/Python.framework/Versions/Current/bin $PATH
set --export PATH /opt/local/bin /opt/local/sbin $PATH
set --export PATH /Users/jmercouris/User $PATH
set --export CLICOLOR "1"
set --export VISUAL "emacs -nw"
set --export EDITOR "$VISUAL"
# Python Specific Environment Variables
set --export VIRTUAL_ENV_DISABLE_PROMPT 1
set --export PYTHONPATH /Users/jmercouris/Projects/scikit-learn
set --export PYTHONPATH $PYTHONPATH /Users/jmercouris/Projects/AudioRead
# Neo4j Database Location
set --export NEO4J_REST_URL 'http://neo4j:passywordy@localhost:7474/db/data/'


# Colorize less things such as man pages
set -x LESS_TERMCAP_mb (printf "\033[01;31m")
set -x LESS_TERMCAP_md (printf "\033[01;31m")
set -x LESS_TERMCAP_me (printf "\033[0m")
set -x LESS_TERMCAP_se (printf "\033[0m")
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\033[0m")
set -x LESS_TERMCAP_us (printf "\033[01;32m")

# Get Battery Status
alias battery='pmset -g batt'
# Open Emacs in a terminal window
alias e="emacs -q"
# Open Emacs in a graphical window
alias ex="open -n -a /Applications/Emacs.app"
# Allow alias usage while sudo
alias sudo='sudo '
# Useful ls alias
alias l='ls -la'
# Useful cd alias
alias c='cd'
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
# Alpine Custom Configuration File
alias alpine='alpine -P ~/.pine.conf'
# Move Up a directory
alias u='cd ../'
# View Git History Graph
alias git_graph='git log --graph --decorate --oneline'

#Folder Shortcut Alias
alias downloads='cd /Users/jmercouris/Downloads'
alias documents='cd /Users/jmercouris/Documents'
alias projects='cd /Users/jmercouris/Projects'
alias desktop='cd /Users/jmercouris/Desktop'
alias tub='cd /Users/jmercouris/Documents/TUB'
alias iit='cd /Users/jmercouris/Documents/IIT'
