alias ls="ls --color=always"
alias pacman="sudo pacman"
alias netctl="sudo netctl"
alias aura="sudo aura"
alias suspend="systemctl suspend"
alias fucking="sudo"
alias wtf="thefuck"
alias shutdown="systemctl poweroff"
alias lock="suspend; slock"
export PATH="/home/caleb/.bin/:/home/caleb/.cabal/bin:${PATH}"
export BROWSER="/usr/bin/firefox"
export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"


function vim() {emacsclient -c $* &|}
