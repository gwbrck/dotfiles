alias latex-update="sudo tlmgr update --self --all"
alias biber-cash-reset="rm -rf `biber --cache`"

alias em="emacsclient -c -n -s ~/.cache/emacsserver/emacsen"
alias em-daemon="emacs --bg-daemon=~/.cache/emacsserver/emacsen" 

alias cm='chezmoi'
alias cmcd='chezmoi cd'
alias cmgst='chezmoi git status'
alias cmgaa='chezmoi git add --all'
alias cmgc='chezmoi git commit -v'
alias cme='chezmoi edit'

alias g='git'
alias ga='git add'
alias gaa='git add --all'
alias gc='git commit -v'
alias gcl='git clone --recurse-submodules'
alias gd='git diff'
alias gss='git status -s'
alias gst='git status'

alias vi='nvim'
alias vim='nvim'

alias l='ls -la'
alias ll='ls -l'
alias la='ls -lA'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias ......='cd ../../../../..'
