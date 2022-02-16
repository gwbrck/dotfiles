if status is-interactive
    alias latex-update="sudo tlmgr update --self --all"
    alias biber-cash-reset="rm -rf `biber --cache`"
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
    alias ..='cd ..'
    alias ...='cd ../..'
    alias ....='cd ../../..'
    alias .....='cd ../../../..'
    alias ......='cd ../../../../..'
    # Commands to run in interactive sessions can go here
end

set fish_greeting ""

set -Ux PYENV_ROOT $HOME/.config/pyenv
set -U fish_user_paths $PYENV_ROOT/bin $fish_user_paths
set -Ux PIPENV_VENV_IN_PROJECT 1 
set -Ux PIPENV_PYTHON $PYENV_ROOT/shims/python

set -Ux VISUAL "emacsclient -c -s $HOME/.cache/emacsserver/emacsen"
set -Ux EDITOR nvim

status is-login; and pyenv init --path | source
status is-interactive; and pyenv init - | source

if [ "$INSIDE_EMACS" = 'vterm' ]
    source {$EMACS_VTERM_PATH}/etc/emacs-vterm.fish
end
