if status is-interactive
    source ~/.config/aliases/aliases.sh
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
