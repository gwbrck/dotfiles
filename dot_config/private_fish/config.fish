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
    set -U fish_color_autosuggestion      brblack
    set -U fish_color_cancel              -r
    set -U fish_color_command             brgreen
    set -U fish_color_comment             brmagenta
    set -U fish_color_cwd                 green
    set -U fish_color_cwd_root            red
    set -U fish_color_end                 brmagenta
    set -U fish_color_error               brred
    set -U fish_color_escape              brcyan
    set -U fish_color_history_current     --bold
    set -U fish_color_host                normal
    set -U fish_color_match               --background=brblue
    set -U fish_color_normal              normal
    set -U fish_color_operator            cyan
    set -U fish_color_param               brblue
    set -U fish_color_quote               yellow
    set -U fish_color_redirection         bryellow
    set -U fish_color_search_match        'bryellow' '--background=brblack'
    set -U fish_color_selection           'normal' '--bold' '--background=brblack'
    set -U fish_color_status              red
    set -U fish_color_user                brgreen
    set -U fish_color_valid_path          --underline
    set -U fish_pager_color_completion    normal
    set -U fish_pager_color_description   yellow
    set -U fish_pager_color_prefix        'normal' '--bold' '--underline'
    set -U fish_pager_color_progress      'brwhite' '--background=cyan'
    # Vi Keybindings
    set -U fish_key_bindings fish_vi_key_bindings
end

set fish_greeting ""

# Ensure that GPG Agent is used as the SSH agent
set -e SSH_AUTH_SOCK
set -e SSH_AGENT_PID
set -Ux SSH_AUTH_SOCK $(gpgconf --list-dirs agent-ssh-socket)
set -Ux GPG_TTY $(tty)

set -Ux PYENV_ROOT $HOME/.config/pyenv
fish_add_path $PYENV_ROOT/bin
fish_add_path ~/bin
set -Ux PIPENV_VENV_IN_PROJECT 1 
set -Ux PIPENV_PYTHON $PYENV_ROOT/shims/python

set -Ux VISUAL "emacsclient -c -s $HOME/.cache/emacsserver/emacsen"
set -Ux EDITOR nvim

status is-login; and pyenv init --path | source
status is-interactive; and pyenv init - | source

if test "$INSIDE_EMACS" = "vterm"
    source {$EMACS_VTERM_PATH}etc/emacs-vterm.fish
end
