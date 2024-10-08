if status is-login
    if test (uname) = Darwin
        if test -d /opt/homebrew/bin
            eval "$(/opt/homebrew/bin/brew shellenv fish)"
        end
    end
end

alias em='emacsclient -cn -a ""'
if status is-interactive
    abbr tn "tmux new -s (pwd | sed 's/.*\///g')"
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
    set -U fish_color_autosuggestion brblack
    set -U fish_color_cancel -r
    set -U fish_color_command brgreen
    set -U fish_color_comment brmagenta
    set -U fish_color_cwd green
    set -U fish_color_cwd_root red
    set -U fish_color_end brmagenta
    set -U fish_color_error brred
    set -U fish_color_escape brcyan
    set -U fish_color_history_current --bold
    set -U fish_color_host normal
    set -U fish_color_match --background=brblue
    set -U fish_color_normal normal
    set -U fish_color_operator cyan
    set -U fish_color_param brblue
    set -U fish_color_quote yellow
    set -U fish_color_redirection bryellow
    set -U fish_color_search_match bryellow '--background=brblack'
    set -U fish_color_selection normal --bold '--background=brblack'
    set -U fish_color_status red
    set -U fish_color_user brgreen
    set -U fish_color_valid_path --underline
    set -U fish_pager_color_completion normal
    set -U fish_pager_color_description yellow
    set -U fish_pager_color_prefix normal --bold --underline
    set -U fish_pager_color_progress brwhite '--background=cyan'
    # Vi Keybindings
    set -U fish_key_bindings fish_vi_key_bindings
end

set fish_greeting ""
set -Ux EDITOR nvim

# Ensure that GPG Agent is used as the SSH agent
set -e SSH_AUTH_SOCK
set -e SSH_AGENT_PID
set -Ux GNUPGHOME $HOME/.config/gnupg
set -Ux PASSWORD_STORE_DIR $HOME/Documents/Archiv/Pass
set -Ux SSH_AUTH_SOCK $(gpgconf --list-dirs agent-ssh-socket)
set -Ux GPG_TTY $(tty)

# this is for mise 
# set -Ux R_EXTRA_CONFIGURE_OPTIONS 'CFLAGS=-I/opt/homebrew/lib CPPFLAGS=-I/opt/homebrew/include LDFLAGS=-L/opt/homebrew/include --enable-R-shlib --with-cairo --with-x=no'

fish_add_path ~/bin
fish_add_path ~/.local/bin #pipx
fish_add_path /opt/homebrew/opt/curl/bin

eval conda "shell.fish" "hook" $argv | source

set -Ux POETRY_CONFIG_DIR $HOME/.config/pypoetry

if test "$INSIDE_EMACS" = vterm
    source {$EMACS_VTERM_PATH}etc/emacs-vterm.fish
end
