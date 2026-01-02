set -g fish_greeting

set -gx EDITOR nvim
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx HOMEBREW_BUNDLE_FILE_GLOBAL "$XDG_CONFIG_HOME/homebrew/Brewfile"
set -gx R_PROFILE_USER "$XDG_CONFIG_HOME/R/.Rprofile"

fish_config theme choose default

set fish_color_comment grey
set fish_color_command brgreen
set fish_color_param brblue

if status is-login
    if test (uname) = Darwin
        if test -d /opt/homebrew/bin
            eval "$(/opt/homebrew/bin/brew shellenv fish)"
        end
    end
end

alias em 'emacsclient -cn -a ""'
abbr bwu 'set -x BW_SESSION $(bw unlock --raw)'

if status is-interactive
    abbr tn "tmux new -s (pwd | sed 's/.*\///g')"
    abbr latex-update "sudo tlmgr update --self --all"
    abbr biber-cash-reset "rm -rf `biber --cache`"
    abbr cm chezmoi
    abbr cmcd 'chezmoi cd'
    abbr cmgst 'chezmoi git status'
    abbr cmgaa 'chezmoi git add --all'
    abbr cmgc 'chezmoi git commit -v'
    abbr cme 'chezmoi edit'
    abbr g git
    abbr ga 'git add'
    abbr gaa 'git add --all'
    abbr gc 'git commit'
    abbr gcl 'git clone --recurse-submodules'
    abbr gd 'git diff'
    abbr gss 'git status -s'
    abbr gst 'git status'
    alias vi nvim
    alias vim nvim
    abbr l 'ls -la'
    abbr .. 'cd ..'
    abbr ... 'cd ../..'
    abbr .... 'cd ../../..'
    abbr ..... 'cd ../../../..'
    abbr ...... 'cd ../../../../..'

end

fish_add_path $HOME/bin
fish_add_path $HOME/.local/bin #pipx & uv
fish_add_path /opt/homebrew/opt/curl/bin

if test (uname) = Darwin
    set -gx SSH_AUTH_SOCK "$HOME/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"
end

if test "$INSIDE_EMACS" = vterm
    source {$EMACS_VTERM_PATH}etc/emacs-vterm.fish
end
