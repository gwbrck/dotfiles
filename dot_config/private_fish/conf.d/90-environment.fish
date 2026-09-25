set -gx EDITOR nvim
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx HOMEBREW_BUNDLE_FILE_GLOBAL "$XDG_CONFIG_HOME/homebrew/Brewfile"
set -gx R_PROFILE "$XDG_CONFIG_HOME/R/Rprofile.site"

if status is-login
    if test (uname) = Darwin
        if test -d /opt/homebrew/bin
            eval "$(/opt/homebrew/bin/brew shellenv fish)"
        end
    end
end

fish_add_path $HOME/bin
fish_add_path $HOME/.local/bin # pipx & uv
fish_add_path /opt/homebrew/opt/curl/bin
fish_add_path $HOME/go/bin

if test (uname) = Darwin
    set -gx SSH_AUTH_SOCK "$HOME/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"
end

if test "$INSIDE_EMACS" = vterm
    source {$EMACS_VTERM_PATH}etc/emacs-vterm.fish
end
