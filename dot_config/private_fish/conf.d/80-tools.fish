alias em 'emacsclient -cn -a ""'
abbr bwu 'set -x BW_SESSION $(bw unlock --raw)'

if status is-interactive
    if command -q devenv
        devenv hook fish | source
    end

    abbr s "sesh connect (sesh list -i | fzf --ansi)"
    abbr latex-update "sudo tlmgr update --self --all"
    abbr biber-cash-reset "rm -rf `biber --cache`"
    abbr cma 'chezmoi apply'
    abbr cme 'chezmoi edit'
    alias vi nvim
    alias vim nvim
end

zoxide init fish | source
