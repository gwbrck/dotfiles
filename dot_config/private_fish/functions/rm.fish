function rm --wraps rm
    for a in $argv
        switch $a
            case "$HOME" "$HOME/" /
                echo "rm: '$a' blocked use 'command rm'." >&2
                return 1
        end
    end
    command rm $argv
end
