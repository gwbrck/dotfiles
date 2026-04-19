function gpublish
    set -l host homeeins-1
    set -l repo (basename (pwd))

    if test (count $argv) -ge 1
        set host $argv[1]
    end

    if test (count $argv) -ge 2
        set repo $argv[2]
    end

    ssh $host "mkdir -p /git; if test ! -d /git/$repo.git; git init --bare /git/$repo.git; end"

    if git remote get-url origin >/dev/null 2>/dev/null
        git remote set-url origin "$host:/git/$repo.git"
    else
        git remote add origin "$host:/git/$repo.git"
    end

    git push -u origin HEAD
end
