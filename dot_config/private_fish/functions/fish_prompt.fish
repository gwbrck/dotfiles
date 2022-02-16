function fish_prompt --description 'Write out the prompt'
    set -l laststatus $status

    set -l git_info
    if set -l git_branch (command git symbolic-ref HEAD 2>/dev/null | string replace refs/heads/ '')
        set git_branch (set_color -o blue)"$git_branch"
        set -l git_status
        if not command git diff-index --quiet HEAD --
            if set -l count (command git rev-list --count --left-right $upstream...HEAD 2>/dev/null)
                echo $count | read -l ahead behind
                if test "$ahead" -gt 0
                    set git_status "$git_status"(set_color red)⬆
                end
                if test "$behind" -gt 0
                    set git_status "$git_status"(set_color red)⬇
                end
            end
            for i in (git status --porcelain | string sub -l 2 | sort | uniq)
                switch $i
                    case "."
                        set git_status "$git_status"(set_color green)✚
                    case " D"
                        set git_status "$git_status"(set_color red)\u018e
                    case "*M*"
                        set git_status "$git_status"(set_color green)✱
                    case "*R*"
                        set git_status "$git_status"(set_color purple)➜
                    case "*U*"
                        set git_status "$git_status"(set_color brown)═
                    case "??"
                        set git_status "$git_status"(set_color red)≠
                end
            end
        else
            set git_status (set_color green):
        end
        set git_info "(git$git_status$git_branch"(set_color white)")"
    end

    # Disable PWD shortening by default.
    set -q fish_prompt_pwd_dir_length
    or set -lx fish_prompt_pwd_dir_length 0

    if test $laststatus -ne 0
        printf "%s\u018e $laststatus %s" (set_color -o red) (set_color normal)
    end
    printf '%s%s%s%s%s%s%s%s%s%s%s%s%s' (set_color -o white) (prompt_pwd) $git_info (set_color blue) ' '
    switch $fish_bind_mode
        case default
          set_color --bold red
          printf '%s ' \u276f
          set_color white
        case insert
          set_color --bold blue
          printf '%s ' \u276f 
          set_color white
        case replace_one
          set_color --bold green
          printf 'R %s ' \u276f
          set_color white
        case visual
          set_color --bold brmagenta
          printf 'V %s ' \u276f
          set_color white
        case '*'
          set_color --bold red
          printf '? %s ' \u276f
          set_color white
     end
end
