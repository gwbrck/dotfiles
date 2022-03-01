function fish_prompt --description 'Write out the prompt'
    set -l laststatus $status

    if not set -q fish_prompt_pwd_dir_length
        set -lx fish_prompt_pwd_dir_length 0
    end
    if not set -q __fish_git_prompt_show_informative_status
        set -g __fish_git_prompt_show_informative_status 1
    end
    if not set -q __fish_git_prompt_hide_untrackedfiles
        set -g __fish_git_prompt_hide_untrackedfiles 1
    end
    if not set -q __fish_git_prompt_showupstream
        set -g __fish_git_prompt_showupstream informative
    end
    if not set -q __fish_git_prompt_char_upstream_ahead
        set -g __fish_git_prompt_char_upstream_ahead "↑"
    end
    if not set -q __fish_git_prompt_char_upstream_behind
        set -g __fish_git_prompt_char_upstream_behind "↓"
    end
    if not set -q __fish_git_prompt_char_stagedstate
        set -g __fish_git_prompt_char_stagedstate "S"
    end
    if not set -q __fish_git_prompt_color_stagedstate
        set -g __fish_git_prompt_color_stagedstate green
    end
    if not set -q __fish_git_prompt_char_dirtystate
        set -g __fish_git_prompt_char_dirtystate "U"
    end
    if not set -q __fish_git_prompt_color_dirtystate
        set -g __fish_git_prompt_color_dirtystate red
    end
    if not set -q __fish_git_prompt_char_untrackedfiles
        set -g __fish_git_prompt_char_untrackedfiles "…"
    end
    if not set -q __fish_git_prompt_color_untrackedfiles
        set -g __fish_git_prompt_color_untrackedfiles red
    end
    if not set -q __fish_git_prompt_char_conflictedstate 
        set -g __fish_git_prompt_char_conflictedstate "x"
    end
    if not set -q __fish_git_prompt_color_invalidstate
        set -g __fish_git_prompt_color_invalidstate red --bold
    end
    if not set -q __fish_git_prompt_char_cleanstate 
        set -g __fish_git_prompt_char_cleanstate ""
    end

    set -q fish_prompt_pwd_dir_length
    or set -lx fish_prompt_pwd_dir_length 0

    set -l suffix
    if functions -q fish_is_root_user; and fish_is_root_user
        set suffix '# '
    else
        set suffix (printf '\u276f ')
    end

    if test $laststatus -ne 0
        printf "%s\u018e $laststatus %s" (set_color -o red) (set_color normal)
    end
    printf '%s%s' (set_color white) (prompt_pwd)
    printf '%s%s' (fish_vcs_prompt) ' '
    switch $fish_bind_mode
        case default
            set_color --bold red
            printf $suffix
            set_color white
        case insert
            set_color --bold blue
            printf $suffix
            set_color white
        case replace_one
            set_color --bold green
            printf 'R %s' $suffix
            set_color white
        case visual
            set_color --bold brmagenta
            printf 'V %s' $suffix
            set_color white
        case '*'
            set_color --bold red
            printf '? %s' $suffix
            set_color white
     end
end
