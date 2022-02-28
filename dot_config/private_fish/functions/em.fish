function em -d "Open in Emacs"
    emacsclient -c -n -s ~/.cache/emacsserver/emacsen $argv || 
    start_em_daemon $argv
end 

function start_em_daemon -d "Start Deamon and open Files"
    emacs --bg-daemon=~/.cache/emacsserver/emacsen &&
    em $argv
end