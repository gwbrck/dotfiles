complete -c gpublish -f

complete -c gpublish \
    -n 'test (count (commandline -opc)) -eq 1' \
    -a '(__fish_print_hostnames)' \
    -d 'SSH host'
