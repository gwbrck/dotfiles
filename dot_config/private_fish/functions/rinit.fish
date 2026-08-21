function rinit --description "Initialize an R devenv project"
    nix flake init -t "path:$HOME/.local/share/devenv/templates#r-project"
end
