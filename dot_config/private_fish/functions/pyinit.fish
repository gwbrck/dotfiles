function pyinit --description "Initialize a Python devenv project"
    nix flake init -t "path:$HOME/.local/share/devenv/templates#python-project"
end
