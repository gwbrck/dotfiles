#!/bin/bash

xcode-select -p >/dev/null 2>&1 || xcode-select --install

if ! type brew >/dev/null; then
  export PATH="/opt/homebrew/bin:${PATH}"
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew bundle --no-lock --file=/dev/stdin <<EOF
tap "homebrew/cask-fonts"
brew "python"
brew "ansible"
brew "ansible-lint"
brew "node"
brew "chezmoi"
brew "cmake"
brew "coreutils"
brew "dasel"
brew "enchant"
brew "gcc"
brew "tesseract"
brew "fish"
brew "git"
brew "gnupg"
brew "graphviz"
brew "jq"
brew "neovim"
brew "ocrmypdf"
brew "ollama"
brew "pandoc"
brew "pass"
brew "pass-otp"
brew "pass-git-helper"
brew "pinentry-mac"
brew "pipenv"
brew "pipx"
brew "poetry"
brew "poppler"
brew "pygments"
brew "pympress"
brew "r"
brew "ripgrep"
brew "tesseract-lang"
brew "tmux"
brew "typst"
brew "wget"
cask "adobe-acrobat-reader"
cask "alacritty"
cask "alt-tab"
cask "deepl"
cask "docker"
cask "figma"
cask "firefox"
cask "font-open-sans"
cask "font-ubuntu-mono-nerd-font"
cask "jasp"
cask "karabiner-elements"
cask "mactex-no-gui"
cask "morgen"
cask "nextcloud"
cask "obsidian"
cask "quarto"
cask "raspberry-pi-imager"
cask "raycast"
cask "rstudio"
cask "signal"
cask "steam"
cask "tunnelblick"
cask "visual-studio-code"
cask "zoom"
cask "zotero@beta"
vscode "alefragnani.project-manager"
vscode "bmalehorn.vscode-fish"
vscode "chrischinchilla.vscode-pandoc"
vscode "foxundermoon.shell-format"
vscode "mads-hartmann.bash-ide-vscode"
vscode "malmaud.tmux"
vscode "mblode.zotero"
vscode "ms-ceintl.vscode-language-pack-de"
vscode "ms-python.debugpy"
vscode "ms-python.python"
vscode "ms-python.vscode-pylance"
vscode "quarto.quarto"
vscode "redhat.ansible"
vscode "redhat.vscode-yaml"
vscode "reditorsupport.r"
vscode "sainnhe.edge"
vscode "sumneko.lua"
vscode "tamasfe.even-better-toml"
vscode "tomoki1207.pdf"
EOF

pipx install kptncook ansible-creator visidata
