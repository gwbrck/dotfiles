#!/bin/bash

xcode-select -p >/dev/null 2>&1 || xcode-select --install

if ! type brew >/dev/null; then
  export PATH="/opt/homebrew/bin:${PATH}"
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew bundle --no-lock --file=/dev/stdin <<EOF
brew "sqlite"
brew "ansible"
brew "ansible-lint"
brew "chezmoi"
brew "cmake"
brew "coreutils"
brew "curl"
brew "dasel"
brew "enchant"
brew "tesseract"
brew "fish"
brew "gcc"
brew "git"
brew "gnupg"
brew "jq"
brew "neovim"
brew "nmap"
brew "node"
brew "ocrmypdf"
brew "pandoc"
brew "pass"
brew "pass-git-helper"
brew "pass-otp"
brew "pinentry-mac"
brew "pipenv"
brew "poetry"
brew "poppler"
brew "pygments"
brew "r"
brew "ripgrep"
brew "ruff"
brew "tesseract-lang"
brew "tmux"
brew "typst"
brew "uv"
brew "wget"
brew "zoxide"
cask "alacritty"
cask "deepl"
cask "docker"
cask "figma"
cask "firefox"
cask "font-fontawesome"
cask "font-juliamono"
cask "font-maple-mono-nf"
cask "font-open-sans"
cask "font-symbols-only-nerd-font"
cask "karabiner-elements"
cask "ghostty"
cask "mactex-no-gui"
cask "miniconda"
cask "nextcloud"
cask "openvpn-connect"
cask "portfolioperformance"
cask "presentation"
cask "quarto"
cask "raspberry-pi-imager"
cask "rstudio"
cask "signal"
cask "spotify"
cask "steam"
cask "visual-studio-code"
cask "whatsapp"
cask "whisky"
cask "zoom"
cask "zotero"
vscode "alefragnani.project-manager"
vscode "bmalehorn.vscode-fish"
vscode "charliermarsh.ruff"
vscode "chrischinchilla.vscode-pandoc"
vscode "esbenp.prettier-vscode"
vscode "foxundermoon.shell-format"
vscode "github.codespaces"
vscode "github.vscode-github-actions"
vscode "mads-hartmann.bash-ide-vscode"
vscode "malmaud.tmux"
vscode "mblode.zotero"
vscode "mechatroner.rainbow-csv"
vscode "mongodb.mongodb-vscode"
vscode "ms-ceintl.vscode-language-pack-de"
vscode "ms-python.debugpy"
vscode "ms-python.python"
vscode "ms-python.vscode-pylance"
vscode "ms-toolsai.jupyter"
vscode "ms-toolsai.jupyter-keymap"
vscode "ms-toolsai.jupyter-renderers"
vscode "ms-toolsai.vscode-jupyter-cell-tags"
vscode "ms-toolsai.vscode-jupyter-slideshow"
vscode "ms-vscode-remote.remote-containers"
vscode "nvarner.typst-lsp"
vscode "quarto.quarto"
vscode "rangav.vscode-thunder-client"
vscode "rdebugger.r-debugger"
vscode "redhat.ansible"
vscode "redhat.vscode-yaml"
vscode "reditorsupport.r"
vscode "sainnhe.edge"
vscode "sumneko.lua"
vscode "tamasfe.even-better-toml"
vscode "tomoki1207.pdf"
vscode "tootone.org-mode"
vscode "zeshuaro.vscode-python-poetry"
EOF

uv tool install kptncook
uv tool install ansible-creator
uv tool install pytr
