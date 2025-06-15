#!/bin/bash
set -eufo pipefail
xcode-select -p >/dev/null 2>&1 || xcode-select --install

if ! type brew >/dev/null; then
  echo "add path"
  export PATH="/opt/homebrew/bin:${PATH}"
  echo "install brew"
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
  echo "brew installed"
fi

/opt/homebrew/bin/brew install git
/opt/homebrew/bin/brew install fish
/opt/homebrew/bin/brew install chezmoi
