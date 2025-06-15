#!/bin/bash

xcode-select -p >/dev/null 2>&1 || xcode-select --install

if ! type brew >/dev/null; then
  export PATH="/opt/homebrew/bin:${PATH}"
  NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

/opt/homebrew/bin/brew install git
/opt/homebrew/bin/brew install fish
/opt/homebrew/bin/brew install chezmoi
