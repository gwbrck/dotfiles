#!/bin/bash
set -eufo pipefail

if ! grep -q "/opt/homebrew/bin/fish" /etc/shells; then
   echo "/opt/homebrew/bin/fish" | sudo tee -a /etc/shells
   chsh -s /opt/homebrew/bin/fish
fi

# defaults write com.lwouis.alt-tab-macos holdShortcut "\\U2318"
# defaults write com.lwouis.alt-tab-macos nextWindowShortcut2 ""
# defaults write com.lwouis.alt-tab-macos hideWindowlessApps true
# defaults write com.lwouis.alt-tab-macos menubarIcon 3
# defaults write com.lwouis.alt-tab-macos previewFocusedWindow true
