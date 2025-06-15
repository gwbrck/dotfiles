#!/bin/bash
set -eufo pipefail

if ! grep -q "/opt/homebrew/bin/fish" /etc/shells; then
  echo "/opt/homebrew/bin/fish" | sudo tee -a /etc/shells
  chsh -s /opt/homebrew/bin/fish
fi

if ! grep -qE "^\s*auth\s+sufficient\s+pam_tid.so" /etc/pam.d/sudo_local 2>/dev/null; then
  sed "s/^#auth/auth/" /etc/pam.d/sudo_local.template | sudo tee /etc/pam.d/sudo_local
fi

defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0
defaults write com.apple.dock orientation right
defaults write com.apple.dock show-recents -bool false
defaults write com.apple.dock expose-animation-duration -float 0.15
defaults write com.apple.dock tilesize -int 64
defaults write com.apple.dock persistent-apps -array \
  '<dict>
        <key>tile-data</key>
        <dict>
            <key>file-data</key>
            <dict>
                <key>_CFURLString</key>
                <string>/System/Applications/Mail.app</string>
                <key>_CFURLStringType</key>
                <integer>0</integer>
            </dict>
        </dict>
    </dict>'
killall Dock

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

defaults write com.apple.finder QLEnableTextSelection -bool true
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
defaults write com.apple.finder ShowPathbar -bool true
killall Finder
