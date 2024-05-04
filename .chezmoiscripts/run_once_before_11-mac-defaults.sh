#!/bin/bash
defaults write org.gpgtools.common UseKeychain NO

defaults write com.apple.dock autohide -bool true
defaults write com.apple.dock autohide-delay -float 0
defaults write com.apple.dock autohide-time-modifier -float 0
defaults write com.apple.dock launchanim -bool false
defaults write com.apple.dock orientation right
defaults write com.apple.dock show-recents -bool false

# Disable shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

defaults write com.apple.finder QLEnableTextSelection -bool true
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false
defaults write com.apple.dock expose-animation-duration -float 0.15

defaults write com.apple.dock tilesize -int 64

killall Dock
