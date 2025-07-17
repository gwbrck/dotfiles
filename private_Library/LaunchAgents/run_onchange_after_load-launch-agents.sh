#!/bin/bash

# Dieses Skript wird ausgeführt, nachdem .plist-Dateien geändert wurden.
set -eu

PLIST_PATH="$HOME/Library/LaunchAgents/com.user.env.r_profile_user.plist"

echo "Reloading Launch Agent: $PLIST_PATH"

# Zuerst entladen (ignoriere Fehler, falls es noch nicht geladen war), dann laden.
launchctl unload "$PLIST_PATH" 2>/dev/null || true
launchctl load "$PLIST_PATH"

# Setze die Variable auch sofort, damit sie in der aktuellen Shell verfügbar ist
launchctl setenv R_PROFILE_USER "$HOME/.config/R/.Rprofile"
