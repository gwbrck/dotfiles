#!/bin/bash
set -eufo pipefail

EXEC_THEME=$(which terminal-theme-toggle)

WORK_DIR=$(mktemp -d)

cd $WORK_DIR

git clone https://github.com/bouk/dark-mode-notify.git

cd dark-mode-notify

swiftc main.swift -o $HOME/bin/dark-mode-notify

echo "
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\"
\"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
    <key>Label</key>
    <string>ke.bou.dark-mode-notify</string>
    <key>KeepAlive</key>
    <true/>
    <key>ProgramArguments</key>
    <array>
       <string>${HOME}/bin/dark-mode-notify</string>
       <string>${EXEC_THEME}</string>
    </array>
</dict>
</plist>
" >~/Library/LaunchAgents/ke.bou.dark-mode-notify.plist

rm -rf WORK_DIR
