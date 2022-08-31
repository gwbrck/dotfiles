# Note: The names for the Arc theme variations are terrible.
# "Darker" is actually LESS DARK than "Dark".

export SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock
gsettings set org.gnome.desktop.interface gtk-theme Arc-Dark


swaymsg client.focused "#5294E2 #5294E2 #FFFFFF #5294E2"
swaymsg client.unfocused "#383C4A #383C4A #D3DAE3 #383C4A #383C4a"
