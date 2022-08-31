# Note: The names for the Arc theme variations are terrible.
# "Darker" is actually LESS DARK than "Dark".
export SWAYSOCK=/run/user/$(id -u)/sway-ipc.$(id -u).$(pgrep -x sway).sock
gsettings set org.gnome.desktop.interface gtk-theme Arc-Lighter

swaymsg client.focused "#5294E2 #5294E2 #FFFFFF #5294E2"
swaymsg client.unfocused "#F5F6F7 #F5F6F7 #5C616C #F5F6F7 #F5F6F7"
