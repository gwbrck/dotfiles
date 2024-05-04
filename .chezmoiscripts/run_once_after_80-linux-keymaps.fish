#!/bin/fish

sudo mkdir -p /etc/interception/dual-function-keys/
sudo mkdir -p /opt/interception/
echo "
TIMING:
  TAP_MILLISEC: 200
  DOUBLE_TAP_MILLISEC: 0


MAPPINGS:
  - KEY: KEY_CAPSLOCK
    TAP: KEY_ESC
    HOLD: KEY_FN_1 
    HOLD_START: BEFORE_CONSUME
  - KEY: KEY_BACKSLASH
    TAP: KEY_BACKSLASH
    HOLD: KEY_FN_1 
    HOLD_START: BEFORE_CONSUME
  - KEY: KEY_SLASH
    TAP: KEY_SLASH
    HOLD: KEY_LEFTCTRL
    HOLD_START: BEFORE_CONSUME
  - KEY: KEY_102ND
    TAP: KEY_102ND
    HOLD: KEY_LEFTCTRL
    HOLD_START: BEFORE_CONSUME
" | sudo tee /etc/interception/dual-function-keys/gwbrck-map.yaml

echo "
- JOB: intercept -g \$DEVNODE | dual-function-keys -c /etc/interception/dual-function-keys/gwbrck-map.yaml | /opt/interception/interception-pipe1 | uinput -d \$DEVNODE
  DEVICE:
    EVENTS:
      EV_KEY: [KEY_CAPSLOCK, KEY_BACKSLASH]
" | sudo tee /etc/interception/udevmon.yaml

sudo cp -f {{ joinPath .chezmoi.sourceDir "bin/interception-pipe1" }} /opt/interception/interception-pipe1

sudo systemctl enable udevmon.service
sudo systemctl restart udevmon.service
