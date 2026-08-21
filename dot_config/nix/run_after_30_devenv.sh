#!/bin/bash
set -euo pipefail

nix_daemon_profile=/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

if [[ -r "$nix_daemon_profile" ]]; then
  # shellcheck disable=SC1090
  source "$nix_daemon_profile"
fi

if command -v devenv >/dev/null 2>&1; then
  exit 0
fi

command -v nix >/dev/null
nix profile add nixpkgs#devenv
