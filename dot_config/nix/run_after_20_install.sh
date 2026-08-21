#!/bin/bash
set -euo pipefail

nix_daemon_profile=/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

if [[ -r "$nix_daemon_profile" ]]; then
  # shellcheck disable=SC1090
  source "$nix_daemon_profile"
fi

if command -v nix >/dev/null 2>&1; then
  exit 0
fi

if [[ -e /nix/receipt.json || -e /nix/nix-installer ]]; then
  echo "Nix installation exists but nix is unavailable; repair it before applying chezmoi again." >&2
  exit 1
fi

curl -sSfL https://artifacts.nixos.org/nix-installer \
  | sh -s -- install --no-confirm --enable-flakes

# The installer cannot update the environment of this running process.
# shellcheck disable=SC1090
source "$nix_daemon_profile"
command -v nix >/dev/null
