#!/usr/bin/env sh
set -euo pipefail

keyfile="$HOME/.config/sops/age/keys.txt"

if [ ! -f "$keyfile" ]; then
  mkdir -p "$(dirname "$keyfile")"
  echo "Generating new age machine key with Secure Enclave..."
  age-plugin-se keygen --access-control=any-biometry >"$keyfile"
  chmod 600 "$keyfile"
fi
