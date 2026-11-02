#!/usr/bin/env bash
set -euo pipefail
output=$(cabal list-bin -v0 monoscope-cli:exe:monoscope "$@")
printf '%s\n' "$output" | grep -E '^/.*/monoscope$'
