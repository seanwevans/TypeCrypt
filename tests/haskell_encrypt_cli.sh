#!/usr/bin/env bash
# Verify the Haskell CLI encrypt command emits hex ciphertext
set -euo pipefail

dir="$(cd "$(dirname "$0")/.." && pwd)"
cd "$dir"

if ! command -v cabal >/dev/null 2>&1; then
  echo "Skipping Haskell CLI encrypt test: cabal not installed"
  exit 0
fi

ct=$(cd haskell && cabal exec runghc -- -isrc ../tests/crypt_hs.hs encrypt int)

if [[ -z "$ct" ]]; then
  echo "Expected ciphertext output" >&2
  exit 1
fi

if [[ ! "$ct" =~ ^[0-9a-f]+$ ]]; then
  echo "Ciphertext is not lowercase hex: $ct" >&2
  exit 1
fi

echo "haskell encrypt cli ok"
