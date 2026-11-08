
#!/usr/bin/env bash
set -euo pipefail

VERSIONS=(
  9.6.7
  9.8.4
  9.10.3
  9.12.4
  9.14.1
)

for version in "${VERSIONS[@]}"; do
  echo
  echo "======================================"
  echo " Testing Tadka with GHC $version"
  echo "======================================"

  ghcup set ghc "$version"

  ghc --numeric-version
  cabal --numeric-version

  cabal clean

  cabal build --only-dependencies all --flags=werror
  cabal build all

  cabal test golden --test-show-details=direct
  cabal test props --test-show-details=direct
  cabal test interop --test-show-details=direct
  cabal test docs --test-show-details=direct
  cabal test hello --test-show-details=direct
  cabal test smoke --test-show-details=direct

  sh tools/check-generated.sh
  sh tools/check-compile-fail.sh

  echo "PASS: GHC $version"
done

echo
echo "ALL FIVE GHC VERSIONS PASSED"
