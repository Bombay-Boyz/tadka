#!/usr/bin/env bash
# apply-and-test-phase-d.sh
#
# Applies phase-d-sum-type-derive.patch to a tadka checkout and verifies it
# end to end: build the library, build the test suites, run them, and run a
# handful of negative (should-fail-to-compile) checks that prove
# deriveDiagnosticSum's completeness check actually rejects bad specs.
#
# Usage:
#   ./apply-and-test-phase-d.sh /path/to/tadka
#
# Expects: the patch file `phase-d-sum-type-derive.patch` in the same
# directory as this script, and a working `cabal`/`ghc` with normal Hackage
# access (this was developed and verified in a network-sandboxed environment
# using a plain `ghc --make` + apt-installed libghc-*-dev packages instead of
# cabal, since that sandbox couldn't reach Hackage -- see the note at the
# bottom of this script if you need to reproduce it the same way).
set -euo pipefail

REPO="${1:?usage: $0 /path/to/tadka}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PATCH="$SCRIPT_DIR/phase-d-sum-type-derive.patch"

if [ ! -f "$PATCH" ]; then
  echo "error: $PATCH not found next to this script" >&2
  exit 1
fi

cd "$REPO"

if [ -n "$(git status --porcelain)" ]; then
  echo "error: $REPO has uncommitted changes -- commit or stash them first" >&2
  exit 1
fi

echo "==> Applying patch"
git apply --check "$PATCH"        # dry run first; aborts cleanly if it won't apply
git apply "$PATCH"

echo "==> Building the library"
cabal build lib:tadka

echo "==> Building and running the property test suite (includes the new Phase13 group)"
cabal test props --test-show-details=streaming

echo "==> Building and running the golden and interop suites (unaffected by this change, run for regression safety)"
cabal test golden --test-show-details=streaming
cabal test interop --test-show-details=streaming

echo "==> Negative-path checks: each of these three must FAIL to compile"
NEG_DIR="$(mktemp -d)"
trap 'rm -rf "$NEG_DIR"' EXIT

cat > "$NEG_DIR/Missing.hs" <<'EOF'
{-# LANGUAGE TemplateHaskell #-}
module Missing where
import Tadka
data T = A { aSrc :: NamedSource } | B { bSrc :: NamedSource }
deriveDiagnosticSum [ ('A, defaultSpec) ] ''T
EOF

cat > "$NEG_DIR/Duplicate.hs" <<'EOF'
{-# LANGUAGE TemplateHaskell #-}
module Duplicate where
import Tadka
data U = A { aSrc :: NamedSource } | B { bSrc :: NamedSource }
deriveDiagnosticSum [ ('A, defaultSpec), ('B, defaultSpec), ('A, defaultSpec) ] ''U
EOF

cat > "$NEG_DIR/NonRecord.hs" <<'EOF'
{-# LANGUAGE TemplateHaskell #-}
module NonRecord where
import Tadka
data V = A { aSrc :: NamedSource } | C Int
deriveDiagnosticSum [ ('A, defaultSpec), ('C, defaultSpec) ] ''V
EOF

for f in Missing Duplicate NonRecord; do
  echo "  -- $f.hs (expect: compile error) --"
  if cabal exec -- ghc -fno-code "$NEG_DIR/$f.hs" -outputdir "$NEG_DIR/out-$f" 2>&1 | tee /tmp/neg-"$f".log | grep -q "error:"; then
    echo "  OK: failed to compile, as expected"
  else
    echo "  UNEXPECTED: $f.hs compiled cleanly -- the completeness check has a gap" >&2
    exit 1
  fi
done

echo "==> All checks passed."

# --- Reproducing this in a network-restricted sandbox (no Hackage access) ---
# This patch was originally verified without cabal at all, using the
# distro's prebuilt Haskell library packages instead of resolving against
# Hackage:
#
#   apt-get install -y ghc cabal-install \
#     libghc-prettyprinter-dev libghc-prettyprinter-ansi-terminal-dev \
#     libghc-ansi-terminal-dev libghc-network-uri-dev libghc-aeson-dev \
#     libghc-hedgehog-dev libghc-megaparsec-dev libghc-attoparsec-dev
#   ghc -XGHC2021 -isrc -itest/props test/props/Main.hs -fno-code   # typecheck everything
#   ghc -XGHC2021 -O0 -isrc -itest/props <small Main that runs Phase13.group> -o /tmp/run
#   /tmp/run                                                        # real run, not just typecheck
#
# Use this fallback only if `cabal build`/`cabal test` can't reach Hackage
# from wherever this script is running.
