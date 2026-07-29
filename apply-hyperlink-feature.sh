#!/usr/bin/env bash
# Applies the OSC-8 hyperlink feature (post-v1 hardening item 1) to a tadka
# checkout. Run from the repository root, e.g.:
#
#   ./apply-hyperlink-feature.sh
#
# This only calls `git apply --check` then `git apply`; it does not commit,
# stage, or touch anything beyond the files the patch lists below. Review the
# diff (hyperlink.patch, next to this script) before running if you'd like to
# see the change first: `git apply --stat hyperlink.patch`.
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
patch_file="$script_dir/hyperlink.patch"

if [ ! -f "$patch_file" ]; then
  echo "error: hyperlink.patch not found next to this script ($patch_file)" >&2
  exit 1
fi

if [ ! -d .git ]; then
  echo "error: run this from the root of your tadka git checkout (no .git found here)" >&2
  exit 1
fi

echo "Checking that the patch applies cleanly (no changes made yet)..."
if ! git apply --check "$patch_file"; then
  echo
  echo "error: patch does not apply cleanly against this checkout." >&2
  echo "This usually means your working tree has diverged from the state" >&2
  echo "this patch was generated against. Nothing has been changed." >&2
  exit 1
fi

echo "Applying patch..."
git apply "$patch_file"

echo
echo "Done. Files changed:"
git status --porcelain -- \
  CHANGELOG.md \
  src/Tadka.hs \
  src/Tadka/Internal/Config.hs \
  src/Tadka/Internal/Render.hs \
  src/Tadka/Internal/Renderer/Graphical.hs \
  src/Tadka/Internal/Terminal.hs \
  tadka.cabal \
  test/golden/Fixtures.hs \
  test/golden/Main.hs \
  test/golden/fixtures/single-label-hyperlink.txt \
  test/props/Hyperlink.hs \
  test/props/Main.hs \
  test/props/TermColor.hs

cat <<'EOF'

Nothing has been staged or committed — review with `git diff` / `git diff --cached`
once you `git add`, then build and test as usual, e.g.:

  cabal build
  cabal test golden
  cabal test props

Note: this was developed and verified against GHC 9.4.7 (apt) plus
apt-installed libghc-*-dev packages, since Hackage wasn't reachable in that
environment. Your GHC 9.10 + cabal/Hackage toolchain should behave
identically for every file this patch touches; the one pre-existing,
unrelated mismatch observed under GHC 9.4.7 was in the "generated-parseerror"
golden fixture (a GHC.Base vs GHC.Internal.Base module-qualification
difference between GHC versions in TH's `pprint` output) — untouched by this
patch and expected to pass as-is on your GHC 9.10 setup.
EOF
