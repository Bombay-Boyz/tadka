#!/bin/sh
# Applies tadka-close-3-gaps.patch (fixes #1, #2, #3 from the code review) to
# a clean tadka checkout, then rebuilds and runs the full test matrix so you
# don't have to trust the diff blindly.
#
# Usage:
#   ./apply-close-3-gaps.sh /path/to/tadka
#
# Requires: a GHC + cabal toolchain that can already build tadka (see
# tadka.cabal's `tested-with`), run from the repository root.
set -e

REPO="${1:?usage: apply-close-3-gaps.sh /path/to/tadka}"
PATCH_DIR="$(cd "$(dirname "$0")" && pwd)"
PATCH="$PATCH_DIR/tadka-close-3-gaps.patch"

cd "$REPO"

if [ -n "$(git status --porcelain)" ]; then
  echo "error: $REPO has uncommitted changes; commit or stash them first." >&2
  exit 1
fi

echo "== checking patch applies cleanly =="
git apply --check "$PATCH"

echo "== applying patch =="
git apply "$PATCH"

echo "== building (werror on, per cabal.project) =="
cabal build lib:tadka

echo "== compile-fail suite (includes the new LabelsWithoutSource fixture) =="
sh tools/check-compile-fail.sh

echo "== golden suite =="
cabal test golden

echo "== property suite (includes the new related-cause and cause-attribution checks) =="
cabal test props

echo "== interop suite =="
cabal test interop

echo
echo "All checks passed. Changed files:"
git diff --stat HEAD
