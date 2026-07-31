#!/usr/bin/env bash
#
# apply-remediation-patch.sh — apply tadka-remediation-issues-1-2-3-4-5.patch
# to a local Tadka checkout, then optionally run the build/test suite.
#
# Covers issues #1-#5 from the remediation plan:
#   1. Ann annotations silently dropped by the graphical handler
#   2. multiple same-kind labels indistinguishable under ColorNever
#   3. deriveDiagnostic couldn't generate diagnosticCause
#   4. StaleReason's unreachable SourceMismatch constructor
#   5. genericContext silently dropping [Span] fields
# Issues #6 and #7 (perf: offsetToLineCol / SourceCode O(n) per call, and
# whatever the plan's remaining item is) are NOT included in this patch.
#
# Usage:
#   cd /path/to/tadka          # the directory containing tadka.cabal
#   /path/to/apply-remediation-patch.sh [path/to/tadka-remediation-issues-1-2-3-4-5.patch]
#
# If no patch path is given, the script looks for
# tadka-remediation-issues-1-2-3-4-5.patch next to itself.

set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
patch_file="${1:-$script_dir/tadka-remediation-issues-1-2-3-4-5.patch}"

if [ ! -f "tadka.cabal" ]; then
  echo "error: tadka.cabal not found in $(pwd)." >&2
  echo "       Run this from the tadka project root (the directory" >&2
  echo "       containing tadka.cabal), e.g.: cd tadka/tadka" >&2
  exit 1
fi

if [ ! -f "$patch_file" ]; then
  echo "error: patch file not found: $patch_file" >&2
  exit 1
fi

echo "== Checking patch applies cleanly (dry run) =="
if command -v git >/dev/null 2>&1 && [ -d .git ]; then
  git apply --check "$patch_file"
  echo "== Applying with git apply =="
  git apply "$patch_file"
else
  patch -p1 --dry-run < "$patch_file"
  echo "== Applying with patch -p1 =="
  patch -p1 < "$patch_file"
fi

echo
echo "Patch applied. Files touched:"
grep '^diff --git' "$patch_file" | sed 's/^diff --git a\///; s/ b\/.*//'

echo
read -r -p "Run the build/test suite now? [y/N] " run_tests
if [[ "$run_tests" =~ ^[Yy]$ ]]; then
  if [ -x "./build-and-test.sh" ]; then
    ./build-and-test.sh
  else
    echo "build-and-test.sh not found/executable here; running the equivalent commands directly."
    cabal build --only-dependencies all --flags=werror
    cabal build all
    cabal test golden --test-show-details=direct
    cabal test props --test-show-details=direct
    cabal test interop --test-show-details=direct
    sh tools/check-generated.sh
    sh tools/check-compile-fail.sh
  fi
else
  echo "Skipped. Run ./build-and-test.sh (or 'cabal build all && make test') whenever you're ready."
fi
