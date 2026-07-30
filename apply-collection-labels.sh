#!/usr/bin/env bash
# Applies collection labels in deriveDiagnostic (post-v1 hardening item 3) to
# a tadka checkout that already has the OSC-8 hyperlink patch applied. Run
# from the repository root, e.g.:
#
#   bash apply-collection-labels.sh
#
# This only calls `git apply --check` then `git apply`; it does not commit,
# stage, or touch anything beyond the files the patch lists below.
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
patch_file="$script_dir/collection-labels.patch"

if [ ! -f "$patch_file" ]; then
  echo "error: collection-labels.patch not found next to this script ($patch_file)" >&2
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
  echo "This patch is built on top of the OSC-8 hyperlink patch from the" >&2
  echo "previous step — make sure that one is applied and committed first." >&2
  echo "Nothing has been changed." >&2
  exit 1
fi

echo "Applying patch..."
git apply "$patch_file"

echo
echo "Done. Files changed:"
git status --porcelain -- \
  CHANGELOG.md \
  src/Tadka/Internal/TH.hs \
  tadka.cabal \
  test/props/Main.hs \
  test/props/LabelCollection.hs \
  test/compile-fail/WrongCollectionType.hs \
  tools/check-compile-fail.sh

cat <<'EOF'

Nothing has been staged or committed. Build and test as usual, e.g.:

  cabal build
  cabal test golden
  cabal test props
  make check-compile-fail

What this adds: two new DiagnosticSpec fields,
specLabelCollectionFields/specSecondaryLabelCollectionFields. Each names a
[Span]-typed field; every element of that field's runtime list becomes its
own label sharing the given text (for a variable number of same-kind
occurrences known only at runtime — every prior declaration of a name, every
match of a banned pattern — where specLabelFields needs one field per label
fixed at splice time). buildContext/buildContextWith are completely
unchanged; this is TH-layer only.

Verified (against GHC 9.4.7 + apt packages, same caveat as the hyperlink
patch — Hackage wasn't reachable in that sandbox, so this hasn't been run
against your real GHC 9.10/cabal toolchain):
  - Full library builds clean with -Wall -Werror.
  - Existing golden suite: no changes to any fixture, including
    generated-parseerror — the splice only emits the new collection-handling
    code when a spec actually declares a collection field, so every existing
    deriveDiagnostic call generates byte-identical code to before.
  - Existing property suite (all prior groups, e.g. Phase 8 and
    Labels' derive-vs-manual checks): unchanged, all pass.
  - New LabelCollection property group: a derived instance with collection
    fields renders identically (graphical/narratable/JSON) to a hand-written
    buildContext/buildContextWith call expanding the same randomly generated
    list, across varying list lengths (0 to 8, plus a 200-element totality
    check), an all-primary-collection case, and a mixed
    fixed-primary-plus-secondary-collection case.
  - New WrongCollectionType compile-fail case: a Span-typed (not
    [Span]-typed) field is correctly rejected at the splice site with a
    specLabelCollectionFields error, alongside all five pre-existing
    compile-fail cases (still correctly rejected, unchanged).
EOF
