#!/usr/bin/env bash
# Applies Phase 12 (multi-source diagnostics) to a tadka checkout at v1.0.0.0
# (i.e. with the issue-remediation, collection-labels, and hyperlink patches
# already applied — this is the current state of the uploaded repo). Run from
# the repository root, e.g.:
#
#   bash apply-phase-12-multifile.sh
#
# This only calls `git apply --check` then `git apply`; it does not commit,
# stage, or touch anything beyond the files the patch lists below.
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
patch_file="$script_dir/phase-12-multifile.patch"

if [ ! -f "$patch_file" ]; then
  echo "error: phase-12-multifile.patch not found next to this script ($patch_file)" >&2
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
  echo "This patch is built on top of v1.0.0.0 with the issue-remediation," >&2
  echo "collection-labels, and hyperlink patches already applied — make sure" >&2
  echo "your checkout is at that state first. Nothing has been changed." >&2
  exit 1
fi

echo "Applying patch..."
git apply "$patch_file"

echo
echo "Done. Files changed:"
git status --porcelain -- \
  CHANGELOG.md \
  Tadka_Phase12_Spec.md \
  src/Tadka.hs \
  src/Tadka/Internal.hs \
  src/Tadka/Internal/Context.hs \
  src/Tadka/Internal/Renderer/Graphical.hs \
  src/Tadka/Internal/Renderer/Json.hs \
  src/Tadka/Internal/Renderer/Narratable.hs \
  tadka.cabal \
  test/golden/Fixtures.hs \
  test/golden/Main.hs \
  test/golden/fixtures/cross-file.txt \
  test/golden/fixtures/json-cross-file.txt \
  test/golden/fixtures/json-cycle.txt \
  test/golden/fixtures/json-single.txt \
  test/golden/fixtures/json-truncated.txt \
  test/golden/fixtures/narr-cross-file.txt \
  test/props/Main.hs \
  test/props/Phase12.hs

cat <<'EOF'

Nothing has been staged or committed. Build and test as usual, e.g.:

  cabal build
  cabal test golden
  cabal test props

IMPORTANT — version bump required before release: this patch changes the
JSON DTO shape (LabelDTO gains a "file" field on every label). Bump
tadka.cabal's version past 1.0.0.0 before publishing. See
Tadka_Phase12_Spec.md §5 for the exact rationale and affected fixtures.

What this adds: a Context can now hold labels resolved against more than one
NamedSource (mkContextMulti / mkContextMultiDegrading / buildContextMulti),
closing the single-source limitation identified when comparing tadka against
Error.Diagnose. Every single-source function (mkContext, mkContextDegrading,
buildContext, buildContextWith) is now implemented as the one-group special
case of its multi-source counterpart, not a second copy of the logic — and is
unchanged in signature and behaviour. All three renderers walk every source
group in order; the derive macro and genericContext remain single-source
only in this phase (see Tadka_Phase12_Spec.md §6 for why, and what's left for
later). Full spec: Tadka_Phase12_Spec.md.

Verified (against GHC 9.4.7 + apt packages — Hackage wasn't reachable in
this sandbox, so this hasn't been run against your real GHC 9.6-9.14/cabal
toolchain; same caveat as the prior collection-labels/hyperlink patches):
  - Full library builds clean with -Wall -Werror.
  - Existing golden suite: every graphical and narratable fixture unchanged,
    byte-for-byte, including generated-parseerror and single-label-hyperlink
    — proven by running the full suite against the new code BEFORE adding
    any new fixture, confirming the only diffs were the three JSON fixtures
    the intentional "file" field change affects (json-single, json-cycle,
    json-truncated; json-cause is unaffected, it has no context at all).
  - New cross-file / narr-cross-file / json-cross-file golden fixtures: a
    real two-file diagnostic (a name imported in one file, defined with a
    conflicting type in another) rendered through all three handlers,
    visually inspected for correctness (two separate gutter blocks in the
    graphical case, two Location: sentences in the narratable case, two
    differently-file-stamped labels in the JSON case).
  - Existing property suite (every prior group, Phase 1 through the
    edge-case/security suites): unchanged, all pass, zero regressions.
  - New Phase12 property group (6 properties, 100 generated cases each):
    mkContextMulti's Left-iff-any-span-out-of-bounds guarantee generalised
    across groups; mkContextMultiDegrading's per-group and cross-group
    count/order preservation; mkContext/mkContextDegrading proven exactly
    equivalent to the one-group case of their multi- counterparts;
    buildContextMulti's all-empty-groups -> NoContext behaviour, its
    dispatch to mkContextMultiDegrading, and its dropping of an empty group
    without affecting the others.
  - This entire patch was additionally verified from a clean-room checkout:
    a fresh `git archive` of the pre-patch commit, patch applied there from
    scratch, full library + both test suites rebuilt and re-run against that
    isolated copy — same results as above, confirming the patch is
    self-contained and doesn't depend on anything left over in the working
    tree it was developed in.
EOF
