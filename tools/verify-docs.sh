#!/usr/bin/env bash
# Builds and runs every example in the README and tutorial, then writes ONE
# report file that can be sent for review.
#
#   tools/verify-docs.sh [report-file]        (default: verify-report.txt)
#
# It checks, on the GHC that `ghc` currently points at:
#   1. the whole project builds with warnings as errors;
#   2. the docs, hello and smoke test-suites pass (examples run, and the README
#      and tutorial match them);
#   3. the examples also build in a FRESH project that lists only the
#      dependencies the documentation tells a reader to add
#      (base, prettyprinter, tadka, text), and the README's complete program
#      runs there.
# Run it once per GHC version (ghcup set ghc <version>) to cover several.
set -u
cd "$(dirname "$0")/.." || exit 2
REPO=$(pwd)
REPORT="${1:-verify-report.txt}"
LOG=$(mktemp)
FAILED=""
: > "$REPORT"

say() { printf '%s\n' "$*" | tee -a "$REPORT"; }

# step NAME COMMAND...  : run and record PASS or FAIL. A pass keeps the
# per-example `ok` lines; a failure keeps the last 80 lines of output.
step() {
  local name="$1"; shift
  say ""
  say "=== $name"
  say "\$ $*"
  if "$@" >"$LOG" 2>&1; then
    say "RESULT: PASS"
    grep -E '^ *ok ' "$LOG" >>"$REPORT" || true
  else
    say "RESULT: FAIL"
    FAILED="$FAILED | $name"
    tail -n 80 "$LOG" >>"$REPORT"
  fi
}

say "verify-docs report, $(date -u +%Y-%m-%dT%H:%M:%SZ)"
say "ghc:   $(ghc --numeric-version 2>&1)"
say "cabal: $(cabal --numeric-version 2>&1)"
say "git:   $(git rev-parse --short HEAD 2>/dev/null || echo unknown)$(git status --porcelain 2>/dev/null | grep -q . && echo ' (uncommitted changes)')"

step "build everything, warnings as errors" cabal build all --flags=werror
step "docs suite (examples + documentation check)" cabal test docs --test-show-details=direct
step "hello suite (README quick-start program)"   cabal test hello --test-show-details=direct
step "smoke suite (tadka-highlight README example)" cabal test smoke --test-show-details=direct

# The real output of every example, for comparison with the documents.
if cabal run test:docs -- --print >docs-output-current.txt 2>"$LOG"; then
  say ""; say "=== example output written to docs-output-current.txt"
else
  say ""; say "=== could not print example output"; tail -n 40 "$LOG" >>"$REPORT"
fi

# Fresh project: only the dependencies the documentation lists.
FRESH=$(mktemp -d)
mkdir -p "$FRESH/src/Examples" "$FRESH/hello"
cp test/docs/Example.hs "$FRESH/src/"
cp test/docs/Examples/*.hs "$FRESH/src/Examples/"
cp test/hello/Main.hs "$FRESH/hello/Main.hs"
cat >"$FRESH/fresh-docs.cabal" <<CABAL
cabal-version: 3.0
name:          fresh-docs
version:       0
build-type:    Simple

library
  default-language: GHC2021
  hs-source-dirs:   src
  exposed-modules:  Example
                    Examples.Quickstart
                    Examples.Derive
                    Examples.Labels
                    Examples.Chain
                    Examples.MultiFile
                    Examples.Family
                    Examples.CustomSource
                    Examples.AdHoc
                    Examples.Highlight
                    Examples.Formats
                    Examples.Configure
  build-depends:    base, prettyprinter, tadka, text

executable hello
  default-language: GHC2021
  hs-source-dirs:   hello
  main-is:          Main.hs
  build-depends:    base, prettyprinter, tadka, text
CABAL
printf 'packages: .\n          %s\n' "$REPO" >"$FRESH/cabal.project"

say ""
say "=== fresh project in $FRESH"
step "fresh project: examples build with only the documented dependencies" \
  bash -c "cd '$FRESH' && cabal build fresh-docs:lib:fresh-docs"
step "fresh project: the README program builds and runs" \
  bash -c "cd '$FRESH' && cabal run hello"

say ""
if [ -z "$FAILED" ]; then
  say "VERIFICATION: ALL STEPS PASSED"
  rm -rf "$FRESH"
else
  say "VERIFICATION: FAILED STEPS:$FAILED"
  say "(fresh project kept at $FRESH for inspection)"
fi
rm -f "$LOG"
say "report written to $REPORT"
[ -z "$FAILED" ]
