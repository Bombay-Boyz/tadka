#!/bin/sh
# Phase 8 exit criterion: each bad deriveDiagnostic use must FAIL to compile
# (a compile error at the splice site, not a runtime failure).
set -e

# Build the local library under the default configuration first so that
# `cabal exec` exposes a consistent tadka (avoids stale/hidden-package errors
# after a build under different flags).
cabal build -v0 lib:tadka >/dev/null 2>&1 || true

fail=0
check() {
  file="test/compile-fail/$1.hs"; want="$2"
  out=$(cabal exec -- ghc -fno-code -package tadka -XTemplateHaskell -XOverloadedStrings -XDeriveGeneric "$file" 2>&1) && {
    echo "FAIL: $1 compiled but should not have"; fail=1; return; }
  if printf '%s' "$out" | grep -qF "$want"; then
    echo "ok   $1 (rejected: \"$want\")"
  else
    echo "FAIL: $1 rejected but without expected message \"$want\""
    printf '%s\n' "$out" | grep -iE "error|specCode|specSourceField|field" | head -3
    fail=1
  fi
}
check WrongType "specSourceField"
check NotAField "is not a field of the target type"
check BadCode   "specCode: invalid literal"
  check TwoSources "exactly one NamedSource field"
  check SuccessCriterion "specLabelFields"
[ "$fail" -eq 0 ] && echo "check-compile-fail: OK" || { echo "check-compile-fail: FAILED"; exit 1; }
