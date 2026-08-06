# Convenience targets. Each test suite is independently runnable, per the
# Phase 0 exit criteria and the cross-phase note "testing is not deferred."

.PHONY: build test test-golden test-props test-interop check-generated check-compile-fail clean gen-width-table format format-check

# All first-party Haskell sources: library, both test suites, the three
# interop sub-libraries, and the standalone tools/ scripts. Kept as one
# variable so format/format-check always agree on what "the codebase" is.
HS_SOURCES := $(shell find src test interop tools -name '*.hs')

build:
	cabal build all

test: test-golden test-props test-interop check-generated check-compile-fail

# Requires ormolu on PATH (`cabal install ormolu` or `ghcup install ormolu`).
# `format` rewrites files in place; `format-check` (used in CI) fails without
# modifying anything if any file isn't already formatted.
format:
	ormolu --mode inplace $(HS_SOURCES)

format-check:
	ormolu --mode check $(HS_SOURCES)

test-golden:
	cabal test golden --test-show-details=direct

test-props:
	cabal test props --test-show-details=direct

# Interop round-trip tests (megaparsec / attoparsec / GHC SrcSpan adapters).
test-interop:
	cabal test interop --test-show-details=direct

# Phase 8 discipline: generated method bodies must be direct calls (vision §6).
check-generated:
	sh tools/check-generated.sh

# Phase 8: each malformed deriveDiagnostic use must fail at compile time.
check-compile-fail:
	sh tools/check-compile-fail.sh

# Scaffold in Phase 0; implemented in Phase 1.
gen-width-table:
	runghc tools/gen-width-table.hs

clean:
	cabal clean
