# Convenience targets. Each test suite is independently runnable.

HS_SOURCES := $(shell git ls-files '*.hs')


.PHONY: build test test-golden test-props test-interop test-docs check-generated check-compile-fail clean gen-width-table

build:
	cabal build all

test: test-golden test-props test-interop test-docs check-generated check-compile-fail

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

# The examples in the README and tutorial, and the check that the documents match them.
test-docs:
	cabal test docs hello --test-show-details=direct

# Interop round-trip tests (megaparsec / attoparsec / GHC SrcSpan adapters).
test-interop:
	cabal test interop --test-show-details=direct

# Generated method bodies must be direct calls to shared functions.
check-generated:
	sh tools/check-generated.sh

# Each malformed deriveDiagnostic use must fail at compile time.
check-compile-fail:
	sh tools/check-compile-fail.sh

# Regenerate the Unicode width tables.
gen-width-table:
	runghc tools/gen-width-table.hs

clean:
	cabal clean
