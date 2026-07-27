# Convenience targets. Each test suite is independently runnable, per the
# Phase 0 exit criteria and the cross-phase note "testing is not deferred."

.PHONY: build test test-golden test-props test-interop check-generated check-compile-fail clean gen-width-table

build:
	cabal build all

test: test-golden test-props test-interop check-generated check-compile-fail

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
