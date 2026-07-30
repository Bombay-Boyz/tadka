# tadka

> **Point at the problem. Say it clearly. Make it beautiful.**

A diagnostic reporting framework for Haskell, targeting full feature parity with
Rust's [`miette`](https://github.com/zkat/miette): structured error types, three
pluggable renderers (graphical / narratable / JSON), error codes with doc links,
related-error chains, labeled multi-span source snippets, and derive-macro
ergonomics.

Design: `Tadka_Vision_v5.md`. Build plan: `Tadka_Implementation_Spec.md`.

## Status

**Phase 11 — consolidation & v1.** Public API reconciled to vision §8
(`Offset`/`Length` are now internal-only; spans are the public position type),
dependency upper bounds added, and all golden + property suites plus the derive
and interop checks run together. The vision's Success Criterion is an
end-to-end test: a misspelled span field fails at compile time, and a staled
span renders a clear in-report reason on every handler. Tagged **v1.0.0.0**.

**Phase 10 — interop helpers.** One-directional adapters turning megaparsec,
attoparsec, and GHC `SrcSpan` positions into tadka `Span`/`Offset`, each a plain
function in its own cabal sub-library so the core never depends on a parser
package. Per-library round-trip tests confirm the converted position resolves to
the same line/column the upstream library reports.

**Phase 9 — generics label-wiring.** `genericContext` derives *only* the
`context` method, via GHC.Generics, for a record with one `NamedSource` field
and one-or-more `Span` fields (span field names become label text), calling the
same `buildContext`. Its `e -> Context` type means it can touch nothing else;
the record shape is checked at compile time. Proved equal to a hand-written twin
across all three handlers

**Phase 8 — `deriveDiagnostic` macro.** An ordinary Template Haskell splice
(`DiagnosticSpec` + `defaultSpec` + `deriveDiagnostic`) that `reify`-validates
field references, types, and code/url literals at splice time and generates a
`Diagnostic` instance whose every method body is a direct call to a shared
`Tadka.Internal` function — proved by a byte-for-byte derived-vs-manual test
across all three handlers. Malformed specs fail at compile time; a CI grep over
the generated golden fixture enforces the no-logic-in-the-splice discipline.

**Phase 7 — JSON report handler.** `renderJson` builds a dedicated
`DiagnosticDTO` (never `deriving ToJSON` on a diagnostic type) with explicit
per-label `stale` flags and per-level `truncated`/`cycleOmitted` flags. It is
the only route to `Output 'TJson = Aeson.Value`. Locked by golden fixtures and
property-tested for stale-flag derivation and totality.

**Phase 6 — narratable report handler.** `renderNarratable` produces the
accessibility-first prose form, with prose equivalents for stale labels and
related-chain truncation/cycles and `Ann` interpreted via `toProseMarker`.
Locked by golden fixtures and property-tested; every field the graphical handler
shows has a narratable equivalent.

**Phase 5 — graphical report handler.** The full graphical report — severity/code
header, `┌─ file:line:col` location, a line-numbered `│`-rail gutter, source
snippets, and Unicode-width-correct underline carets with per-label cycling —
plus stale-label degradation and related-chain nesting driven by the Phase 3
walk. Locked by four byte-for-byte golden fixtures and property-tested for
non-negative width-aware carets, palette cycling, and totality.

## Module layout (fixed in Phase 0)

| Module                                                | Role                                                             | Compatibility    |
| ----------------------------------------------------- | ---------------------------------------------------------------- | ---------------- |
| `Tadka`                                               | Sole supported public API surface                                | Stable (from v1) |
| `Tadka.Internal`                                      | Shared functions both the derive macro and manual instances call | None             |
| `Tadka.Internal.Width`                                | Bundled Unicode width / grapheme table                           | None             |
| `Tadka.Internal.Context`                              | `buildContext` and context construction                          | None             |
| `Tadka.Internal.Renderer.{Graphical,Narratable,Json}` | The three report handlers                                        | None             |
| `Tadka.Internal.TH`                                   | `deriveDiagnostic` splice                                        | None             |
| `Tadka.Internal.Generics`                             | Generics-based `context` label-wiring                            | None             |
| `Tadka.Internal.Interop`                              | Namespace for Phase 10 parser adapters                           | None             |

## Building

Requires **GHC 9.10.1** and **Cabal ≥ 3.0** (install via [ghcup](https://www.haskell.org/ghcup/)).
`cabal.project` pins a Hackage `index-state` for reproducible builds, so the
first build fetches and compiles the dependencies (this needs network access).

```sh
cabal build all          # build the library, interop sub-libraries, and tests
cabal test all           # run every suite (golden, properties, interop)

# or individually:
cabal test golden        # byte-for-byte rendering fixtures
cabal test props         # Hedgehog property suite
cabal test interop       # megaparsec / attoparsec / GHC SrcSpan round-trips
```

The `werror` flag is enabled project-wide in `cabal.project` (warnings are
errors); the released package leaves it off by default so new GHC warnings can't
break downstream installs.

### Makefile

```sh
make build                 # cabal build all
make test                  # all suites + the two discipline checks below
make test-golden           # golden suite
make test-props            # property suite
make test-interop          # interop round-trip suite
make check-generated       # assert derive-macro output is logic-free
make check-compile-fail    # assert each bad deriveDiagnostic use fails to compile
```

Design notes: `Tadka_Vision_v5.md`. Build plan: `Tadka_Implementation_Spec.md`.
Haskell principles the codebase follows: `principles.md`
