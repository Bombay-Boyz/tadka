# Contributing to `tadka`

## Enforced conventions (review-enforced, not type-enforced)

These two conventions are the crux of the v5 design. They are **not** checked by
the type system, so they are checked here and — for the derive macro — by a CI
grep check added in Phase 8. Bake both into your mental model before touching
Phase 4 (renderer scaffolding) or Phase 5 (graphical handler), the largest
surfaces where a shortcut could quietly violate either.

1. **One path** (Phases 4, 7). Every `GraphicalOptions` / `NarratableOptions` /
   `JsonOptions` value is constructed only by `selectRenderer`. Every
   `Aeson.Value` for a diagnostic is produced only by the JSON handler's DTO
   conversion. Do not add a second construction site.

2. **Provably thin** (Phase 8). Every method body `deriveDiagnostic` generates
   must be a direct, unmodified call to a plain, exported `Tadka.Internal`
   function that a manual instance could also call. If a new field needs logic
   the current `Tadka.Internal` surface can't express as a plain call, extract a
   new plain function first — never grow logic inside a `Q` splice.

## Scope boundaries — permanent, not "later"

- No fourth render `Target`; `Target`/`Output` are closed by design.
- No `FromJSON` decode path in v1.
- No dedicated interop helper beyond `megaparsec`, `attoparsec`, GHC `SrcSpan`.

## Style

Follow `principles.docx`: total functions, explicit error handling (`Maybe` /
`Either` / custom error types, never silent failure), illegal states made
unrepresentable via smart constructors and unexported constructors, IO at the
edges, property-based tests alongside unit tests, warnings-as-errors.

### Lint & format

Install once: `cabal install hlint ormolu`.

- `make lint` — runs `hlint` over `src/`, `test/`, `interop/`, `tools/` using
  the repo's `.hlint.yaml`.
- `make format` — runs `ormolu --mode inplace` over the same sources; run this
  before committing.
- `make format-check` — same, but only checks and never rewrites; this is what
  CI runs.

Both also run in CI (`lint-and-format` job) on every push and PR — a PR with
unformatted code or a real hlint warning won't go green. If hlint's take on
something conflicts with a deliberate style choice (see the export-list note
in `.hlint.yaml` for a precedent), add a scoped ignore with a comment, not a
blanket one.

## Review checklist — `deriveDiagnostic` discipline (vision §6)

When changing `Tadka.Internal.TH` or the `Tadka.Internal` surface it calls:

- [ ] Every generated `Diagnostic` method body is a **direct, unmodified call**
      to a function exported from `Tadka.Internal` (or a class default). No
      `case`/`if`/`let`/`where`/lambda logic inside the `Q` splice.
- [ ] If a new field needs behaviour the current `Tadka.Internal` surface can't
      express as a plain call, a new plain function is extracted **there first**
      (with its own Phase-1/2/3-style test), rather than adding logic to the splice.
- [ ] The only exception is the default `message` (`pretty . show`).
- [ ] `tools/check-generated.sh` still passes (CI enforces this), and
      `tools/check-compile-fail.sh` still rejects malformed specs.
