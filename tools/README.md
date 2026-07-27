# tools/

Developer tooling that is **not** part of the published package.

- `gen-width-table.hs` — regenerates `Tadka.Internal.Width`'s data table from a
  pinned Unicode Character Database snapshot. Scaffold in Phase 0; implemented
  in Phase 1. Run: `runghc tools/gen-width-table.hs`.
