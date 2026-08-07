# tools/

Developer tooling that is **not** part of the published package.

- `gen-width-table.hs` — regenerates `Tadka.Internal.Width`'s data table from a
  pinned Unicode Character Database snapshot. Run: `runghc tools/gen-width-table.hs`.
- `check-generated.sh` — discipline check: greps the `generated-parseerror`
  golden fixture for anything that isn't a single function application,
  confirming every `deriveDiagnostic`-generated method body is a direct call
  to a shared function rather than inline logic. Run: `sh tools/check-generated.sh`.
  Wired into `make test` and CI.
- `check-compile-fail.sh` — confirms each malformed `deriveDiagnostic` use
  under `test/compile-fail/` fails to compile with the expected error message
  at the splice site, rather than compiling or failing for the wrong reason.
  Run: `sh tools/check-compile-fail.sh`. Wired into `make test` and CI.
