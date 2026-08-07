Thanks for taking a look. This is a small, one-person project — bug reports, doc fixes, and small pull requests are all genuinely welcome. No contribution is too small to be worth opening.
  
  ## Getting started
  
  ```sh
  git clone https://github.com/Bombay-Boyz/tadka.git
  cd tadka
  cabal build all
  cabal test all
  ```
  
  Or, with `make`:
  
  ```sh
  make build
  make test
  ```
  
  There are three independently-runnable test suites — `golden`, `props`, and `interop` (round-trip checks for the megaparsec/attoparsec/GHC `SrcSpan` adapters) — so `cabal test golden`, `cabal test props`, or `cabal test interop` work fine on their own if you're only touching one area.
  
  ## Two rules that actually matter
  
  Nothing else here is checked by the compiler, so it's worth knowing these before you dig in:
  
  **Renderers are built one way.** Every `GraphicalOptions` / `NarratableOptions` / `JsonOptions` value comes from `selectRenderer`, and every `Aeson.Value` for a diagnostic comes from the JSON handler's own DTO conversion. If a change seems to need a second way to build one of these, that's usually a sign to adjust `selectRenderer` (or the DTO conversion) rather than add a parallel path.
  
  **Generated code stays thin.** Every method body `deriveDiagnostic` produces has to be a plain call to an ordinary, exported `Tadka.Internal` function — the same one a hand-written instance could call. If a field needs logic the current surface can't express as a plain call, add that as a new function first, then have the macro call it. A CI check enforces this automatically.
  
  ## What's out of scope
  
  Not "later" — genuinely not planned:
  
  - A fourth output format. Graphical, narratable, and JSON are it.
  - Reading JSON back into a diagnostic. The JSON output is one-way.
  - An adapter for every parser library. `megaparsec`, `attoparsec`, and GHC's `SrcSpan` are covered; anything else can build a `Span` by hand.
  
  Want to argue one of these should change? Open an issue first — it's a design conversation, not a PR.
  
  ## Style
  
  Total functions over partial ones. Errors handled explicitly — `Maybe`, `Either`, or a proper error type, never a silent failure. If a value has a rule it must follow, give it a smart constructor rather than trusting every caller to remember. Tests for anything with a real invariant, not just an example to pin down.
  
  Format with `ormolu` (`cabal install ormolu`):
  
  ```sh
  make format         # rewrites files in place
  make format-check    # checks only, doesn't rewrite
  ```
  
  We don't run hlint here — `-Wall`/`-Wcompat` already catch the substantive issues, and hlint's style opinions fought enough deliberate choices in this codebase that it wasn't worth gating merges on.
  
  ## Touching the derive macro
  
  If you're changing `Tadka.Internal.TH` or what it calls into, check before opening the PR:
  
  - [ ] Every generated method body is a direct call to a `Tadka.Internal` function or a class default — no `case`/`if`/`let` inside the splice.
  - [ ] A field needing real logic gets that logic as a new plain function first, with its own test, before the macro calls it.
  - [ ] The one exception is the default `message` (`pretty . show`).
  - [ ] `tools/check-generated.sh` and `tools/check-compile-fail.sh` both pass.
  
  ## Opening a PR
  
  Small and focused is easier to review than large and sweeping. Include a test if the change isn't purely cosmetic. Not sure an idea fits? Open an issue and ask first — saves us both time if the answer turns out to be "that's out of scope."
