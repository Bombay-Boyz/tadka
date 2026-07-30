# Changelog for `tadka`

All notable changes are recorded here. The library follows the phasewise plan
in `Tadka_Implementation_Spec.md`, itself derived from `Tadka_Vision_v5.md`.

## Unreleased — miette-parity hardening

### Collection labels in `deriveDiagnostic`
- New `specLabelCollectionFields`/`specSecondaryLabelCollectionFields` on
  `DiagnosticSpec`: each names a `[Span]`-typed field (validated at splice
  time, same as `specLabelFields`), and every element of that field's runtime
  list becomes its own label sharing the given text — for a variable number of
  same-kind occurrences (every prior declaration of a name, every match of a
  banned pattern) known only when the diagnostic is built, where
  `specLabelFields` needs one field per label fixed at splice time. Rendered
  after all fixed-field labels, in field order then list order; an empty
  runtime list simply contributes no labels.
- TH-layer only, as intended: `buildContext`/`buildContextWith` are completely
  unchanged, since they already accept a plain, arbitrary-length list — the
  splice just expands a collection field into that same shape and appends it.
  A spec with no collection fields generates byte-identical code to before
  this feature existed (the expansion is only spliced in when at least one
  collection field is actually declared), so no existing derived instance's
  generated code changes shape.
- New `WrongCollectionType` compile-fail case (a `Span`-typed, not
  `[Span]`-typed, field must be rejected at the splice site) alongside the
  existing ones in `tools/check-compile-fail.sh`. A new `LabelCollection`
  property group proves a derived instance with collection fields renders
  identically, across all three handlers, to a hand-written
  `buildContext`/`buildContextWith` call expanding the same randomly generated
  list — for varying list lengths (including empty and a 200-element
  totality check), an all-primary-only collection, and a mixed
  fixed-primary-plus-secondary-collection instance.

### Terminal hyperlinks: OSC 8 for the `= see:` URL
- The graphical handler now wraps a diagnostic's `url` in an OSC 8 terminal
  hyperlink escape when `HyperlinkMode` allows it, so a supporting terminal
  renders the `= see:` line as a clickable link instead of plain text a user
  must select and copy by hand. New `HyperlinkMode` (`Auto`/`Always`/`Never`,
  mirroring `ColorMode`/`UnicodeMode`) and `withHyperlinkMode`; `TerminalCaps`
  gains `capNoHyperlink`/`capForceHyperlink` (`NO_HYPERLINK`/`FORCE_HYPERLINK`
  — the latter an existing convention from the `supports-hyperlinks` package)
  and `resolveHyperlink` resolves `Auto` the same three-tier way `resolveColor`
  does. Defaults to `HyperlinkNever`, not `Auto` (unlike colour/Unicode): OSC 8
  has no reliable capability query the way TTY-ness does, so defaulting off
  keeps every existing caller's output byte-for-byte unchanged until they opt
  in. Narratable and JSON output are untouched — this is a graphical-only
  affordance. Only ever wraps an already-validated `Url` (never raw `Text`), so
  the wrap is injection-safe by construction rather than by an extra runtime
  check: `mkUrl`'s absolute-URI grammar has no production admitting a raw
  control byte. A new `single-label-hyperlink` golden fixture pins the exact
  escape bytes; a dedicated property group proves the resolver mirrors colour's
  three-tier law, the wrap touches only the URL (a diagnostic without one
  renders identically under `Always` and `Never`), and `Never` output never
  contains an escape.

### Security hardening (untrusted-input robustness)
- **Terminal-escape / control-character injection (High).** Raw control
  characters (`ESC`, `BEL`, `BS`, DEL, C1) in attacker-controlled source or label
  text previously passed through verbatim into graphical and narratable output,
  even in `ColorNever` mode — a terminal-injection vector when diagnosing
  untrusted code. All three handlers now strip control characters from rendered
  text (source lines keep `\t` for tab expansion; the substitution is
  width-preserving so caret columns are unmoved). JSON is included because aeson
  escapes only `<0x20`, leaving DEL/C1 raw. A property asserts no control
  character other than the `\n` line separator survives in any handler's output
  over adversarial generated input.
- **Unbounded output from wide spans (Medium, availability).** A span across a
  huge line range rendered output proportional to the span (a line-1→5000 span
  produced 5002 lines). The planner now caps the contiguous range and falls back
  to a bounded context window with elision, so output is proportional to the
  diagnostic, not the span (that case now renders ~11 lines); every labelled line
  still appears. Small diagnostics are unchanged, so golden fixtures stay
  byte-identical. A property bounds output for a 10000-line span.


### Robustness: production edge cases mined from miette's history
- Added a `Production edge cases (from miette)` property group translating
  miette's bug-fix history into tadka tests, and fixed the two gaps it exposed:
  CRLF (`\r\n`) sources left a stray carriage return in rendered lines
  (miette #37) — `SourceCode` now strips a trailing `\r` per line; and a newline
  embedded in a label or message could break the caret/gutter layout
  (miette #318) — the graphical and narratable handlers now flatten newlines in
  rendered text fragments to spaces (JSON keeps them, escaped). tadka was
  already robust to the rest: zero-length/point spans (#204/#159/#32), spans
  past end-of-line/EOF (#221/#347), empty sources (#183), offset-0 labels,
  wide-char + tab alignment (#202), combining marks (#312/#314), nested/
  overlapping spans (#316), and multi-line spans not skipping lines (#81), plus
  a totality sweep over out-of-range spans on all three targets.


### Snippet renderer rework — Phase III: multi-line span rendering
- Multi-line spans (start line < end line) are no longer clamped to the start
  line: they render with a connector gutter between the rail and the source —
  `\x256D` opening, `\x2502` continuation, `\x2570` closing (ASCII `/`, `|`,
  `\\`) — with the label shown inline on the closing line. The pure core lives
  in `Tadka.Internal.Renderer.Layout`: greedy interval-graph lane assignment so
  overlapping spans get distinct lanes while disjoint spans reuse one, plus a
  `cellAt` classifier. It is glyph-free and Int-only, so it is fully proven:
  distinct spans on one lane never share a line (the collision proof), lanes are
  contiguous, coverage is order-preserving, and `cellAt` classifies correctly.
  End-to-end properties add that every multi-line span draws exactly one opening
  and one closing corner and that the label text is shown — the second of which
  caught a real lane-reuse bug (a lane hosting two disjoint spans rendered only
  the first) before it could ship. Single-line diagnostics are unaffected (the
  gutter has zero width when there are no multi-line spans), so all existing
  golden fixtures stay byte-identical; a `multi-line` fixture locks the art.
- Note: multiple single-line labels on the same line still render as stacked
  caret lines (correct, and unchanged) rather than horizontally packed onto one
  line with routed connectors. True same-line packing is deferred as optional
  polish; it does not fall out of the multi-line lane engine as cleanly as first
  thought, and stacked carets are unambiguous.

### Snippet renderer rework — Phase II: context lines + gap elision
- New pure planner `Tadka.Internal.Renderer.LinePlan` (`PlanEntry`, `planLines`)
  decides which source lines to render and where to elide, as an IR between
  resolved labels and glyphs. `withContextLines n` shows n lines around each
  labelled line and elides the gaps (a `⋮` marker); unset (default) renders the
  contiguous labelled range with no elision, so all golden fixtures stay
  byte-identical. The graphical window fetch now derives from the plan's shown
  lines (via `SourceCode.scLineCount`), not the anchor range. Properties prove
  the planner is total, `Nothing` reproduces the contiguous range, and with
  context every in-range anchor is shown, line numbers strictly increase and
  stay in bounds, and every elision hides at least one line.

### Snippet renderer rework — Phase I: pluggable SourceCode
- New `Tadka.Internal.SourceCode` class: a total, windowed source-reading seam
  (`scName`, `scLines (firstLine, lastLine)`). `NamedSource` is the canonical
  in-memory instance; the graphical and narratable handlers now fetch only the
  line window they render through it, so a lazy/file-backed instance is possible
  later. Pure refactor — all golden fixtures byte-identical. Properties prove the
  instance is total and its windows equal a filter of the full line enumeration.


### Totality: no partial functions in the library
- Removed every use of a partial function primitive from `src/`. `head`/`!!`,
  `maximum`/`minimum`, and `Data.Array.(!)` are gone from all call sites: caret
  layout and line lookup now pattern-match `drop`; gutter/line-range use
  `foldr max`/`foldr min` with seeds; palette indexing uses total `NonEmpty`
  operations; and array indexing is encapsulated in a single total `atMay`
  (guarded by `inRange`, returning `Maybe`). Behaviour is unchanged — all 14
  golden fixtures remain byte-identical — so this is a pure totality hardening.


### Primary vs secondary labels
- Labels now carry a `LabelKind` (`Primary`/`Secondary`). `buildContext` marks
  everything `Primary` (so existing callers and fixtures are unchanged); a new
  `buildContextWith` takes explicit kinds, and the derive macro gained
  `specSecondaryLabelFields`. All three handlers are kind-aware: the graphical
  and narratable reports anchor their location on the first primary label; the
  graphical handler draws primary labels with `^` in the severity colour and
  secondary labels with `-` in a palette colour; the narratable handler leads
  with "The problem is at" vs "Related context is at"; and the JSON DTO gains a
  per-label `primary` flag. Properties prove the location anchors on the primary,
  the JSON flag tracks the kind, `^`/`-` by kind, and — extending the Phase 8
  guarantee — a derived instance with secondary fields renders byte-identically
  to a hand-written `buildContextWith` instance across all three handlers.

### Cause chain (diagnostic_source analogue)
- New `diagnosticCause :: e -> Maybe SomeDiagnostic` class method (default
  `Nothing`) and a total, depth- and cycle-safe `walkCauses` (cycle detection by
  `diagnosticId`, mirroring `walkRelated`). The chain renders as lightweight
  linear provenance — graphical `= caused by: …` lines, narratable `Caused by: …`
  sentences, and a JSON `causes` array — kept deliberately distinct from the
  tree-shaped `related`. The shared generator now emits causes, so the Phase 11
  render-totality-over-every-target property covers cause chains too; dedicated
  properties prove cyclic chains terminate on every target, an id-cyclic cause's
  marker renders at most once, and a real chain produces a "caused by" line.
- Hardened `tools/check-compile-fail.sh` to build the library under the default
  configuration first, so `cabal exec` always exposes a consistent `tadka`.

### Terminal detection & ANSI colour (graphical handler)
- `reportDiagnostic` now detects the sink's capabilities and resolves `ColorAuto`
  / `UnicodeAuto` to concrete modes before rendering: `NO_COLOR` disables colour,
  `CLICOLOR_FORCE` forces it, otherwise colour follows TTY status; Unicode follows
  a UTF-8 locale check (`LC_ALL` > `LC_CTYPE` > `LANG`). So piped/CI output no
  longer risks stray escapes, and non-UTF-8 terminals get the ASCII box glyphs
  automatically. `selectRenderer` stays pure — resolution is the only new IO.
- New `Tadka.Internal.Terminal`: `detectTerminalCaps` (IO) plus the pure, total
  `resolveColor` / `resolveUnicode` / `resolveConfig`. Properties prove explicit
  modes pass through untouched; `NO_COLOR` always wins; force beats TTY; `Auto`
  otherwise follows TTY/locale; `resolveConfig` eliminates every `Auto`, is
  idempotent, and changes only the two mode fields.
- The graphical handler now emits ANSI: the severity/code header is coloured by
  severity (bold), and each label's carets + text take their palette colour
  (`withLabelPalette`) — delivering the per-label colour deferred in Phase 5.
  Under `ColorNever` no ANSI is emitted (proved over generated diagnostics) and
  underline glyphs cycle `^`/`~`/`-`, so all golden fixtures stay byte-identical.
  A structural property proves colour adds only ANSI and a uniform caret glyph,
  never a layout change.

### Tab-stop expansion (graphical handler)
- Source lines are rendered with tabs expanded to the next tab stop, and caret
  columns are computed with the same tab-aware `displayColumnAt`, so a caret now
  aligns under a span on a tab-indented line instead of drifting. Tab width is
  configurable via `withTabWidth` (default 4); character columns reported by the
  narratable and JSON handlers are unchanged (a tab is one character).
- New total helpers `Tadka.Internal.Width.displayColumnAt` / `expandTabs`, with
  properties proving: expansion leaves no tabs; expanded width equals
  `displayColumnAt` of the whole line; `displayColumnAt` is monotonic; a tab
  always lands on a tab stop; and — the alignment guarantee — a caret's display
  offset equals the width of the tab-expanded source preceding the span. A
  `tab-indented` golden fixture locks the visual result.

## 1.0.0.0 — v1

First release. All eleven phases of the implementation spec are complete; the
public API matches vision §8.

### Phase 11 — Consolidation & Release Audit
- Public API surface reconciled to vision §8: `Offset`/`Length` (and their
  constructors, accessors, error types, and the `spanOffset`/`spanLength`
  accessors that exposed them) are no longer public — spans are the public
  position type, and the offset representation lives in "Tadka.Internal.Types"
  with no compatibility guarantee. A negative compile check confirms they are
  unreachable from `Tadka`.
- Dependency upper bounds added to the library and every interop sub-library;
  `cabal check` is clean. Version set to `1.0.0.0`.
- Consolidated golden suite (10 fixtures: the six canonical renderings, the
  prose and JSON depth-truncation forms, the JSON cycle form, and the
  generated-instance discipline fixture) and property suite (all groups from
  Phases 1–9) run together via `make test` / CI.
- New consolidated properties: `render` totality broadened to a single Hedgehog
  property over *every* target; a cycle-detection marker property (a repeated
  `diagnosticId`'s marker renders at most once, on all three handlers); and the
  vision's Success Criterion end-to-end — a misspelled span field is a compile
  error (`test/compile-fail/SuccessCriterion.hs`), and a genuinely staled span
  renders a clear in-report reason (graphical, narratable, and an explicit JSON
  `"stale":true`) rather than a silently shorter report.


### Phase 10 — Interop Helpers
- One-directional adapters turning parser positions into tadka `Span`/`Offset`,
  each a plain function against Phase 1/2 types with no new core surface:
  - `Tadka.Interop.Megaparsec`: `offsetFromError`/`spanFromError` from a
    megaparsec `ParseError`'s stream `errorOffset`.
  - `Tadka.Interop.Attoparsec`: `consumedOffset`/`spanFromConsumed` (attoparsec
    reports no line/column, so position is recovered as characters consumed).
  - `Tadka.Interop.GHC`: `spanFromSrcSpan` converting a GHC `SrcSpan` (with the
    source text, to turn 1-based line/column into an offset).
- Each adapter is a **separate cabal sub-library** (`interop-megaparsec`,
  `interop-attoparsec`, `interop-ghc`) depending on `tadka`, so the core library
  never depends on a parser package and no core module can import interop — the
  adapters are one-directional by construction. Minimum upstream versions are
  pinned (`megaparsec >=9.0`, `attoparsec >=0.14`, the GHC 9.10 `ghc` library)
  and noted in each module's haddock.
- Exit criteria met: a per-library round-trip test (`test-suite interop`)
  constructs a known failure, converts its position, resolves against the same
  source, and confirms the line/column matches what the library reports
  (megaparsec, GHC) or the consumed offset (attoparsec); and an audit confirms
  the core library has no parser/ghc dependency and imports no interop module.

### Phase 9 — Generics-Based Label-Wiring Derivation
- `genericContext :: (Generic e, ...) => e -> Context` derives __only__ the
  `context` method, via GHC.Generics, for a record with exactly one
  `NamedSource` field and one or more `Span` fields — using each span field's
  record-selector name as its label text and calling the same `buildContext`.
  Used as `context = genericContext` inside an otherwise hand-written instance.
- Deliberately scoped: its type (`e -> Context`) can touch nothing else, so
  `code`/`severity`/`help`/`url`/`message`/`diagnosticId` stay hand-written
  (each defaultable). This is one method, not "most of the ergonomics" — for the
  fuller path use `deriveDiagnostic`.
- The record shape is checked at compile time (a `Nat`-counting type family over
  the generic `Rep`): zero or several `NamedSource` fields, or no `Span` field,
  is a type error — never a silent guess about which field was meant. A
  compile-fail test (`TwoSources`) covers the two-source case.
- Exit criteria met: a property proves a generics-wired instance renders
  byte-for-byte identically to a hand-written `buildContext` twin across all
  three handlers, and the `e -> Context` signature confirms no other method is
  touched.

### Phase 8 — Derive Macro (`deriveDiagnostic`)
- `DiagnosticSpec` (with `specCode`, `specSeverity`, `specHelp`, `specUrl`,
  `specSourceField`, `specLabelFields`, `specRelated`, `specId`, `specMessage`)
  and `defaultSpec`, plus `deriveDiagnostic :: DiagnosticSpec -> Name -> Q [Dec]`,
  an ordinary TH splice (no type-level DSL).
- `reify`-validated at splice time: `specSourceField` must be `NamedSource`,
  each `specLabelFields` name `Span`, `specId` `Text` or `DiagnosticId`, and
  `specRelated` `[SomeDiagnostic]`; `specCode`/`specUrl` literals run through
  `mkDiagnosticCode`/`mkUrl`. Any mismatch is a compile error at the splice site.
  The default `message` (`pretty . show`) requires `Show`, checked via `isInstance`.
- Every generated method body is a direct call to a shared function exported
  from `Tadka.Internal` (`buildContext`, `unsafeDiagnosticCode`, `unsafeUrl`,
  `mkDiagnosticId`) or the field accessor — the derive path and a hand-written
  instance are two doors into the same room. `mkDiagnosticId` was added to the
  `Tadka.Internal` export list, and that module's haddock now states the
  discipline.
- Exit criteria met: compile-fail tests (`test/compile-fail/`, run by
  `tools/check-compile-fail.sh`) reject a wrong-typed field, a non-field name,
  and an invalid code literal at compile time; a property proves a derived
  instance renders byte-for-byte identically to a hand-written twin across all
  three handlers; the generated instance source is captured as a golden fixture
  and `tools/check-generated.sh` (a **required** CI check) fails the build on any
  non-direct-call method body; and a `CONTRIBUTING.md` checklist entry records
  the review convention.

### Phase 7 — JSON Report Handler + DTO
- Dedicated `DiagnosticDTO` / `LabelDTO` with hand-written `ToJSON` — never
  `deriving ToJSON` on a `Diagnostic`-bearing type. `ToJSON` only in v1;
  `FromJSON` is deferred.
- DTO shape per the canonical example: `code`, `severity`, `message`, `labels`
  (each `line`, `column`, `length`, `text`, and an explicit `stale`), `help`,
  `url`, `related`, `truncated`, `cycleOmitted`. The `stale` flag is derived
  from `LabelState` (stale labels carry null `line`/`column`/`length`), never
  inferred from absence.
- `related` recurses into nested DTOs; `truncated`/`cycleOmitted` are set from
  the Phase 3 walk's `TerminationReason` — the same two values the graphical and
  narratable handlers consume, serialized instead of prose-rendered.
- `renderJson` builds the DTO inside the `'TJson` branch and is the only route
  to `Output 'TJson = Aeson.Value` (audited: no `Diagnostic` type produces a
  `Value` except through this conversion). `render` now dispatches `'TJson`.
- Golden fixtures: `json-single` (byte-for-byte to the vision example, modulo
  the same coherent column correction as the other handlers), `json-cycle`
  (`cycleOmitted: true`), and `json-truncated` (nested `truncated: true`). The
  runner serializes the `Value` with a deterministic ordered pretty-printer to
  match the vision's canonical layout. Property suite: ok labels serialize
  `stale:false` with positions, stale labels `stale:true` with null positions,
  and the handler is total over the shared generated set.

### Phase 6 — Narratable Report Handler
- `renderNarratable` produces the accessibility-first prose form: an
  `Error,`/`Warning,`/`Advice,` opener with an optional `code X:` clause (dropped
  when there is no code), a `Location: file, line N, column M.` sentence, a
  `Source line N: "…".` readout, and a `The problem is at column(s) …, labeled: …`
  sentence per label.
- `LabelStale` has a prose equivalent of the graphical degraded marker
  (`A labeled position could not be shown because …`, carrying the stale reason
  and label) — never silently omitted.
- Related chains render as prose, consuming the Phase 3 walk: `Related: code —
  message.`, a cycle sentence, and an `N more related diagnostics … omitted at
  the depth limit.` marker (with correct singular/plural).
- `toProseMarker :: Ann -> Text` interprets `Ann` at this handler's boundary
  (inline code and file names are surrounded with quotes); rendered via
  `renderSimplyDecorated` so annotated content reads naturally.
- `render` now dispatches the `'TNarratable` branch to `renderNarratable`.
- Golden fixtures: `narr-single` (same diagnostic as the graphical single-label
  fixture, so the two handlers stay cross-consistent) and `narr-truncated`
  (a related chain past the depth limit, exercising the prose truncation marker).
  Property suite: `AnnCode` renders quoted, and the handler is total over the
  same shared generated set (now in `GenDiag`) used by Phase 5's smoke check.
- Field-coverage cross-check confirmed: code, severity, location, source line,
  label text, stale reason, help, url, and related each have a narratable
  equivalent — no field is dropped between renderers.

### Phase 5 — Graphical Report Handler
- `renderGraphical` renders a full graphical report: `error[code]: message`
  header (no brackets when there is no code; `advice:`/`warning:`/`error:` per
  severity), a `┌─ file:line:col` location line, a line-numbered gutter with a
  `│` rail, source lines sliced from the single stored `NamedSource`, and
  underline carets positioned by display width via `Tadka.Internal.Width`
  (combining marks, East-Asian-width, and emoji handled).
- Per-label underline cycling: label index *i* selects palette entry *i mod p*
  (`labelStyle`); under `ColorNever` the underline character cycles `^`/`~`/`-`
  so labels stay distinguishable in plain text. `UnicodeAscii` degrades the
  box-drawing glyphs to `|`/`+`/`-`.
- Stale labels render as `(span unavailable — source no longer matches at this
  position)` in place of a source line, in original order.
- Related chains consume the Phase 3 walk: each `CycleOmitted` node renders
  `= related: (cycle omitted)`, each `DepthTruncated` node
  `= related: (N more related diagnostics omitted)`, and ordinary nodes render
  a `= related: code — message` summary plus their own (indented) snippet.
- `render` now dispatches the `'TGraphical` branch to `renderGraphical`.
- Golden suite (byte-for-byte): single-label, multi-label, degraded, and
  cycle-omitted fixtures, rendered `ColorNever`/`UnicodeAlways` for determinism.
  Property suite: caret layout is non-negative, never collapses, and is
  width-aware (doubling as a width-table sync check); palette cycling equals
  *i mod p*; and the handler is total over fuelled generated diagnostics
  (including stale labels and both id-bearing and `Nothing`-id self-cycles).

  Deviations from the vision's hand-drawn examples (which are internally
  inconsistent) are deliberate and documented in the fixtures: correct column
  numbers, coherent underline-character cycling, full diagnostic codes in
  related summaries, and real line numbers rather than fabricated ones.

### Phase 4 — Renderer/Config Scaffolding
- `Tadka.Internal.Config`: `Target` (`TGraphical`/`TNarratable`/`TJson`,
  closed), `ColorMode`, `UnicodeMode`, opaque `Config` with `defaultConfig` and
  the `withColorMode` / `withUnicodeMode` / `withRelatedDepthLimit` /
  `withLabelPalette` / `withTarget` setters, plus the default six-colour palette.
- Three renderer modules with opaque `GraphicalOptions` / `NarratableOptions` /
  `JsonOptions` (no public constructor or accessor) and placeholder
  `renderGraphical` / `renderNarratable` / `renderJson` bodies (Phases 5–7).
- `Tadka.Internal.Render`: the `Renderer (t :: Target)` GADT (constructors
  exported for pattern matching), the closed `Output` type family, `SomeRenderer`,
  `render`, and `reportDiagnostic`. `selectRenderer` is the sole constructor of
  any `*Options` value and the sole reader of `Config` — the "one path".
- Property suite (Phase 4): an explicit `withTarget` override always yields the
  matching renderer constructor (even after other setters), the no-target
  default is graphical, and each target's render path runs. Verified by audit
  that `selectRenderer` is the only `*Options` construction site, and by
  negative compile check that the `*Options`/`Config` constructors are
  unreachable from `Tadka`.

### Phase 3 — `Diagnostic` Typeclass & Related/Cycle Walk
- `Tadka.Internal.Diagnostic`: the `Diagnostic` class (only `message`
  mandatory, no `Show` superclass; the other seven methods defaulted) and the
  `SomeDiagnostic` existential (no `Show` constraint).
- `Tadka.Internal.Related`: the single renderer-agnostic `related`-chain walk
  (`walkRelated`) producing a `RelatedTree` of `(SomeDiagnostic,
  TerminationReason)` where `TerminationReason` is `NotTerminated` /
  `DepthTruncated` / `CycleOmitted`. Cycle detection is by `diagnosticId` along
  the current path; termination is always guaranteed by the finite depth
  budget (`defaultRelatedDepth = 8`). Factored once here so Phases 5–7 share it
  rather than reimplementing.
- Property suite (Phase 3): cycle nodes are visited once and never descended;
  a structurally infinite `Nothing`-only chain still terminates by depth (v4
  fallback intact); the walk is total and depth-bounded for any fuelled tree.

### Phase 2 — Span Resolution & `Context`
- `Tadka.Internal.Span`: resolution-indexed `SpanF` GADT (`Span` /
  `ResolvedSpan`), `mkSpan`, `resolveSpan`, `LineCol`, `StaleReason`,
  `SpanError`. `ResolvedSpan` carries positions only — no owned text (v5 fix);
  its raw constructors are hidden so a resolved span can only come from
  `resolveSpan`.
- `Tadka.Internal.Context`: `Labeled`, `LabelState` (`LabelOk` / `LabelStale`),
  `Context` (`NoContext` / `HasLabels`), `mkContext` (strict), and
  `mkContextDegrading` (total). The degrading path turns an unresolvable span
  into a `LabelStale` marker in its original position, never dropping it — so
  label count and ordering are independent of which spans stayed valid.
- `Tadka.Internal.Context.buildContext` — the single plain dispatch both the
  derive macro (Phase 8) and manual instances call; re-exported from
  `Tadka.Internal`.
- `Tadka.Internal.Ann` (`Ann`) pulled forward from Phase 3, since `Labeled` /
  `buildContext` need `Doc Ann`; the renderer-boundary interpreters remain in
  Phases 5–7.
- Property suite (Phase 2): `resolveSpan` bounds safety, `mkContext` Left iff a
  span is out of bounds, the `mkContextDegrading` count/order guarantee, and
  `buildContext` dispatch. Suite reorganised into per-phase modules.

### Phase 1 — Primitive Types, Smart Constructors & the Width Table
- `Tadka.Internal.Types`: validated `Offset`, `Length`, `NamedSource`,
  `DiagnosticCode`, `Url`, `Severity`, and `DiagnosticId`, each with a smart
  constructor that enforces its invariant and a hidden raw constructor
  ("illegal states unrepresentable"). Explicit custom error types
  (`OffsetError`, `LengthError`, `SourceError`, `CodeError`, `UrlError`).
- Diagnostic-code grammar `^[a-z][a-z0-9_]*::E[0-9]{4,}$` validated without a
  regex dependency; URLs validated as absolute URIs via `network-uri`.
- `Severity` display strings centralised in `severityLabels` /
  `severityJsonTag` (one source of truth for Phases 5–7).
- Internal `unsafeDiagnosticCode` / `unsafeUrl` exposed only via
  `Tadka.Internal` for Phase 8 splice use; verified unreachable from `Tadka`.
- `Tadka.Internal.Width`: `charWidth`, `textWidth`, `graphemeBreakProperty`,
  and `isExtendedPictographic`, backed by a generated, checked-in
  `Tadka.Internal.Width.Table` (UCD 15.1.0), with binary-searched ranges.
- `tools/gen-width-table.hs` implemented: fetches four pinned UCD 15.1.0 files
  and emits the table; UCD version recorded in the generated header.
- Property suite (Hedgehog): smart-constructor rejection (with an independent
  grammar oracle), `NamedSource` round-trip, and width point lookups.
- Gated `-Werror` behind a manual `werror` flag (enabled for dev/CI via
  `cabal.project`) so `cabal check` passes and released builds stay installable.

### Phase 0 — Repo & Tooling Setup
- Cabal package skeleton with the fixed public/internal module layout
  (`Tadka` public; `Tadka.Internal.*` no-compatibility-guarantee).
- Dependency pins: `text`, `prettyprinter`, `prettyprinter-ansi-terminal`,
  `ansi-terminal`, `template-haskell`, `aeson`.
- `tools/gen-width-table.hs` scaffold (implemented in Phase 1).
- Two independently-runnable test suites (`golden`, `props`), each passing
  trivially on zero tests.
- CI skeleton building the library and running both suites independently.
- Warnings-as-errors (`-Wall -Wcompat -Werror`) from the first commit.
