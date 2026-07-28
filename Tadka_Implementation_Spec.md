# tadka — Phasewise Implementation Spec

**Derived from:** `Tadka_Vision_v5.md` (Full Parity, Provably Correct, Deterministically Rendered)
**Purpose:** Break the v5 vision into ordered, buildable phases with clear entry/exit criteria, so implementation can proceed module by module without backtracking on dependencies.

**Ordering principle:** every phase only depends on types and functions already built in an earlier phase. Nothing in a later phase requires re-opening an earlier one, except where explicitly noted as a planned extension point (e.g. Phase 5 adding to the render path Phase 4 scaffolds).

---

## Phase Map (at a glance)

| Phase | Name | Depends on | Approx. LOC |
|---|---|---|---|
| 0 | Repo & Tooling Setup | — | — |
| 1 | Primitive Types & Smart Constructors (incl. `Tadka.Internal.Width`) | 0 | ~490–590 |
| 2 | Span Resolution & `Context` (incl. `buildContext`) | 1 | (included above) |
| 3 | `Diagnostic` Typeclass, `Ann`, Related/Cycle Walk | 1, 2 | — |
| 4 | Renderer/Config Scaffolding | 3 | ~230–330 |
| 5 | Graphical Report Handler | 4 | ~850–1050 |
| 6 | Narratable Report Handler | 4 | ~220–320 |
| 7 | JSON Report Handler + DTO | 4 | ~240–340 |
| 8 | Derive Macro (`deriveDiagnostic`) | 1–7 | ~420–620 |
| 9 | Generics-Based Label-Wiring Derivation | 2, 3 | ~150–200 |
| 10 | Interop Helpers | 1, 2 | ~150–250 |
| 11 | Full Test Suite Consolidation & Release Prep | all | ~700–950 |

**Total: ~3,450–4,650 LOC** by direct sum of the line items above. The vision document's own Project Scope section states its total as "~3,450–4,310 LOC," which undersums its own itemized breakdown by ~340 at the high end (the `Tadka.Internal.Width` line item appears to be double-counted out of that total somewhere, or omitted from it — the vision doc doesn't show its arithmetic). This spec's phase totals are summed directly from the same per-component estimates the vision doc gives; reconcile against the vision doc before publishing final numbers rather than treating either total as authoritative.

Each phase below includes: goal, concrete deliverables, module layout, exit criteria (what must be true/tested before moving on), and open risks specific to that phase.

---

## Phase 0 — Repo & Tooling Setup

**Goal:** stand up the project skeleton so every later phase has somewhere correct to land.

**Deliverables:**
- Cabal/Hackage package skeleton: `tadka` (public), `Tadka.Internal.*` (no compatibility guarantee, stated in the module haddock).
- Dependency pins: `prettyprinter`, `prettyprinter-ansi-terminal`, `ansi-terminal`, `text`, `template-haskell`, `aeson`.
- `tools/` script scaffold for regenerating the Unicode width table (implemented in Phase 1, scripted here).
- CI skeleton: build + golden tests + Hedgehog properties as separate, independently runnable suites (each later phase adds to these, none of them wait for Phase 11 to exist).
- Module layout convention fixed now, since Phase 6's "generated code is provably thin" convention depends on the public/internal split being unambiguous from the start:
  - `Tadka` — public re-export surface (§8 of vision doc)
  - `Tadka.Internal` — plain, exported, independently-testable functions the derive macro and manual instances both call
  - `Tadka.Internal.Width` — bundled Unicode table
  - `Tadka.Internal.Context`, `.Renderer.Graphical`, `.Renderer.Narratable`, `.Renderer.Json`, `.TH`, `.Generics`, `.Interop.*`

**Exit criteria:**
- `cabal build` succeeds on an empty package with the module stubs above.
- CI pipeline runs (and passes trivially) on zero tests.

---

## Phase 1 — Primitive Types & Smart Constructors

**Corresponds to vision §2 (partial), §4, §5 (partial), and the Infrastructure section.**

**Goal:** every value that can be constructed at all is already valid — this is the foundation the "can't construct a value that can't render correctly" promise rests on.

**Deliverables:**
1. `Tadka.Internal.Width` — bundled East-Asian-width + grapheme-cluster-break table generated from the Unicode Character Database, checked into the repo, with the `tools/` regeneration script wired up (per vision §0/Infrastructure — this is a named, owned decision, not a borrowed library).
2. `Offset`/`Length` newtypes with `mkOffset`/`mkLength` (reject negative values; zero-length is a valid point span).
3. `NamedSource` two-field record (`sourceName`, `sourceText`), constructor **not exported**, `mkNamedSource :: Text -> Text -> Either SourceError NamedSource` (rejects empty name; empty content is legitimate).
4. `DiagnosticCode` newtype, `mkDiagnosticCode` (non-empty, matches project code grammar `^[a-z][a-z0-9_]*::E[0-9]{4,}$`).
5. `Url` newtype, `mkUrl` (must parse as absolute URI).
6. `Severity` (`SevAdvice | SevWarning | SevError`, `Eq, Ord, Show, Enum, Bounded`), with the header-text mapping table specified as data (not scattered string literals) so Phases 5 and 6 — which each render severity as prefixed header/prose text (`error:`/`warning:`/`advice:` and `Error,`/`Warning,`/`Advice,` respectively) — read from one place. Phase 7's JSON handler does not use this table; it serializes `Severity` as a bare lowercase string (`"error"`/`"warning"`/`"advice"`) via a separate, simpler mapping.
7. `DiagnosticId` newtype, `mkDiagnosticId :: Text -> DiagnosticId` (total — any `Text` is valid, it's an opaque comparison key).
8. Internal `unsafeDiagnosticCode` / `unsafeUrl` constructors under `Tadka.Internal` only, unreachable from the public surface — needed by Phase 8, specified here so the invariant ("only ever constructed from an already-validated `Either`") is fixed before anything calls them.

**Exit criteria:**
- Hedgehog property: smart constructors reject exactly the inputs their documented invariant says they reject, run against valid and adversarial generators.
- Hedgehog property: `NamedSource`'s accessors round-trip values passed to `mkNamedSource` for any pair that passes validation.
- No constructor for `NamedSource`, `DiagnosticCode`, `Url`, or `DiagnosticId`'s "unsafe" variants is exported from `Tadka`.
- Width table regeneration script runs end-to-end against a UCD snapshot and produces a checked-in, versioned artifact.

**Risks:** the width table is the one piece of this phase with an external data dependency (UCD version drift). Pin a UCD version explicitly in the regeneration script and record it in a comment at the top of the generated file.

---

## Phase 2 — Span Resolution & `Context`

**Corresponds to vision §2 (the rest of it).**

**Goal:** implement the "resolved-or-explicitly-stale, never silently shorter" guarantee that is the headline fix in v5.

**Deliverables:**
1. `Resolution` kind (`Unresolved | Resolved`) and `SpanF` GADT — `Span = SpanF 'Unresolved`, `ResolvedSpan = SpanF 'Resolved`. `ResolvedSpan` carries **positions only** (`Offset`, `Length`, `LineCol`, `LineCol`) — no owned text, per the v5 changelog fix; text is sliced at render time via `Text.take`/`Text.drop` against the single `NamedSource` stored per `Context`.
2. `resolveSpan :: NamedSource -> Span -> Either SpanError ResolvedSpan`.
3. `StaleReason` (`SpanOutOfBounds | SourceMismatch`), `LabelState` (`LabelOk ResolvedSpan | LabelStale StaleReason`), `Labeled a`.
4. `Context` (`NoContext | HasLabels NamedSource (NonEmpty (Labeled LabelState))`).
5. `mkContext :: NamedSource -> NonEmpty (Labeled Span) -> Either ContextError Context` — strict, fails if *any* span is out of bounds. Intended call site: a domain error type's own smart constructor (vision §2 example, `mkParseError`), not `context` itself.
6. `mkContextDegrading :: NamedSource -> NonEmpty (Labeled Span) -> Context` — total; a span that fails resolution becomes `LabelStale reason` **in its original position**, never dropped. Falls back to `NoContext` only when the input list is empty. This is the renamed, behavior-changed `mkContextLenient` from v4 — implement it as new code, not a rename of the old function, since the invariant it must satisfy (label count/order never changes) is strictly stronger than v4's.
7. `Tadka.Internal.Context.buildContext :: NamedSource -> [(Span, Maybe (Doc Ann))] -> Context` — empty list → `NoContext`, else → `mkContextDegrading`. Built here, not in Phase 8, even though its only caller until Phase 8 is test code: vision changelog point 4 frames it as plain, independently-testable core infrastructure that *both* the derive macro and manual instances call, not a TH-specific artifact. Landing it in this phase means Phase 8 only has to generate a call to an already-proven function, which is the entire point of the "provably thin" discipline (§6 of the vision doc, Phase 8 below).

**Exit criteria:**
- Hedgehog: `resolveSpan` never returns a `ResolvedSpan` whose implied range reads outside `sourceText`.
- Hedgehog: `mkContext` returns `Left` iff at least one generated span is out-of-bounds for the paired source.
- Hedgehog: `mkContextDegrading` **never changes label count** — for any generated `NonEmpty (Labeled Span)` of length *n*, the result always has exactly *n* labels, each `LabelOk` or `LabelStale`, and `LabelStale` iff resolving that specific span against that specific source would have been the failure that made `mkContext` return `Left` on a single-span input. This is the property that replaces v4's weaker guarantee — write it before writing `mkContextDegrading`'s implementation, since it's the spec, not a check added after the fact.
- Unit test: `buildContext` on an empty list returns `NoContext`; on a non-empty list, returns exactly what `mkContextDegrading` returns for the same arguments (i.e. `buildContext` is a thin dispatch, not a second implementation of the degrading logic).
- Manual round-trip test of the `mkParseError` pattern from vision §2: construct via `mkContext`, discard the `Either`'s `Context`, confirm `context` (once Phase 3 wires it) recomputes the same resolvable value from stored fields.

**Risks:** this phase's correctness property is the crux of the whole v5 revision — do not proceed to Phase 5 (which visually depends on `LabelStale` rendering) until the label-count invariant property is green.

---

## Phase 3 — `Diagnostic` Typeclass, `Ann`, Related/Cycle Walk

**Corresponds to vision §1, §3, §5.**

**Goal:** define the single interface every renderer consumes, and the traversal algorithm for `related` chains that Phases 5–7 will each apply.

**Deliverables:**
1. `Ann` (`AnnEmphasis | AnnCode | AnnFilename | AnnKeyword`, `Eq, Show, Enum, Bounded`) — deliberately no per-label-index constructor (that's a Phase 5 rendering concern, see §7).
2. `Diagnostic` typeclass exactly as specified: `message` mandatory (no default, no `Show` superclass); `context`, `code`, `severity`, `help`, `url`, `related`, `diagnosticId` all defaulted.
3. `SomeDiagnostic` existential wrapper (`forall e. Diagnostic e => SomeDiagnostic e`), no `Show` constraint on it.
4. The **related/cycle walk algorithm**, factored as a renderer-agnostic traversal function that Phases 5/6/7 each call rather than each reimplementing:
   - carries a depth counter, initialized from a plain `Natural` parameter (default value 8) and a `Set DiagnosticId` of ids seen on the current path; the walk function itself has no dependency on `Config` — Phase 4 later threads its `withRelatedDepthLimit` setting into this same `Natural` parameter, but that's a one-directional consumer relationship, not a dependency of this phase on Phase 4;
   - if next diagnostic's `diagnosticId` is `Just i` and `i` is already visited → stop with cycle marker, do not descend;
   - if `Just i` and unvisited → add to visited set, continue;
   - if `Nothing` → continue under depth limit alone (old v4 behavior, always terminates).
   - Output of the walk is a renderer-agnostic tree/list of `(SomeDiagnostic, TerminationReason)` where `TerminationReason ∈ {NotTerminated, DepthTruncated, CycleOmitted}`, so each handler only has to decide *how* to render each termination reason, not re-derive *when* one occurred.

**Exit criteria:**
- Hedgehog: for a generated `SomeDiagnostic` chain where two nodes share a `diagnosticId`, the walk visits the second occurrence at most once and never descends into its `related` a second time.
- Hedgehog: diagnostics generated with `diagnosticId = Nothing` throughout are checked against the old v4 property (depth-limited, potentially-repeated, but always-terminating) — confirms the fallback path is intact, not just the new path.
- The walk function itself is total for a fuel-bounded generator (fuel decremented per recursive step, forced to `[]` at zero) — required because a `Nothing`-only chain carries no acyclicity guarantee and an unfuelled generator would not terminate. This fueling is a **test-generator concern**, not a production behavior — production correctness comes from the depth limit, which is always finite.

**Risks:** getting this factored out as one shared function (rather than three separate walks in Phases 5/6/7) is what makes "distinct truncation shapes for each [depth-limit vs. cycle], in every handler including JSON" (vision, What tadka Does) achievable without three copies of the same logic drifting apart. Do not let Phase 5 inline its own walk under time pressure — it is the one shortcut most likely to reintroduce the v4-style silent-divergence bug this whole revision exists to close.

---

## Phase 4 — Renderer/Config Scaffolding

**Corresponds to vision §7 (structure only — handler bodies are Phases 5–7).**

**Goal:** build the single path from configuration to a constructible renderer, before any handler exists to plug into it, so Phases 5–7 each have exactly one place to attach.

**Deliverables:**
1. `Target` (`TGraphical | TNarratable | TJson`, closed), `Output` closed type family (`Doc Ann | Text | Aeson.Value`).
2. `Renderer (t :: Target)` GADT with exported constructors (`Graphical`, `Narratable`, `Json`) — but `GraphicalOptions`/`NarratableOptions`/`JsonOptions` are opaque (no exported constructor or accessor) at this phase; their **content** is filled in as Phases 5–7 land, but the opaque type boundary is fixed now.
3. `Config` (constructor not exported), `defaultConfig`, and setters: `withColorMode`, `withUnicodeMode`, `withRelatedDepthLimit`, `withLabelPalette`, `withTarget`.
4. `ColorMode` (`ColorAuto | ColorAlways | ColorNever`), `UnicodeMode` (`UnicodeAuto | UnicodeAlways | UnicodeAscii`).
5. `SomeRenderer`, `selectRenderer :: Config -> SomeRenderer` — the **only** function that constructs any `*Options` value or wraps one in a `Renderer` constructor. At this phase it can be implemented against `render`-less stub `Options` types (a marker field is enough); Phases 5–7 extend the `Options` payload without touching `selectRenderer`'s role as sole constructor.
6. `render :: Diagnostic e => Renderer t -> e -> Output t` — signature fixed now; body dispatches to Phase 5/6/7 implementations as they land (a `case` per constructor, each arm initially a placeholder that Phases 5–7 replace).
7. `reportDiagnostic :: Diagnostic e => Config -> e -> IO ()` — selects, renders, writes to the correct sink; can be fully implemented once at least the `Options` plumbing exists, even before all three handler bodies are complete, since each arm is independent.

**Exit criteria:**
- Property: `selectRenderer` composed with `withTarget t` always yields a `SomeRenderer` whose pattern-matched constructor corresponds to `t` — an explicit override is never overridden again by detection. (Testable now, before Phase 5–7 fill in handler bodies, since it only depends on `selectRenderer`'s dispatch logic.)
- Confirm by code review (this is a project-convention check, not type-enforced, per vision §6/§7 discipline): no function outside `selectRenderer` constructs a `GraphicalOptions`/`NarratableOptions`/`JsonOptions` value.

**Risks:** it's tempting to build Phase 5's `GraphicalOptions` fields first and retrofit `selectRenderer` around them. Build the scaffolding phase-pure instead — stub `Options` types, real dispatch logic — so the "one path" invariant is verified structurally before there's anything interesting inside the `Options` values to tempt a second construction site.

---

## Phase 5 — Graphical Report Handler

**Corresponds to vision §7 (graphical body) and the Graphical examples under Canonical Rendering Examples.**

**Goal:** the largest and highest-risk phase — multi-span layout, gutter alignment, Unicode width correctness, label-palette coloring, and related-chain nesting all live here.

**Deliverables:**
1. `GraphicalOptions` real payload (color mode, unicode mode, label palette, depth limit — all threaded from `Config` via `selectRenderer`).
2. Header rendering: `error[CODE]: message` / `error: message` (no code → no brackets), with `SevAdvice`/`SevWarning`/`SevError` → `advice:`/`warning:`/`error:` per vision §4.
3. Source snippet block: gutter with line numbers, `┌─ file:line:col` location line, one or more labeled source lines with underline carets, sliced from the single stored `NamedSource` via `Text.take`/`Text.drop` (never from a per-label copy — this depends on Phase 2's `ResolvedSpan` carrying positions only).
4. **Per-label underline palette cycling (new in v5):** label index *N* in a `Context`'s label list gets `withLabelPalette` entry *N mod (length palette)*. Purely positional, computed entirely inside this handler — touches nothing in `Ann`. Default: six-color palette distinguishable in light/dark themes, degrading to distinct underline characters (`^`, `~`, `-`) under `ColorNever`.
5. Gutter/caret column computation over `Text` containing combining marks, East-Asian-width characters, and emoji — using `Tadka.Internal.Width` (Phase 1) directly.
6. `LabelStale` rendering: `(span unavailable — source no longer matches at this position)` in place of a source line, per the canonical degraded-label example — the label still appears, in order, with no underline/source to show.
7. Related-diagnostic nesting: consumes Phase 3's walk output, rendering each `DepthTruncated` node as `(N more related diagnostics omitted)` and each `CycleOmitted` node as `(cycle omitted)`, both as specified one-liners.
8. Wire `render` for the `'TGraphical` branch (completing Phase 4's placeholder).

**Exit criteria:**
- Golden tests, byte-for-byte, for: single label, multi-label with palette, degraded label, cycle-omitted (4 of the vision's 6 canonical fixtures).
- Property: gutter/caret column computation never goes negative and never overlaps the line-number gutter, generated over text with combining marks/East-Asian-width/emoji — this property doubles as confirmation the bundled width table (Phase 1) stays in sync with what this layout engine assumes.
- Property: label palette cycling — for `Context` with *k* labels and palette length *p*, label *i*'s underline style equals palette entry *(i mod p)*, generated over *k* and *p* independently.
- Manual visual check against every graphical example in the vision doc's Canonical Rendering Examples section (single label, multi-label+related, degraded, cycle-omitted).
- Totality smoke check (lightweight precursor to Phase 11's full property): `render` for `'TGraphical`, evaluated to normal form inside `try @SomeException`, does not throw for a small fuel-bounded set of generated `SomeDiagnostic` values, including ones with `LabelStale` labels and `Nothing`-`diagnosticId` cycles. This is not the full Hedgehog property (that's consolidated in Phase 11) — it's a fast, phase-local check so a totality violation in this handler is caught here, not three phases later.

**Risks:** highest LOC estimate in the whole project (~850–1050) and the one place Unicode correctness, ANSI styling, and the related-walk output all intersect. Build it in the sub-order above (header → snippet → palette → width-aware gutter → stale → nesting) rather than attempting all pieces simultaneously — each sub-deliverable is independently golden-testable.

---

## Phase 6 — Narratable Report Handler

**Corresponds to vision §7 (narratable body) and the Narratable canonical example.**

**Goal:** the accessibility-first renderer — prose, not layout.

**Deliverables:**
1. `NarratableOptions` real payload.
2. Prose template: `Error, code X: message.` / omits the "code X" clause when `code` is `Nothing`. `Severity` → `Error,`/`Warning,`/`Advice,` prefix.
3. Location line ("Location: file, line N, column M."), source-line readout, label readout ("The problem is at columns N through M, labeled: ...").
4. `LabelStale` prose equivalent of the graphical degraded marker — same information, prose form, never silently omitted.
5. Related-chain prose form, consuming Phase 3's walk output with narratable-appropriate truncation/cycle phrasing.
6. `toProseMarker :: Ann -> Text` interpretation of `Ann` at this handler's boundary.
7. Wire `render` for `'TNarratable`.

**Exit criteria:**
- Golden test matching the vision doc's Narratable canonical example byte-for-byte.
- Golden fixture for a `related` chain past the depth limit, checking the prose truncation marker.
- Confirm every field the graphical handler can show (code, severity, location, source line, label text, stale reason, help, url, related) has a narratable equivalent — no field silently dropped between renderers.
- Totality smoke check: `render` for `'TNarratable`, evaluated inside `try @SomeException`, does not throw for the same small fuel-bounded generated set used in Phase 5's smoke check.

**Risks:** low technical risk relative to Phase 5; the main failure mode is inconsistency with Phase 5's field coverage (a stale reason or cycle marker rendered in graphical but forgotten in narratable). Cross-check against Phase 5's field list explicitly before marking this phase done.

---

## Phase 7 — JSON Report Handler + DTO

**Corresponds to vision §7 (JSON body) and the JSON canonical example.**

**Goal:** the machine-readable renderer, with explicit fields for every state the other two renderers show visually.

**Deliverables:**
1. Dedicated `DiagnosticDTO` type — **never** `deriving ToJSON` on internal `Diagnostic`-bearing types. `ToJSON` only in v1; `FromJSON` explicitly deferred (per vision's What tadka Does Not Do).
2. DTO shape per the canonical example: `code`, `severity`, `message`, `labels` (each with `line`, `column`, `length`, `text`, and an explicit `"stale": true/false` reflecting `LabelOk`/`LabelStale` — never inferred from absence), `help`, `url`, `related`, `truncated`, `cycleOmitted`.
3. `related` field recurses into nested DTOs, with `truncated`/`cycleOmitted` set from Phase 3's walk output — the same two `TerminationReason` values Phases 5/6 already consume, just serialized instead of prose-rendered.
4. `JsonOptions` real payload; `Json` constructor is the only route to `Output 'TJson = Aeson.Value` — the DTO conversion inside the `'TJson` render branch is the only path, not merely the recommended one.
5. Wire `render` for `'TJson`.

**Exit criteria:**
- Golden test matching the vision doc's JSON canonical example byte-for-byte.
- Golden fixture for a `related` chain past the depth limit, checking `"truncated": true` shape, and a separate fixture for `"cycleOmitted": true`.
- Confirm no code path produces `Aeson.Value` for a `Diagnostic` other than through this handler's DTO conversion (review-level check, mirrors Phase 4's "one path" discipline).
- Totality smoke check: `render` for `'TJson`, evaluated inside `try @SomeException`, does not throw for the same small fuel-bounded generated set used in Phases 5/6's smoke checks.

**Risks:** low; this phase is mostly transcription of Phase 3's walk output and Phase 2's `LabelState` into a DTO shape. The one correctness-sensitive point is making sure `stale` is derived from `LabelState`, not re-inferred from whether a source line happens to be present in the DTO (which would reopen the exact silent-inference problem v5 exists to close).

---

## Phase 8 — Derive Macro (`deriveDiagnostic`)

**Corresponds to vision §6.**

**Goal:** the primary ergonomic entry point, built last among the "core" phases because it only generates calls into functions that must already exist and already be tested (Phases 1–7).

**Deliverables:**
1. `DiagnosticSpec` record (`specCode`, `specSeverity`, `specHelp`, `specUrl`, `specSourceField`, `specLabelFields`, `specRelated`, `specId`, `specMessage`) and `defaultSpec`.
2. `deriveDiagnostic :: DiagnosticSpec -> Name -> Q [Dec]`, an ordinary TH splice (no type-level DSL, no `DerivingVia`):
   - `reify` validates `specSourceField` names a `NamedSource`-typed field, every `specLabelFields` name a `Span`-typed field, `specId`'s field (if given) is `Text`- or `DiagnosticId`-typed, and — closing a gap the vision doc leaves implicit — `specRelated`'s field (if given) is `[SomeDiagnostic]`-typed. Any mismatch is `qReport True "..."`, failing the compile. Generated `related` is a direct accessor call to that field (`related e = specRelatedField e`); omitted `specRelated` → class default `[]`, exactly parallel to how `specId`/`diagnosticId` is handled. This behavior is not spelled out in the vision document's `reify` bullet list (which names only the source/label/id fields) — it's specified here so Phase 8 has an unambiguous target rather than an implementation-time judgment call.
   - `specCode`/`specUrl`, if present, are run through `mkDiagnosticCode`/`mkUrl` **at splice time**; a `Left` is a compile error, not a value that typechecks today and misrenders later. Generated `code`/`url` methods embed the result via `unsafeDiagnosticCode`/`unsafeUrl` (Phase 1) — safe here specifically because splice-time validation already occurred.
   - Generated `context` is a direct call to `Tadka.Internal.Context.buildContext`, built and independently tested in Phase 2 — this phase only generates the call site, it does not introduce or modify `buildContext` itself.
   - Generated `diagnosticId` (new in v5): wraps the `specId` field via `mkDiagnosticId` if `Text`, used directly if already `DiagnosticId`. Omitted `specId` → class default `Nothing`.
   - `message` defaults to `pretty . show` when `specMessage` is `Nothing` and no other field covers it, requiring `Show e` — checked by `reify`; absent `Show` is a compile error naming the missing instance, not a runtime error.
3. **Enforced discipline (v5's core Phase-8 change):** every method body the splice generates must be a direct, unmodified call to a plain, exported `Tadka.Internal` function that a manual instance could also call. If a future field needs logic the current `Tadka.Internal` surface can't express as a plain call, the fix is extracting a new plain function first — not adding logic inside the `Q` splice. This is a code-review convention, not type-enforced, which makes it the sharpest scope-creep risk in the whole spec — nothing stops a rushed change from adding logic inside a `Q` splice, because the compiler won't catch it. Two backstops are **required**, not optional, for this reason: (a) the module haddock for `Tadka.Internal` states the convention explicitly, and (b) a CI check greps generated-output golden fixtures (Phase 11) for anything that isn't a single function application and fails the build if it finds one. This CI check is a Phase 8 exit-criterion, not a nice-to-have — do not mark Phase 8 done without it in place.

**Exit criteria:**
- Compile-fail tests: misspelled field name, wrong field type (`Span`-typed field passed where `NamedSource` expected, etc.), invalid `specCode`/`specUrl` literal — each produces a compile error at the splice site, not a runtime failure.
- Golden test: the `ParseError`/`UnexpectedToken` example from vision §6 derives and renders identically (byte-for-byte across all three handlers) to a hand-written manual `Diagnostic` instance for the same type — this is the concrete check that "the derive path and the manual path are two doors into the same room."
- Review checklist item, tracked as a repo `CONTRIBUTING.md` entry: no generated method body contains anything beyond a direct call to an already-tested `Tadka.Internal` function.
- CI check (required, not optional): the generated-output grep check described above runs in CI and fails the build on any non-single-function-application method body; this must be in place before Phase 8 is considered done, not added opportunistically later.

**Risks:** the temptation here is writing convenience logic directly inside the `Q` splice "just this once" for a field the current internal API doesn't cleanly support. Treat any such urge as a signal to add a Phase-1/2/3-style plain function first, even if it means briefly stepping back into an earlier phase's module.

---

## Phase 9 — Generics-Based Label-Wiring Derivation

**Corresponds to vision §6, "GHC-generics path, scoped precisely."**

**Goal:** a second, smaller derivation path, scoped to exactly `context` — not a competitor to Phase 8.

**Deliverables:**
1. `Generic`-based derivation for any record with exactly one `NamedSource`-typed field and one or more `Span`-typed fields, using each field's name as label text, calling the same `buildContext` from Phase 2.
2. Explicitly does **not** derive `code`, `severity`, `help`, `url`, `message`, or `diagnosticId` — those need a small hand-written completion (each defaultable per §1/Phase 3).
3. Documentation stating precisely what this path covers, matching the vision doc's framing ("smaller than 'most of the ergonomics,' stated as exactly that") — this is a documentation deliverable as much as a code one, to prevent scope creep into Phase 8's territory.

**Exit criteria:**
- Golden test: a record with one `NamedSource` field and two `Span` fields derives `context` via generics, producing output identical to the equivalent hand-written `buildContext` call.
- Confirm (by type signature / absence of instance methods) that this derivation genuinely does not touch `code`/`severity`/`help`/`url`/`message`/`diagnosticId`.

**Risks:** low. Main risk is scope creep — resist adding more derived methods here; that's what Phase 8 is for.

---

## Phase 10 — Interop Helpers

**Corresponds to vision's Infrastructure section ("conversion helpers for megaparsec, attoparsec, and GHC SrcSpan positions").**

**Goal:** make `tadka` a drop-in fit for the ecosystems named in "Where tadka Will Be Used."

**Deliverables:**
1. `megaparsec` position → `Span`/`Offset` conversion helpers.
2. `attoparsec` position → `Span`/`Offset` conversion helpers.
3. GHC `SrcSpan` → `Span` conversion helpers.
4. Each helper is a plain function against Phase 1/2 types only — no new core-type surface introduced here.

**Exit criteria:**
- Round-trip test per library: construct a known parse failure with each library, convert its position type, resolve against the same source text, confirm line/column output matches what the source library itself reports.
- No interop module is a dependency of any core (`Tadka`, `Tadka.Internal.*`) module — these are one-directional adapters, not core surface.

**Risks:** version drift in the three upstream libraries' position types. Pin minimum supported versions explicitly and note them in the interop module haddocks.

---

## Phase 11 — Full Test Suite Consolidation & Release Prep

**Goal:** every property and golden fixture named across Phases 1–10 exists, runs together, and the public API surface matches vision §8 exactly before calling this v1.

**Deliverables:**
1. Consolidated golden test suite: the **six** canonical renderings (single label, multi-label+palette, degraded label, cycle-omitted, narratable, JSON) plus the depth-limit-truncation fixture (both prose and JSON `"truncated": true` forms) — all pinned byte-for-byte, per handler, per vision §"Testing Strategy."
2. Consolidated Hedgehog property suite, collecting every property named in Phases 1–7:
   - `resolveSpan` never returns an out-of-bounds `ResolvedSpan`.
   - `mkContext` returns `Left` iff a span is out-of-bounds.
   - `mkContextDegrading` label-count invariance.
   - `render` totality: evaluated to normal form inside `try @SomeException` for every `Renderer t` and every fuel-bounded generated `SomeDiagnostic`; any exception fails the property. This broadens the phase-local totality smoke checks from Phases 5–7 (which used a small fixed generated set) into the full Hedgehog-generated property.
   - Cycle detection: for a chain where two nodes share a `diagnosticId`, a marker planted in the second node's `message` appears at most once in rendered output; `diagnosticId = Nothing` chains checked against the old v4 (depth-limited, terminating) property instead.
   - `selectRenderer` + `withTarget` dispatch correctness.
   - Gutter/caret never negative, never overlaps line-number gutter, over Unicode-hard text.
   - Label palette cycling correctness.
   - Smart constructor rejection correctness (Phase 1, re-run here as part of the full suite, not just standalone).
   - `NamedSource` accessor round-trip.
3. Public API surface audit against vision §8's full re-export list — confirm `Tadka` exports exactly that list, `GraphicalOptions`/`NarratableOptions`/`JsonOptions` are opaque-only, and `Tadka.Internal.*` (including `buildContext`, `unsafeDiagnosticCode`/`unsafeUrl`, DTO, width/gutter machinery) is documented as having no compatibility guarantee.
4. Haddock pass across the public surface, including the module-level "no compatibility guarantee" notice on `Tadka.Internal`.
5. Package metadata / changelog reflecting this as the v1 release, referencing the v4→v5 changelog already in the vision document.

**Exit criteria (= v1 ship criteria):**
- All golden and property tests green in CI, run as one suite.
- Public API surface audit passes with zero discrepancies against vision §8.
- The Success Criterion scenario in the vision doc is exercised as an end-to-end test: derive `Diagnostic` for a small error type in a handful of lines + one splice, confirm compile-time failure on a misspelled span field name, confirm a deliberately-staled span renders a clear in-report reason rather than a shorter output.

---

## Cross-Phase Notes

- **Testing is not deferred to Phase 11 in practice.** Every phase above lists its own exit-criteria tests; Phase 11 is consolidation and the final API-surface/release audit, not the first time tests are written. Treat the Phase 11 LOC estimate (~700–950) as spread across Phases 1–10's own test code, with Phase 11 itself mostly wiring and the audit pass.
- **The "one path" / "provably thin" conventions (Phases 4 and 8) are review-enforced, not type-enforced.** Bake both into `CONTRIBUTING.md` before Phase 5 starts, since Phase 5 is the largest surface area where a shortcut could quietly violate either convention.
- **Do not build a fourth `Target` or a `FromJSON` path anywhere in Phases 1–11.** Both are explicit, permanent scope exclusions per the vision document's What tadka Does Not Do — not omissions to "get to later" within this spec.
