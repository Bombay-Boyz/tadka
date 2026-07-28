# tadka

# Vision Document (v5: Full Parity, Provably Correct, Deterministically Rendered)

**Tagline:** *Point at the problem. Say it clearly. Make it beautiful.*

---

# Changelog Since v4

This revision closes four review findings and adds four smaller fixes. Nothing
here changes the project's scope or its "no silently wrong report" promise —
it makes that promise hold in more cases, and states plainly where it still
doesn't.

1. **`mkContextLenient` no longer drops labels — it marks them.** Renamed
   `mkContextDegrading`. A stale span used to vanish from the rendered output
   with no trace; it now renders as an explicit degraded label. Two
   diagnostics with identical logical content now always produce the same
   *shape* of output (same label count, same ordering), never a silently
   shorter one. (§2)
2. **`ResolvedSpan` no longer owns its own text.** `NamedSource` is stored
   once per `Context`; spans carry only offsets. Text is sliced on demand at
   render time via `Text.take`/`drop`, which is O(1) and shares the backing
   buffer — no per-label copy, and no ambiguity about how big a "snippet"
   is. (§2)
3. **Related-diagnostic cycles get a real identity check, not just a depth
   cutoff.** An opt-in `diagnosticId` method lets `render` recognize "I've
   already rendered this exact diagnostic in this chain" and stop with a
   `(cycle omitted)` marker instead of grinding to the depth limit. Diagnostics
   that don't provide an id fall back to the old depth-limit-only behavior,
   which still terminates (§5).
4. **The derive macro's generated code is now provably thin.** Every
   TH-generated method body is a direct call to a plain, exported,
   independently-testable function (`Tadka.Internal.buildContext`, etc.). The
   discipline is now a checked one: nothing is allowed inside a `Q` splice
   that isn't first written as a normal function a manual instance could also
   call. (§6)
5. Per-label underline coloring (needed for the two-label canonical example
   to actually be legible) is now specified explicitly as a `GraphicalReportHandler`
   concern, not left implicit. (§7)
6. `NamedSource` is a two-field record, not an opaque `(Text, Text)` tuple.
   (§4)
7. `Severity` is now fully defined instead of referenced-but-unshown. (§4)
8. The grapheme-cluster/East-Asian-width dependency is now a named, owned
   decision instead of "an existing library." (§0, Infrastructure)

---

# Scope Clarifications (Post-v5 Review)

Two ambiguities surfaced during implementation-spec review, closed here so
neither reads as an implicit commitment:

1. **`trifecta` interop.** "Where `tadka` Will Be Used" named `trifecta`
   alongside `megaparsec`/`attoparsec` as a parser library `tadka` serves,
   but only `megaparsec`/`attoparsec`/GHC `SrcSpan` were ever scoped as
   *dedicated conversion helpers* (Infrastructure, above). That's now
   stated explicitly in both places below — `trifecta` remains usable
   with `tadka` via manual `Span` construction, but no dedicated,
   maintained `trifecta` adapter is v1 scope.
2. **"Long-Term Vision" gating.** That section listed future
   possibilities without stating they're out of v1 scope as clearly as
   "What `tadka` Does Not Do" states its permanent exclusions. It now
   says so explicitly, so it can't be read as deferred-but-implied v1
   work.

Nothing else in this document changed as a result of this review.

---

# Executive Summary

`tadka` is an open-source diagnostic reporting framework for Haskell,
targeting full feature parity with Rust's `miette` from its first
release: structured error types, three pluggable report renderers, error
codes with documentation links, related-error chains, labeled multi-span
source snippets, and derive-macro ergonomics for defining new diagnostics
with almost no boilerplate.

`tadka` sits on `prettyprinter`, `ansi-terminal`, `text`,
`template-haskell`, and `aeson`, and contributes what `miette` contributes
in Rust — nothing else: no parsing, no semantic analysis, no IO
orchestration.

Every type is designed so a value that can't be rendered correctly can't
be constructed, and every worked example in this document is the actual
shape the design produces. Where a value's *content* can genuinely go
stale between construction and render (a source span pointing at text
that changed underneath it), the design no longer hides that by silently
shrinking the output — it says so, in the output, deterministically.

---

# The Problem

Haskell has no `miette`. Every parser-combinator project, every DSL, every
compiler-shaped tool currently either hand-rolls string formatting, prints
bare line/column numbers, or does both inconsistently within the same
codebase. `tadka` is the same design `miette` already proved out in Rust,
built correctly the first time.

---

# The Central Question

> **Given any error value, how do we get a beautiful, structured,
> actionable diagnostic report out of it, with the least ceremony from
> the author, with no way to produce a report that is silently wrong or
> silently different from run to run, and with a render pipeline that
> typechecks end to end rather than relying on a convention the types
> don't actually enforce?**

---

# Philosophy

An error is a message, a context (where, precisely, in what source), a
code, a severity, help, related diagnostics, and a url. `tadka` treats
all seven as first-class, and treats "does this field combination
typecheck all the way from construction to rendered output, and does it
render the same way every time given the same content" as a design
question to answer once, in the types, rather than re-verify by hand in
every worked example.

---

# Building on Existing Infrastructure

- `prettyprinter` — layout engine for all rendering
- `prettyprinter-ansi-terminal` — color/style, applied only at the
  graphical renderer's boundary (Architecture §7)
- `ansi-terminal` — terminal capability detection
- `Tadka.Internal.Width` — a small, bundled East-Asian-width and
  grapheme-cluster-break table generated from the Unicode Character
  Database, checked into the repo and regenerated by a script under
  `tools/`. This is a deliberate choice over binding `text-icu` (a system
  ICU dependency is a heavyweight, portability-risking ask for what this
  library needs) and over depending on a third-party pure-Haskell package
  (maturity and maintenance risk for a component the caret-alignment
  correctness property depends on directly). Owning a small, versioned,
  regeneratable table is the boring, dependable option.
- `template-haskell` — a single ordinary splice, `deriveDiagnostic`, that
  runs in the `Q` monad and can fail the compile (Architecture §6);
  no type-level DSL, no `DerivingVia`
- `aeson` — the JSON report handler, via a dedicated wire-format DTO
  (Architecture §8)
- conversion helpers for `megaparsec`, `attoparsec`, and GHC `SrcSpan`
  positions

---

# Architecture

## 1. The `Diagnostic` Typeclass

```haskell
class Diagnostic e where
  message      :: e -> Doc Ann
  context      :: e -> Context
  code         :: e -> Maybe DiagnosticCode
  severity     :: e -> Severity
  help         :: e -> Maybe (Doc Ann)
  url          :: e -> Maybe Url
  related      :: e -> [SomeDiagnostic]
  diagnosticId :: e -> Maybe DiagnosticId

  severity     _ = SevError
  context      _ = NoContext
  code         _ = Nothing
  help         _ = Nothing
  url          _ = Nothing
  related      _ = []
  diagnosticId _ = Nothing
```

`message` is the only mandatory method — no default, and no `Show`
superclass. A concrete error type is free to `deriving (Show)` for its
own debugging purposes without that choice leaking into what users see;
`deriveDiagnostic` (§6) *can* default `message` to `pretty . show` when
`Show` is present and no explicit message is given, but that is the
derive macro's choice to make per instance, not something the class
itself requires of every `Diagnostic`.

`diagnosticId` is new in v5 and entirely opt-in. It exists for exactly
one purpose: letting `render` recognize when the same diagnostic
reappears in a `related` chain, so it can stop with a cycle marker
instead of descending again (§5). Its default `Nothing` means "I have no
opinion on identity" — such a diagnostic still gets the old, always-safe
depth-limit protection, just not the sharper cycle detection. Nothing
about the existing seven methods, or any existing manual instance, needs
to change for `diagnosticId` to exist; it's additive.

## 2. `Context`: only ever holds spans that have already been checked against real source text — and never silently forgets one

```haskell
newtype Offset = Offset Int    -- mkOffset, rejects < 0
newtype Length = Length Int    -- mkLength, rejects < 0 (0 is a valid point span)

data Resolution = Unresolved | Resolved

data SpanF (r :: Resolution) where
  RawSpan      :: Offset -> Length -> SpanF 'Unresolved
  ResolvedSpan :: Offset -> Length -> LineCol -> LineCol -> SpanF 'Resolved

type Span         = SpanF 'Unresolved
type ResolvedSpan = SpanF 'Resolved
```

`ResolvedSpan` no longer carries its own extracted `Text` (v4 did this;
removed in v5). It carries only positions. The source text a
`ResolvedSpan` refers to lives exactly once per `Context` — see below —
and the renderer slices out whatever substring it needs (the label span
itself, or the full line for gutter display) with `Text.take`/`Text.drop`
against that one stored source. Since `Text` slicing is O(1) and shares
the underlying buffer rather than copying it, this isn't a performance
optimization over the v4 design so much as a correctness/API one: a
`ResolvedSpan`'s job is to name a position, not to own a rendering of it,
and the renderer — which knows whether it needs one line or three lines
of context around a label — is the right place to decide how much text to
slice.

```haskell
data StaleReason = SpanOutOfBounds | SourceMismatch

data LabelState
  = LabelOk ResolvedSpan
  | LabelStale StaleReason
  -- ^ resolution against the paired source failed for this one label;
  -- it is still present in the label list, in its original position,
  -- with no source line to show, rather than absent.

data Labeled a = Labeled { labelSpan :: a, labelText :: Maybe (Doc Ann) }

data Context
  = NoContext
  | HasLabels NamedSource (NonEmpty (Labeled LabelState))

resolveSpan :: NamedSource -> Span -> Either SpanError ResolvedSpan

-- strict: fails if *any* span is out of bounds against src. Intended use
-- is inside a domain error type's own smart constructor (see note below),
-- so that by the time a caller holds e.g. a `ParseError` value, its
-- context is already known to be fully, permanently resolvable.
mkContext :: NamedSource -> NonEmpty (Labeled Span) -> Either ContextError Context

-- total: resolves every span it can; a span that doesn't resolve becomes
-- `LabelStale reason` in the same position rather than being dropped.
-- Falls back to NoContext only if the input list itself is empty.
-- Used by generated instances, where a compile-time TH splice has no way
-- to know in advance whether a runtime source will still match a runtime
-- span, and "the label is visibly marked degraded" is strictly better
-- than either "crash" or "the label silently isn't there."
mkContextDegrading :: NamedSource -> NonEmpty (Labeled Span) -> Context
```

A `Context` value, once constructed by either function, holds only
`ResolvedSpan`s or explicitly-tagged `LabelStale` markers — the renderer
never receives a span it has to check bounds on itself, and it never
receives a label list whose length silently depends on which spans
happened to still be valid. `NoContext` is the one and only "no
source-anchored information" state; `HasLabels` is the one and only "at
least one label, and a single source they're all checked against" state.

**Why this fixes the determinism problem, not just relabels it.** In v4,
`mkContextLenient` degraded exactly the spans that would have failed
`mkContext`, and *removed* them — so the same diagnostic, rendered twice
against two different snapshots of "the same" source (one where a span
happens to still be in range, one where it doesn't), produced two
outputs with different label *counts*. In v5, `mkContextDegrading`
produces the same label *count* and *ordering* every time, for a given
diagnostic and a given source — only the content of an individual label
(source line + underline vs. a stale marker) can differ. That's still a
real difference when the underlying source genuinely changed, which is
unavoidable — a stale span is stale — but it's no longer a difference the
reader has to notice by absence. It's a difference the reader is told
about.

**Where the strict path is meant to be used.** `mkContext` remains
useful, but not as something `context` calls directly (it can't — `context`
is total, per §1, and pushing `Either` into the class signature would
break `render`'s totality guarantee, tested in §"Testing Strategy"). Its
intended call site is a domain error type's own smart constructor:

```haskell
mkParseError :: NamedSource -> Text -> Text -> Span -> Either ContextError ParseError
mkParseError src got expected at = do
  ctx <- mkContext src (Labeled at (Just "unexpected token here") :| [])
  pure (ParseError src got expected at)  -- ctx discarded here; context is recomputed
                                          -- from the stored fields at render time,
                                          -- but its resolvability has now been proven
                                          -- once, at construction
```

An author who wants the stronger guarantee — "if I hold a `ParseError`
value, I know every one of its labels will render fully, forever" —
gets it by validating at construction time via `mkContext`, not by
threading an `Either` through every render call. An author using
`deriveDiagnostic` gets the weaker, always-safe guarantee automatically,
with explicit degradation instead of silent loss.

**Where the `NamedSource` comes from.** A `Diagnostic` instance typically
owns its own source — cheaply, since `Text` is an immutable, pointer-shared
value in GHC, so an error type storing a `NamedSource` field alongside its
`Span` fields is a pointer copy, not a deep copy, exactly as a parser
combinator library already keeps its input in scope while producing an
error value. `context` is a plain total accessor that reads that stored
field and calls `mkContextDegrading` — it does not require `render` to be
handed a source separately, and it does not require every error site to
re-derive one from nothing.

## 3. `Ann`: a renderer-agnostic annotation vocabulary

```haskell
data Ann = AnnEmphasis | AnnCode | AnnFilename | AnnKeyword
  deriving (Eq, Show, Enum, Bounded)
```

`message`, `help`, and label text are `Doc Ann`, never `Doc AnsiStyle`.
Each renderer interprets `Ann` at its own boundary:

```haskell
toAnsiStyle   :: Ann -> AnsiStyle   -- GraphicalReportHandler only
toProseMarker :: Ann -> Text        -- NarratableReportHandler only
-- JsonReportHandler walks the Doc for plain text, discards Ann
```

`Ann` is deliberately unchanged from v4 and deliberately does **not**
grow a per-label-index constructor. `Ann` annotates the semantic content
an author writes — a keyword, a filename, emphasis — the same content
regardless of which renderer or which label position it ends up in.
Which color the *second* underline in a multi-label graphical render gets
is not something the diagnostic's author has an opinion about, and isn't
semantic content; it's a purely positional rendering decision. That
belongs to the `GraphicalReportHandler`, not to `Ann` — see §7 for where
it's actually specified.

## 4. `NamedSource`, `DiagnosticCode`, `Url`, `Severity`

```haskell
data NamedSource = NamedSource
  { sourceName :: Text
  , sourceText :: Text
  }
  -- constructor not exported; sourceName/sourceText accessors are
  -- exported read-only

mkNamedSource :: Text -> Text -> Either SourceError NamedSource
-- rejects an empty name; empty *content* is a legitimate source

newtype DiagnosticCode = DiagnosticCode Text        -- constructor not exported
mkDiagnosticCode :: Text -> Either CodeError DiagnosticCode
-- non-empty, matches the project's code grammar, e.g.
-- ^[a-z][a-z0-9_]*::E[0-9]{4,}$

newtype Url = Url Text                               -- constructor not exported
mkUrl :: Text -> Either UrlError Url
-- must parse as an absolute URI

data Severity = SevAdvice | SevWarning | SevError
  deriving (Eq, Ord, Show, Enum, Bounded)
-- SevAdvice   -> graphical header "advice:", narratable "Advice, ..."
-- SevWarning  -> graphical header "warning:", narratable "Warning, ..."
-- SevError    -> graphical header "error:", narratable "Error, ..."
-- default (§1) is SevError
```

`NamedSource` moved from an anonymous `(Text, Text)` pair (v4) to a
two-field record (v5) purely to remove the `fst`/`snd` ambiguity at every
internal call site — no behavioral change, no relaxation of the
unexported-constructor invariant.

No constructor is exported for `NamedSource`, `DiagnosticCode`, or `Url`;
every value in existence has already passed validation, so nothing
downstream re-checks these invariants.

## 5. Related Diagnostics

```haskell
newtype DiagnosticId = DiagnosticId Text   -- constructor not exported
mkDiagnosticId :: Text -> DiagnosticId     -- total; any Text is a valid id,
                                            -- it's an opaque comparison key,
                                            -- not a validated format

data SomeDiagnostic = forall e. Diagnostic e => SomeDiagnostic e
related :: Diagnostic e => e -> [SomeDiagnostic]
```

No `Show` needed on the existential — every operation a renderer
performs goes through `Diagnostic`'s methods, `diagnosticId` included.

Renderers walk `related` carrying two pieces of state: a depth counter
(as in v4) and a `Set DiagnosticId` of ids seen so far on the current
path. At each step:

- if the next diagnostic's `diagnosticId` is `Just i` and `i` is already
  in the visited set, rendering stops there with a `(cycle omitted)`
  marker — the diagnostic is not descended into again;
- otherwise, if `diagnosticId` is `Just i`, `i` is added to the visited
  set for the remainder of that path, and rendering continues normally;
- if `diagnosticId` is `Nothing`, no identity information exists for that
  diagnostic — rendering continues under the depth limit alone, exactly
  as in v4.

This means diagnostics that never set `diagnosticId` (the common case —
most leaf error types have no reason to) behave identically to v4:
depth-limited, always-terminating, no cycle-specific short-circuiting.
Diagnostics that do set it (typically only ones a compiler or DSL author
expects to appear in genuinely cyclic relationship graphs — "conflicting
instance A references conflicting instance B references conflicting
instance A") get the sharper behavior. Nothing is required of an author
who doesn't need this.

Both truncation forms are specified per handler, not left implicit:
graphical and narratable render a one-line "(N more related diagnostics
omitted)" for depth-limit truncation and a one-line "(cycle omitted)" for
identity-based truncation; the JSON DTO emits `{"truncated": true}` for
the former and `{"cycleOmitted": true}` for the latter, so a consumer
parsing the DTO has an explicit field to check for each case rather than
one shape doing double duty.

The default depth limit remains 8, a `Config` field (§7).

## 6. The Derive Macro — an ordinary TH splice, validated in `Q`, generating only calls to shared functions

```haskell
data ParseError = UnexpectedToken
  { errSource :: NamedSource
  , got       :: Text
  , expected  :: Text
  , at        :: Span
  }
  deriving (Show)

deriveDiagnostic defaultSpec
  { specCode        = Just "tadka::E0001"
  , specHelp        = Just "did you forget a semicolon?"
  , specSourceField = Just 'errSource
  , specLabelFields = [('at, "unexpected token here")]
  }
  ''ParseError
```

```haskell
data DiagnosticSpec = DiagnosticSpec
  { specCode        :: Maybe Text
  , specSeverity    :: Severity
  , specHelp        :: Maybe Text
  , specUrl         :: Maybe Text
  , specSourceField :: Maybe Name    -- must name a NamedSource-typed field
  , specLabelFields :: [(Name, Text)] -- each Name must name a Span-typed field
  , specRelated     :: Maybe Name    -- must name a [SomeDiagnostic]-typed field
  , specId          :: Maybe Name    -- must name a Text- or DiagnosticId-typed field
  , specMessage     :: Maybe (Q Exp)
  }

defaultSpec :: DiagnosticSpec

deriveDiagnostic :: DiagnosticSpec -> Name -> Q [Dec]
```

`specId` is new in v5: naming a field there generates a `diagnosticId`
method wrapping that field (via `mkDiagnosticId` if it's `Text`, or used
directly if it's already `DiagnosticId`). Omitting `specId` generates the
class default, `Nothing` — the ordinary, expected case.

`deriveDiagnostic` is plain Template Haskell, not `DerivingVia` over a
type-level list — there is no attempt to parse or validate a `Symbol` at
the type level, which would need either a hand-written type-level parser
or a compiler plugin to do properly. Instead, the splice runs in `Q`,
where reification and reporting are already the right tools for the job:

- `reify ''ParseError` checks that `errSource` exists and has type
  `NamedSource`, that every `Name` in `specLabelFields` exists and has
  type `Span`, and that `specId`'s field (if given) has type `Text` or
  `DiagnosticId` — a mismatch on any of them is a `qReport True "..."` and
  the module fails to compile.
- `specCode`/`specUrl`, if present, are run through `mkDiagnosticCode`/
  `mkUrl` at splice time; a `Left` is a compile error via the same
  `qReport`, not a value that typechecks today and misrenders later. The
  generated `code`/`url` methods embed the validated result using an
  internal, unexported `unsafeDiagnosticCode`/`unsafeUrl` constructor —
  safe here specifically because the splice has already proven the input
  valid; this constructor is never reachable from outside
  `Tadka.Internal`.
- The generated `context` method is a direct call to one plain,
  exported, independently-tested function:

  ```haskell
  -- Tadka.Internal.Context
  buildContext :: NamedSource -> [(Span, Maybe (Doc Ann))] -> Context
  buildContext src fields = case NE.nonEmpty (map (uncurry Labeled) fields) of
    Nothing -> NoContext
    Just ls -> mkContextDegrading src ls
  ```

  ```haskell
  -- generated
  context e = buildContext (errSource e)
    [(at e, Just (pretty "unexpected token here"))]
  ```

  `buildContext` is the *only* place the empty-list-to-`NoContext`
  decision and the call into `mkContextDegrading` are written. A manual
  `Diagnostic` instance that wants the same behavior calls the exact same
  function; there is no second copy of that decision to drift out of sync.
  This is the concrete form of the v4→v5 change: **every method the
  derive macro generates is required to be a direct, unmodified call to a
  plain function that also appears in `Tadka.Internal`'s export list** —
  if a future feature can't be expressed that way, the fix is to extract
  a new plain function first, not to grow logic inside the `Q` splice.
  This is a project convention enforced by code review, not by the type
  system, and is written down here so it stays enforced as the spec
  grows.
- `message`, if `specCode`/`specHelp`/etc. don't cover it and `specMessage`
  is `Nothing`, defaults to `pretty . show`, requiring `Show e` (checked
  by `reify` — absent `Show`, this is a compile error naming the missing
  instance, not a runtime `error`). This one piece of logic — choosing a
  message when none is given — has no manual-instance equivalent and
  isn't expected to: a manual instance always writes `message` explicitly,
  by definition, so there's nothing for it to drift out of sync with.

For diagnostics needing a value-dependent message (the common case — "
undefined variable `foo`" embeds the parsed name) `specMessage = Just
[| \e -> "undefined variable" <+> pretty (name e) |]` supplies an actual
expression; for anything the spec can't express at all, a direct
hand-written `Diagnostic` instance is written against §1 — the derive
path and the manual path are two doors into the same room, and as of v5
that's a checked convention, not just a stated intention.

**GHC-generics path, scoped precisely:** a second, non-TH derivation
exists for label-wiring only — it derives `context` automatically for any
`Generic` record with exactly one `NamedSource`-typed field and one or
more `Span`-typed fields, using each field's name as label text, by
calling the same `buildContext`. It does not derive `code`, `severity`,
`help`, `url`, `message`, or `diagnosticId` — those have no structural
source — so a generics-only instance still needs a small hand-written
completion of those (each defaultable per §1). This is smaller than "most
of the ergonomics" and is stated as exactly that.

## 7. `Renderer`, `Config`, and the one path that connects them

```haskell
data Target = TGraphical | TNarratable | TJson

data Renderer (t :: Target) where
  Graphical  :: GraphicalOptions  -> Renderer 'TGraphical
  Narratable :: NarratableOptions -> Renderer 'TNarratable
  Json       :: JsonOptions       -> Renderer 'TJson

type family Output (t :: Target) where
  Output 'TGraphical  = Doc Ann
  Output 'TNarratable = Text
  Output 'TJson       = Aeson.Value

render :: Diagnostic e => Renderer t -> e -> Output t
```

`Target` is a closed, three-constructor type and `Output` is a closed
type family, deliberately. This is stated explicitly here as a permanent
scope boundary, not an oversight: `tadka` ships exactly the three
renderers `miette` ships, and a fourth, custom render target (a Markdown
renderer, an editor-native format) is out of scope forever, not deferred.
See "What `tadka` Does Not Do."

`GraphicalOptions`, `NarratableOptions`, and `JsonOptions` carry no
exported constructor and no exported field accessors — nothing in `tadka`
lets a caller build one directly. `Renderer`'s three constructors *are*
exported, so ordinary pattern matching works at call sites, but since an
`Options` value can only ever come from one function, there is exactly
one route to a `Renderer t` that does anything:

```haskell
newtype Config = Config (...)   -- constructor not exported
defaultConfig :: Config
withColorMode         :: ColorMode     -> Config -> Config
withUnicodeMode       :: UnicodeMode   -> Config -> Config
withRelatedDepthLimit :: Natural       -> Config -> Config
withLabelPalette      :: NonEmpty AnsiStyle -> Config -> Config
withTarget            :: Target        -> Config -> Config   -- explicit override

data ColorMode   = ColorAuto | ColorAlways | ColorNever
data UnicodeMode = UnicodeAuto | UnicodeAlways | UnicodeAscii

data SomeRenderer = forall t. SomeRenderer (Renderer t)

selectRenderer :: Config -> SomeRenderer
-- the only function that constructs GraphicalOptions/NarratableOptions/
-- JsonOptions values (from Config's fields and, absent an explicit
-- withTarget override, terminal-capability/NO_COLOR/--json detection),
-- and the only function that wraps them in Graphical/Narratable/Json

reportDiagnostic :: Diagnostic e => Config -> e -> IO ()
-- selects a renderer, renders, and writes to the right sink
-- (Doc -> terminal via prettyprinter, Text -> stdout, Value -> stdout
-- as JSON) — covers the common case with no manual existential unwrap
```

**Per-label underline coloring (new in v5).** When a `Context` has more
than one label, the `GraphicalReportHandler` assigns each label's
underline a color cycling through `withLabelPalette`'s list (default: a
six-color palette distinguishable under both light and dark terminal
themes, degrading to distinct underline characters — `^`, `~`, `-` — when
`ColorMode` resolves to `ColorNever`). This is purely positional: label
index *N* in the `Context`'s label list gets palette entry *N mod
(length palette)*. It requires no author input and touches nothing in
`Ann` (§3) — it's computed entirely inside the graphical handler from
information the handler already has. The two-label canonical example in
this document (§ "Canonical Rendering Examples") is now shown with this
applied.

Because every `Options` value traces back to `selectRenderer`, and
`selectRenderer` is the only place `Config`'s fields are read, there is
one path from configuration to a renderer that does anything — not two
that could drift apart. A caller who needs the specific `Renderer t`
`selectRenderer` chose (rather than the `reportDiagnostic` convenience)
pattern-matches on the `SomeRenderer`:

```haskell
case selectRenderer cfg of
  SomeRenderer r@(Graphical _)  -> putDoc (render r err)
  SomeRenderer r@(Narratable _) -> Text.putStrLn (render r err)
  SomeRenderer r@(Json _)       -> BSL.putStrLn (Aeson.encode (render r err))
```

**JSON DTO.** `Output 'TJson = Aeson.Value` is produced from a dedicated
`DiagnosticDTO` type, never by deriving `ToJSON` on internal
`Diagnostic`-bearing types. `Json` is the only constructor whose
`Options` type reaches `render`'s `'TJson` branch, so the DTO conversion
inside that branch is the only route to `Aeson.Value` — not merely the
recommended one. The DTO's label representation carries an explicit
`"stale": true/false` field per label (reflecting `LabelOk`/`LabelStale`,
§2), so a machine consumer can distinguish a degraded label from a fully
resolved one without inferring it from absence. v1 ships `ToJSON` only; a
reviewed `FromJSON` decode path is future work, added once a concrete
consumer needs one.

## 8. Public API surface

`Tadka` re-exports: `Diagnostic`, `Context`, `Labeled`, `LabelState`,
`StaleReason`, `Span`/`ResolvedSpan`/`resolveSpan`,
`mkContext`/`mkContextDegrading`, `Ann`, `Severity`,
`DiagnosticCode`/`mkDiagnosticCode`, `Url`/`mkUrl`,
`NamedSource`/`mkNamedSource` (with `sourceName`/`sourceText` accessors),
`DiagnosticId`/`mkDiagnosticId`, `SomeDiagnostic`, `Target`,
`Renderer(..)` (constructors visible for pattern matching, per §7),
`SomeRenderer`, `selectRenderer`, `render`, `reportDiagnostic`, `Config`
and its setters (including `withLabelPalette`),
`DiagnosticSpec`/`defaultSpec`/`deriveDiagnostic`.
`GraphicalOptions`/`NarratableOptions`/`JsonOptions` are exported as
opaque type names only (no constructor, no accessor) — visible for type
signatures, unconstructible outside `Tadka.Internal`. Everything else —
the TH internals, `buildContext` and its siblings (exported from
`Tadka.Internal` specifically so both derive-macro output and manual
instances can call them, but with no compatibility guarantee), the DTO,
gutter/Unicode-width machinery, the internal
`unsafeDiagnosticCode`/`unsafeUrl` — lives under `Tadka.Internal.*` with
no compatibility guarantee, from the first commit.

---

# Canonical Rendering Examples

## Graphical Handler — Single Label

```
error[tadka::E0001]: undefined variable `foo`
  ┌─ example.hs:3:10
  │
3 │   let x = foo + 1
  │           ^^^ not in scope
  │
  = help: did you mean `bar`?
  = see: https://example.org/errors/E0001
```

## Graphical Handler — Multiple Labels (palette-colored), Related Diagnostic

```
error[tadka::E0042]: type mismatch
  ┌─ example.hs:5:1
  │
3 │ addOne :: Int -> Int
  │           --- expected because of this        [color 1]
4 │ addOne x = x
5 │ result = addOne "hi"
  │                 ^^^^ found `String`, expected `Int`   [color 2]
  │
  = help: convert with `show` or change the annotation
  = related: E0043 — conflicting instance defined here
      ┌─ Prelude.hs:120:1
      │
  120 │ instance Num String where ...
      │ ^^^^^^^^ conflicting instance
```

(`[color N]` denotes which `withLabelPalette` entry the underline uses;
omitted from plain-text renderings, present as actual ANSI color codes in
a real terminal.)

## Graphical Handler — Degraded Label (source went stale between construction and render)

```
error[tadka::E0001]: undefined variable `foo`
  ┌─ example.hs
  │
  │ (span unavailable — source no longer matches at this position)
  │
  = help: did you mean `bar`?
```

## Graphical Handler — Cycle Omitted in Related Chain

```
error[tadka::E0043]: conflicting instance defined here
  ┌─ Prelude.hs:120:1
  │
120 │ instance Num String where ...
  │ ^^^^^^^^ conflicting instance
  │
  = related: (cycle omitted)
```

## Narratable Handler (Accessibility Mode)

```
Error, code tadka::E0001: undefined variable "foo".
Location: example.hs, line 3, column 10.
Source line 3: "let x = foo + 1".
The problem is at columns 10 through 12, labeled: not in scope.
Help: did you mean "bar"?
More information: https://example.org/errors/E0001
```

## JSON Handler

```json
{
  "code": "tadka::E0001",
  "severity": "error",
  "message": "undefined variable `foo`",
  "labels": [
    { "line": 3, "column": 10, "length": 3, "text": "not in scope", "stale": false }
  ],
  "help": "did you mean `bar`?",
  "url": "https://example.org/errors/E0001",
  "related": [],
  "truncated": false,
  "cycleOmitted": false
}
```

A diagnostic with no code renders its graphical header as `error:
undefined variable \`foo\`` (no brackets); the narratable handler omits
the "code X" clause entirely.

---

# Testing Strategy

**Golden tests** pin the six canonical renderings above, byte-for-byte,
per handler (single label, multi-label with palette, degraded label,
cycle-omitted, narratable, JSON), plus a fixture for a `related` chain
past the depth limit (checking both the prose truncation marker and the
JSON `"truncated": true` shape from §5).

**Hedgehog properties:**

- `resolveSpan` never returns a `ResolvedSpan` whose implied text range
  reads outside `sourceText`, generated over `NamedSource`/`Span` pairs
  that are sometimes in-bounds, sometimes deliberately not.
- `mkContext` returns `Left` iff at least one generated span is
  out-of-bounds for the paired source.
- `mkContextDegrading` **never changes the number of labels**: for any
  generated `NonEmpty (Labeled Span)` of length *n*, the resulting
  `HasLabels` always has exactly *n* labels, each either `LabelOk` or
  `LabelStale`, and a label is `LabelStale` if and only if resolving that
  specific span against that specific source would have been the failure
  that made `mkContext` return `Left` on an input containing only that
  span. This is the property that replaces v4's weaker "degrades exactly
  the spans that would have failed" — it additionally pins the label
  *count* as invariant, which is the property that was missing before.
- `render` is total for every `Renderer t` and every generated,
  depth-fuelled `SomeDiagnostic` — evaluated to normal form inside
  `try @SomeException`; any exception fails the property. The
  `SomeDiagnostic` generator takes an explicit fuel parameter, decremented
  on each recursive `related` step and forced to `[]` at zero — required
  because a `Nothing`-`diagnosticId` chain carries no acyclicity
  guarantee (§5), so an unfuelled generator does not terminate and every
  property built on it would silently inherit that.
- **Cycle detection**: for a generated `SomeDiagnostic` chain where two
  nodes share a `diagnosticId`, `render` visits the second occurrence at
  most once and its `related` list is never descended into a second
  time — checked by counting how many times a marker planted in that
  node's `message` appears in the rendered output (at most once).
  Diagnostics generated with `diagnosticId = Nothing` throughout are
  checked against the *old* v4 property instead (depth-limited,
  potentially-repeated traversal, but always terminating).
- `selectRenderer` composed with `withTarget t` always yields a
  `SomeRenderer` whose pattern-matched constructor corresponds to `t` —
  i.e. an explicit override is never overridden again by detection.
- Gutter/caret column computation never goes negative and never overlaps
  the line-number gutter, generated over `Text` containing combining
  marks, East-Asian-width characters, and emoji, using
  `Tadka.Internal.Width`'s table directly (so the property doubles as
  a check that the bundled table stays in sync with what the layout
  engine assumes).
- Label palette cycling: for a `Context` with *k* labels and a palette of
  length *p*, label *i*'s underline style equals palette entry *(i mod
  p)*, generated over `k` and `p` independently.
- Smart constructors (`mkOffset`, `mkLength`, `mkDiagnosticCode`, `mkUrl`,
  `mkNamedSource`) reject exactly the inputs their documented invariant
  says they reject, generated with a mix of valid and adversarial inputs.
- `NamedSource`'s `sourceName`/`sourceText` accessors round-trip the
  values passed to `mkNamedSource` exactly, for any pair that passes
  validation.

---

# Where `tadka` Will Be Used

## Parser Libraries
Full-featured error reporting for `megaparsec` and `attoparsec`, both of
which get dedicated, maintained conversion helpers as part of v1 (see
Infrastructure, above). `tadka` doesn't require a library-specific
adapter to be usable — a `trifecta` user, or a user of any other
position-reporting parser library, can construct `Span`/`NamedSource`
values manually from whatever position type their library exposes — but
no dedicated `trifecta` helper is built or maintained as v1 scope. Also
includes related-error chains for "expected one of these three tokens,
each defined at a different grammar rule."

## Compilers and DSL Tooling
`rustc`-quality diagnostics, including related-diagnostic chains showing
"expected type defined here" alongside "mismatch found here," and cycle
detection for the genuinely-cyclic "conflicting instance" case that
motivated `diagnosticId`.

## CI/CD and Build Systems
The JSON handler gives build systems structured, machine-parseable
diagnostics instead of scraped terminal text, including explicit
`stale`/`truncated`/`cycleOmitted` fields rather than silences to infer
meaning from.

## Editor and LSP Integration
The JSON DTO is a natural basis for LSP `Diagnostic` objects.

## Accessibility-Conscious Tooling
The narratable handler is a first-class renderer, present at launch.

## Teaching Tools
Linked error-code documentation gives students a concrete next step.

---

# What `tadka` Does

- `Diagnostic` typeclass: `message` mandatory, seven defaulted fields
  (including opt-in `diagnosticId`), no `Show` superclass — **in v1**
- `Context`, resolved-or-explicitly-stale (`NoContext` / `HasLabels
  NamedSource (NonEmpty (Labeled LabelState))`), via
  `mkContext`/`mkContextDegrading` — **in v1**
- `deriveDiagnostic`: an ordinary TH splice over a typed `DiagnosticSpec`,
  validating field names/types and literal code/url values in `Q`,
  failing the compile on mismatch, generating only direct calls to
  exported `Tadka.Internal` functions — **in v1**
- Scoped-down GHC-generics label-wiring derivation — **in v1**
- Resolution-indexed `Span`/`ResolvedSpan` — bounds-checked once, safe to
  index thereafter, positions only (no owned text) — **in v1**
- Related/chained diagnostics to a documented, configurable depth limit,
  plus opt-in identity-based cycle detection via `diagnosticId`, with
  distinct truncation shapes for each in every handler including JSON —
  **in v1**
- `NamedSource` (two-field record), `DiagnosticCode`, `Url`,
  `DiagnosticId` as validated, smart-constructed types — **in v1**
- `Renderer (t :: Target)` / `Output t`, with `Config` -> `selectRenderer`
  as the single path to a constructible `Renderer`, `Options` types
  opaque outside that path, and per-label underline palette cycling in
  the graphical handler — **in v1**
- Dedicated JSON DTO with explicit `stale`/`truncated`/`cycleOmitted`
  fields; `ToJSON` only, `FromJSON` deferred — **in v1**
- A bundled, versioned East-Asian-width/grapheme-cluster table
  (`Tadka.Internal.Width`) instead of an external ICU dependency —
  **in v1**
- Golden tests for exact output, Hedgehog properties for invariants
  (including label-count invariance under degradation and cycle-marker
  correctness), across all three handlers — **in v1**

---

# What `tadka` Does Not Do

- Parse source code, or infer what's wrong with it.
- Manage IO, file-watching, or project-wide diagnostic aggregation.
- Use linear types. `NamedSource` is an immutable, pointer-shared
  snapshot, not a handle; nothing here owns a resource needing "consumed
  exactly once," and rendering the same diagnostic through all three
  handlers is a feature, not a bug to prevent. Revisited only if a future
  version streams source from a handle rather than holding it as `Text`.
- Decode JSON back into Haskell values in v1 — additive future work, not
  a v1 commitment.
- Support a custom or third-party fourth render `Target`. `Target` and
  `Output` are closed by design; this is a permanent scope boundary, not
  deferred work — a project needing a render format other than
  graphical/narratable/JSON should consume the JSON DTO and build its own
  renderer on top, rather than extending `tadka` itself.
- Guarantee a stale span still shows its source line. A `LabelStale`
  label is guaranteed to *appear*, in order, with an explicit reason — it
  is not guaranteed to recover the text that's no longer there to
  recover.
- Ship a dedicated conversion helper for `trifecta` or any parser library
  beyond `megaparsec`, `attoparsec`, and GHC `SrcSpan` in v1. Usable
  manually via `Span`'s public constructors regardless — this is a scope
  boundary on maintained interop helpers, not on which libraries can work
  with `tadka`.

---

# Project Scope

- Core types (`Diagnostic`, `Context`, `Span`/`ResolvedSpan`,
  `NamedSource`, `DiagnosticId`, smart constructors): ~340 LOC
- Graphical report handler (multi-span, gutter alignment, Unicode width,
  label-palette cycling, related nesting + truncation + cycle marker):
  ~850–1050 LOC
- Narratable report handler: ~220–320 LOC
- JSON report handler + DTO + truncation/cycle/stale shapes: ~240–340 LOC
- `deriveDiagnostic` TH splice + `DiagnosticSpec` + `Q`-time validation: ~420–620 LOC
- Generics-based label-wiring derivation: ~150–200 LOC
- `Renderer`/`Config`/`selectRenderer`/`reportDiagnostic`: ~230–330 LOC
- `Tadka.Internal.Width` bundled table + generation script: ~150–250 LOC
  (excludes the generated data file itself)
- Interop helpers (`megaparsec`, `attoparsec`, GHC `SrcSpan`): ~150–250 LOC
- Test suite: golden fixtures + Hedgehog properties: ~700–950 LOC

**Total library code: approximately 3,450–4,310 LOC.**

---

# Long-Term Vision

Everything below is out of v1 scope: unscheduled, unsized, and not to be
started, stubbed, or planned for under the v1 implementation spec. This
is a different kind of boundary than "What `tadka` Does Not Do" above —
those are permanent exclusions; these are things `tadka` *might* do
later, contingent on v1 shipping first and a concrete consumer or need
materializing for each one individually.

- a companion Hackage-wide style guide encouraging `Diagnostic` instances
  for public error types
- editor plugin reference implementations consuming the JSON DTO
- a curated error-code documentation site convention, mirroring `rustc`'s
  `--explain`
- a reviewed `FromJSON` decode path, once a concrete consumer needs one

---

# Success Criterion

> "I derived `Diagnostic` for my error type in five lines of a record
> update and one splice. My users get beautiful, accessible,
> machine-readable error reports. I never wrote a line of rendering
> code, and the compiler stopped me — at compile time, not in a bug
> report — the one time I misspelled the field my span lived in. And the
> one time a span genuinely went stale between construction and render,
> my users saw a clear reason why, in the report itself, instead of a
> diagnostic that just quietly had one fewer label than it should have."

---

# Motto

> **Point at the problem. Say it clearly. Make it beautiful.**
