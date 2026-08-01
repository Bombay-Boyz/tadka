# Tadka Phase 12 — Multi-Source Diagnostics

Status: implemented, tested, not yet released. Targets the next version after
`1.0.0.0` (bump required — see §5).

## 1. Motivation

Every `Context` before this phase held labels resolved against exactly one
`NamedSource`. A diagnostic whose story genuinely spans two files — "this
import in module A disagrees with the definition in module B" — had no way to
show both locations in one report; the only source anchor available was
whichever single file the author's `Context` happened to hold.

This is a real capability gap, not a cosmetic one, and it is the one place
this library's design was behind comparable Haskell diagnostic libraries
(`Error.Diagnose` supports reports spanning multiple files). Closing it is
additive: every existing single-source guarantee is preserved exactly, and a
single-source `Context` is now the one-group special case of the
generalised representation, not a different code path.

## 2. Goals

- A `Context` can hold labels anchored across more than one source, in a
  caller-specified order.
- Every guarantee a single-source `Context` already had — never drop a label,
  never reorder labels, degrade a bad span to `LabelStale` in place rather
  than discarding it — holds identically *per group*, plus a matching
  guarantee on group order itself (groups are never dropped, reordered, or
  merged).
- All three renderers (graphical, narratable, JSON) present a multi-source
  `Context` coherently: a real file/location per label, not just "the
  labels," so a machine consumer (JSON) or a screen-reader user (narratable)
  can tell which file each label belongs to as easily as a terminal user
  reading the graphical gutters can.
- Zero behavioural change for every existing single-source caller. Golden
  fixtures prove this rather than asserting it: every pre-Phase-12 graphical
  and narratable fixture is required to render byte-identical, unchanged.

## 3. Explicit non-goals (no scope creep)

These were considered and deliberately deferred, not overlooked:

- **Message-type polymorphism** (the `Ann` vocabulary staying closed vs. an
  open, caller-supplied message type, as in `Error.Diagnose`). This is a
  separate, philosophically distinct decision — tightening vs. loosening the
  "no renderer ever guesses how to interpret content" guarantee — and is
  intentionally out of scope for this phase. If pursued at all, it is a v2,
  opt-in escape hatch (an explicit `AnnCustom` case supplying all three
  renderings up front), not a retroactive change to v1's closed vocabulary.
  See the project's Phase-12-adjacent design discussion for the full
  reasoning; nothing in this phase touches `Ann`.
- **Derive-macro / generics multi-source support.** `deriveDiagnostic` and
  `genericContext` remain single-source only (§6). `buildContextMulti` is a
  hand-written-instance entry point in this phase.
- **Cross-file connector semantics in the graphical renderer** (e.g., a
  visual line connecting a label in file A to a label in file B). Each
  group renders its own self-contained gutter block; blocks are not visually
  linked to each other beyond sequential ordering and a separator rail. This
  is a legitimate future enhancement, not a requirement this phase carries.

## 4. Design

### 4.1 `Context` representation

```haskell
data SourceGroup = SourceGroup
  { sgSource :: NamedSource
  , sgLabels :: NonEmpty (Labeled LabelState)
  }

data Context
  = NoContext
  | HasLabels (NonEmpty SourceGroup)
```

Previously `HasLabels` carried one `NamedSource` and one label list directly.
`Context`'s public export (`Tadka.Context (NoContext)`) has always hidden the
`HasLabels` constructor from the public API — only `NoContext` is
pattern-matchable outside `Tadka.Internal.*` — so this representation change
is invisible to any caller who only used the public smart constructors and
`contextLabelStates`. It only affects the three renderer modules and
`Tadka.Internal.Context` itself, all of which carry the "no compatibility
guarantee" label already.

`contextLabelStates` flattens across every group, in group order, exactly as
before for the one-group case. A new `contextSourceGroups` is the renderers'
shared way to walk a context group-by-group.

### 4.2 Construction API

```haskell
mkContextMulti         :: NonEmpty (NamedSource, NonEmpty (Labeled Span)) -> Either ContextError Context
mkContextMultiDegrading :: NonEmpty (NamedSource, NonEmpty (Labeled Span)) -> Context
buildContextMulti       :: NonEmpty (NamedSource, [(Span, LabelKind, Maybe (Doc Ann))]) -> Context
```

`mkContext`, `mkContextDegrading`, `buildContext`, and `buildContextWith` are
now implemented as the one-group special case:

```haskell
mkContext src labels = mkContextMulti ((src, labels) :| [])
mkContextDegrading src labels = mkContextMultiDegrading ((src, labels) :| [])
```

`buildContext`/`buildContextWith` needed no code changes at all — they only
ever called `mkContextDegrading`, whose new implementation is transparent to
them. This mirrors the project's existing discipline (`buildContext` as "a
thin wrapper, not a second copy of the degrading logic") one level deeper:
the single-source functions are now thin wrappers around the multi-source
ones, not parallel implementations that could drift apart.

`buildContextMulti` mirrors `buildContext`'s empty-list convention per
source: a source paired with an empty entry list contributes no group at
all (a source nobody labels shouldn't appear in the rendered output); the
result is `NoContext` only if every group is empty.

`ContextError` is unchanged (`newtype ContextError = ContextError SpanError`)
— it still names only the first span that failed to resolve, now in
group-then-label order, matching how the pre-Phase-12 single-source version
named only the first failing span with no per-label index either.

### 4.3 Renderers

**Graphical** (`Tadka.Internal.Renderer.Graphical`): the pre-existing
single-group snippet logic (location line, gutter, source lines, carets,
connector lanes for multi-line spans) is untouched internally — extracted
into `groupSnippetLines`, called once per source group, with the group
blocks joined by the same lone-rail-line separator convention `relatedChild`
already uses between a nested diagnostic's own snippet and its related
forest. `gutterWidth` now takes the maximum line-number width needed across
every group (not just one), so the shared indentation used by help/see/
related lines stays visually aligned regardless of how many files are
involved. A one-group `Context` produces exactly the single block and exact
byte output it always did — proven by golden fixture, not merely asserted.

**Narratable** (`Tadka.Internal.Renderer.Narratable`): `contextSentences` is
now `concatMap groupSentences . contextSourceGroups` — one location sentence
plus its label readouts per group, in group order. Same one-group-identical
guarantee, same proof method.

**JSON** (`Tadka.Internal.Renderer.Json`): `LabelDTO` gains a `file :: Text`
field, populated from the label's own group's source name
(`Tadka.sourceName`), present on stale labels too (a stale label still names
the file its now-unresolvable span was meant for). This is the one
renderer-level change that is *not* one-group-identical — seen next.

## 5. Breaking change: JSON DTO gains `file`

Adding `file` to `LabelDTO` changes the shape of every JSON diagnostic that
has at least one label — independent of whether that diagnostic is
single-source or multi-source. This is intentional and necessary: the
pre-Phase-12 JSON output never named which file a label belonged to at all,
which was already a real gap for a *single*-file diagnostic (a JSON consumer
had no way to know the file without threading it through some other
channel). Closing the multi-source gap made this omission impossible to
leave in place, since a multi-file report absolutely needs it — so it is
fixed for every diagnostic, not just multi-file ones.

Affected golden fixtures: `json-single`, `json-cycle`, `json-truncated`
(each gains `"file": "<name>"` on its one label, key-ordered right after
`"labels"` opens, before `"line"`, matching `LabelDTO`'s own field order).
`json-cause` is unaffected — its diagnostic (`CauseNode`) never sets a
`context`, so it has no labels to begin with.

**Action required before release:** bump `tadka`'s version past `1.0.0.0`
(this is a DTO-breaking change for any consumer parsing the JSON target) —
see `tadka.cabal`'s `version` field and the release-readiness discussion this
phase followed from.

## 6. Deferred: derive-macro and generics multi-source support

`Tadka.Internal.TH`'s `deriveDiagnostic` and `Tadka.Internal.Generics`'
`genericContext` are unchanged by this phase and remain single-source only:

- `deriveDiagnostic`'s `DiagnosticSpec` still validates exactly one
  `NamedSource`-typed field (`specSourceField`) at splice time.
- `genericContext` still type-errors at compile time unless the record has
  exactly one `NamedSource` field (`CheckSource (CountField NamedSource (Rep e))`).

Neither module pattern-matches `HasLabels`/`SourceGroup` directly — both only
ever call `buildContext`/`buildContextWith`, whose signatures and behaviour
are unchanged — so this phase required zero edits to either module. Extending
either to multi-source is a real, separate design question (what spec syntax
names which label maps to which of several source fields?) left for a future
phase once `buildContextMulti`'s hand-written usage has had a chance to
surface what that mapping should actually look like in practice, rather than
speculatively designing it now.

## 7. Testing

**Golden** (`test/golden/`): a new `crossFile` fixture (two sources —
`ModuleA.hs` importing a name, `ModuleB.hs` defining it with a conflicting
type — one label each, `Secondary` then `Primary`) is rendered through all
three handlers as `cross-file`, `narr-cross-file`, `json-cross-file`. Every
pre-existing fixture is re-verified unchanged except the three JSON ones
`file` intentionally affects (§5) — this is checked by running the full
existing golden suite against the new code before adding any new fixture,
confirming the diff is exactly and only the JSON schema change, then adding
the new fixture afterward.

**Property** (`test/props/Phase12.hs`, 6 properties):

1. `mkContextMulti` is `Left` iff any span, in any group, is out of bounds.
2. `mkContextMultiDegrading` never changes any group's label count or order
   (count preserved per group and in total; `LabelStale` appears in exactly
   the positions whose span fails to resolve, attributed to the correct
   group).
3. `mkContext`/`mkContextDegrading` produce exactly the same
   `contextLabelStates` as the one-group case of their multi- counterparts
   (both the success and failure shape, for the strict constructor).
4. `buildContextMulti` with every group's entry list empty produces
   `NoContext`.
5. `buildContextMulti` dispatches to `mkContextMultiDegrading` (same
   resulting label states for the same input, reshaped).
6. `buildContextMulti` drops an empty group without affecting the labels
   contributed by the other groups.

All 6 pass at 100 generated cases each (20 for the all-empty case, since its
generator space is small). The full pre-existing property suite (every
group from Phase 1 through the edge-case/security suites) was re-run after
this phase's changes and passes with zero regressions.

## 8. Migration notes (for whenever this ships)

- Any JSON consumer that parses `LabelDTO`-shaped objects and rejects unknown
  keys will need updating for the new `file` key.
- Any code that pattern-matched `Tadka.Internal.Context.HasLabels` directly
  (there is no compatibility guarantee for this — only `Tadka.hs`'s
  `Context (NoContext)` export is public) needs updating for the new shape;
  use `contextSourceGroups`/`contextLabelStates` instead of matching the
  constructor.
- Everyone else — anyone using only the public `Tadka` module's smart
  constructors (`mkContext`, `buildContext` via a derived instance, etc.) —
  needs no changes at all.
