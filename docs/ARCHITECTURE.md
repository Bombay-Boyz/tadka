# tadka — how it's put together

This replaces the old phase-by-phase build plan, which was written before
any of this existed. The code is written now; this describes what's
actually there, for anyone reading the source for the first time.

## Layout

```
Tadka                          public surface — the only module you import
Tadka.Internal                 shared functions the derive macro and
                                hand-written instances both call
Tadka.Internal.Types           validated primitives: Offset, Length,
                                NamedSource, DiagnosticCode, Url, Severity,
                                DiagnosticId
Tadka.Internal.Span            Span / ResolvedSpan, bounds-checked resolution
Tadka.Internal.Context         Context, SourceGroup, label state, buildContext
Tadka.Internal.Diagnostic      the Diagnostic typeclass, SomeDiagnostic
Tadka.Internal.Related         the related-diagnostic walk, cycle detection
Tadka.Internal.Config          Config, Target, the render-time options
Tadka.Internal.Render          selectRenderer, render, reportDiagnostic
Tadka.Internal.Renderer.*      Graphical, Narratable, Json — one module each
Tadka.Internal.Renderer.Layout lane assignment for overlapping labels
Tadka.Internal.Renderer.LinePlan   merging/eliding source-line windows
Tadka.Internal.Width           the bundled Unicode width table + lookup
Tadka.Internal.TH              deriveDiagnostic, deriveDiagnosticSum
Tadka.Internal.Generics        the smaller genericContext derivation
Tadka.Internal.Terminal        locale/UTF-8 and color-capability detection
interop/megaparsec, /attoparsec, /ghc     separate sub-libraries, each a
                                one-directional adapter to Span
```

Everything under `Tadka.Internal.*` carries no compatibility guarantee —
that's the point of the split. `Tadka` is the only module meant to be
depended on directly.

One leftover worth knowing about: `Tadka.Internal.Interop` is an empty
namespace module from an early plan to put the parser adapters under
`Tadka.Internal`. That's not where they ended up — they're the three
separate sub-libraries listed above instead, so the core library never
depends on a parser package. The empty module should probably just be
deleted; it's dead weight, not a placeholder for anything upcoming.

## The two guarantees, and where they live

**"Nothing constructs into an invalid value"** lives entirely in
`Tadka.Internal.Types` and `Tadka.Internal.Span`. Every raw constructor is
unexported; the only way to get a value is through a smart constructor that's
already checked the invariant. Nothing downstream re-checks.

**"Nothing silently disappears"** lives in `Tadka.Internal.Context`.
`mkContext`/`mkContextMulti` reject a bad span outright; `mkContextDegrading`/
`mkContextMultiDegrading` instead turn a span that fails resolution into a
`LabelStale` marker, in its original position, rather than dropping it. A
`Context` can hold labels across more than one source file — `mkContext` is
now a one-group special case of `mkContextMulti`, not a second copy of the
same logic, which is the one place in the codebase that actually matches its
own stated discipline throughout (see the note on `TH.hs` below, which
doesn't quite).

## The derive macro

`Tadka.Internal.TH` generates a `Diagnostic` instance from a `DiagnosticSpec`
record — field names, validated at splice time via `reify`, so a misspelled
field or a type mismatch fails the build rather than the test suite. Every
generated method body is meant to be a direct call to a plain function
`Tadka.Internal` already exports; the derive path and a hand-written
instance are supposed to be two doors into the same room.

In practice, the single-constructor path (`*Method`, producing a `Q Dec`)
and the sum-type path (`*MethodArm`, producing a `Q Exp`, for
`deriveDiagnosticSum`) are two separate implementations of the same
branching logic, kept in sync by hand rather than by one calling the other.
A byte-for-byte golden test currently proves they agree on the shapes that
test exercises. It's the one spot where the module's own stated
design goal isn't quite what the code does — worth collapsing into one
shared helper at some point, not urgent.

## Rendering

`Config` is built only through `defaultConfig` plus `with*` setters — the
record constructor isn't exported, so `selectRenderer` is the only function
that ever turns a `Config` into a working `Renderer`. Each renderer module
(`Graphical`, `Narratable`, `Json`) is otherwise independent and doesn't
import from its siblings.

The graphical renderer is the largest module by a wide margin (~470 lines).
Column math for carets and gutters routes through `Tadka.Internal.Width` so
combining marks and East-Asian-width characters don't throw the alignment
off; `Renderer.Layout` handles lane assignment when labels overlap, and
`Renderer.LinePlan` decides which source lines to show and where to elide a
gap. `HyperlinkMode` wraps the `see:` URL line in an OSC 8 escape when the
terminal supports it, independent of color.

## Testing

Two suites, run independently:

- **Golden**: 20 fixed fixtures, byte-for-byte, covering single/multi-label
  output, degraded labels, cycle-omitted chains, and the narratable/JSON
  equivalents.
- **Property** (Hedgehog): 24 files covering span resolution bounds, label-
  count invariance under degradation, cycle-marker correctness, renderer
  totality (nothing throws), gutter/caret math never going negative, and
  smart-constructor rejection.

Both are wired into CI, along with a compile-fail suite (`tools/check-
compile-fail.sh`) that confirms specific malformed `deriveDiagnostic` uses
fail at the splice site with the expected message, and a discipline check
(`tools/check-generated.sh`) that greps generated golden output for anything
that isn't a single function application — the automated half of the "provably
thin" claim above, at least for the fixture shapes the golden suite exercises.

Build is checked against GHC 9.6 through 9.14.

## What's not covered by any of this

No `FromJSON` — the JSON renderer is output only. No fourth `Target`. No
`trifecta` adapter, though `Span` can be built manually against any
position type. These are stated boundaries, not gaps waiting to be filled.
