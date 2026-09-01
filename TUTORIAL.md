# tadka: a usage tutorial

This is a step-by-step guide to using `tadka`, aimed at someone who has never
touched the library before. If you just want the five-minute version, the
[README](README.md) has a shorter one. This document goes deeper: it walks
through every piece of the public API in `Tadka`, in the order you're likely
to need them.

If you already know [`miette`](https://github.com/zkat/miette) from Rust,
skim the headings — the shapes will feel familiar.

## Table of contents

1. [Installing](#1-installing)
2. [The core idea](#2-the-core-idea)
3. [Your first diagnostic, by hand](#3-your-first-diagnostic-by-hand)
4. [The same thing, derived](#4-the-same-thing-derived)
5. [Rendering: the three targets](#5-rendering-the-three-targets)
6. [Configuring the output](#6-configuring-the-output)
7. [Multiple labels in one diagnostic](#7-multiple-labels-in-one-diagnostic)
8. [A variable number of labels](#8-a-variable-number-of-labels)
9. [Related diagnostics and cause chains](#9-related-diagnostics-and-cause-chains)
10. [Diagnostics that span more than one file](#10-diagnostics-that-span-more-than-one-file)
11. [Deriving a whole family of errors at once](#11-deriving-a-whole-family-of-errors-at-once)
12. [Hooking up a parser (megaparsec / attoparsec / GHC)](#12-hooking-up-a-parser-megaparsec--attoparsec--ghc)
13. [Where to look next](#13-where-to-look-next)

---

## 1. Installing

Add `tadka` to your `.cabal` file's `build-depends`, same as any other
package:

```cabal
build-depends: base, tadka
```

`tadka` needs GHC 9.6 or newer. If you're using the parser-interop helpers
(section 12), you'll also depend on `tadka:interop-megaparsec`,
`tadka:interop-attoparsec`, or `tadka:interop-ghc` — each is its own
sub-library, so you only pull in a parsing package if you actually use it.

The only module you should import from application code is `Tadka`.
Everything under `Tadka.Internal.*` is visible but explicitly *not* part of
the stable API — see the note at the end of the README.

## 2. The core idea

`tadka` is built around one typeclass:

```haskell
class Diagnostic e where
  diagnosticMessage  :: e -> Doc Ann        -- the only required method
  diagnosticCode      :: e -> Maybe DiagnosticCode
  diagnosticSeverity   :: e -> Severity
  diagnosticHelp      :: e -> Maybe Text
  diagnosticUrl        :: e -> Maybe Url
  diagnosticContext    :: e -> Context
  diagnosticRelated    :: e -> [SomeDiagnostic]
  diagnosticCause      :: e -> Maybe SomeDiagnostic
  diagnosticId         :: e -> Maybe DiagnosticId
  -- (all but diagnosticMessage have sensible defaults)
```

If your error type implements this, `tadka` can turn a value of that type
into a graphical report, a screen-reader-friendly sentence, or a JSON
object — you write the instance (or let the derive macro write it for you)
once, and get all three renderers for free.

The two ideas worth understanding before anything else:

- **`Context`** is *where* the error is: a source file's text, plus zero or
  more labeled spans pointing into it (`^^^ here`, `~~~ also relevant`, ...).
  You build one with `mkContext` (or let the derive macro build it from your
  record fields).
- **`Config`** is *how* to render: which of the three targets, color on or
  off, Unicode box-drawing or ASCII, how many lines of surrounding source to
  show, and so on. You start from `defaultConfig` and tweak it with `with*`
  functions.

## 3. Your first diagnostic, by hand

Before reaching for the derive macro, it's worth writing one instance
manually once, so the generated code doesn't feel like magic.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (Text)
import Prettyprinter (pretty)
import Tadka

data UnboundVariable = UnboundVariable
  { uvSource :: NamedSource
  , uvName   :: Text
  , uvAt     :: Span
  }

instance Diagnostic UnboundVariable where
  diagnosticMessage e = "undefined variable `" <> pretty (uvName e) <> "`"
  diagnosticCode    _ = either (const Nothing) Just (mkDiagnosticCode "demo::E0001")
  diagnosticHelp    _ = Just "check for typos, or import the module that defines it"
  diagnosticContext e =
    either (const NoContext) id $ do
      ctx <- mkContext (uvSource e) [(uvAt e, Primary, "not found in this scope")]
      pure ctx
```

Two things worth noticing:

- `diagnosticMessage` is the *only* method with no default. Everything else
  falls back to something reasonable (`Nothing`, `SevError`, `NoContext`,
  `[]`) if you don't override it.
- `mkContext`, `mkDiagnosticCode`, and friends are *validating* smart
  constructors — they return `Either SomeError a`, not a partial function.
  Bad input (an out-of-bounds span, a malformed error code) is a value you
  handle, not a runtime crash.

## 4. The same thing, derived

Hand-writing that instance for every error constructor gets old fast. The
`deriveDiagnostic` Template Haskell macro generates it from a `DiagnosticSpec`
that just says *which field is which*:

```haskell
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

deriveDiagnostic
  defaultSpec
    { specCode        = Just "demo::E0001"
    , specMessage     = Just [| \e -> "undefined variable `" <> pretty (uvName e) <> "`" |]
    , specHelp        = Just "check for typos, or import the module that defines it"
    , specUrl         = Just "https://example.org/errors/E0001"
    , specSourceField = Just 'uvSource
    , specLabelFields = [ ('uvAt, "not found in this scope") ]
    }
  ''UnboundVariable
```

This produces exactly the instance from section 3, plus the code and URL.
`defaultSpec` starts every field empty/`Nothing`/`SevError`; you only set
what your type actually has. The full field list on `DiagnosticSpec`:

| Field | What it takes | What it wires up |
|---|---|---|
| `specCode` | `Text` literal | `diagnosticCode` (validated at compile time via `mkDiagnosticCode`) |
| `specSeverity` | `Severity` | `diagnosticSeverity` (defaults to `SevError`) |
| `specHelp` | `Text` literal | `diagnosticHelp` |
| `specUrl` | `Text` literal | `diagnosticUrl` (validated via `mkUrl`) |
| `specSourceField` | field `Name`, must be `NamedSource`-typed | which field holds the source text |
| `specLabelFields` | `[(Name, Text)]`, fields must be `Span`-typed | primary labels + their captions |
| `specSecondaryLabelFields` | same shape | secondary (dimmer/less prominent) labels |
| `specLabelCollectionFields` | `[(Name, Text)]`, fields must be `[Span]`-typed | one primary label *per element* — see section 8 |
| `specSecondaryLabelCollectionFields` | same, secondary | see section 8 |
| `specRelated` | field `Name`, must be `[SomeDiagnostic]`-typed | `diagnosticRelated` — see section 9 |
| `specCause` | field `Name`, must be `Maybe SomeDiagnostic`-typed | `diagnosticCause` — see section 9 |
| `specId` | field `Name`, `Text`- or `DiagnosticId`-typed | `diagnosticId` (used to break cycles in related/cause chains) |
| `specMessage` | `Q Exp` of type `e -> Doc Ann` | `diagnosticMessage`; if omitted, falls back to `pretty . show` (needs `deriving Show`) |

Every field name you pass in (`'uvSource`, `'uvAt`, ...) is checked against
your actual record at splice time — get the type wrong (say, a `Text` field
where a `Span` is expected) and it's a compile error, not a runtime one.

## 5. Rendering: the three targets

Once you have a `Diagnostic` instance, rendering is one function:

```haskell
reportDiagnostic :: Diagnostic e => Config -> e -> IO ()
```

`Config`'s `Target` field picks the shape:

```haskell
reportDiagnostic defaultConfig err
-- error[demo::E0001]: undefined variable `foo`
--   ┌─ Main.hs:1:15
--   │
-- 1 │ main = print (foo + 1)
--   │               ^^^ not found in this scope
--   │
--   = help: check for typos, or import the module that defines it

reportDiagnostic (withTarget TNarratable defaultConfig) err
-- Plain prose, meant for a screen reader: "Error demo::E0001: undefined
-- variable foo. In Main.hs, line 1, column 15: not found in this scope. ..."

reportDiagnostic (withTarget TJson defaultConfig) err
-- {"code":"demo::E0001","severity":"error","message":"undefined variable `foo`", ...}
```

If you want the rendered text rather than printing it straight to stdout,
`render` and `selectRenderer` give you that lower-level access — that's what
the JSON case usually wants, since you'll typically be sending the bytes
somewhere rather than printing them.

## 6. Configuring the output

`Config` is built by starting at `defaultConfig` and layering `with*`
functions, each returning a new `Config`:

```haskell
myConfig :: Config
myConfig =
    withColorMode ColorNever
  $ withUnicodeMode UnicodeAlways
  $ withHyperlinkMode HyperlinkAlways
  $ withContextLines 3
  $ withTabWidth 4
  $ withRelatedDepthLimit 5
  $ withTarget TGraphical
  $ defaultConfig
```

What each knob does:

- `withColorMode` — `ColorAlways` / `ColorNever` / `ColorAuto` (detects a
  terminal).
- `withUnicodeMode` — box-drawing characters (`┌─`, `│`) vs. plain ASCII
  fallback, for terminals/fonts that don't have them.
- `withHyperlinkMode` — whether `specUrl` renders as a clickable terminal
  hyperlink (OSC 8) where the terminal supports it.
- `withContextLines` — how many lines of source before/after a label to show.
- `withTabWidth` — how a literal tab in source code counts for column math.
- `withLabelPalette` — customize the colors used to distinguish multiple
  labels in the same snippet.
- `withRelatedDepthLimit` — caps how deep a `related`/`cause` chain renders
  before truncating (see section 9) — protects against runaway or cyclic
  chains.
- `withTarget` — `TGraphical` / `TNarratable` / `TJson`.

## 7. Multiple labels in one diagnostic

A single error often needs to point at more than one span — "defined here"
*and* "used here", say. Just list more than one entry in `specLabelFields`,
or mix primary and secondary:

```haskell
data ShadowedBinding = ShadowedBinding
  { sbSrc      :: NamedSource
  , sbNewAt    :: Span
  , sbOldAt    :: Span
  , sbName     :: Text
  }

deriveDiagnostic
  defaultSpec
    { specMessage             = Just [| \e -> "`" <> pretty (sbName e) <> "` shadows an earlier binding" |]
    , specSourceField         = Just 'sbSrc
    , specLabelFields         = [ ('sbNewAt, "new binding here") ]
    , specSecondaryLabelFields = [ ('sbOldAt, "previous binding was here") ]
    }
  ''ShadowedBinding
```

Primary labels get the brighter/underline treatment; secondary labels are
visually de-emphasized but still point at real source.

## 8. A variable number of labels

Sometimes you don't know how many spans there'll be until runtime — every
prior shadowed declaration of a name, every match of a banned pattern. A
fixed `specLabelFields` entry can't express that (it's one name, one span,
decided at compile time), so there's a separate field for it:
`specLabelCollectionFields` (and its secondary counterpart), which takes a
field of type `[Span]` and expands it to one label per element at build
time:

```haskell
data MultipleShadows = MultipleShadows
  { msSrc       :: NamedSource
  , msName      :: Text
  , msPriorAt   :: [Span]   -- however many prior bindings there were
  }

deriveDiagnostic
  defaultSpec
    { specMessage                        = Just [| \e -> "`" <> pretty (msName e) <> "` shadowed multiple times" |]
    , specSourceField                    = Just 'msSrc
    , specSecondaryLabelCollectionFields = [ ('msPriorAt, "shadowed here") ]
    }
  ''MultipleShadows
```

Every element of `msPriorAt` becomes its own secondary label with the same
caption. You can freely mix fixed fields and collection fields on the same
type; labels are ordered fixed-fields-first, then collection elements in
list order.

## 9. Related diagnostics and cause chains

Two different relationships between errors are supported, matching
`miette`'s vocabulary:

- **`related`** — a *list* of other diagnostics that are relevant context,
  but siblings, not a chain ("also see these 2 other places this pattern
  breaks").
- **`cause`** — a single, *optional* diagnostic that's the underlying reason
  this one happened, forming a linear "caused by" chain, the way exceptions
  often wrap each other.

Both take a `SomeDiagnostic` — an existential wrapper so a field can hold
*any* `Diagnostic`-implementing type, not just its own:

```haskell
data ImportError = ImportError { ieSrc :: NamedSource, ieAt :: Span, ieCause :: Maybe SomeDiagnostic }

deriveDiagnostic
  defaultSpec
    { specMessage     = Just [| \_ -> "failed to resolve import" |]
    , specSourceField = Just 'ieSrc
    , specLabelFields = [ ('ieAt, "this import") ]
    , specCause       = Just 'ieCause
    }
  ''ImportError

-- wrap any other Diagnostic value with `SomeDiagnostic` to attach it:
someImportErr { ieCause = Just (SomeDiagnostic underlyingParseError) }
```

Chains can legitimately point back at themselves indirectly (a group of
diagnostics that reference each other). `tadka` handles this safely: set
`specId` on your type so diagnostics carry an identity, and a chain that
loops back to an id it's already rendered stops there instead of looping
forever; an id-less cycle is still bounded by `withRelatedDepthLimit`. You
don't need to do anything extra for this — it's just worth knowing the
depth limit exists and is there for exactly this reason.

## 10. Diagnostics that span more than one file

If an error involves two files at once (an import that disagrees with a
definition elsewhere, say), a single `NamedSource` isn't enough. Use
`mkContextMulti` instead of `mkContext` — it takes a list of
`(NamedSource, [(Span, LabelKind, Text)])` groups instead of one source and
one label list:

```haskell
diagnosticContext e =
  either (const NoContext) id $
    mkContextMulti
      [ (fileASource e, [(fileASpan e, Primary, "imported here")])
      , (fileBSource e, [(fileBSpan e, Primary, "but not exported here")])
      ]
```

There's also `mkContextMultiDegrading`, the total counterpart: instead of
returning `Left` on the first out-of-bounds span, it degrades that one
label to "stale" in place and keeps the rest, which is usually what you want
for a diagnostic renderer that must never itself crash. (`mkContextDegrading`
is the equivalent single-source version.)

The derive macro (`deriveDiagnostic`) is single-source only — if you need
multi-file labels, write that one instance by hand as above; everything else
about the type (severity, help, related, cause) can still come from a normal
`Diagnostic` instance sitting alongside it.

## 11. Deriving a whole family of errors at once

A compiler or parser usually has one big sum type of possible errors, not
one type per error. `deriveDiagnosticSum` derives all constructors' instances
in a single splice, pairing each constructor with its own `DiagnosticSpec`:

```haskell
data CompileError
  = ParseFailure { crSrc :: NamedSource, crAt :: Span, peId :: DiagnosticId }
  | TypeMismatch { crSrc :: NamedSource, crAt :: Span, tmExpected :: Text, tmActual :: Text }
  | UndefinedVar { crSrc :: NamedSource, crAt :: Span, uvName :: Text, uvRelated :: [SomeDiagnostic] }

deriveDiagnosticSum
  [ ( 'ParseFailure
    , defaultSpec { specCode = Just "tadka::E0101", specSourceField = Just 'crSrc
                  , specLabelFields = [('crAt, "here")], specId = Just 'peId
                  , specMessage = Just [| \_ -> "unexpected token" |] }
    )
  , ( 'TypeMismatch
    , defaultSpec { specSeverity = SevWarning, specSourceField = Just 'crSrc
                  , specLabelFields = [('crAt, "here")]
                  , specMessage = Just [| \e -> "type mismatch: expected " <> pretty (tmExpected e) |] }
    )
  , ( 'UndefinedVar
    , defaultSpec { specSourceField = Just 'crSrc, specLabelFields = [('crAt, "used here")]
                  , specRelated = Just 'uvRelated
                  , specMessage = Just [| \e -> "undefined variable " <> pretty (uvName e) |] }
    )
  ]
  ''CompileError
```

A couple of things fall out of using a real sum type this way:

- Shared fields across constructors (`crSrc`, `crAt` here) should be named
  and typed identically if you want GHC's ordinary record-selector totality
  checks to stay happy for *those* fields — the fields that genuinely differ
  per constructor (`peId`, `tmExpected`, `uvName`, ...) are inherently
  partial selectors, which is just what a heterogeneous sum type is. If that
  triggers `-Wincomplete-record-selectors` under `-Wall -Werror` for you,
  that's expected for this shape — silence it locally with
  `{-# OPTIONS_GHC -Wno-incomplete-record-selectors #-}` (alongside
  `-Wno-partial-fields`, which covers the older, related warning) at the top
  of the module defining the sum type, rather than upstream in `tadka`.
- Every constructor named in the list must actually exist on the type, and
  every constructor on the type must appear in the list — `deriveDiagnosticSum`
  checks both directions at splice time, so adding a new constructor to
  `CompileError` without giving it a spec is a compile error, not a silent
  gap in your error handling.

## 12. Hooking up a parser (megaparsec / attoparsec / GHC)

If you're already tracking positions with one of these, you don't need to
convert them to `tadka` spans by hand — each interop sub-library does it:

```cabal
build-depends: base, tadka, tadka:interop-megaparsec  -- or interop-attoparsec / interop-ghc
```

```haskell
import Tadka.Interop.Megaparsec (megaparsecErrorSpan)   -- illustrative; check the module for exact names
```

- `Tadka.Interop.Megaparsec` — converts megaparsec's position/error types.
- `Tadka.Interop.Attoparsec` — converts attoparsec's.
- `Tadka.Interop.GHC` — converts GHC's own `SrcSpan`/`RealSrcSpan` (from
  `GHC.Types.SrcLoc`), useful if you're writing a GHC plugin or working with
  GHC's API directly.

These are one-directional adapters (parser type → `tadka` `Span`); the core
`tadka` library itself has no dependency on any parsing package, so you only
pay for the interop you actually import.

## 13. Where to look next

- The [README](README.md) has the condensed version of section 3–5 as one
  runnable program, if you want something to copy-paste and run immediately.
- `Tadka`'s own Haddock documentation (generated from `src/Tadka.hs`) is the
  authoritative field-by-field reference once you're past this tutorial.
- The `test/props/` directory in this repo is, in effect, a very large set of
  worked examples — every feature above has a corresponding property test
  showing it in use (`LabelCollection.hs` for section 8, `Cause.hs` for
  section 9, `Phase12.hs` for section 10, `Phase13.hs` for section 11, and so
  on).
