# Tadka tutorial

This tutorial builds up from one hand-written diagnostic to derived families of
errors, several files, causes, and your own source representation. Every Haskell
block here is a verbatim excerpt of a file that Tadka's test suites compile, and
every block of program output is checked against what its example really prints
(see [The documentation is tested](README.md#the-documentation-is-tested)). The
complete, runnable versions are in `test/docs/Examples` and `test/hello`.

## 1. Installing

Add `tadka` to `build-depends`. The examples also use `prettyprinter` (messages
are its `Doc` values) and `text`:

```cabal
build-depends:
    base
  , prettyprinter
  , tadka
  , text
```

The examples use `OverloadedStrings`, and the library is built with
`default-language: GHC2021`. Tadka is tested with GHC 9.6.7, 9.8.4, 9.10.3,
9.12.4, and 9.14.1.

Optional packages, each separate so the core stays small:

- `tadka:interop-megaparsec`, `tadka:interop-attoparsec`, and
  `tadka:interop-ghc`: convert a parser's position into a Tadka `Span`.
- `tadka-highlight`: syntax highlighting. It is GPL-2.0-only; see
  [section 14](#14-syntax-highlighting).

## 2. The idea

A *diagnostic* is any type with an instance of the `Diagnostic` class. The class
says how to present a value; it does not change how you create or propagate it.

| Method | Meaning | Default |
|---|---|---|
| `message` | The headline, a `Doc Ann`. | required |
| `context` | Where the error is: sources and labelled spans. | no source |
| `code` | A stable code such as `demo::E0001`. | none |
| `severity` | `SevError`, `SevWarning`, or `SevAdvice`. | `SevError` |
| `help` | A suggestion. | none |
| `url` | A documentation link. | none |
| `related` | Further diagnostics, as `SomeDiagnostic`. | none |
| `diagnosticId` | An identity, used to stop repeated diagnostics. | none |
| `diagnosticCause` | One underlying diagnostic. | none |

You write the instance by hand (section 3), derive it (section 4), or skip the
type altogether with an ad hoc diagnostic (section 13).

## 3. A first diagnostic

Start with a type holding what went wrong and where. The location is a source
(`NamedSource`: some text and a name) and a `Span` (an offset and a length, both
counted in Unicode code points):

```haskell
data UnboundVariable = UnboundVariable
  { uvName   :: Text
  , uvSource :: NamedSource
  , uvAt     :: Span
  }
```

Build the diagnostic code once. A code is a lowercase namespace, `::`, the
letter `E`, and at least four digits; `mkDiagnosticCode` returns a `Left` for
anything else. Then write the instance: the message, the code, a help line, and a
`context` with one primary label:

```haskell
unboundCode :: DiagnosticCode
unboundCode = either (error . show) id (mkDiagnosticCode "demo::E0001")

instance Diagnostic UnboundVariable where
  message e = pretty ("unbound variable " <> uvName e)
  code _    = Just unboundCode
  help _    = Just (pretty ("check the spelling, or import the name" :: Text))
  context e = mkContextDegrading (uvSource e)
                (Labeled (uvAt e) Primary (Just (pretty ("not in scope" :: Text))) :| [])
```

`mkContextDegrading` always gives a `Context`. If a span does not fit its source,
that label is marked stale and the rest of the report survives. The `mkContext`
variant returns an `Either` instead, for callers who prefer to reject bad spans.

Make a value. The span is the eight characters of `quantity`, starting at
offset 20:

```haskell
-- | @quantity@ is the eight characters starting at offset 20.
demo :: UnboundVariable
demo = UnboundVariable "quantity" source at
  where
    source = either (error . show) id (mkNamedSource "demo.hs" "let total = price * quantity\n")
    at     = either (error . show) id (mkSpan 20 8)
```

Print it:

```haskell
main :: IO ()
main = reportDiagnostic defaultConfig demo
```

<!-- output: quickstart -->
```text
error[demo::E0001]: unbound variable quantity
  ┌─ demo.hs:1:21
  │
1 │ let total = price * quantity
  │                     ^^^^^^^^ not in scope
  │
  = help: check the spelling, or import the name
```

The complete program, with its imports, is in the
[README](README.md#quick-start).

## 4. Deriving the instance

When the diagnostic follows the shape of the type, a Template Haskell splice
writes the instance. Describe the fields with a record update on `defaultSpec`:

```haskell
data TypeMismatch = TypeMismatch
  { tmSource   :: NamedSource
  , tmAt       :: Span
  , tmExpected :: Text
  , tmActual   :: Text
  }
  deriving (Show)

deriveDiagnostic defaultSpec
  { specCode        = Just "demo::E0002"
  , specHelp        = Just "convert one side explicitly"
  , specSourceField = Just 'tmSource
  , specLabelFields = [('tmAt, "type mismatch here")]
  , specMessage     = Just [| \e -> pretty ("expected " <> tmExpected e <> ", found " <> tmActual e) |]
  }
  ''TypeMismatch
```

or with the flatter `diagnosticSpec` list:

```haskell
data ArityError = ArityError
  { aeSource :: NamedSource
  , aeCall   :: Span
  , aeWanted :: Int
  , aeGiven  :: Int
  }
  deriving (Show)

deriveDiagnostic
  (diagnosticSpec
     [ diagCode "demo::E0003"
     , diagHelp "remove the extra arguments"
     , diagSource 'aeSource
     , diagLabel 'aeCall "called here"
     , diagMessage [| \e -> pretty ("expected " <> T.pack (show (aeWanted e))
                                    <> " arguments, got " <> T.pack (show (aeGiven e))) |]
     ])
  ''ArityError
```

Both build the same `DiagnosticSpec`; use whichever reads better. These are its
fields:

| Field | Type | Meaning |
|---|---|---|
| `specCode` | `Maybe Text` | Code, validated when the splice runs. |
| `specSeverity` | `Severity` | Default `SevError`. |
| `specHelp` | `Maybe Text` | Help line. |
| `specUrl` | `Maybe Text` | Documentation URL, validated when the splice runs. |
| `specSourceField` | `Maybe Name` | A `NamedSource` or `SomeSource` field. |
| `specLabelFields` | `[(Name, Text)]` | `Span` fields shown as primary labels, with their text. |
| `specSecondaryLabelFields` | `[(Name, Text)]` | The same, as secondary labels. |
| `specLabelCollectionFields` | `[(Name, Text)]` | `[Span]` fields; every element is a primary label with the same text. |
| `specSecondaryLabelCollectionFields` | `[(Name, Text)]` | The same, as secondary labels. |
| `specLabelCollectionFieldsWith` | `[(Name, Q Exp)]` | An `e -> [(Span, Text)]` expression, so each label has its own text. |
| `specSecondaryLabelCollectionFieldsWith` | `[(Name, Q Exp)]` | The same, as secondary labels. |
| `specRelated` | `Maybe Name` | A `[SomeDiagnostic]` field. |
| `specCause` | `Maybe Name` | A `Maybe SomeDiagnostic` field. |
| `specId` | `Maybe Name` | A `Text` or `DiagnosticId` field. |
| `specMessage` | `Maybe (Q Exp)` | An `e -> Doc Ann` expression; without it the message is `pretty . show`. |

The names in `diagnosticSpec` are `diagCode`, `diagSeverity`, `diagHelp`,
`diagUrl`, `diagSource`, `diagLabel`, `diagSecondaryLabel`,
`diagLabelCollection`, `diagSecondaryLabelCollection`,
`diagLabelCollectionWith`, `diagSecondaryLabelCollectionWith`, `diagRelated`,
`diagCause`, `diagId`, and `diagMessage`.

Names are checked against the record when the splice runs. A field that does not
exist, a field of the wrong type, a malformed code, labels without a source, or
two source fields are compile-time errors with a message that names the
offending setting.

<!-- output: derive -->
```text
error[demo::E0002]: expected Int, found Text
  ┌─ demo.hs:1:17
  │
1 │ total = count + "1"
  │                 ^^^ type mismatch here
  │
  = help: convert one side explicitly

error[demo::E0003]: expected 2 arguments, got 3
  ┌─ demo.hs:1:1
  │
1 │ area 3 4 5
  │ ^^^^^^^^^^ called here
  │
  = help: remove the extra arguments
```

## 5. Output formats

One diagnostic, three renderings, chosen with `withTarget`:

```haskell
-- | Choose the output format with 'withTarget'.
narratableConfig, jsonConfig :: Config
narratableConfig = withTarget TNarratable defaultConfig
jsonConfig       = withTarget TJson defaultConfig
```

`TGraphical`, the default, is the report in section 3. `TNarratable` is prose for
screen readers and logs:

<!-- output: format-narratable -->
```text
Error, code demo::E0001: unbound variable quantity.
Location: demo.hs, line 1, column 21.
Source line 1: "let total = price * quantity".
The problem is at columns 21 through 28, labeled: not in scope.
Help: check the spelling, or import the name
```

`TJson` is for tools:

<!-- output: format-json -->
```text
{"causes":[],"code":"demo::E0001","cycleOmitted":false,"help":"check the spelling, or import the name","labels":[{"column":21,"file":"demo.hs","length":8,"line":1,"primary":true,"stale":false,"text":"not in scope"}],"message":"unbound variable quantity","related":[],"schemaVersion":1,"severity":"error","truncated":false,"url":null}
```

`schemaVersion` changes only when the shape of the JSON changes in a way a
consumer must handle; it is independent of the package version.

## 6. Configuring

Start from `defaultConfig` and apply `with...` functions:

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

| Setting | Function | Default |
|---|---|---|
| Colour | `withColorMode` | `ColorAuto` |
| Unicode box drawing | `withUnicodeMode` | `UnicodeAuto` |
| Clickable links | `withHyperlinkMode` | `HyperlinkNever` |
| Related and cause depth | `withRelatedDepthLimit` | 8 |
| Tab width | `withTabWidth` | 4 |
| Context lines | `withContextLines` | unset: the lines from the first to the last label |
| Label colours | `withLabelPalette` | built-in |
| Output format | `withTarget` | graphical |
| Cause detail | `withCauseDetail` | `CauseSummary` |
| Highlighting | `withHighlighter` | none |

Without `withContextLines`, a snippet shows every line from the first labelled
line to the last (a range of more than 100 lines falls back to two lines of
context). With `withContextLines n` it shows `n` lines of context around each
labelled line and elides the gaps between them.

`reportDiagnostic` prints to standard output; `reportDiagnosticTo` prints to any
`Handle`; `renderDiagnosticTo` returns the report as `Text`. Colour and Unicode
are detected for the handle that is written to, so a report sent to `stderr`
adapts to `stderr`. A handle whose encoding cannot represent a character gets
`?` in its place, never an exception.

## 7. Primary and secondary labels

A primary label marks the offending span; a secondary label points at related
context. Derived:

```haskell
data ShadowedBinding = ShadowedBinding
  { sbSource :: NamedSource
  , sbNewAt  :: Span
  , sbOldAt  :: Span
  , sbName   :: Text
  }

deriveDiagnostic defaultSpec
  { specMessage =
      Just [| \e -> "`" <> pretty (sbName e) <> "` shadows an earlier binding" |]
  , specSourceField          = Just 'sbSource
  , specLabelFields          = [('sbNewAt, "new binding here")]
  , specSecondaryLabelFields = [('sbOldAt, "previous binding was here")]
  }
  ''ShadowedBinding
```

or by hand, with `Labeled` and `LabelKind`:

```haskell
data Clash = Clash NamedSource Span Span

instance Diagnostic Clash where
  message _ = "conflicting definitions"
  context (Clash src first second) =
    mkContextDegrading src
      (  Labeled second Primary (Just "redefined here")
      :| [Labeled first Secondary (Just "first defined here")])
```

The derived version prints:

<!-- output: labels-derive -->
```text
error: `x` shadows an earlier binding
  ┌─ demo.hs:2:5
  │
1 │ let x = 1
  │     - previous binding was here
2 │ let x = 2
  │     ^ new binding here
```

and the hand-written one:

<!-- output: labels-manual -->
```text
error: conflicting definitions
  ┌─ demo.hs:2:1
  │
1 │ x = 1
  │ - first defined here
2 │ x = 2
  │ ^ redefined here
```

## 8. A variable number of labels

When the number of spans is known only at run time, give the derive macro a
`[Span]` field. Every element becomes a label:

```haskell
data MultipleDefinitions = MultipleDefinitions
  { mdSource :: NamedSource
  , mdName   :: Text
  , mdLatest :: Span
  , mdPrior  :: [Span]
  }

deriveDiagnostic defaultSpec
  { specMessage =
      Just [| \e -> "`" <> pretty (mdName e) <> "` is defined more than once" |]
  , specSourceField                    = Just 'mdSource
  , specLabelFields                    = [('mdLatest, "latest definition")]
  , specSecondaryLabelCollectionFields = [('mdPrior, "also defined here")]
  }
  ''MultipleDefinitions
```

With two prior definitions, the report is:

<!-- output: labels-collection -->
```text
error: `x` is defined more than once
  ┌─ demo.hs:3:1
  │
1 │ x = 1
  │ - also defined here
2 │ x = 2
  │ ~ also defined here
3 │ x = 3
  │ ^ latest definition
```

An empty list contributes no labels. To give each element its own text, use
`specSecondaryLabelCollectionFieldsWith` (or the primary variant) with an
`e -> [(Span, Text)]` expression.

## 9. Causes and related diagnostics

A *cause* is the one underlying diagnostic that led to this one. *Related*
diagnostics are further context. Both are `SomeDiagnostic`, so they can be of
different types. Here an import error has a cause with a source of its own:

```haskell
data ImportError = ImportError
  { ieSource :: NamedSource
  , ieAt     :: Span
  , ieCause  :: Maybe SomeDiagnostic
  }

deriveDiagnostic defaultSpec
  { specMessage     = Just [| \_ -> "failed to resolve import" |]
  , specSourceField = Just 'ieSource
  , specLabelFields = [('ieAt, "this import")]
  , specCause       = Just 'ieCause
  }
  ''ImportError
```

```haskell
-- | The underlying problem, which has a source of its own.
data ConfigError = ConfigError
  { ceSource :: NamedSource
  , ceAt     :: Span
  }

deriveDiagnostic defaultSpec
  { specMessage     = Just [| \_ -> "unknown module path" |]
  , specSourceField = Just 'ceSource
  , specLabelFields = [('ceAt, "no such module")]
  }
  ''ConfigError
```

With the default configuration the cause is one line:

<!-- output: cause-summary -->
```text
error: failed to resolve import
  ┌─ Main.hs:1:8
  │
1 │ import Data.Mapp
  │        ^^^^^^^^^ this import
  │
  = caused by: unknown module path
```

A diagnostic can also collect others as related diagnostics:

```haskell
newtype BuildFailed = BuildFailed [SomeDiagnostic]

instance Diagnostic BuildFailed where
  message (BuildFailed ds) =
    pretty ("build failed with " <> T.pack (show (length ds)) <> " errors")
  related (BuildFailed ds) = ds
```

Here the related diagnostics are shown one after another; the first one's cause
is a one-line summary, as for any related diagnostic:

<!-- output: related -->
```text
error: build failed with 2 errors
  = related: failed to resolve import
    ┌─ Main.hs:1:8
    │
  1 │ import Data.Mapp
    │        ^^^^^^^^^ this import
    │
    = caused by: unknown module path
  = related: disk full
```

To show each cause of the reported diagnostic as a full nested report, with its
own snippet, help, and related diagnostics:

```haskell
-- | The default shows each cause as one line; 'CauseFull' nests its report.
fullCauses :: Config
fullCauses = withCauseDetail CauseFull plainConfig
```

The cause now brings its own source and label:

<!-- output: cause-full -->
```text
error: failed to resolve import
  ┌─ Main.hs:1:8
  │
1 │ import Data.Mapp
  │        ^^^^^^^^^ this import
  │
  = caused by: unknown module path
    ┌─ modules.cfg:1:11
    │
  1 │ unknown = Data.Mapp
    │           ^^^^^^^^^ no such module
```

Rendering is bounded however the diagnostics are connected. Cause chains and
related trees stop at the related-depth limit, and a `diagnosticId` stops a
diagnostic from being rendered twice. Only the reported diagnostic's own causes
are expanded by `CauseFull`; the causes of related diagnostics are always
one-line summaries, so even a cyclic structure produces a small report.

## 10. Several files

One diagnostic can label spans in more than one source. `mkContextMulti` takes
a non-empty list of sources, each with its labels:

```haskell
data CrossFileError = CrossFileError
  { cfeSourceA :: NamedSource
  , cfeSourceB :: NamedSource
  , cfeAtA     :: Span
  , cfeAtB     :: Span
  }

instance Diagnostic CrossFileError where
  message _ = "these declarations disagree"

  context e =
    either (const NoContext) id $
      mkContextMulti
        ( (cfeSourceA e, Labeled (cfeAtA e) Primary (Just "defined here") :| [])
          :| [ (cfeSourceB e, Labeled (cfeAtB e) Primary (Just "used here") :| []) ]
```

`mkContextMulti` returns an `Either`; the example falls back to no context if a
span does not fit. `mkContextMultiDegrading` always returns a `Context` and marks
a misfit label stale. The derive macros and `genericContext` work with a single
source.

<!-- output: multi-file -->
```text
error: these declarations disagree
  ┌─ A.hs:1:1
  │
1 │ value = 42
  │ ^^^^^ defined here
  │
  ┌─ B.hs:1:7
  │
1 │ use = value + 1
  │       ^^^^^ used here
```

## 11. A family of errors from one sum type

`deriveDiagnosticSum` derives the instance for a sum type, giving each
constructor its own `DiagnosticSpec`. Constructor-specific record fields trigger
GHC's partial-field warnings, so a module doing this switches them off:

```haskell
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
#if __GLASGOW_HASKELL__ >= 910
{-# OPTIONS_GHC -Wno-incomplete-record-selectors #-}
#endif
```

```haskell
data CompileError
  = ParseFailure
      { crSource :: NamedSource
      , crAt     :: Span
      }
  | TypeMismatch
      { tmSource   :: NamedSource
      , tmAt       :: Span
      , tmExpected :: String
      }

deriveDiagnosticSum
  [ ( 'ParseFailure
    , defaultSpec
        { specMessage     = Just [| \_ -> "unexpected token" |]
        , specSourceField = Just 'crSource
        , specLabelFields = [('crAt, "here")]
        }
    )
  , ( 'TypeMismatch
    , defaultSpec
        { specSeverity    = SevWarning
        , specMessage     = Just [| \e -> "type mismatch, expected " <> pretty (tmExpected e) |]
        , specSourceField = Just 'tmSource
        , specLabelFields = [('tmAt, "here")]
        }
    )
  ]
  ''CompileError
```

The two constructors print:

<!-- output: family-error -->
```text
error: unexpected token
  ┌─ Main.hs:1:5
  │
1 │ x = foo
  │     ^^^ here
```

<!-- output: family-warning -->
```text
warning: type mismatch, expected Int
  ┌─ Main.hs:1:5
  │
1 │ y = 'c'
  │     ^^^ here
```

The second one is a warning because its `DiagnosticSpec` sets `specSeverity` to
`SevWarning`.

## 12. Your own source representation

Spans point into a `SourceCode`. The built-in `NamedSource` is a strict `Text`
and a name, but any type with an instance works, for example an editor buffer
that is never one big `Text`. The instance answers five questions: the name, the
length in code points, the number of lines, a window of lines, and the line and
column of an offset:

```haskell
-- | A named buffer: its lines, without line terminators.
data Buffer = Buffer
  { bufName  :: Text
  , bufLines :: [Text]
  }

-- A source always has at least one line, even when empty.
linesOf :: Buffer -> [Text]
linesOf b = if null (bufLines b) then [T.empty] else bufLines b

instance SourceCode Buffer where
  scName = bufName

  -- Length in code points, counting one character per line break.
  scLength b = T.length (T.intercalate "\n" (linesOf b))

  scLineCount b = length (linesOf b)

  -- A window of lines, 1-based and inclusive, clipped to what exists.
  scLines b (lo, hi) =
    [ (n, l) | (n, l) <- zip [1 ..] (linesOf b), n >= max 1 lo, n <= hi ]

  -- Line and column (both 1-based) of a code-point offset.
  scLocate b offset = go 1 offset (linesOf b)
    where
      go n o (l : rest)
        | o <= T.length l || null rest = LineCol n (o + 1)
        | otherwise                    = go (n + 1) (o - T.length l - 1) rest
      go n o []                        = LineCol n (o + 1)
```

Pass the value wherever a source is expected:

```haskell
-- | A diagnostic whose source is the buffer.
data NameClash = NameClash Buffer Span

instance Diagnostic NameClash where
  message _ = pretty ("name defined twice" :: Text)
  context (NameClash buf at) =
    mkContextDegrading buf (Labeled at Primary (Just (pretty ("second definition" :: Text))) :| [])
```

<!-- output: custom-source -->
```text
error: name defined twice
  ┌─ scratch buffer:3:1
  │
3 │ x = 3
  │ ^ second definition
```

A diagnostic stores its sources as `SomeSource`, which lets one context mix
source types; wrap a value with `someSource`. The derive macros accept a field
of type `NamedSource` or `SomeSource`.

## 13. Diagnostics without a type of their own

For an error that does not deserve its own type, build a diagnostic from text.
`adHoc` makes one from `Text`, `fromError` from any `Exception`, `wrapErr` makes
one the cause of another, and `withCode`, `withSeverity`, `withHelp`, `withUrl`,
and `withSourceCode` fill in the rest:

```haskell
loadFailed :: AdHocDiagnostic
loadFailed =
    withHelp "check that the file exists and is readable"
  . withCode (either (error . show) id (mkDiagnosticCode "demo::E0004"))
  $ wrapErr "could not load the configuration" (fromError (userError "config.toml: not found"))
```

Any diagnostic can also be thrown and caught like an exception:

```haskell
-- | Throw it, catch it as a 'SomeDiagnostic', and print the report.
report :: IO ()
report =
  catchDiagnostic (throwDiagnostic loadFailed)
    (\(SomeDiagnostic e) -> reportDiagnostic defaultConfig e)
```

<!-- output: ad-hoc -->
```text
error[demo::E0004]: could not load the configuration
  = help: check that the file exists and is readable
  = caused by: user error (config.toml: not found)
```

`installPanicHook` makes an uncaught diagnostic exception print as a full report
on standard error. Note that `intoDiagnostic` uses `show`, so a `String` comes
out quoted; prefer `fromError` for exceptions and `adHoc` for text.

## 14. Syntax highlighting

Tadka has no highlighter of its own. A `Highlighter` takes a source's name and
its lines and returns the same lines with colour added:

```haskell
-- | Receives the source's name and all its lines; returns one coloured line
-- per input line, or 'Nothing' to leave the source plain.
keywords :: Highlighter
keywords = Highlighter
  { highlighterName = "keywords"
  , runHighlighter  = \_ ls -> Just (map (T.replace "let" "\ESC[36mlet\ESC[0m") ls)
  }

-- | Colour must be on for a highlighter to run.
coloured :: Config
coloured = withHighlighter keywords (withColorMode ColorAlways (withUnicodeMode UnicodeAlways defaultConfig))
```

A highlighter can only add colour. Tadka keeps a line only if removing its
colour codes gives back exactly the original line, so a highlighter cannot change
text, move a caret, or emit another terminal escape sequence. Highlighting does
nothing under `ColorNever`.

For real syntax highlighting use the separate `tadka-highlight` package:

```haskell
config :: Config
config = withHighlighter (skylightingAuto pygments) defaultConfig
```

**`tadka-highlight` is GPL-2.0-only**, because the `skylighting` library it
depends on is GPL-2. `tadka` is MIT and has no dependency on either.

## 15. Parsers and compilers

The optional sub-libraries convert a parser's position into a `Span`:
`Tadka.Interop.Megaparsec.spanFromError`, `Tadka.Interop.Attoparsec.spanFromConsumed`
(with `consumedOffset`), and `Tadka.Interop.GHC.spanFromSrcSpan` (with
`offsetFromLineCol`). They do not build a diagnostic; you write the instance and
use the span in its `context`.

## Where next

The [README](README.md) has the overview and the configuration table. The
`test/docs/Examples` directory holds the complete source of every example in
this tutorial.
