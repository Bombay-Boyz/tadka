# Tadka: A Usage Tutorial

This is a step-by-step guide to using **Tadka**, aimed at someone who has never
used the library before. If you just want the five-minute version, the
[README](README.md) has a shorter introduction. This document goes deeper:
it walks through the public API in `Tadka` in the order you're likely to need it.

If you already know Rust's [`miette`](https://github.com/zkat/miette), the
overall shape will feel familiar.

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
12. [Hooking up a parser or compiler](#12-hooking-up-a-parser-or-compiler)
13. [Where to look next](#13-where-to-look-next)


> **Copy-paste rule.** Every section that says **Complete program** contains a full `Main.hs` you can copy into a small Cabal project and run. Other Haskell blocks are deliberately shorter API fragments; they are not presented as standalone programs.

### One-time project setup

For the runnable examples below, create this minimal Cabal project once:

```sh
mkdir tadka-demo
cd tadka-demo
cat > tadka-demo.cabal <<'EOF'
cabal-version: 3.0
name: tadka-demo
version: 0.1.0.0
build-type: Simple

executable tadka-demo
  main-is: Main.hs
  build-depends:
      base
    , tadka
  default-language: GHC2021
EOF
```

Then put one of the complete `Main.hs` examples below into `Main.hs` and run:

```sh
cabal run
```

The project deliberately lists only `base` and `tadka`: the examples use only Tadka's public API and do not require application code to depend directly on Tadka's implementation modules.

## 1. Installing

Add `tadka` to your `.cabal` file's `build-depends`, just like any other
package:

```cabal
build-depends:
    base,
    tadka
```

`tadka` needs GHC 9.6 or newer. If you're using the parser/compiler interop
helpers, add the corresponding sub-library:

```text
tadka:interop-megaparsec
tadka:interop-attoparsec
tadka:interop-ghc
```

Each interop package is a separate sub-library, so the core package does not
pull in a parser or compiler API unless you use the relevant integration.

The application-facing module is:

```haskell
import Tadka
```

Modules under `Tadka.Internal.*` are implementation-facing and are not part of
the stable application API.

## 2. The core idea

Tadka is built around one typeclass:

```haskell
class Diagnostic e where
  message :: e -> Doc Ann
  context :: e -> Context
  code :: e -> Maybe DiagnosticCode
  severity :: e -> Severity
  help :: e -> Maybe (Doc Ann)
  url :: e -> Maybe Url
  related :: e -> [SomeDiagnostic]
  diagnosticId :: e -> Maybe DiagnosticId
  diagnosticCause :: e -> Maybe SomeDiagnostic
```

Only `message` is required. The other methods have defaults, so a minimal
diagnostic can be very small.

Once your error type has a `Diagnostic` instance, Tadka can render the same
value as graphical terminal output, accessible prose, or JSON.

The two ideas worth understanding first are:

- **`Context`** describes *where* the error is: source text plus labeled spans.
- **`Config`** describes *how* the diagnostic is rendered: target, color,
  Unicode, hyperlinks, context lines, tab width, and other output settings.

You can therefore keep your application's existing error-propagation design
and add reporting separately.

## 3. Your first diagnostic, by hand

Before using Template Haskell, write one instance manually once. This makes the
derived version easier to understand.

**Complete program — copy this entire block into `Main.hs`:**

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (NonEmpty (..))
import Tadka

data UnboundVariable = UnboundVariable
  { uvSource :: NamedSource
  , uvAt     :: Span
  }

instance Diagnostic UnboundVariable where
  message _ = "undefined variable"

  context e =
    either (const NoContext) id $
      mkContext (uvSource e)
        (Labeled (uvAt e) Primary (Just "not found in this scope") :| [])

  code _ =
    either (const Nothing) Just (mkDiagnosticCode "demo::E0001")

  help _ =
    Just "check for typos, or import the module that defines it"

main :: IO ()
main =
  case ( mkNamedSource "Main.hs" "main = print (foo + 1)\n"
       , mkSpan 14 3
       ) of
    (Right source, Right at) ->
      reportDiagnostic defaultConfig (UnboundVariable source at)
    (Left _, _) ->
      putStrLn "could not create the source"
    (_, Left _) ->
      putStrLn "could not create the span"
```

Run it with:

```sh
cabal run
```

You should get a graphical diagnostic pointing at `foo`. The exact terminal
box-drawing characters can vary with configuration and terminal capabilities.

The important point is that the `Diagnostic` instance describes the diagnostic
without changing the underlying error type. The smart constructors such as
`mkContext` and `mkDiagnosticCode` validate their inputs and return `Either`
values.

For example:

```haskell
mkDiagnosticCode "demo::E0001"
```

returns an `Either`, allowing the application to decide what to do if the code
is invalid.

## 4. The same thing, derived

Hand-writing a `Diagnostic` instance for every error type can become repetitive.
Tadka provides Template Haskell support through `deriveDiagnostic`.

**Complete program — replace `Main.hs` with this:**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Tadka

data UnboundVariable = UnboundVariable
  { uvSource :: NamedSource
  , uvAt     :: Span
  }

$(deriveDiagnostic defaultSpec
    { specMessage     = Just [| \_ -> "undefined variable" |]
    , specCode        = Just "demo::E0001"
    , specHelp        = Just "check for typos, or import the module that defines it"
    , specSourceField = Just 'uvSource
    , specLabelFields = [('uvAt, "not found in this scope")]
    } ''UnboundVariable)

main :: IO ()
main =
  case ( mkNamedSource "Main.hs" "main = print (foo + 1)\n"
       , mkSpan 14 3
       ) of
    (Right source, Right at) ->
      reportDiagnostic defaultConfig (UnboundVariable source at)
    (Left _, _) ->
      putStrLn "could not create the source"
    (_, Left _) ->
      putStrLn "could not create the span"
```

Run it:

```sh
cabal run
```

This is the basic pattern to copy into a real application: define your error
type, describe its fields once with `DiagnosticSpec`, and let the derive macro
generate the `Diagnostic` instance.

The principal fields of `DiagnosticSpec` are:

| Field | Purpose |
|---|---|
| `specCode` | Diagnostic code |
| `specSeverity` | Diagnostic severity |
| `specMessage` | Expression producing `Doc Ann` |
| `specHelp` | Help text |
| `specUrl` | Documentation URL |
| `specSourceField` | Field containing the `NamedSource` |
| `specLabelFields` | Fixed primary labels |
| `specSecondaryLabelFields` | Fixed secondary labels |
| `specLabelCollectionFields` | Collections of primary spans |
| `specSecondaryLabelCollectionFields` | Collections of secondary spans |
| `specRelated` | Related diagnostics |
| `specCause` | Underlying diagnostic cause |
| `specId` | Diagnostic identity used for cycle protection |

Field names supplied to the derivation are checked against the actual record at
splice time. This makes many mistakes compile-time errors rather than runtime
failures.

## 5. Rendering: the three targets

Once you have a `Diagnostic` instance, the simplest reporting API is:

```haskell
reportDiagnostic :: Diagnostic e => Config -> e -> IO ()
```

For example:

```haskell
reportDiagnostic defaultConfig err
```

The three output targets are:

```haskell
TGraphical
TNarratable
TJson
```

Graphical output is intended for terminal users and can show source snippets
with labels:

```text
error[demo::E0001]: undefined variable `foo`

 --> Main.hs:1:15
  |
1 | main = print (foo + 1)
  |               ^^^ not found in this scope
  |
  = help: check for typos, or import the module that defines it
```

Narratable output presents the same information as prose, making it suitable
when graphical terminal formatting is not appropriate.

JSON output provides a machine-readable representation for programs and
services.

If you need rendered output rather than direct reporting, the lower-level
`render` and `selectRenderer` APIs are available.

## 6. Configuring the output

Start from `defaultConfig` and layer the `with*` functions you need:

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

The main configuration controls are:

- `withColorMode` — `ColorAlways`, `ColorNever`, or `ColorAuto`.
- `withUnicodeMode` — Unicode terminal drawing versus ASCII-compatible
  output.
- `withHyperlinkMode` — terminal hyperlinks for documentation URLs.
- `withContextLines` — surrounding source lines shown around labels.
- `withTabWidth` — tab width used for source positioning.
- `withLabelPalette` — palette used to distinguish labels.
- `withRelatedDepthLimit` — maximum depth rendered for related/cause chains.
- `withTarget` — `TGraphical`, `TNarratable`, or `TJson`.

## 7. Multiple labels in one diagnostic

A diagnostic often needs to point at more than one location.

For example:

```haskell
data ShadowedBinding = ShadowedBinding
  { sbSource :: NamedSource
  , sbNewAt  :: Span
  , sbOldAt  :: Span
  , sbName   :: Text
  }
  deriving stock (Show)

$(deriveDiagnostic defaultSpec
    { specMessage =
        Just [| \e -> "`" <> pretty (sbName e) <> "` shadows an earlier binding" |]
    , specSourceField =
        Just 'sbSource
    , specLabelFields =
        [('sbNewAt, "new binding here")]
    , specSecondaryLabelFields =
        [('sbOldAt, "previous binding was here")]
    } ''ShadowedBinding)
```

Primary labels receive the more prominent treatment. Secondary labels remain
visible but are visually de-emphasized.

For manually constructed contexts, use `Labeled` values:

```haskell
mkContext source
  (Labeled span1 Primary (Just "the problem is here")
    :| [Labeled span2 Secondary (Just "this is also relevant")])
```

## 8. A variable number of labels

Sometimes the number of spans is known only at runtime: for example, every
previous declaration of a name.

For this case, use `specLabelCollectionFields` or
`specSecondaryLabelCollectionFields`.

```haskell
data MultipleShadows = MultipleShadows
  { msSource :: NamedSource
  , msName   :: Text
  , msPrior  :: [Span]
  }
  deriving stock (Show)

$(deriveDiagnostic defaultSpec
    { specMessage =
        Just [| \e -> "`" <> pretty (msName e) <> "` shadowed multiple times" |]
    , specSourceField =
        Just 'msSource
    , specSecondaryLabelCollectionFields =
        [('msPrior, "shadowed here")]
    } ''MultipleShadows)
```

Each element of `msPrior` becomes a separate secondary label with the same
caption.

Fixed label fields and collection fields can be combined. Fixed fields are
emitted first, followed by collection elements in list order.

## 9. Related diagnostics and cause chains

Tadka distinguishes two relationships:

- **`related`** — a list of diagnostics that provide additional context.
- **`diagnosticCause`** — a single underlying diagnostic forming a cause chain.

Both use `SomeDiagnostic`, allowing diagnostics of different concrete types to
be connected.

For example:

```haskell
data ImportError = ImportError
  { ieSource :: NamedSource
  , ieAt     :: Span
  , ieCause  :: Maybe SomeDiagnostic
  }
  deriving stock (Show)

$(deriveDiagnostic defaultSpec
    { specMessage =
        Just [| \_ -> "failed to resolve import" |]
    , specSourceField =
        Just 'ieSource
    , specLabelFields =
        [('ieAt, "this import")]
    , specCause =
        Just 'ieCause
    } ''ImportError)
```

An underlying diagnostic can then be attached with:

```haskell
SomeDiagnostic underlyingParseError
```

Related and cause chains can contain recursive references. Tadka protects
rendering in two ways:

1. `DiagnosticId` can identify diagnostics and stop a chain when an already
   rendered identity is encountered.
2. `withRelatedDepthLimit` provides a depth bound even when diagnostics do not
   have an identity.

This prevents recursive diagnostic structures from causing unbounded rendering.

## 10. Diagnostics that span more than one file

When a diagnostic refers to several source files, use `mkContextMulti`.

**Complete program — replace `Main.hs` with this:**

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (NonEmpty (..))
import Tadka

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
        )

main :: IO ()
main =
  case ( mkNamedSource "A.hs" "value = 42\n"
       , mkNamedSource "B.hs" "use = value + 1\n"
       , mkSpan 0 5
       , mkSpan 6 5
       ) of
    (Right sourceA, Right sourceB, Right atA, Right atB) ->
      reportDiagnostic defaultConfig
        (CrossFileError sourceA sourceB atA atB)
    _ ->
      putStrLn "could not construct the example diagnostic"
```

Run it:

```sh
cabal run
```

There is also `mkContextMultiDegrading`. This version can preserve the rest of a
diagnostic when an individual span has become stale because its source text has
changed. For a single source, the corresponding function is
`mkContextDegrading`.

The `deriveDiagnostic` helper is designed around a single source field. For a
multi-file diagnostic, a normal `Diagnostic` instance gives you direct control
over the complete context.

## 11. Deriving a whole family of errors at once

A compiler or parser often has one sum type containing many error constructors.
`deriveDiagnosticSum` can derive the diagnostics for the whole family in one
splice.

**Complete program — replace `Main.hs` with this:**

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Tadka

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

$(deriveDiagnosticSum
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
          , specMessage     = Just [| \_ -> "type mismatch" |]
          , specSourceField = Just 'tmSource
          , specLabelFields = [('tmAt, "here")]
          }
      )
    ]
    ''CompileError)

main :: IO ()
main =
  case (mkNamedSource "Main.hs" "x = foo\n", mkSpan 4 3) of
    (Right source, Right at) ->
      reportDiagnostic defaultConfig (ParseFailure source at)
    _ ->
      putStrLn "could not construct the example diagnostic"
```

Run it:

```sh
cabal run
```

`deriveDiagnosticSum` checks that the constructors and specifications agree.
Adding a constructor without a corresponding specification therefore becomes a
compile-time error.

Sum types with constructor-specific record fields can also trigger GHC's
record-selector warnings under `-Wall -Werror`. Those warnings concern the
application's sum type, not Tadka's diagnostic machinery.

## 12. Hooking up a parser or compiler

If your application already uses one of the supported parser/compiler APIs,
the corresponding interop sub-library can convert its source locations into
Tadka spans.

In Cabal:

```cabal
build-depends:
    base,
    tadka,
    tadka:interop-megaparsec
```

The available integrations are:

```text
Tadka.Interop.Megaparsec
Tadka.Interop.Attoparsec
Tadka.Interop.GHC
```

They provide adapters from the corresponding source-location/error
representations into Tadka's `Span` model.

The core `tadka` package itself does not depend on these parser/compiler
packages. Use only the integration packages your application needs.

## 13. Where to look next

- The [README](README.md) is the shorter introduction and quick-start guide.
- The `Tadka` module is the primary application-facing API.
- The Haddock documentation generated from `src/Tadka.hs` is the detailed API
  reference.
- The test suite contains additional examples of labels, collections,
  related/cause chains, multi-source diagnostics, rendering, and parser/compiler
  integration.

When in doubt, prefer the public `Tadka` API over `Tadka.Internal.*`.
