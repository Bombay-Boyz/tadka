# tadka

`tadka` is a small library for writing error types in Haskell that render
themselves as nice diagnostic reports — the kind of thing you get from
`rustc`: a pointer at the exact bit of source that's wrong, an error code,
a line of help text, maybe a link to more detail.

It's heavily inspired by, and tries to follow the shape of, the Rust crate
[`miette`](https://github.com/zkat/miette) by Kat Marchán and its
contributors. If you've used `miette` and liked it, `tadka` is an attempt to
bring that same experience to Haskell — same idea (a `Diagnostic` typeclass,
labeled spans, pluggable renderers), rebuilt from scratch for this language
and its conventions. Credit for the design belongs to them; any rough edges
in this port are ours.

This is a young library. It's tested reasonably thoroughly (golden fixtures,
property tests, compile-fail tests for the derive macro), but it hasn't had
much real-world mileage yet. Treat it accordingly — read the code before you
depend on it for anything important, and expect the internal modules (see
below) to move around.

## What it does

- A `Diagnostic` typeclass your error type implements. Only a message is
  required; everything else (source labels, error code, severity, help text,
  a doc link, related errors, an underlying cause) is optional and defaulted.
- Three renderers: a graphical one (the `┌─ file:line:col` / underline style
  you'd expect), a narratable one (plain prose, meant for screen readers),
  and a JSON one (for tools that want to consume diagnostics rather than
  read them).
- A `deriveDiagnostic` Template Haskell macro so you don't have to hand-write
  the typeclass instance for the common case — you describe which record
  field is the source, which fields are spans, and it generates the rest.
- Support for a diagnostic that points into more than one file at once (an
  import that disagrees with a definition elsewhere, for example).
- Small helpers to convert positions from `megaparsec`, `attoparsec`, and
  GHC's own `SrcSpan` into `tadka`'s span type, so you don't have to do that
  by hand if you're already using one of those.

## Installing

Not on Hackage yet. For now, point at it as a source or git dependency in
your `cabal.project`. It needs GHC 9.6 or newer.

## A quick example

Here's a small, complete program: one error type, derived instead of
hand-written, rendered three different ways.

```haskell
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Tadka

-- An error type. `uvSource` holds the file it happened in, `uvAt` is the
-- span of the bad identifier, `uvName` is just extra data for the message.
data UnboundVariable = UnboundVariable
  { uvSource :: NamedSource
  , uvName   :: Text
  , uvAt     :: Span
  }

-- No hand-written Diagnostic instance: this describes the shape, and
-- deriveDiagnostic generates the instance from it.
deriveDiagnostic
  defaultSpec
    { specCode        = Just "demo::E0001"
    , specHelp        = Just "check for typos, or import the module that defines it"
    , specUrl         = Just "https://example.org/errors/E0001"
    , specSourceField = Just 'uvSource
    , specLabelFields = [ ('uvAt, "not found in this scope") ]
    }
  ''UnboundVariable

main :: IO ()
main = do
  let code = "main = print (foo + 1)\n"
  --                        ^^^ offset 14, length 3

  Right source <- pure (mkNamedSource "Main.hs" code)
  Right at     <- pure (mkSpan 14 3)

  let err = UnboundVariable source "foo" at

  putStrLn "-- graphical (what you'd see in a terminal) --"
  reportDiagnostic defaultConfig err

  putStrLn "\n-- narratable (prose, for a screen reader) --"
  reportDiagnostic (withTarget TNarratable defaultConfig) err

  putStrLn "\n-- JSON (for a tool, not a person) --"
  reportDiagnostic (withTarget TJson defaultConfig) err
```

Run it (`cabal run` or `runghc`, once `tadka` is a dependency) and you should
see something close to:

```
error: [demo::E0001] undefined variable
┌─ Main.hs:1:15
│
1 │ main = print (foo + 1)
│               ^^^ not in scope
│
= help: check for typos, or import the module that defines it
= see: https://example.org/errors/E0001
```

followed by the same information again as a couple of plain-English
sentences, and then again as a JSON object with `"code"`, `"labels"`,
`"help"`, and so on. Same underlying data, three shapes — pick whichever
one fits the surface you're writing for (terminal, accessibility tooling,
or machine consumer of the diagnostic).

That's most of the day-to-day API surface. Beyond this, `mkContextMulti` lets
a diagnostic label more than one file at once, and `related` /
`diagnosticCause` on the typeclass let you attach other diagnostics as a
tree or a linear "caused by" chain — both of which the derive macro also
supports, if you'd rather not write the instance by hand.

## Testing this repo

```sh
cabal build all
cabal test all
```

or, if you have `make`:

```sh
make build
make test
```

## A note on where things stand

The public, supported entry point is the `Tadka` module. Everything under
`Tadka.Internal.*` is exposed (the derive macro and hand-written instances
need to share real functions, not just an interface), but it isn't part of
the stable API and can change without warning. If you're only importing
`Tadka`, you're on solid ground; if you reach into `Tadka.Internal`, you're
opting into some churn.
 
## License

Tadka is licensed under the Mozilla Public License 2.0 (MPL-2.0).

Commercial and open-source use are both permitted. If you distribute
modifications to MPL-covered files, those modifications must remain under
MPL-2.0. See the LICENSE file for details.

## Thanks

Again — `tadka` exists because [`miette`](https://github.com/zkat/miette)
showed what a good diagnostic-reporting library could look like. Thank you
to Kat Marchán and everyone who's worked on it.
