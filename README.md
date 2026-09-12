# Tadka

**Tadka** is a Haskell library for structured error diagnostics and source-span reporting.

It turns ordinary error values into useful diagnostic reports with source
locations, labeled spans, error codes, help text, related diagnostics, and
underlying causes.

The same diagnostic can be rendered as graphical terminal output, accessible
prose, or JSON.

`tadka` is useful for compilers, parsers, command-line tools, static
analyzers, DSLs, and other developer tooling.

It is inspired by Rust's [`miette`](https://github.com/zkat/miette), but is
designed around Haskell's types and conventions.

## Why tadka?

**Tadka** adds a reporting layer to your existing error types. It does not
replace your application's error-propagation mechanism.

You can keep using:

```haskell
Either MyError a
```

or:

```haskell
ExceptT MyError IO a
```

or another error-handling design. Add a `Diagnostic` instance to describe how
the error should be presented.

The main features are:

- Source-aware diagnostics with labeled spans.
- Graphical, narratable, and JSON output.
- Primary and secondary labels, including collections of labels.
- Diagnostics spanning multiple source files.
- Error codes, severity, help text, and documentation URLs.
- Related diagnostics and underlying cause chains.
- Bounded rendering and cycle protection for diagnostic chains.
- Unicode-aware source positioning, including tabs, combining characters,
  East Asian width, and emoji.
- Explicit handling of stale source spans when source text has changed.
- Adapters for GHC `SrcSpan`, Megaparsec, and Attoparsec.

## What it looks like

A graphical diagnostic can look along these lines:

```text
error[E1001]: unexpected token

 --> example.td:4:9
  |
4 | let x = foo(
  |         ^^^ expected an expression
  |
  = help: check the expression following `foo`
```

The same diagnostic can be rendered as accessible prose or as JSON, depending
on the selected target.

## Installation

Add `tadka` to your package's dependencies:

```cabal
build-depends:
    tadka
```

Then import the public API:

```haskell
import Tadka
```

`tadka` is designed to work with ordinary Cabal projects and existing
`Either`, `ExceptT`, or custom error types.

## Five-minute example

Define an ordinary error type:

```haskell
data MyError
  = UnexpectedToken Span
  | MissingName Span
  deriving stock (Show)
```

Give it a diagnostic description:

```haskell
instance Diagnostic MyError where
  message = \case
    UnexpectedToken _ -> "unexpected token"
    MissingName _     -> "missing name"

  context = \case
    UnexpectedToken span ->
      mkContext source
        (Labeled span Primary (Just "expected an expression") :| [])

    MissingName span ->
      mkContext source
        (Labeled span Primary (Just "a name is required here") :| [])

  code = const (Just (mkDiagnosticCode "E1001"))
```

Then report it:

```haskell
reportDiagnostic defaultConfig err
```

The reporting layer remains separate from the way your application creates,
returns, or handles `MyError`.

## Deriving diagnostics

For error types where the diagnostic information follows a regular structure,
`tadka` provides Template Haskell support.

For example:

```haskell
data ParseError = ParseError
  { parseMessage :: Text
  , parseSpan    :: Span
  }
  deriving stock (Show)

$(deriveDiagnostic defaultSpec
    { specMessage     = [| \e -> pretty (parseMessage e) |]
    , specSourceField = Just 'parseSpan
    })
```

The derivation API can also describe error codes, help text, URLs, primary and
secondary labels, related diagnostics, causes, and diagnostic identity.

## Source spans

`tadka` keeps source text and source locations explicit.

A `Span` identifies a range in a named source. It can be resolved against the
source text to obtain line and column information:

```haskell
resolved <- resolveSpan source span
```

The implementation accounts for details that can make source positioning
non-trivial, including:

- tabs
- Unicode characters
- combining characters
- East Asian character width
- emoji
- large source offsets

When source text has changed and a span can no longer be trusted, `tadka`
represents that condition explicitly rather than silently presenting a
misleading location.

## Labeled source context

A diagnostic can contain primary and secondary labels:

```haskell
mkContext source
  (Labeled span1 Primary (Just "the problem is here")
    :| [Labeled span2 Secondary (Just "this is also relevant")])
```

Multiple source files are supported as well.

`tadka` can also degrade stale spans when source text no longer matches the
coordinates stored by an error value.

## Rendering

**Tadka** currently provides three output targets:

- `TGraphical` — terminal-oriented diagnostic output.
- `TNarratable` — accessible prose-oriented output.
- `TJson` — machine-readable JSON output.

A `Config` controls reporting behavior such as color, Unicode, hyperlinks,
context lines, tab width, related-diagnostic depth, label palettes, and the
selected target.

For example:

```haskell
let config =
      withTarget TGraphical
      $ withColorMode ColorAuto
      $ defaultConfig

reportDiagnostic config err
```

The renderer selection and lower-level rendering APIs are also available when
applications need more control.

## Existing error types

**Tadka** does not require a new application-wide error architecture.

If an application already has error types, they can remain ordinary Haskell
values. A `Diagnostic` instance describes how each error should be presented
to a human or another tool.

This separation is useful when the same error value needs to be:

- returned through `Either`
- propagated through `ExceptT`
- logged
- displayed in a terminal
- returned as JSON
- embedded in another diagnostic

## Related diagnostics and causes

Diagnostics can contain related diagnostics and an underlying cause chain.

This lets an application represent structures such as:

```text
top-level error
    |
    +-- related diagnostic
    |
    +-- underlying cause
            |
            +-- underlying cause
```

**Tadka** bounds traversal depth and supports diagnostic identity so that
recursive or cyclic structures cannot cause unbounded rendering.

## Parser and compiler integration

**Tadka** includes integration packages for common Haskell tooling:

```text
tadka:interop-ghc
tadka:interop-megaparsec
tadka:interop-attoparsec
```

These adapters allow existing parser and compiler source locations to be
translated into **Tadka** source-span information without requiring the
application to redesign its diagnostics.

## JSON output

JSON output is intended for applications that need to pass diagnostics to
other programs, scripts, editor tooling, or services.

The JSON representation should be treated as an output format of the
corresponding Tadka release. If another program depends on its exact shape,
pinning the `tadka` version is the safest approach.

## Accessibility

The narratable renderer provides a prose-oriented representation of the same
structured diagnostic.

This is useful when graphical terminal formatting, Unicode drawing
characters, or visual source annotations are not appropriate.

## Public API boundary

The primary application-facing API is:

```haskell
import Tadka
```

The package also exposes internal modules for implementation-level use, but
they are not intended to provide the same compatibility guarantees as the
public API.

Applications should prefer the public `Tadka` module unless they have a
specific reason to use an internal module.

## Testing and engineering

**Tadka** is tested across multiple supported GHC versions and includes:

- unit and property tests
- renderer tests
- golden tests
- parser/compiler interop tests
- generated-code checks
- compile-failure checks
- adversarial source-position cases
- Unicode and tab-positioning cases
- stale-source handling
- multi-source diagnostics
- related/cause-chain protection

The project is developed with warnings enabled and maintains explicit
dependency bounds.

## Current scope

**Tadka** focuses on structured diagnostic representation and rendering.

It intentionally does not attempt to be all of the following at once:

- an application-wide error-handling framework
- an LSP implementation
- an editor integration layer
- a syntax-highlighting engine
- a general-purpose logging framework
- a replacement for parser-specific error types

Those concerns can be built around the diagnostic representation when an
application needs them.

## License

**Tadka** is released under the Mozilla Public License 2.0 (MPL-2.0).

See [`LICENSE`](LICENSE) for the complete license text.

## Acknowledgement

The design is inspired in part by Rust's [`miette`](https://github.com/zkat/miette),
particularly its approach to structured diagnostics, source snippets, labels,
and human-friendly reporting.

Tadka is an independent Haskell implementation designed around Haskell's
type system, libraries, and conventions.
