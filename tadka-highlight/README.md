# tadka-highlight

Syntax highlighting for [Tadka](https://github.com/Bombay-Boyz/tadka)
diagnostics, using the [`skylighting`](https://hackage.haskell.org/package/skylighting)
library.

## Licence: read this first

`skylighting` is distributed under the **GPL (version 2)**. So this package is
**GPL-2.0-only**, and a program that links it is subject to the GPL.

The `tadka` package itself is **MIT**-licensed and has no dependency on
`skylighting` or on this package. If you want Tadka's reports without any GPL
code in your program, depend on `tadka` alone: highlighting is optional, and
snippets are shown as plain text without it.

## Use

Add `tadka-highlight` next to `tadka` in `build-depends`, then give the
configuration a highlighter. Import `pygments` from `Skylighting`, `skylightingAuto`
from `Tadka.Highlight.Skylighting`, and everything else from `Tadka`:

```haskell
config :: Config
config = withHighlighter (skylightingAuto pygments) defaultConfig
```

The syntax is chosen from the source's name, so `Main.hs` is read as Haskell.
`skylightingLanguage` forces one syntax instead, and `skylightingWith` takes
any function from a source name to a syntax.

Highlighting needs colour: it does nothing under `ColorNever`. A highlighter can
only add colour. Tadka keeps a highlighted line only if removing the colour
codes gives back exactly the original line, so a highlighter cannot change the
text, move a caret, or emit any other terminal escape sequence.

This example is built and run by the package's `smoke` test-suite
(`cabal test smoke`), so it cannot go stale.
