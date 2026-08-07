# tadka

*Point at the problem. Say it clearly.*

## What this is

`tadka` is a small Haskell library for turning error values into readable
diagnostic reports — the kind of thing `rustc` or `miette` produce: a
message, a pointer at the exact bit of source that's wrong, maybe a code,
maybe some help text, maybe a chain of related errors underneath.

It was built after noticing that most parser and DSL projects in Haskell
either print bare line/column numbers or hand-roll their own formatting,
inconsistently, project to project. `miette` had already solved this well
in Rust. `tadka` borrows its shape — it does not claim to match it feature
for feature, and it isn't trying to be a general pretty-printing framework.
It does one thing: take an error, produce a report, in one of three formats.

## Why it's shaped the way it is

Two habits run through the whole codebase:

1. **Make bad values impossible to construct.** Offsets can't be negative.
   A named source can't have an empty name. A diagnostic code has to match
   a fixed grammar. None of this is checked at render time — it's checked
   once, at the point of construction, so nothing downstream has to worry
   about it again.

2. **Never silently drop something.** If a span in a diagnostic no longer
   lines up with the source text — because the text changed underneath it,
   say — the label doesn't just vanish from the output. It renders as a
   labeled "stale" marker, in its original position, with a stated reason.
   Two diagnostics with the same logical content always produce the same
   *shape* of report. If something's missing, the report says so instead of
   quietly being shorter.

Everything else in the design follows from taking those two points
seriously rather than treating them as nice-to-haves.

## What's actually here

- A `Diagnostic` typeclass. `message` is the only required method; `context`,
  `code`, `severity`, `help`, `url`, `related`, `diagnosticId`, and
  `diagnosticCause` all have sensible defaults, so a minimal instance is one
  line.
- Three renderers: graphical (the `rustc`-style boxed output with gutters and
  underlines), narratable (screen-reader-friendly prose), and JSON (for tools,
  not people).
- A `Context` type that resolves source spans once and remembers whether each
  one is still valid — including labels spread across *more than one* source
  file, not just one.
- A `deriveDiagnostic` splice, so most error types need a record of fields and
  one line of Template Haskell rather than a hand-written instance. A second
  splice, `deriveDiagnosticSum`, does the same for sum types with several
  constructors.
- A smaller GHC-generics path that derives just the `context` method for the
  common one-source-many-spans shape, when you don't need the full macro.
- Related-diagnostic chains, with an opt-in identity check so a genuinely
  cyclic chain (two errors that reference each other) terminates with a
  visible marker instead of hitting a raw depth limit.
- Its own small, bundled Unicode width table, so caret alignment stays correct
  across combining marks, East-Asian-width characters, and emoji, without
  pulling in a system ICU dependency.
- Conversion helpers for `megaparsec`, `attoparsec`, and GHC's `SrcSpan`, each
  as an optional sub-library — using `tadka` doesn't require any of the
  parser libraries it happens to know about.

## What it doesn't do

- Parse anything, or guess what's wrong with your input. It only reports what
  you already know.
- Manage IO, file-watching, or aggregate diagnostics across a whole project.
- Decode JSON back into Haskell values. The JSON renderer is output-only;
  nothing reads it back in.
- Support a fourth output format grafted onto the existing three. If you need
  something else, the JSON output is meant to be a reasonable base to build
  your own renderer against.
- Guarantee a stale span can show you the line it used to point at. It
  guarantees the label still *appears*, with a reason — not that the text
  is recoverable.
- Ship a maintained adapter for every parser library that exists. Three are
  covered. Anything else can still construct spans manually; there's just no
  dedicated helper for it.

## Where this might actually get used

Parser error reporting, small compiler or DSL tooling, CI output that needs
to be machine-parseable as well as human-readable, and anywhere a screen
reader is in the loop and prose output matters as much as the boxed one.

## Honest state of things

This is a hobby project, built by one person, reviewed once by an outside
pass. The core rendering and resolution logic held up under that review. A
few rough edges remain — one internal file where two code paths do the same
thing independently instead of sharing a helper, some documentation that
lagged behind the code for a stretch. None of that is hidden; it's tracked
in the repo's own issue history rather than pretended away here.

## What "done" looks like

Deriving `Diagnostic` for an error type is a record update and one splice.
Misspelling the field a span lives in is a compile error, not something a
user finds later. A span that's gone stale between construction and render
shows up in the report with a reason, instead of just quietly being one
label shorter than it should be.
