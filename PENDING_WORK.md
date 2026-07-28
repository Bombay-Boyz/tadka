# tadka — Pending Work & Roadmap

> Snapshot of what is **done**, what is **pending**, and the **standards** the
> remaining work must follow. Companion to the source tree.
> Point at the problem. Say it clearly. Make it beautiful.

---

## 1. Status

- **v1.0.0.0 shipped** (all eleven implementation-spec phases).
- **Post-v1 "miette-parity hardening" — largely complete**, batched under
  `## Unreleased — miette-parity hardening` in `CHANGELOG.md`. Version string is
  still `1.0.0.0`; bump to `1.1.0.0` when the series is cut (see §5).
- Test surface now: **19 property modules (~90 properties), 16 golden fixtures**,
  an interop round-trip suite, and two discipline checks — all green under
  `-Werror`, `cabal check` clean.

### Completed in this series

| Item | Tier | Status |
|------|------|--------|
| Tab-stop expansion (caret alignment on tab-indented source) | 1 | ✅ |
| Terminal detection (`NO_COLOR`/TTY/locale) + real per-label ANSI colour | 1 | ✅ |
| Primary vs secondary labels (`^`/`-`, location anchors on primary, JSON `primary`) | 2 | ✅ |
| Cause chain (`diagnosticCause` + `walkCauses`, all three handlers) | 2 | ✅ |
| Totality pass — **zero partial functions** in the library | — | ✅ |
| **Phase I** — pluggable `SourceCode` (windowed reads) | 2 | ✅ |
| **Phase II** — context lines + gap elision (`withContextLines`, `⋮`) | 2 | ✅ |
| **Phase III** — multi-line span rendering (lane connector engine) | 1 | ✅ |
| Robustness — miette edge-case suite (CRLF, newline-in-label, etc.) | — | ✅ |

The one true capability gap versus miette — multi-line spans — is **closed**.

---

## 2. Pending work

All remaining items are Tier-3 polish or architectural depth. None blocks a
"comparable to miette on rendering, ahead on correctness" claim.

### 2.1. Same-line multi-label packing

- **Tier:** 3. **Effort:** medium. **Status:** pending.
- Multiple labels on one line currently render as **stacked caret lines** —
  correct and unambiguous, just not horizontally packed onto one line with
  routed connectors. Honest note: this did **not** fall out of the multi-line
  lane engine as first hoped; it is a distinct interval/lane problem on the
  horizontal axis. Build only if the compact look matters.

### 2.2. Lazy / file-backed `SourceCode` instance

- **Tier:** 2. **Effort:** medium. **Status:** partially done.
- The `SourceCode` **class and windowed interface exist** (Phase I) and
  `NamedSource` is the eager instance. What is *not* done is a genuinely lazy
  instance (file/mmap) that reads only the requested window, and making
  `Context` generic over the backend (it still holds `NamedSource`). `resolveSpan`
  also still consumes the whole `NamedSource` text. Do this only if targeting
  compiler/LSP/large-file users; the interface was designed to allow it additively.

### 2.3. Width-aware wrapping / reflow

- **Tier:** 3. Wrap long messages/help to terminal width with gutter-aligned
  continuations. Terminals soft-wrap anyway.

### 2.4. Syntax highlighting

- **Tier:** 3. Interleave language syntax colour with label colour. miette
  gates this behind an optional `syntect` feature; heavy deps, high effort.

### 2.5. OSC-8 terminal hyperlinks

- **Tier:** 3. Emit the code/URL as a clickable link. Plain-text URL is fine today.

### 2.6. Collection labels in the derive macro

- **Tier:** 3. One record field yielding many labels (miette's `#[label(collection)]`).
  tadka's derive wires a fixed set of named `Span` fields (primary + secondary).

---

## 3. Comparison scorecard (tadka vs miette 7.6.0)

Legend: ✅ done · 🔜 pending · ⏸️ deferred (Tier 3) · ⭐ tadka advantage

| Capability | Status |
|------------|--------|
| Diagnostic protocol (code/severity/help/url/labels/related) | ✅ |
| Three renderers (graphical / narratable / JSON) | ✅ |
| Derive macro + generics derivation path | ✅ |
| Interop: megaparsec / attoparsec / GHC `SrcSpan` | ✅ |
| Tab-stop-correct carets | ✅ |
| Terminal detection + ANSI colour | ✅ |
| Primary vs secondary labels | ✅ |
| Cause chain | ✅ |
| Context lines + gap elision | ✅ |
| **Multi-line span rendering** | ✅ |
| Pluggable `SourceCode` interface (eager instance) | ✅ (lazy backend 🔜) |
| Unicode-width-correct carets | ✅ ⭐ |
| Stale/invalid-span first-class rendering | ✅ ⭐ |
| Cycle detection by `diagnosticId` (related + causes) | ✅ ⭐ |
| Totality / no partial functions / property proofs | ✅ ⭐ |
| CRLF & newline-in-label robustness (miette #37/#318) | ✅ |
| Same-line multi-label packing | ⏸️ (2.1) |
| Lazy/file-backed source | 🔜/⏸️ (2.2) |
| Width-aware wrapping | ⏸️ (2.3) |
| Syntax highlighting | ⏸️ (2.4) |
| OSC-8 hyperlinks | ⏸️ (2.5) |
| Collection labels in derive | ⏸️ (2.6) |

---

## 4. Suggested order for any further work

Everything left is independent and optional; pick by audience:

1. **Compiler/LSP/large-file audience →** 2.2 (lazy backend) first.
2. **"Looks maximally like miette" →** 2.1 (same-line packing), then 2.4 (syntax
   highlighting) and 2.3 (wrapping).
3. **Otherwise →** ship as-is; the rest is polish.

---

## 5. Release chores

- **Version bump.** Still `1.0.0.0`. The series added public API additively
  (`withTabWidth`, `withContextLines`, `LabelKind`, `buildContextWith`,
  `diagnosticCause`, `specSecondaryLabelFields`, `Tadka.Internal.Terminal`,
  `Tadka.Internal.SourceCode`, `Tadka.Internal.Renderer.{LinePlan,Layout}`) plus
  behaviour changes (colour, primary/secondary glyphs, multi-line). Bump PVP
  minor → `1.1.0.0` and date the changelog.
- **Golden note.** The graphical suite grew (`multi-line`, `context-elision`,
  `with-cause`, `tab-indented`); regenerate deliberately with
  `BIN=$(cabal list-bin tadka:test:golden); GEN_GOLDEN=1 "$BIN"` and eyeball.

---

## 6. Non-negotiable standards for remaining work

The correctness identity is what makes tadka worth being a *second* miette. Do
**not** trade it away for polish.

- **Total functions** — no partial primitives anywhere (currently zero); bound
  every recursion, detect every cycle by `diagnosticId`.
- **Property proofs, not spot checks** — every invariant gets a Hedgehog
  property. The bar is already set: caret alignment under tab-expanded source;
  `resolveConfig` eliminates every `Auto`; colour adds only ANSI; derive output
  equals hand-written byte-for-byte; lane assignment is collision-free; every
  multi-line span opens and closes exactly once. Two shipped-bug catches
  (context window range; lane reuse) came straight from these.
- **Learn from miette's scars** — the `Production edge cases (from miette)` group
  should grow as their changelog does.
- **Preserve the ⭐ advantages** (stale handling, cycle safety, display-width
  carets, totality).
- **Golden determinism** — plain (`ColorNever`) output stays byte-stable; new
  features gate behind config/kinds so fixtures don't churn unless intended.
- **Minimal, bounded deps; `cabal check` clean; discipline checks green.**

---

## 7. Quick reference

```sh
cabal build all            # library + interop sub-libs + tests
cabal test all             # golden + props + interop
make test                  # the above + check-generated + check-compile-fail
```

Design: `Tadka_Vision_v5.md` · Build plan: `Tadka_Implementation_Spec.md` ·
Principles: `principles.md` · Changelog: `CHANGELOG.md`.
