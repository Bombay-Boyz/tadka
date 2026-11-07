-- | Public API surface for @tadka@.
--
-- This module is the sole supported entry point. It re-exports the public
-- vocabulary defined across @Tadka.Internal.*@. Anything reachable only via
-- @Tadka.Internal.*@ carries no compatibility guarantee.
--
-- The module exports the validated primitive types, resolution-indexed spans,
-- 'Context' construction, the 'Diagnostic' class and its renderers, the
-- derive macros, ad hoc diagnostics, and the exception bridge. Representation
-- details such as @Offset@\/@Length@ live under "Tadka.Internal".
module Tadka
  ( -- * Named source
    NamedSource
  , sourceName
  , sourceText
  , mkNamedSource
  , SourceError (..)
    -- * Pluggable sources
    --
    -- The extension point for "bring your own source representation" (a rope
    -- buffer, an LSP virtual document, a streamed huge file): write a
    -- 'SourceCode' instance alongside the built-in 'NamedSource' one. Every
    -- context builder, 'resolveSpan', the renderers and both derive macros
    -- accept it: pass your value to 'buildContext' and friends, or keep it in
    -- a 'SomeSource' field of a derived record. 'SomeSource' also lets one
    -- multi-source context mix source types.
  , SourceCode (..)
  , SomeSource (..)
  , someSource
    -- * Spans
    --
    -- Spans are the public position type; @Offset@\/@Length@ are the internal
    -- offset representation and live in "Tadka.Internal.Types" (no compatibility
    -- guarantee)
  , Span
  , ResolvedSpan
  , mkSpan
  , SpanBuildError (..)
  , resolvedStart
  , resolvedEnd
  , LineCol (..)
  , resolveSpan
  , SpanError (..)
  , spanErrorReason
  , StaleReason (..)
    -- * Annotations
  , Ann (..)
    -- * The Diagnostic class
  , Diagnostic (..)
  , SomeDiagnostic (..)
    -- * Exception-propagation bridge
    --
    -- 'Diagnostic' deliberately has no 'Control.Exception.Exception'
    -- superclass or instance (see "Tadka.Internal.Exception"); these are the
    -- one, documented way to make a 'Diagnostic' value participate in
    -- ordinary 'Control.Exception.throwIO'\/'Control.Exception.catch'
    -- propagation, and to install it as GHC's top-level uncaught-exception
    -- printer (the Haskell analogue of miette's @set_panic_hook@).
  , DiagnosticException (..)
  , diagnosticShow
  , throwDiagnostic
  , catchDiagnostic
  , installPanicHook
    -- * Ad hoc diagnostics
    --
    -- The "I just want /something/ printed nicely right now" escape hatch:
    -- lift a 'Show'-only error, a caught 'Control.Exception.SomeException',
    -- or a plain message into a minimal 'Diagnostic', without writing a
    -- dedicated type for it. See "Tadka.Internal.AdHoc".
  , AdHocDiagnostic
  , adHoc
  , wrapErr
  , intoDiagnostic
  , fromError
  , fromSomeException
  , withCode
  , withSeverity
  , withHelp
  , withUrl
  , withSourceCode
    -- * Context
  , Context (NoContext)
  , Labeled (..)
  , LabelKind (..)
  , LabelState (..)
  , contextLabelStates
  , mkContext
  , mkContextDegrading
  , ContextError (..)
    -- ** Multi-source context
    --
    -- A 'Context' can hold labels resolved against more than one source. These
    -- are the entry points for a diagnostic whose labels span several files.
    -- The derive macros and 'genericContext' take a single source, so a
    -- multi-source diagnostic is written as a hand-written instance.
  , mkContextMulti
  , mkContextMultiDegrading
    -- * Rendering configuration
  , Config
  , defaultConfig
  , withColorMode
  , withUnicodeMode
  , withHyperlinkMode
  , withRelatedDepthLimit
  , withTabWidth
  , withContextLines
  , withLabelPalette
  , withTarget
  , withCauseDetail
  , withHighlighter
  , withoutHighlighter
  , ColorMode (..)
  , UnicodeMode (..)
  , HyperlinkMode (..)
  , CauseDetail (..)
  , Highlighter (..)
    -- * Renderers and the render path
  , Target (..)
  , Output
  , Renderer (..)
  , SomeRenderer (..)
  , GraphicalOptions
  , NarratableOptions
  , JsonOptions
  , selectRenderer
  , render
  , reportDiagnostic
  , reportDiagnosticTo
  , renderDiagnosticTo
    -- * Derive macro
  , DiagnosticSpec (..)
  , defaultSpec
    -- ** A flatter, declarative alternative to a 'defaultSpec' record update
  , SpecEntry
  , diagnosticSpec
  , diagCode
  , diagSeverity
  , diagHelp
  , diagUrl
  , diagSource
  , diagLabel
  , diagSecondaryLabel
  , diagLabelCollection
  , diagSecondaryLabelCollection
  , diagLabelCollectionWith
  , diagSecondaryLabelCollectionWith
  , diagRelated
  , diagCause
  , diagId
  , diagMessage
  , deriveDiagnostic
  , DiagnosticSumSpec
  , deriveDiagnosticSum
    -- * Generics label-wiring (context only)
  , genericContext
    -- * Diagnostic codes
  , DiagnosticCode
  , unDiagnosticCode
  , mkDiagnosticCode
  , CodeError (..)
    -- * URLs
  , Url
  , unUrl
  , mkUrl
  , UrlError (..)
    -- * Severity
  , Severity (..)
    -- * Diagnostic identity
  , DiagnosticId
  , unDiagnosticId
  , mkDiagnosticId
  ) where

import Tadka.Internal.AdHoc
import Tadka.Internal.Ann
import Tadka.Internal.Config
import Tadka.Internal.Context
import Tadka.Internal.Diagnostic
import Tadka.Internal.Exception
import Tadka.Internal.Generics (genericContext)
import Tadka.Internal.Render
import Tadka.Internal.Renderer.Graphical (GraphicalOptions)
import Tadka.Internal.Renderer.Json (JsonOptions)
import Tadka.Internal.Renderer.Narratable (NarratableOptions)
import Tadka.Internal.SourceCode (SomeSource (..), SourceCode (..), someSource)
import Tadka.Internal.Span
import Tadka.Internal.TH
import Tadka.Internal.Types
