-- | Public API surface for @tadka@.
--
-- This module is the sole supported entry point. It re-exports the public
-- vocabulary defined across @Tadka.Internal.*@. Anything reachable only via
-- @Tadka.Internal.*@ carries no compatibility guarantee.
--
-- Phases 1–2 populate the validated primitive types, resolution-indexed spans,
-- and 'Context' construction. The @Diagnostic@ class, renderers, and the derive
-- macro, and interop adapters complete the surface. The public API matches
-- vision §8; @Offset@\/@Length@ and other representation details live under
-- "Tadka.Internal" with no compatibility guarantee.
module Tadka
  ( -- * Named source
    NamedSource
  , sourceName
  , sourceText
  , mkNamedSource
  , SourceError (..)
    -- * Spans
    --
    -- Spans are the public position type; @Offset@\/@Length@ are the internal
    -- offset representation and live in "Tadka.Internal.Types" (no compatibility
    -- guarantee), per vision §8.
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
    -- * Context
  , Context (NoContext)
  , Labeled (..)
  , LabelKind (..)
  , LabelState (..)
  , contextLabelStates
  , mkContext
  , mkContextDegrading
  , ContextError (..)
    -- * Rendering configuration
  , Config
  , defaultConfig
  , withColorMode
  , withUnicodeMode
  , withRelatedDepthLimit
  , withTabWidth
  , withContextLines
  , withLabelPalette
  , withTarget
  , ColorMode (..)
  , UnicodeMode (..)
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
    -- * Derive macro
  , DiagnosticSpec (..)
  , defaultSpec
  , deriveDiagnostic
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

import Tadka.Internal.Ann
import Tadka.Internal.Config
import Tadka.Internal.Context
import Tadka.Internal.Diagnostic
import Tadka.Internal.Generics (genericContext)
import Tadka.Internal.Render
import Tadka.Internal.Renderer.Graphical (GraphicalOptions)
import Tadka.Internal.Renderer.Json (JsonOptions)
import Tadka.Internal.Renderer.Narratable (NarratableOptions)
import Tadka.Internal.Span
import Tadka.Internal.TH
import Tadka.Internal.Types
