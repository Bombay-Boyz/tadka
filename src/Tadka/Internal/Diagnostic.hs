{-# LANGUAGE ExistentialQuantification #-}


-- No compatibility guarantee.
module Tadka.Internal.Diagnostic
  ( Diagnostic (..)
  , SomeDiagnostic (..)
  ) where

import           Prettyprinter        (Doc)

import           Tadka.Internal.Ann     (Ann)
import           Tadka.Internal.Context (Context (NoContext))
import           Tadka.Internal.Types
                   (DiagnosticCode, DiagnosticId, Severity (SevError), Url)

-- | Everything a renderer needs from an error value. Only 'message' is
-- required; every other method has a total default.
class Diagnostic e where
  -- | The headline message. Mandatory.
  message      :: e -> Doc Ann
  -- | Source-anchored labels, already resolved-or-explicitly-stale.
  context      :: e -> Context
  -- | An optional documented error code (e.g. @tadka::E0001@).
  code         :: e -> Maybe DiagnosticCode
  -- | Severity; defaults to 'SevError'.
  severity     :: e -> Severity
  -- | Optional help text.
  help         :: e -> Maybe (Doc Ann)
  -- | Optional documentation URL.
  url          :: e -> Maybe Url
  -- | Related diagnostics, walked as a (possibly cyclic) chain.
  related      :: e -> [SomeDiagnostic]
  -- | Opt-in identity, used solely for cycle detection in @related@ walks.
  diagnosticId :: e -> Maybe DiagnosticId
  -- | The underlying cause, rendered as a linear \"caused by\" chain, separate
  -- from the (tree-shaped) @related@ diagnostics. Defaults to 'Nothing'.
  diagnosticCause :: e -> Maybe SomeDiagnostic

  context      _ = NoContext
  code         _ = Nothing
  severity     _ = SevError
  help         _ = Nothing
  url          _ = Nothing
  related      _ = []
  diagnosticId _ = Nothing
  diagnosticCause _ = Nothing

-- | An existentially-wrapped diagnostic. No @Show@ constraint: every operation
-- a renderer performs goes through 'Diagnostic''s methods.
data SomeDiagnostic = forall e. Diagnostic e => SomeDiagnostic e
