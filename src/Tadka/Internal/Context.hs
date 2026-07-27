-- | Labels, label state, and 'Context' construction (vision §2, §6).
--
-- The headline v5 guarantee lives here: a 'Context' holds only spans that were
-- already checked against real source text, and it /never silently forgets one/.
-- 'mkContextDegrading' turns a span that fails resolution into a
-- 'LabelStale' marker in its original position rather than dropping it, so the
-- label count and ordering of a rendered diagnostic never depend on which spans
-- happened to still be valid.
--
-- @buildContext@ is the single, plain, independently-testable dispatch that
-- both the derive macro (Phase 8) and hand-written instances call; landing it
-- here (not in Phase 8) is what lets the macro stay "provably thin" — it only
-- has to generate a call to this already-proven function.
--
-- No compatibility guarantee.
module Tadka.Internal.Context
  ( -- * Labels
    Labeled (..)
  , LabelKind (..)
  , LabelState (..)
    -- * Context
  , Context (..)
  , contextLabelStates
    -- * Construction
  , ContextError (..)
  , mkContext
  , mkContextDegrading
  , buildContext
  , buildContextWith
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Prettyprinter      (Doc)

import           Tadka.Internal.Ann   (Ann)
import           Tadka.Internal.Span
                   (ResolvedSpan, Span, SpanError, StaleReason, resolveSpan,
                    spanErrorReason)
import           Tadka.Internal.Types (NamedSource)

-- | A span (in some resolution state) paired with optional label prose.
-- The @a@ is the span-shaped payload: input labels are @Labeled Span@, and a
-- constructed context holds @Labeled LabelState@.
-- | Whether a label points at the error itself ('Primary') or at supporting
-- context ('Secondary'). Handlers emphasise primary labels and anchor the
-- report location on the first primary one.
data LabelKind = Primary | Secondary
  deriving (Eq, Show, Enum, Bounded, Ord)

data Labeled a = Labeled
  { labelSpan :: a
  , labelKind :: LabelKind
  , labelText :: Maybe (Doc Ann)
  }
  deriving (Show)
  -- No 'Eq': @Doc Ann@ has no 'Eq' instance. Compare via 'labelSpan'/'labelKind'.

-- | The per-label outcome of resolving against the context's source.
data LabelState
  = LabelOk ResolvedSpan
    -- ^ resolved successfully; carries positions only.
  | LabelStale StaleReason
    -- ^ resolution failed for this one label; it stays present, in its
    -- original position, with no source line to show, rather than absent.
  deriving (Eq, Show)

-- | Either there is no source-anchored information at all ('NoContext'), or
-- there is exactly one source and a non-empty, order-preserving list of labels
-- checked against it ('HasLabels'). There is no third state and no
-- "silently shorter" state.
data Context
  = NoContext
  | HasLabels NamedSource (NonEmpty (Labeled LabelState))
  deriving (Show)

-- | The label states of a context in order (empty for 'NoContext'). Useful for
-- asserting the count/ordering guarantee.
contextLabelStates :: Context -> [LabelState]
contextLabelStates NoContext           = []
contextLabelStates (HasLabels _ lbls)  = fmap labelSpan (NE.toList lbls)

-- | Why 'mkContext' (the strict constructor) rejected its input: the first
-- span that failed to resolve.
newtype ContextError = ContextError SpanError
  deriving (Eq, Show)

-- | Strict construction: succeeds only if /every/ span resolves. Intended for a
-- domain error type's own smart constructor, so that holding such a value
-- proves its context is fully resolvable. Returns 'Left' for the first
-- out-of-bounds span.
mkContext :: NamedSource -> NonEmpty (Labeled Span) -> Either ContextError Context
mkContext src labels =
  case traverse resolveLabel labels of
    Left err   -> Left (ContextError err)
    Right lbls -> Right (HasLabels src lbls)
  where
    resolveLabel :: Labeled Span -> Either SpanError (Labeled LabelState)
    resolveLabel (Labeled sp k txt) =
      fmap (\rs -> Labeled (LabelOk rs) k txt) (resolveSpan src sp)

-- | Total construction: resolves every span it can; a span that fails becomes
-- 'LabelStale' /in the same position/, never dropped. The result always has
-- exactly as many labels, in the same order, as the input — this is the
-- count/ordering guarantee that is the whole point of the type.
mkContextDegrading :: NamedSource -> NonEmpty (Labeled Span) -> Context
mkContextDegrading src = HasLabels src . fmap resolveOrStale
  where
    resolveOrStale :: Labeled Span -> Labeled LabelState
    resolveOrStale (Labeled sp k txt) =
      case resolveSpan src sp of
        Right rs  -> Labeled (LabelOk rs) k txt
        Left err  -> Labeled (LabelStale (spanErrorReason err)) k txt

-- | The shared dispatch both the derive macro and manual instances call.
-- An empty label list is the only route to 'NoContext'; a non-empty one goes
-- straight to 'mkContextDegrading'. This is intentionally a thin wrapper, not a
-- second copy of the degrading logic.
buildContext :: NamedSource -> [(Span, Maybe (Doc Ann))] -> Context
buildContext _   []       = NoContext
buildContext src (x : xs) = mkContextDegrading src (fmap toLabeled (x :| xs))
  where
    toLabeled (sp, txt) = Labeled sp Primary txt   -- unmarked labels are Primary

-- | Like 'buildContext', but each label carries an explicit 'LabelKind'. The
-- shared function both the derive macro (when secondary labels are requested)
-- and manual instances call for primary\/secondary labelling.
buildContextWith :: NamedSource -> [(Span, LabelKind, Maybe (Doc Ann))] -> Context
buildContextWith _   []       = NoContext
buildContextWith src (x : xs) = mkContextDegrading src (fmap toLabeled (x :| xs))
  where
    toLabeled (sp, k, txt) = Labeled sp k txt
