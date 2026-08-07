
--
-- No compatibility guarantee.
module Tadka.Internal.Context
  ( -- * Labels
    Labeled (..)
  , LabelKind (..)
  , LabelState (..)
    -- * Context
  , Context (..)
  , SourceGroup (..)
  , contextLabelStates
  , contextSourceGroups
    -- * Construction
  , ContextError (..)
  , mkContext
  , mkContextDegrading
  , mkContextMulti
  , mkContextMultiDegrading
  , buildContext
  , buildContextWith
  , buildContextMulti
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Prettyprinter      (Doc)

import           Tadka.Internal.Ann   (Ann)
import           Tadka.Internal.Span
                   (ResolvedSpan, Span, SpanError, StaleReason, resolveSpan,
                    spanErrorReason)
import           Tadka.Internal.Types (NamedSource)


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


data SourceGroup = SourceGroup
  { sgSource :: NamedSource
  , sgLabels :: NonEmpty (Labeled LabelState)
  }
  deriving (Show)


data Context
  = NoContext
  | HasLabels (NonEmpty SourceGroup)
  deriving (Show)

-- | The label states of a context in order, flattened across every group in
-- group order (empty for 'NoContext'). Useful for asserting the
-- count/ordering guarantee.
contextLabelStates :: Context -> [LabelState]
contextLabelStates NoContext       = []
contextLabelStates (HasLabels gs)  =
  concatMap (fmap labelSpan . NE.toList . sgLabels) (NE.toList gs)

-- | The source groups of a context in order (empty for 'NoContext'). The
-- renderers' one shared way to walk a context group-by-group.
contextSourceGroups :: Context -> [SourceGroup]
contextSourceGroups NoContext      = []
contextSourceGroups (HasLabels gs) = NE.toList gs

-- | Why 'mkContext'\/'mkContextMulti' (the strict constructors) rejected their
-- input: the first span that failed to resolve, in group-then-label order.
newtype ContextError = ContextError SpanError
  deriving (Eq, Show)

-- | Strict, multi-source construction: succeeds only if /every/ span in
-- /every/ group resolves against its own group's source. Returns 'Left' for
-- the first out-of-bounds span encountered in group-then-label order.
mkContextMulti
  :: NonEmpty (NamedSource, NonEmpty (Labeled Span))
  -> Either ContextError Context
mkContextMulti groups = HasLabels <$> traverse resolveGroup groups
  where
    resolveGroup :: (NamedSource, NonEmpty (Labeled Span)) -> Either ContextError SourceGroup
    resolveGroup (src, labels) =
      case traverse (resolveLabel src) labels of
        Left err   -> Left (ContextError err)
        Right lbls -> Right (SourceGroup src lbls)

    resolveLabel :: NamedSource -> Labeled Span -> Either SpanError (Labeled LabelState)
    resolveLabel src (Labeled sp k txt) =
      fmap (\rs -> Labeled (LabelOk rs) k txt) (resolveSpan src sp)


mkContextMultiDegrading :: NonEmpty (NamedSource, NonEmpty (Labeled Span)) -> Context
mkContextMultiDegrading = HasLabels . fmap resolveGroup
  where
    resolveGroup :: (NamedSource, NonEmpty (Labeled Span)) -> SourceGroup
    resolveGroup (src, labels) = SourceGroup src (fmap (resolveOrStale src) labels)

    resolveOrStale :: NamedSource -> Labeled Span -> Labeled LabelState
    resolveOrStale src (Labeled sp k txt) =
      case resolveSpan src sp of
        Right rs  -> Labeled (LabelOk rs) k txt
        Left err  -> Labeled (LabelStale (spanErrorReason err)) k txt

-- | Strict construction: succeeds only if /every/ span resolves. Intended for a
-- domain error type's own smart constructor, so that holding such a value
-- proves its context is fully resolvable. Returns 'Left' for the first
-- out-of-bounds span.
--
-- A one-group special case of 'mkContextMulti' — not a second copy of the
-- resolution logic.
mkContext :: NamedSource -> NonEmpty (Labeled Span) -> Either ContextError Context
mkContext src labels = mkContextMulti ((src, labels) :| [])

-- | Total construction: resolves every span it can; a span that fails becomes
-- 'LabelStale' /in the same position/, never dropped. The result always has
-- exactly as many labels, in the same order, as the input — this is the
-- count/ordering guarantee that is the whole point of the type.
--
-- A one-group special case of 'mkContextMultiDegrading' — not a second copy
-- of the degrading logic.
mkContextDegrading :: NamedSource -> NonEmpty (Labeled Span) -> Context
mkContextDegrading src labels = mkContextMultiDegrading ((src, labels) :| [])

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


buildContextMulti :: NonEmpty (NamedSource, [(Span, LabelKind, Maybe (Doc Ann))]) -> Context
buildContextMulti groups =
  maybe NoContext mkContextMultiDegrading
    (NE.nonEmpty (concatMap nonEmptyGroup (NE.toList groups)))
  where
    nonEmptyGroup
      :: (NamedSource, [(Span, LabelKind, Maybe (Doc Ann))])
      -> [(NamedSource, NonEmpty (Labeled Span))]
    nonEmptyGroup (src, entries) =
      case NE.nonEmpty (map toLabeled entries) of
        Nothing  -> []
        Just lbs -> [(src, lbs)]

    toLabeled (sp, k, txt) = Labeled sp k txt
