-- | The single, renderer-agnostic @related@-chain traversal. Phases 5, 6, and 7 each call this rather than re-deriving /when/ a
-- chain terminates; a handler only decides /how/ to render each termination
-- reason. Factoring it here is what keeps "distinct truncation shapes for
-- depth-limit vs. cycle, in every handler including JSON" achievable without
-- three copies of the logic drifting apart.
--
-- The walk carries a depth budget (a plain 'Natural'; Phase 4 threads
-- @Config@'s @withRelatedDepthLimit@ into it — a one-directional consumer
-- relationship, not a dependency of this module on Phase 4) and a
-- @Set DiagnosticId@ of ids seen on the current path. Production termination is
-- always guaranteed by the finite depth budget; cycle detection via
-- 'diagnosticId' is the sharper, opt-in refinement.
--
-- No compatibility guarantee.
module Tadka.Internal.Related
  ( TerminationReason (..)
  , RelatedTree (..)
  , defaultRelatedDepth
  , walkRelated
  , walkCauses
  , flattenRelated
  ) where

import           Data.Set                (Set)
import qualified Data.Set                as Set
import           Numeric.Natural         (Natural)

import           Tadka.Internal.Diagnostic
                   (Diagnostic (diagnosticCause, diagnosticId, related), SomeDiagnostic (..))
import           Tadka.Internal.Types    (DiagnosticId)

-- | Why a node's @related@ list is represented the way it is.
data TerminationReason
  = NotTerminated
    -- ^ fully expanded (its children — themselves possibly truncated — are present).
  | DepthTruncated
    -- ^ the node has related diagnostics, but the depth budget was exhausted;
    -- children omitted. A handler renders "(N more related diagnostics
    -- omitted)", computing N as @length (related d)@.
  | CycleOmitted
    -- ^ this node's 'diagnosticId' was already on the current path; it is not
    -- descended into again. A handler renders "(cycle omitted)".
  deriving (Eq, Show, Enum, Bounded)

-- | The result of a walk: a diagnostic, its expanded children (empty when
-- truncated), and why. This is the renderer-agnostic shape all handlers consume.
data RelatedTree = RelatedTree
  { relatedDiag        :: SomeDiagnostic
  , relatedChildren    :: [RelatedTree]
  , relatedTermination :: TerminationReason
  }

-- | The default @related@ depth limit (vision §5; a @Config@ field in Phase 4).
defaultRelatedDepth :: Natural
defaultRelatedDepth = 8

-- | Walk a diagnostic and its @related@ chain into a 'RelatedTree', detecting
-- cycles by 'diagnosticId' and bounding depth by the given budget. Total for
-- any input — even a structurally infinite one — because the budget is finite.
walkRelated :: Natural -> SomeDiagnostic -> RelatedTree
walkRelated limit = go limit Set.empty
  where
    go :: Natural -> Set DiagnosticId -> SomeDiagnostic -> RelatedTree
    go depth visited sd@(SomeDiagnostic e) =
      case diagnosticId e of
        Just i | i `Set.member` visited ->
          RelatedTree sd [] CycleOmitted
        mId ->
          let visited' = maybe visited (`Set.insert` visited) mId
              kids     = related e
          in if null kids
               then RelatedTree sd [] NotTerminated
               else if depth == 0
                      then RelatedTree sd [] DepthTruncated
                      else RelatedTree sd (map (go (depth - 1) visited') kids) NotTerminated

-- | Pre-order flatten to @(diagnostic, reason)@ pairs. Convenient for handlers
-- that render a flat list and for tests.
flattenRelated :: RelatedTree -> [(SomeDiagnostic, TerminationReason)]
flattenRelated (RelatedTree d kids term) =
  (d, term) : concatMap flattenRelated kids


-- | Follow 'diagnosticCause' from a root into a linear chain, up to a depth
-- budget and with cycle detection by 'diagnosticId' (a cause whose id was
-- already seen ends the chain). Returns the causes in order, excluding the
-- root. Total: the budget bounds depth and the visited set bounds cycles.
walkCauses :: Natural -> SomeDiagnostic -> [SomeDiagnostic]
walkCauses limit root@(SomeDiagnostic e0) = go limit (seed e0) root
  where
    seed e = maybe Set.empty (`Set.insert` Set.empty) (diagnosticId e)
    go :: Natural -> Set DiagnosticId -> SomeDiagnostic -> [SomeDiagnostic]
    go 0 _ _ = []
    go depth visited (SomeDiagnostic e) =
      case diagnosticCause e of
        Nothing -> []
        Just c@(SomeDiagnostic ce) ->
          case diagnosticId ce of
            Just i | i `Set.member` visited -> []
            mId -> c : go (depth - 1) (insMaybe mId visited) c
    insMaybe mId v = maybe v (`Set.insert` v) mId
