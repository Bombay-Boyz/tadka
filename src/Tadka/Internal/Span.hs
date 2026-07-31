{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Resolution-indexed spans (vision §2). A 'Span' names a position in some
-- source; a 'ResolvedSpan' is one that has been checked against a specific
-- 'NamedSource' and additionally carries its start/end 'LineCol'. The type
-- index makes "unchecked" and "checked" distinct types, so a renderer can only
-- ever be handed positions that were already validated.
--
-- 'ResolvedSpan' carries positions only — never owned text (v5 changelog fix).
-- The source text lives once per 'Tadka.Internal.Context.Context'; the renderer
-- slices whatever substring it needs with @Text.take@ / @Text.drop@ (O(1),
-- buffer-sharing) against that single stored source.
--
-- No compatibility guarantee.
module Tadka.Internal.Span
  ( -- * Resolution index
    Resolution (..)
  , SpanF
  , Span
  , ResolvedSpan
    -- * Building unresolved spans
  , mkSpan
  , SpanBuildError (..)
    -- * Accessors
  , spanOffset
  , spanLength
  , resolvedStart
  , resolvedEnd
  , LineCol (..)
    -- * Resolution
  , resolveSpan
  , SpanError (..)
  , spanErrorReason
  , StaleReason (..)
  ) where

import           Data.Bifunctor (first)
import           Data.Text      (Text)
import qualified Data.Text      as T

import           Tadka.Internal.Types
                   (Length, NamedSource, Offset, LengthError, OffsetError,
                    mkLength, mkOffset, sourceText, unLength, unOffset)

-- | Whether a span has been checked against a concrete source.
data Resolution = Unresolved | Resolved

-- | A span, indexed by whether it has been resolved. The data constructors are
-- not exported: an unresolved span is built with 'mkSpan', and a resolved span
-- can only be produced by 'resolveSpan', so a 'ResolvedSpan' with inconsistent
-- line/column data is unrepresentable.
data SpanF (r :: Resolution) where
  RawSpan      :: Offset -> Length -> SpanF 'Unresolved
  ResolvedSpan :: Offset -> Length -> LineCol -> LineCol -> SpanF 'Resolved

deriving instance Show (SpanF r)
deriving instance Eq (SpanF r)

-- | An unresolved span: raw offset and length, not yet checked against a source.
type Span = SpanF 'Unresolved

-- | A resolved span: positions checked against a specific source, with
-- computed start/end line-column pairs.
type ResolvedSpan = SpanF 'Resolved

-- | A one-based line/column position.
data LineCol = LineCol
  { lcLine   :: !Int
  , lcColumn :: !Int
  }
  deriving (Eq, Ord, Show)

-- | Why 'mkSpan' rejected its inputs.
data SpanBuildError
  = SpanBadOffset OffsetError
  | SpanBadLength LengthError
  deriving (Eq, Show)

-- | Build an unresolved 'Span' from a raw offset and length, validating both
-- (non-negative). This is the only exported way to construct a 'Span'.
mkSpan :: Int -> Int -> Either SpanBuildError Span
mkSpan o l = do
  off <- first SpanBadOffset (mkOffset o)
  len <- first SpanBadLength (mkLength l)
  pure (RawSpan off len)

-- | The starting offset of any span.
spanOffset :: SpanF r -> Offset
spanOffset (RawSpan o _)          = o
spanOffset (ResolvedSpan o _ _ _) = o

-- | The length of any span.
spanLength :: SpanF r -> Length
spanLength (RawSpan _ l)          = l
spanLength (ResolvedSpan _ l _ _) = l

-- | The start position of a resolved span.
resolvedStart :: ResolvedSpan -> LineCol
resolvedStart (ResolvedSpan _ _ s _) = s

-- | The end position of a resolved span.
resolvedEnd :: ResolvedSpan -> LineCol
resolvedEnd (ResolvedSpan _ _ _ e) = e

-- | Why a span failed to resolve. 'resolveSpan', working from raw offsets,
-- only ever reports out-of-bounds — 'StaleReason' has exactly one
-- constructor because that is the only failure mode any producer in this
-- library can construct today. (A previously-present 'SourceMismatch'
-- constructor had no producer and was removed; see CHANGELOG.md. Real
-- source-identity tracking, if ever added, is new scope for a future
-- producer, not a reason to keep an unreachable constructor "just in case".)
data StaleReason
  = SpanOutOfBounds
  deriving (Eq, Show)

-- | The concrete failure returned by 'resolveSpan'.
data SpanError = SpanOutOfBoundsError
  { spanErrorSpanEnd     :: !Int  -- ^ offset + length (character index)
  , spanErrorSourceChars :: !Int  -- ^ number of characters in the source
  }
  deriving (Eq, Show)

-- | Map a 'SpanError' to the 'StaleReason' recorded in a degraded label.
spanErrorReason :: SpanError -> StaleReason
spanErrorReason SpanOutOfBoundsError{} = SpanOutOfBounds

-- | Resolve a span against a source, computing its line/column positions.
-- Fails with 'SpanOutOfBoundsError' if the span's end lies beyond the source.
-- Offsets and lengths are measured in characters (code points).
resolveSpan :: NamedSource -> Span -> Either SpanError ResolvedSpan
resolveSpan src (RawSpan off len) =
  if end > n
    then Left (SpanOutOfBoundsError { spanErrorSpanEnd = end, spanErrorSourceChars = n })
    else Right (ResolvedSpan off len (offsetToLineCol txt o) (offsetToLineCol txt end))
  where
    txt = sourceText src
    o   = unOffset off
    end = o + unLength len
    n   = T.length txt

-- | One-based line/column of a character offset into the text. Assumes
-- @0 <= off <= T.length txt@ (guaranteed by 'resolveSpan''s bounds check).
offsetToLineCol :: Text -> Int -> LineCol
offsetToLineCol txt off = LineCol lineNo col
  where
    prefix = T.take off txt
    lineNo = T.count "\n" prefix + 1
    col    = T.length (snd (T.breakOnEnd "\n" prefix)) + 1
