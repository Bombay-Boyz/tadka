{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}


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

import           Tadka.Internal.SourceCode (SourceCode (..))
import           Tadka.Internal.Types
                   (Length, LineCol (..), Offset, LengthError, OffsetError,
                    mkLength, mkOffset, unLength, unOffset)

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


data StaleReason
  = SpanOutOfBounds
  deriving (Eq, Show)

-- | The concrete failure returned by 'resolveSpan'.
data SpanError = SpanOutOfBoundsError
  { spanErrorSpanEnd     :: !Integer  -- ^ offset + length (character index)
  , spanErrorSourceChars :: !Int      -- ^ number of characters in the source
  }
  deriving (Eq, Show)

-- | Map a 'SpanError' to the 'StaleReason' recorded in a degraded label.
spanErrorReason :: SpanError -> StaleReason
spanErrorReason SpanOutOfBoundsError{} = SpanOutOfBounds

-- | Resolve a span against a source, computing its line/column positions.
-- Fails with 'SpanOutOfBoundsError' if the span's end lies beyond the source.
-- Offsets and lengths are measured in characters (code points).
resolveSpan :: SourceCode s => s -> Span -> Either SpanError ResolvedSpan
resolveSpan src (RawSpan off len) =
  if end > toInteger n
    then Left
      (SpanOutOfBoundsError
        { spanErrorSpanEnd = end
        , spanErrorSourceChars = n
        })
    else
      Right
        (ResolvedSpan
          off
          len
          (scLocate src o)
          (scLocate src (fromInteger end)))
  where
    o   = unOffset off
    l   = unLength len
    end = toInteger o + toInteger l
    n   = scLength src
