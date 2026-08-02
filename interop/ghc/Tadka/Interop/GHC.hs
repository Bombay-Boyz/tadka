{-# LANGUAGE OverloadedStrings #-}

-- | One-directional adapter: GHC 'SrcSpan' → tadka 'Span' (spec Phase 10).
--
-- A plain function against Phase 1/2 types; no core module depends on this.
-- Targets GHC 9.4.7 through 9.14.1 (see the @ghc@ bound in @tadka.cabal@).
-- Only 'GHC.Types.SrcLoc''s 'SrcSpan'/'RealSrcSpan' accessors are used, which
-- have been stable public GHC API across that whole range — no CPP needed,
-- but this should be reconfirmed by an actual per-version CI build rather
-- than assumed from this note.
--
-- No compatibility guarantee.
module Tadka.Interop.GHC
  ( SrcSpanConvError (..)
  , spanFromSrcSpan
  , offsetFromLineCol
  ) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Types.SrcLoc (SrcSpan (..), srcSpanEndCol, srcSpanEndLine,
                                   srcSpanStartCol, srcSpanStartLine)

import           Tadka            (Span, mkSpan)

-- | Why a 'SrcSpan' could not be converted.
data SrcSpanConvError
  = UnhelpfulSrcSpan          -- ^ the span was an @UnhelpfulSpan@ (no real location)
  | LineColOutOfBounds Int Int -- ^ a 1-based (line, column) not present in the source
  | NegativeSpanLength        -- ^ end preceded start
  deriving (Eq, Show)

-- | Total, safe indexing into a list: 'Nothing' out of bounds, never a partial
-- crash. Kept local (mirrors 'Tadka.Internal.Width.atMay') so no caller in this
-- module ever reaches for the partial @(!!)@ directly.
atMayList :: [a] -> Int -> Maybe a
atMayList xs i
  | i < 0     = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      []      -> Nothing

-- | Convert a 1-based (line, column) into a 0-based character offset within the
-- given source, or 'Nothing' if the position is not in bounds. Column may point
-- one past the end of a line (the end-of-line position GHC uses). Total: the
-- line lookup is tied directly to the 'Maybe' via 'atMayList', so an
-- out-of-range line can never reach the column check below it.
offsetFromLineCol :: Text -> Int -> Int -> Maybe Int
offsetFromLineCol src line col = do
  here <- atMayList ls (line - 1)
  if col < 1 || col > T.length here + 1
    then Nothing
    else Just (before + (col - 1))
  where
    ls     = T.splitOn "\n" src
    before = sum (map ((+ 1) . T.length) (take (line - 1) ls))  -- +1 per newline

-- | Convert a GHC 'SrcSpan' to a tadka 'Span', given the source text (needed to
-- turn GHC's 1-based line/column into a character offset and length).
--
-- The four-way case split below is exhaustive over which of the two
-- endpoints resolved: both, only the start, only the end, or neither. Each
-- arm names the specific endpoint at fault rather than defaulting to the
-- start, so 'LineColOutOfBounds' always describes the position that was
-- actually out of bounds.
spanFromSrcSpan :: Text -> SrcSpan -> Either SrcSpanConvError Span
spanFromSrcSpan _   (UnhelpfulSpan _) = Left UnhelpfulSrcSpan
spanFromSrcSpan src (RealSrcSpan rss _) =
  case (offsetFromLineCol src (srcSpanStartLine rss) (srcSpanStartCol rss),
        offsetFromLineCol src (srcSpanEndLine rss)   (srcSpanEndCol rss)) of
    (Nothing, _) -> Left (LineColOutOfBounds (srcSpanStartLine rss) (srcSpanStartCol rss))
    (_, Nothing) -> Left (LineColOutOfBounds (srcSpanEndLine rss)   (srcSpanEndCol rss))
    (Just s, Just e)
      | e >= s    -> mkSpanNonNegative s (e - s)
      | otherwise -> Left NegativeSpanLength

-- | 'mkSpan', specialised to a call site where both arguments are already
-- non-negative by construction (`max 0` is a no-op on them): 'mkSpan' can
-- only fail on a negative offset or a negative length, so with both clamped
-- here — not merely reasoned to be non-negative three functions away in
-- 'offsetFromLineCol' — its 'Left' case is unreachable by local inspection,
-- not by trusting a distant invariant. 'mkSpan' is still the one used, so a
-- future tightening of its validation is not silently bypassed here.
mkSpanNonNegative :: Int -> Int -> Either SrcSpanConvError Span
mkSpanNonNegative s len = either (const (Left NegativeSpanLength)) Right (mkSpan (max 0 s) (max 0 len))
