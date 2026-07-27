{-# LANGUAGE OverloadedStrings #-}

-- | One-directional adapter: GHC 'SrcSpan' → tadka 'Span' (spec Phase 10).
--
-- A plain function against Phase 1/2 types; no core module depends on this.
-- Minimum supported: the @ghc@ library shipped with GHC 9.10.
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

-- | Convert a 1-based (line, column) into a 0-based character offset within the
-- given source, or 'Nothing' if the position is not in bounds. Column may point
-- one past the end of a line (the end-of-line position GHC uses).
offsetFromLineCol :: Text -> Int -> Int -> Maybe Int
offsetFromLineCol src line col
  | line < 1 || line > length ls = Nothing
  | col  < 1 || col  > T.length here + 1 = Nothing
  | otherwise = Just (before + (col - 1))
  where
    ls     = T.splitOn "\n" src
    here   = ls !! (line - 1)
    before = sum (map ((+ 1) . T.length) (take (line - 1) ls))  -- +1 per newline

-- | Convert a GHC 'SrcSpan' to a tadka 'Span', given the source text (needed to
-- turn GHC's 1-based line/column into a character offset and length).
spanFromSrcSpan :: Text -> SrcSpan -> Either SrcSpanConvError Span
spanFromSrcSpan _   (UnhelpfulSpan _) = Left UnhelpfulSrcSpan
spanFromSrcSpan src (RealSrcSpan rss _) =
  case (offsetFromLineCol src (srcSpanStartLine rss) (srcSpanStartCol rss),
        offsetFromLineCol src (srcSpanEndLine rss)   (srcSpanEndCol rss)) of
    (Just s, Just e)
      | e >= s    -> either (const (Left NegativeSpanLength)) Right (mkSpan s (e - s))
      | otherwise -> Left NegativeSpanLength
    _ -> Left (LineColOutOfBounds (srcSpanStartLine rss) (srcSpanStartCol rss))
