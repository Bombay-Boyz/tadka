-- | Character-display-width and grapheme-cluster-break lookups, backed by the
-- generated 'Tadka.Internal.Width.Table' (vision §0, Infrastructure; spec
-- Phase 1). A deliberately owned, versioned, regeneratable table rather than a
-- @text-icu@ dependency.
--
-- Phase 1 exposes the table and its point lookups ('charWidth', 'textWidth',
-- 'graphemeBreakProperty', 'isExtendedPictographic'). The full UAX #29
-- segmentation algorithm that consumes these lives with its caret-alignment
-- use site in the graphical handler (Phase 5).
--
-- No compatibility guarantee.
module Tadka.Internal.Width
  ( -- * Display width
    charWidth
  , textWidth
    -- * Tab-aware display columns
  , displayColumnAt
  , expandTabs
    -- * Grapheme-cluster-break properties (UAX #29)
  , GBProp (..)
  , graphemeBreakProperty
  , isExtendedPictographic
    -- * Provenance
  , ucdVersion
  ) where

import           Data.Array            (Array, bounds, inRange, listArray, (!))
import           Data.Char             (ord)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.List             (sortOn)
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Tadka.Internal.Width.Table

-- | The Grapheme_Cluster_Break property value of a code point (UAX #29),
-- with 'GBOther' as the catch-all. @Extended_Pictographic@ is tracked
-- separately (see 'isExtendedPictographic') because it is an independent
-- property, not a Grapheme_Cluster_Break value.
data GBProp
  = GBOther
  | GBCR
  | GBLF
  | GBControl
  | GBExtend
  | GBZWJ
  | GBRegionalIndicator
  | GBPrepend
  | GBSpacingMark
  | GBL
  | GBV
  | GBT
  | GBLV
  | GBLVT
  deriving (Eq, Show, Enum, Bounded)

-- | Monospace display width of a single code point: @0@ for zero-width marks
-- (general categories Mn, Me, Cf), @2@ for East-Asian Wide/Fullwidth, else @1@.
--
-- This is a per-code-point width; grapheme-aware width (combining sequences,
-- ZWJ emoji) is 'textWidth' / the Phase 5 segmentation, not this function.
charWidth :: Char -> Int
charWidth c =
  let x = ord c
  in if inRanges zeroArr x
       then 0
       else if inRanges wideArr x then 2 else 1

-- | Display width of a 'Text' as the sum of its code-point widths. Zero-width
-- combining marks contribute @0@, so this is correct for the common case;
-- exotic ZWJ/emoji clusters are refined by the Phase 5 segmentation.
textWidth :: Text -> Int
textWidth = T.foldl' (\acc c -> acc + charWidth c) 0

-- | The 0-based display column reached after the first @n@ characters of a
-- line, expanding tabs to the next multiple of the tab width and counting every
-- other character by its 'charWidth'. Total: a tab width below 1 is treated as
-- 1, and @n@ is clamped to the line. This is exactly @textWidth@ of the
-- tab-expanded prefix (see 'expandTabs'), so a caret placed at this column sits
-- under the rendered source.
displayColumnAt :: Int -> Text -> Int -> Int
displayColumnAt tw line n = T.foldl' step 0 (T.take (max 0 n) line)
  where
    w = max 1 tw
    step col c
      | c == '\t' = col + (w - (col `mod` w))
      | otherwise = col + charWidth c

-- | Expand tabs in a line to spaces, honouring tab stops at multiples of the
-- tab width and the display width of preceding characters. The result contains
-- no tab characters, and its 'textWidth' equals @'displayColumnAt' tw line
-- (T.length line)@. Total; a tab width below 1 is treated as 1.
expandTabs :: Int -> Text -> Text
expandTabs tw line = TL.toStrict (TB.toLazyText (snd (T.foldl' step (0, mempty) line)))
  where
    w = max 1 tw
    step :: (Int, TB.Builder) -> Char -> (Int, TB.Builder)
    step (col, acc) c
      | c == '\t' = let n = w - (col `mod` w) in (col + n, acc <> TB.fromText (T.replicate n (T.singleton ' ')))
      | otherwise = (col + charWidth c, acc <> TB.singleton c)

-- | The Grapheme_Cluster_Break property of a code point, or 'GBOther'.
graphemeBreakProperty :: Char -> GBProp
graphemeBreakProperty c = fromMaybe GBOther (searchRanges gbArr (ord c))

-- | Whether a code point has the @Extended_Pictographic@ property (needed by
-- UAX #29 rule GB11 in Phase 5).
isExtendedPictographic :: Char -> Bool
isExtendedPictographic c = inRanges extPictArr (ord c)

-- Range arrays, built once from the generated ascending, coalesced lists.

wideArr :: Array Int (Int, Int)
wideArr = mkArr wideRanges

zeroArr :: Array Int (Int, Int)
zeroArr = mkArr zeroWidthRanges

extPictArr :: Array Int (Int, Int)
extPictArr = mkArr gbExtendedPictographic

gbArr :: Array Int (Int, Int, GBProp)
gbArr = listArray (0, length tagged - 1) (sortOn (\(lo, _, _) -> lo) tagged)
  where
    tagged =
      concat
        [ tag GBCR gbCR, tag GBLF gbLF, tag GBControl gbControl
        , tag GBExtend gbExtend, tag GBZWJ gbZWJ
        , tag GBRegionalIndicator gbRegionalIndicator
        , tag GBPrepend gbPrepend, tag GBSpacingMark gbSpacingMark
        , tag GBL gbL, tag GBV gbV, tag GBT gbT, tag GBLV gbLV, tag GBLVT gbLVT
        ]
    tag p = map (\(lo, hi) -> (lo, hi, p))

mkArr :: [(Int, Int)] -> Array Int (Int, Int)
mkArr xs = listArray (0, length xs - 1) xs

-- | Binary search: is @x@ inside any inclusive range in the (ascending) array?
-- | Total array indexing: 'Nothing' when the index is out of bounds. The
-- binary searches below only ever index in bounds, but this keeps the partial
-- @(!)@ encapsulated so no caller uses a partial function.
atMay :: Array Int e -> Int -> Maybe e
atMay arr i
  | inRange (bounds arr) i = Just (arr ! i)
  | otherwise              = Nothing

inRanges :: Array Int (Int, Int) -> Int -> Bool
inRanges arr x = go lo0 hi0
  where
    (lo0, hi0) = bounds arr
    go lo hi
      | lo > hi = False
      | otherwise =
          case atMay arr ((lo + hi) `div` 2) of
            Nothing     -> False          -- unreachable: lo <= mid <= hi
            Just (a, b)
              | x < a     -> go lo (mid - 1)
              | x > b     -> go (mid + 1) hi
              | otherwise -> True
          where mid = (lo + hi) `div` 2

-- | Binary search returning the payload of the range containing @x@, if any.
searchRanges :: Array Int (Int, Int, a) -> Int -> Maybe a
searchRanges arr x = go lo0 hi0
  where
    (lo0, hi0) = bounds arr
    go lo hi
      | lo > hi = Nothing
      | otherwise =
          case atMay arr ((lo + hi) `div` 2) of
            Nothing        -> Nothing      -- unreachable: lo <= mid <= hi
            Just (a, b, v)
              | x < a     -> go lo (mid - 1)
              | x > b     -> go (mid + 1) hi
              | otherwise -> Just v
          where mid = (lo + hi) `div` 2
