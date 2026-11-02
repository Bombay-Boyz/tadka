
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
    -- * Grapheme clustering (UAX #29), built from the two properties above
  , graphemeClusters
  , clusterWidth
    -- * Provenance
  , ucdVersion
  ) where

import           Data.Array            (Array, bounds, inRange, listArray, (!))
import           Data.Char             (ord)
import           Data.List             (sortOn)
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB
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


charWidth :: Char -> Int
charWidth c =
  let x = ord c
  in if inRanges zeroArr x
       then 0
       else if inRanges wideArr x then 2 else 1

-- | Display width of a 'Text': the sum of its grapheme clusters' widths (see
-- 'graphemeClusters' and 'clusterWidth'). Text in which no cluster can span
-- more than one code point (see 'isSimple') takes a per-character fast path
-- that gives the identical answer without segmenting.
textWidth :: Text -> Int
textWidth t
  | isSimple t = T.foldl' (\acc c -> acc + charWidth c) 0 t
  | otherwise  = sum (map clusterWidth (graphemeClusters t))

-- | True when every code point is below U+0300 and none is a carriage
-- return. The generated table has no Extend, ZWJ, SpacingMark, Prepend,
-- Regional_Indicator or Hangul-jamo code point below U+0300, so the only
-- way two such characters could share a cluster is CR LF; excluding CR
-- therefore makes every character its own cluster. This is the common case
-- (ASCII and Latin-1 source lines) and is what the fast paths below test.
isSimple :: Text -> Bool
isSimple = T.all (\c -> c < '\x300' && c /= '\r')

-- | The 0-based display column reached after the first @n@ characters of a
-- line, expanding tabs to the next multiple of the tab width and counting
-- every other grapheme cluster by its 'clusterWidth'. Total: a tab width
-- below 1 is treated as 1, and @n@ is clamped to the line. This is exactly
-- @textWidth@ of the tab-expanded prefix (see 'expandTabs'), so a caret
-- placed at this column sits under the rendered source. A tab character
-- always forms its own single-character cluster (its Grapheme_Cluster_Break
-- value is @Control@, which UAX #29 never merges with a neighbour either
-- side), so clustering a line containing tabs never interferes with this
-- function's tab-stop arithmetic.
displayColumnAt :: Int -> Text -> Int -> Int
displayColumnAt tw line n
  | isSimple prefix = T.foldl' stepChar 0 prefix
  | otherwise       = go 0 (graphemeClusters prefix)
  where
    prefix = T.take (max 0 n) line
    w      = max 1 tw

    stepChar :: Int -> Char -> Int
    stepChar col c
      | c == '\t' = col + (w - (col `mod` w))
      | otherwise = col + charWidth c

    go :: Int -> [Text] -> Int
    go !col []         = col
    go !col (cl : cls)
      | cl == tabCluster = go (col + (w - (col `mod` w))) cls
      | otherwise        = go (col + clusterWidth cl) cls

tabCluster :: Text
tabCluster = T.singleton '\t'

-- | Expand tabs in a line to spaces, honouring tab stops at multiples of the
-- tab width and the display width of preceding grapheme clusters (see
-- 'displayColumnAt'). The result contains no tab characters, and its
-- 'textWidth' equals @'displayColumnAt' tw line (T.length line)@. Total; a
-- tab width below 1 is treated as 1.
expandTabs :: Int -> Text -> Text
expandTabs tw line
  | not (T.any (== '\t') line) = line   -- clusters concatenate back to the input
  | isSimple line = TL.toStrict (TB.toLazyText (snd (T.foldl' stepChar (0, mempty) line)))
  | otherwise     = TL.toStrict (TB.toLazyText (go 0 mempty (graphemeClusters line)))
  where
    w = max 1 tw

    pad :: Int -> TB.Builder
    pad n = TB.fromText (T.replicate n (T.singleton ' '))

    stepChar :: (Int, TB.Builder) -> Char -> (Int, TB.Builder)
    stepChar (!col, acc) c
      | c == '\t' = let n = w - (col `mod` w) in (col + n, acc <> pad n)
      | otherwise = (col + charWidth c, acc <> TB.singleton c)

    go :: Int -> TB.Builder -> [Text] -> TB.Builder
    go !_   acc []         = acc
    go !col acc (cl : cls)
      | cl == tabCluster =
          let n = w - (col `mod` w) in go (col + n) (acc <> pad n) cls
      | otherwise = go (col + clusterWidth cl) (acc <> TB.fromText cl) cls

-- | The Grapheme_Cluster_Break property of a code point, or 'GBOther'.
graphemeBreakProperty :: Char -> GBProp
graphemeBreakProperty c = fromMaybe GBOther (searchRanges gbArr (ord c))

-- | Whether a code point has the @Extended_Pictographic@ property (needed by
-- UAX #29 rule GB11).
isExtendedPictographic :: Char -> Bool
isExtendedPictographic c = inRanges extPictArr (ord c)

-- === Grapheme clustering (UAX #29) =========================================
--
-- The segmentation below implements rules GB3-GB9b, GB11, and GB12/GB13 of
-- UAX #29 (<https://www.unicode.org/reports/tr29/>), using only this
-- module's own 'graphemeBreakProperty'/'isExtendedPictographic' — the two
-- properties the generated table already carries. GB9c (the
-- Indic_Conjunct_Break rule, for a handful of Brahmic scripts) and GB10
-- (the deprecated emoji-modifier rule GB10 was withdrawn from recent UAX #29
-- revisions and replaced by extending GB9's Extend handling) are
-- intentionally out of scope: nothing in the generated table tracks
-- Indic_Conjunct_Break, and adding it would mean generating a third table
-- this fix has no data for, not wiring up an existing one. What's here
-- covers every case this fix's gold-standard review called out by name:
-- decomposed Hangul syllables (GB6/7/8) and Extended_Pictographic ZWJ
-- sequences (GB9/GB11).

-- | Which of three states an @Extended_Pictographic Extend*@ run is in,
-- tracked through a cluster so GB11 (@ExtPict Extend* ZWJ x ExtPict@) can
-- be applied by looking only at the state carried from the previous
-- character, never by re-scanning backwards.
data ExtPictRun = XPNone | XPOpen | XPClosedByZWJ
  deriving (Eq)

-- | Per-character state threaded through 'graphemeClusters', carrying
-- exactly what the next character's break decision needs: this character's
-- own 'GBProp', the state of any open @Extended_Pictographic Extend*@ run
-- (GB11), and whether an odd, as-yet-unpaired run of
-- 'GBRegionalIndicator' characters ends here (GB12\/GB13).
data ClusterState = ClusterState
  { csProp       :: !GBProp
  , csExtPictRun :: !ExtPictRun
  , csRIOdd      :: !Bool
  }

startState :: Char -> ClusterState
startState c = ClusterState
  { csProp       = p
  , csExtPictRun = if isExtendedPictographic c then XPOpen else XPNone
  , csRIOdd      = p == GBRegionalIndicator
  }
  where p = graphemeBreakProperty c

-- | The state after a character has been decided to join the current
-- cluster (i.e. 'breakBefore' said not to break before it).
advanceState :: ClusterState -> Char -> ClusterState
advanceState prev c = ClusterState
  { csProp       = p
  , csExtPictRun = nextExtPictRun (csExtPictRun prev) c p
  , csRIOdd      = p == GBRegionalIndicator && not (csRIOdd prev)
  }
  where p = graphemeBreakProperty c

nextExtPictRun :: ExtPictRun -> Char -> GBProp -> ExtPictRun
nextExtPictRun run c p
  | isExtendedPictographic c     = XPOpen
  | run == XPOpen, p == GBExtend = XPOpen
  | run == XPOpen, p == GBZWJ    = XPClosedByZWJ
  | otherwise                    = XPNone

-- | UAX #29 rules GB3-GB9b\/GB11\/GB12\/GB13: should a cluster boundary be
-- inserted between the character 'ClusterState' describes and the next
-- character @c@? 'False' means @c@ joins the current cluster. Rules are
-- tried in the Unicode-specified order; the first that applies decides.
breakBefore :: ClusterState -> Char -> Bool
breakBefore prev c
  | p1 == GBCR, p2 == GBLF                               = False -- GB3
  | p1 `elem` [GBControl, GBCR, GBLF]                     = True  -- GB4
  | p2 `elem` [GBControl, GBCR, GBLF]                     = True  -- GB5
  | p1 == GBL, p2 `elem` [GBL, GBV, GBLV, GBLVT]          = False -- GB6
  | p1 `elem` [GBLV, GBV], p2 `elem` [GBV, GBT]           = False -- GB7
  | p1 `elem` [GBLVT, GBT], p2 == GBT                     = False -- GB8
  | p2 == GBExtend || p2 == GBZWJ                         = False -- GB9
  | p2 == GBSpacingMark                                   = False -- GB9a
  | p1 == GBPrepend                                       = False -- GB9b
  | csExtPictRun prev == XPClosedByZWJ, isExtendedPictographic c
                                                            = False -- GB11
  | p1 == GBRegionalIndicator, p2 == GBRegionalIndicator, csRIOdd prev
                                                            = False -- GB12/GB13
  | otherwise                                              = True  -- GB999
  where
    p1 = csProp prev
    p2 = graphemeBreakProperty c

-- | Segment text into extended grapheme clusters (UAX #29), using this
-- module's own 'graphemeBreakProperty'\/'isExtendedPictographic'. Every
-- returned 'Text' is non-empty, and concatenating the result reproduces the
-- input exactly: total, and a no-op partition (one cluster per character)
-- on text with no combining marks, ZWJ sequences, Hangul jamo, or regional
-- indicators — every plain-ASCII line segments exactly as it always
-- implicitly did before this was wired in.
graphemeClusters :: Text -> [Text]
graphemeClusters t = case T.uncons t of
  Nothing        -> []
  Just (c0, rest) -> map T.pack (go (startState c0) [c0] (T.unpack rest))
  where
    go :: ClusterState -> [Char] -> [Char] -> [[Char]]
    go _  acc []       = [reverse acc]
    go st acc (c : cs)
      | breakBefore st c = reverse acc : go (startState c) [c] cs
      | otherwise        = go (advanceState st c) (c : acc) cs

-- | Display width of one grapheme cluster (as produced by 'graphemeClusters').
--
-- A single-code-point cluster is exactly 'charWidth' of that character. The
-- @Extended_Pictographic@ property is deliberately /not/ treated as "wide":
-- it also covers narrow symbols such as (c), (R), TM, arrows and dingbats,
-- which terminals draw in one cell. A multi-code-point cluster is two cells
-- only when it is an emoji presentation sequence (see 'emojiPresentation');
-- otherwise it takes the width of its leading code point, so a decomposed
-- Hangul syllable counts once, at its leading jamo's width.
clusterWidth :: Text -> Int
clusterWidth cluster = case T.uncons cluster of
  Nothing -> 0
  Just (c0, rest)
    | T.null rest               -> charWidth c0
    | emojiPresentation c0 rest -> 2
    | otherwise                 -> charWidth c0

-- | Whether a multi-code-point cluster (leading code point and the rest)
-- renders as a two-cell emoji: a flag (two regional indicators), a base
-- followed by the emoji variation selector U+FE0F (this includes keycaps),
-- a base followed by a skin-tone modifier, or a ZWJ sequence of pictographs.
emojiPresentation :: Char -> Text -> Bool
emojiPresentation c0 rest =
     (isRI c0 && T.any isRI rest)
  || (basePict && T.any (== '\xFE0F') rest)
  || (basePict && T.any isEmojiModifier rest)
  || (basePict && T.any (== '\x200D') rest && T.any isExtendedPictographic rest)
  where
    basePict = isExtendedPictographic c0 || isKeycapBase c0
    isRI c   = graphemeBreakProperty c == GBRegionalIndicator
    isEmojiModifier c = c >= '\x1F3FB' && c <= '\x1F3FF'
    isKeycapBase c    = c == '#' || c == '*' || (c >= '0' && c <= '9')

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
