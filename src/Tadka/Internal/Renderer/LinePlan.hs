-- | Phase II of the snippet-renderer rework: a pure planner that decides which
-- source lines to render and where to elide, independent of any glyph drawing.
--
-- Keeping this as a pure IR between resolved labels and rendered text means the
-- fiddly selection logic (context windows, window merging, gap elision) is
-- fully property-testable without producing a single character of output.
--
-- No compatibility guarantee (see "Tadka.Internal").
module Tadka.Internal.Renderer.LinePlan
  ( PlanEntry (..)
  , planLines
  , mergeIntervals
  ) where

import           Data.List (sortOn)

-- | One entry in a rendered snippet: either show a specific 1-based source
-- line, or elide a run of @n@ hidden lines (@n >= 1@).
data PlanEntry
  = ShowLine !Int
  | ElideLines !Int
  deriving (Eq, Show)

-- | Decide which 1-based source lines to render for a set of labelled anchor
-- lines, given the total number of available lines.
--
--   * 'Nothing' context: render the contiguous range from the lowest to the
--     highest anchor — the historical behaviour, with no elision.
--   * @'Just' ctx@: render each anchor with @ctx@ lines of context above and
--     below, merge overlapping or adjacent windows, and elide the gaps between
--     them.
--
-- Total: an empty anchor list yields an empty plan, negative context is treated
-- as zero, and every line number is clamped to @[1, total]@.
planLines :: Maybe Int -> Int -> [Int] -> [PlanEntry]
planLines _ _ [] = []
planLines Nothing total anchors@(a : as)
  | hi - lo + 1 <= safetyCap = [ ShowLine l | l <- [lo .. hi] ]
  | otherwise                = planLines (Just fallbackContext) total anchors  -- bound huge ranges
  where
    lo = clamp1 (foldr min a as)
    hi = clampTop total (foldr max a as)
planLines (Just ctx) total (a : as) =
  emit (mergeIntervals [ (clamp1 (x - c), clampTop total (x + c)) | x <- a : as ])
  where c = max 0 ctx

-- | Upper bound on lines rendered contiguously (with no context set) before
-- falling back to a context window with elision. This keeps a span across a
-- huge line range from producing output proportional to the span rather than
-- to the diagnostic — every labelled line still appears, the gaps are elided.
safetyCap :: Int
safetyCap = 100

fallbackContext :: Int
fallbackContext = 2

-- | Merge inclusive intervals, combining those that overlap or merely touch
-- (a gap of at most one line), returned sorted and disjoint.
mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
mergeIntervals ivs = go (sortOn fst ivs)
  where
    go []  = []
    go [i] = [i]
    go ((lo, hi) : (lo2, hi2) : rest)
      | lo2 <= hi + 1 = go ((lo, max hi hi2) : rest)
      | otherwise     = (lo, hi) : go ((lo2, hi2) : rest)

-- | Turn disjoint, sorted intervals into a plan, eliding the gaps between them.
emit :: [(Int, Int)] -> [PlanEntry]
emit []                              = []
emit [(lo, hi)]                      = [ ShowLine l | l <- [lo .. hi] ]
emit ((lo, hi) : nxt@((lo2, _) : _)) =
     [ ShowLine l   | l <- [lo .. hi] ]
  ++ [ ElideLines g | let g = lo2 - hi - 1, g > 0 ]
  ++ emit nxt

clamp1 :: Int -> Int
clamp1 = max 1

clampTop :: Int -> Int -> Int
clampTop total = min (max 1 total)
