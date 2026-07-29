-- | Phase III of the snippet-renderer rework: the pure core of multi-line span
-- rendering — assigning each multi-line span a /lane/ (a column in the connector
-- gutter) so that spans sharing any line never collide, and deciding what each
-- lane shows on a given line.
--
-- This module is deliberately free of glyphs, colour, and label text: it works
-- only on @(startLine, endLine)@ intervals, so the fiddly assignment algorithm
-- is fully property-testable. The graphical handler maps its labels to
-- intervals, assigns lanes here, then draws.
--
-- No compatibility guarantee (see "Tadka.Internal").
module Tadka.Internal.Renderer.Layout
  ( CellKind (..)
  , assignLanes
  , laneCount
  , cellAt
  ) where

import           Data.List (sortOn)

-- | What a lane shows at a particular line, for the span occupying it.
data CellKind
  = Open      -- ^ the span starts on this line
  | Through   -- ^ the span passes through (strictly between start and end)
  | Close     -- ^ the span ends on this line
  | Blank     -- ^ the span does not touch this line
  deriving (Eq, Show)

-- | Assign a 0-based lane to each inclusive @(start, end)@ interval by greedy
-- interval-graph colouring: intervals that share any line get distinct lanes,
-- while disjoint intervals may reuse a lane. The result is paired back in the
-- original input order, so callers can zip it against their labels.
--
-- Total: any list of intervals (including inverted or negative ones) yields an
-- assignment; no partial functions are used.
assignLanes :: [(Int, Int)] -> [(Int, (Int, Int))]
assignLanes ivs =
  let indexed  = zip [0 :: Int ..] ivs
      sorted   = sortOn (\(_, (s, _)) -> s) indexed
      assigned = go [] sorted                       -- [(originalIndex, lane)]
  in [ (laneOf i assigned, iv) | (i, iv) <- indexed ]
  where
    go :: [Int] -> [(Int, (Int, Int))] -> [(Int, Int)]
    go _        []                    = []
    go laneEnds ((idx, (s, e)) : rest) =
      let lane = pickLane laneEnds s
      in (idx, lane) : go (setLane lane e laneEnds) rest

    laneOf i assigned = case lookup i assigned of
      Just l  -> l
      Nothing -> 0        -- unreachable: every index is assigned

-- | Lowest lane whose current occupant ends before this span starts, or a new
-- lane (the current count) if none is free.
pickLane :: [Int] -> Int -> Int
pickLane ends s = go 0 ends
  where
    go i []       = i
    go i (e : es)
      | e < s     = i
      | otherwise = go (i + 1) es

-- | Record that @lane@ now ends at @e@, extending the list if it is a new lane.
setLane :: Int -> Int -> [Int] -> [Int]
setLane lane e ends =
  let (before, after) = splitAt lane ends
  in before ++ [e] ++ drop 1 after

-- | Number of lanes an assignment uses (0 when empty).
laneCount :: [(Int, (Int, Int))] -> Int
laneCount = foldr (\(l, _) acc -> max (l + 1) acc) 0

-- | What an interval shows on a given line. Total.
cellAt :: (Int, Int) -> Int -> CellKind
cellAt (s, e) l
  | l == s && s < e        = Open
  | l == e && s < e        = Close
  | s < l && l < e         = Through
  | otherwise              = Blank
