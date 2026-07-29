{-# LANGUAGE OverloadedStrings #-}
-- | Phase III core — lane assignment. The load-bearing proof is that any two
-- distinct spans placed on the same lane have disjoint line ranges
-- (collision-free interval colouring); supporting properties pin coverage,
-- lane contiguity, and the cell classifier.
module LayoutSpec (group) where

import           Hedgehog
import qualified Hedgehog.Gen                    as Gen
import qualified Hedgehog.Range                  as Range

import           Tadka.Internal.Renderer.Layout  (CellKind (..), assignLanes,
                                                  cellAt, laneCount)

group :: Group
group = Group "Layout: lane assignment (Phase III)"
  [ ("same lane implies disjoint line ranges", prop_noCollision)
  , ("one lane per input interval, in order",  prop_coverage)
  , ("lanes are contiguous 0..count-1",        prop_contiguous)
  , ("cellAt classifies open/through/close",   prop_cellAt)
  ]

genIv :: Gen (Int, Int)
genIv = do
  a <- Gen.int (Range.linear 1 12)
  b <- Gen.int (Range.linear 1 12)
  pure (min a b, max a b)

genIvs :: Gen [(Int, Int)]
genIvs = Gen.list (Range.linear 0 8) genIv

overlaps :: (Int, Int) -> (Int, Int) -> Bool
overlaps (s1, e1) (s2, e2) = s1 <= e2 && s2 <= e1

-- THE proof: two distinct assigned spans on one lane never share a line.
prop_noCollision :: Property
prop_noCollision = property $ do
  ivs <- forAll genIvs
  let assigned = zip [0 :: Int ..] (assignLanes ivs)   -- (position, (lane, iv))
  assert $ and
    [ not (overlaps a b)
    | (i, (la, a)) <- assigned
    , (j, (lb, b)) <- assigned
    , i < j
    , la == lb
    ]

prop_coverage :: Property
prop_coverage = property $ do
  ivs <- forAll genIvs
  map snd (assignLanes ivs) === ivs        -- one entry per input, order preserved

prop_contiguous :: Property
prop_contiguous = property $ do
  ivs <- forAll genIvs
  let assigned = assignLanes ivs
      lanes    = map fst assigned
      n        = laneCount assigned
  assert (all (>= 0) lanes)
  assert (all (< n) lanes)
  assert (null lanes || all (`elem` lanes) [0 .. n - 1])   -- no unused lane below the count

prop_cellAt :: Property
prop_cellAt = property $ do
  (s, e) <- forAll (Gen.filterT (\(a, b) -> a < b) genIv)
  l      <- forAll (Gen.int (Range.linear 0 14))
  cellAt (s, e) s === Open
  cellAt (s, e) e === Close
  let expected | l == s         = Open
               | l == e         = Close
               | s < l && l < e = Through
               | otherwise      = Blank
  cellAt (s, e) l === expected
