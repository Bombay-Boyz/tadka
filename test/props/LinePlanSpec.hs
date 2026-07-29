{-# LANGUAGE OverloadedStrings #-}

-- | Phase II — the pure line planner. Properties pin selection/elision without
-- rendering: the plan is total, 'Nothing' reproduces the contiguous range with
-- no elision, and with context every in-range anchor is shown, line numbers are
-- strictly increasing and in bounds, and every elision hides at least one line.
module LinePlanSpec (group) where

import           Hedgehog
import qualified Hedgehog.Gen                     as Gen
import qualified Hedgehog.Range                   as Range

import           Tadka.Internal.Renderer.LinePlan (PlanEntry (..), planLines)

group :: Group
group = Group "Line plan (Phase II)"
  [ ("empty anchors give an empty plan",        prop_empty)
  , ("Nothing is the contiguous range, no elision", prop_contiguous)
  , ("context shows every in-range anchor",     prop_covers)
  , ("shown line numbers strictly increase",    prop_monotonic)
  , ("shown lines are within [1, total]",       prop_bounds)
  , ("every elision hides at least one line",   prop_elidePos)
  ]

genCtx :: Gen (Maybe Int)
genCtx = Gen.choice [pure Nothing, Just <$> Gen.int (Range.linear 0 4)]

genTotal :: Gen Int
genTotal = Gen.int (Range.linear 0 20)

genAnchors :: Gen [Int]
genAnchors = Gen.list (Range.linear 0 6) (Gen.int (Range.linear (-2) 22))

shown :: [PlanEntry] -> [Int]
shown p = [ l | ShowLine l <- p ]

elides :: [PlanEntry] -> [Int]
elides p = [ n | ElideLines n <- p ]

prop_empty :: Property
prop_empty = property $ do
  mc <- forAll genCtx
  t  <- forAll genTotal
  planLines mc t [] === []

prop_contiguous :: Property
prop_contiguous = property $ do
  t  <- forAll genTotal
  as <- forAll (Gen.filterT (not . null) genAnchors)
  let p   = planLines Nothing t as
      lo  = max 1 (minimum as)
      hi  = min (max 1 t) (maximum as)
  shown p === [lo .. hi]
  elides p === []

prop_covers :: Property
prop_covers = property $ do
  c  <- forAll (Gen.int (Range.linear 0 4))
  t  <- forAll genTotal
  as <- forAll genAnchors
  let p        = planLines (Just c) t as
      capped   = max 1 t
      inRange  = [ x | x <- as, x >= 1, x <= capped ]
  assert (all (`elem` shown p) inRange)

prop_monotonic :: Property
prop_monotonic = property $ do
  mc <- forAll genCtx
  t  <- forAll genTotal
  as <- forAll genAnchors
  let ls = shown (planLines mc t as)
  assert (and (zipWith (<) ls (drop 1 ls)))

prop_bounds :: Property
prop_bounds = property $ do
  mc <- forAll genCtx
  t  <- forAll genTotal
  as <- forAll genAnchors
  let ls = shown (planLines mc t as)
  assert (all (\l -> l >= 1 && l <= max 1 t) ls)

prop_elidePos :: Property
prop_elidePos = property $ do
  mc <- forAll genCtx
  t  <- forAll genTotal
  as <- forAll genAnchors
  assert (all (>= 1) (elides (planLines mc t as)))
