{-# LANGUAGE OverloadedStrings #-}

-- | Tab-stop expansion (post-v1 hardening): tabs in rendered source lines
-- expand to the next tab stop, and carets align under the /expanded/ source.
-- The alignment property is the mathematical statement of the fix: a caret's
-- leading-space count equals the display width of the tab-expanded source that
-- precedes the span.
module Tabs (group) where

import           Data.Text                          (Text)
import qualified Data.Text                          as T

import           Hedgehog
import qualified Hedgehog.Gen                       as Gen
import qualified Hedgehog.Range                     as Range

import           Tadka.Internal.Renderer.Graphical  (caretLayout)
import           Tadka.Internal.Width               (displayColumnAt, expandTabs, textWidth)

group :: Group
group = Group "Tab-aware source rendering"
  [ ("expandTabs leaves no tab characters",        prop_noTabs)
  , ("expanded width == displayColumnAt of line",  prop_widthConsistent)
  , ("displayColumnAt is monotonic",               prop_monotonic)
  , ("a tab always lands on a tab stop",           prop_tabStop)
  , ("caret aligns under the expanded source",     prop_alignment)
  , ("caret layout stays non-negative with tabs",  prop_nonNeg)
  ]

-- Unicode-hard text that also includes tabs.
genScalar :: Gen Char
genScalar = Gen.frequency
  [ (4, Gen.filterT (\c -> c /= '\n') (Gen.enum ' ' '~'))
  , (3, pure '\t')
  , (2, Gen.enum '\x0300' '\x036F')   -- combining marks (width 0)
  , (2, Gen.enum '\x4E00' '\x4E30')   -- CJK (width 2)
  , (1, Gen.enum '\x1F600' '\x1F610') -- emoji (width 2)
  ]

genLine :: Gen Text
genLine = Gen.text (Range.linear 0 40) genScalar

genTabW :: Gen Int
genTabW = Gen.int (Range.linear 1 8)

prop_noTabs :: Property
prop_noTabs = property $ do
  tw   <- forAll genTabW
  line <- forAll genLine
  assert (not (T.any (== '\t') (expandTabs tw line)))

prop_widthConsistent :: Property
prop_widthConsistent = property $ do
  tw   <- forAll genTabW
  line <- forAll genLine
  textWidth (expandTabs tw line) === displayColumnAt tw line (T.length line)

prop_monotonic :: Property
prop_monotonic = property $ do
  tw   <- forAll genTabW
  line <- forAll genLine
  let n = T.length line
  a <- forAll (Gen.int (Range.linear 0 n))
  b <- forAll (Gen.int (Range.linear 0 n))
  let (lo, hi) = (min a b, max a b)
  assert (displayColumnAt tw line lo <= displayColumnAt tw line hi)

-- The defining property of a tab stop: the column immediately after a tab is a
-- multiple of the tab width.
prop_tabStop :: Property
prop_tabStop = property $ do
  tw   <- forAll genTabW
  line <- forAll genLine
  let tabIdxs = [ i | (i, c) <- zip [0 ..] (T.unpack line), c == '\t' ]
  mapM_ (\i -> displayColumnAt tw line (i + 1) `mod` tw === 0) tabIdxs

-- THE alignment guarantee: the caret's display offset equals the width of the
-- tab-expanded source preceding the span, so the caret sits under it exactly.
prop_alignment :: Property
prop_alignment = property $ do
  tw   <- forAll genTabW
  line <- forAll genLine
  let n = T.length line
  startCol <- forAll (Gen.int (Range.linear 1 (n + 1)))
  spanLen  <- forAll (Gen.int (Range.linear 0 (n + 5)))
  let (dispStart, _) = caretLayout tw line startCol spanLen
  dispStart === textWidth (expandTabs tw (T.take (startCol - 1) line))

prop_nonNeg :: Property
prop_nonNeg = property $ do
  tw   <- forAll genTabW
  line <- forAll genLine
  let n = T.length line
  startCol <- forAll (Gen.int (Range.linear 1 (n + 1)))
  spanLen  <- forAll (Gen.int (Range.linear 0 (n + 5)))
  let (dispStart, caretWidth) = caretLayout tw line startCol spanLen
  assert (dispStart >= 0)
  assert (caretWidth >= 1)
