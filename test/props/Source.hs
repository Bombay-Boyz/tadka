{-# LANGUAGE OverloadedStrings #-}

-- | Phase I — the pluggable 'SourceCode' seam. The canonical 'NamedSource'
-- instance is total and its windowed reads agree, by definition, with a filter
-- of the full line enumeration.
module Source (group) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Tadka                      (NamedSource, mkNamedSource, sourceName,
                                             sourceText)
import           Tadka.Internal.SourceCode  (SourceCode (..))

group :: Group
group = Group "SourceCode (Phase I)"
  [ ("scName is the source name",            prop_name)
  , ("scLines (1, huge) enumerates all lines", prop_full)
  , ("scLines window == filtered enumeration", prop_window)
  ]

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

genSource :: Gen NamedSource
genSource = do
  name <- Gen.text (Range.linear 1 8) Gen.alpha
  segs <- Gen.list (Range.linear 0 6) (Gen.text (Range.linear 0 10) (Gen.filterT (/= '\n') Gen.ascii))
  pure (rightOrErr (mkNamedSource name (T.intercalate (T.singleton '\n') segs)))

allLines :: NamedSource -> [(Int, Text)]
allLines ns = zip [1 ..] (map dropCR (T.splitOn (T.singleton '\n') (sourceText ns)))
  where dropCR t = case T.stripSuffix (T.singleton '\r') t of
          Just t' -> t'
          Nothing -> t

prop_name :: Property
prop_name = property $ do
  ns <- forAll genSource
  scName ns === sourceName ns

prop_full :: Property
prop_full = property $ do
  ns <- forAll genSource
  scLines ns (1, 1000000) === allLines ns

-- The defining property, exercised over arbitrary (including negative/inverted)
-- ranges: a window is exactly the lines of the full enumeration within it.
prop_window :: Property
prop_window = property $ do
  ns <- forAll genSource
  lo <- forAll (Gen.int (Range.linear (-3) 12))
  hi <- forAll (Gen.int (Range.linear (-3) 12))
  scLines ns (lo, hi) === [ p | p@(n, _) <- allLines ns, n >= lo, n <= hi ]
