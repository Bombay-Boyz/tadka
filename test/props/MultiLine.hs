{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Phase III end-to-end: rendering multi-line spans. The invariant proved here
-- ties the pure lane engine to actual output — every multi-line span draws
-- exactly one opening corner and one closing corner (every opened lane closes),
-- regardless of how lanes overlap.
module MultiLine (group) where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Tadka
import           Tadka.Internal             (buildContext)

group :: Group
group = Group "Multi-line rendering (Phase III)"
  [ ("every multi-line span opens and closes once", prop_balanced)
  , ("a multi-line label's text is shown",          prop_labelShown)
  ]

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- 10 lines of 5 chars each; line L (1-based) starts at offset (L-1)*6.
srcTen :: NamedSource
srcTen = rightOrErr (mkNamedSource "m.hs" (T.intercalate (T.singleton '\n') (replicate 10 "aaaaa")))

spanOfLines :: Int -> Int -> Span
spanOfLines a b = rightOrErr (mkSpan start (endOff - start))
  where start  = (a - 1) * 6
        endOff = (b - 1) * 6 + 3

data ML = ML Context
instance Diagnostic ML where
  message _   = "multi-line diagnostic"
  context (ML c) = c

mkDiag :: [(Int, Int)] -> ML
mkDiag pairs = ML (buildContext srcTen [ (spanOfLines a b, Just "here") | (a, b) <- pairs ])

gfx :: Diagnostic e => e -> Text
gfx e = case selectRenderer (withColorMode ColorNever (withUnicodeMode UnicodeAlways (withTarget TGraphical defaultConfig))) of
  SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _                            -> ""

open, close :: Text
open  = T.singleton '\x256D'   -- ╭
close = T.singleton '\x2570'   -- ╰

genPairs :: Gen [(Int, Int)]
genPairs = Gen.list (Range.linear 0 4) $ do
  a <- Gen.int (Range.linear 1 9)
  b <- Gen.int (Range.linear (a + 1) 10)   -- strictly multi-line
  pure (a, b)

prop_balanced :: Property
prop_balanced = property $ do
  pairs <- forAll genPairs
  let out = gfx (mkDiag pairs)
  T.count open  out === length pairs
  T.count close out === length pairs

prop_labelShown :: Property
prop_labelShown = withTests 1 . property $ do
  let out = gfx (mkDiag [(2, 5)])
  assert ("here" `T.isInfixOf` out)
  assert (T.count open out == 1 && T.count close out == 1)
