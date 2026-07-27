{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase 5 properties: width-aware caret layout, palette cycling, and a
-- totality smoke check for the graphical handler.
module Phase5 (group) where

import           Control.Exception             (SomeException, evaluate, try)
import           Control.Monad.IO.Class         (liftIO)
import qualified Data.List.NonEmpty             as NE
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Prettyprinter                  (LayoutOptions (..),
                                                 PageWidth (Unbounded), layoutPretty)
import           Prettyprinter.Render.Terminal  (AnsiStyle, Color (..), color)
import           Prettyprinter.Render.Text      (renderStrict)

import           Hedgehog
import qualified Hedgehog.Gen                   as Gen
import qualified Hedgehog.Range                 as Range

import           Tadka
import           Tadka.Internal.Renderer.Graphical (caretGlyph, caretLayout, labelStyle)
import           Tadka.Internal.Width           (textWidth)

import           GenDiag                        (genGD, genLine, selfJust, selfNothing)

group :: Group
group = Group "Phase 5 - graphical handler"
  [ ("caret layout is non-negative and width-aware", prop_caretLayout)
  , ("caret glyph distinguishes primary/secondary",  prop_caretGlyph)
  , ("label palette cycles as (i mod p)",             prop_paletteCycling)
  , ("graphical render is total (fuelled diagnostics)", prop_totality)
  , ("graphical render is total (pathological cycles)", prop_totalityCycles)
  ]

-- === caret layout =========================================================

prop_caretLayout :: Property
prop_caretLayout = property $ do
  line <- forAll genLine
  tabW <- forAll (Gen.int (Range.linear 1 8))
  let n = T.length line
  startCol <- forAll (Gen.int (Range.linear 1 (n + 1)))
  spanLen  <- forAll (Gen.int (Range.linear 0 (n + 5)))
  let (dispStart, caretWidth) = caretLayout tabW line startCol spanLen
  -- never negative, never collapses
  assert (dispStart >= 0)
  assert (caretWidth >= 1)
  -- width-aware: for tab-free text (genLine emits none) the display offset is
  -- exactly the prefix width; the tab-inclusive case is proved in the Tabs group
  dispStart === textWidth (T.take (startCol - 1) line)
  -- caret starts within (or at the end of) the line's display extent, so it
  -- can never intrude on the fixed line-number gutter to its left
  assert (dispStart <= textWidth line)

prop_caretGlyph :: Property
prop_caretGlyph = withTests 1 . property $ do
  caretGlyph Primary   === '^'
  caretGlyph Secondary === '-'

-- === palette cycling ======================================================

palColors :: [AnsiStyle]
palColors = map color [Red, Green, Yellow, Blue, Magenta, Cyan, White, Black]

prop_paletteCycling :: Property
prop_paletteCycling = property $ do
  p <- forAll (Gen.int (Range.linear 1 (length palColors)))
  i <- forAll (Gen.int (Range.linear 0 60))
  let palette = NE.fromList (take p palColors)
  labelStyle palette i === (NE.toList palette !! (i `mod` p))

-- === totality =============================================================

renderText :: Diagnostic e => e -> Text
renderText e = case selectRenderer (withColorMode ColorNever (withTarget TGraphical defaultConfig)) of
  SomeRenderer r@(Graphical _) ->
    renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _ -> ""

prop_totality :: Property
prop_totality = property $ do
  fuel <- forAll (Gen.int (Range.linear 0 3))
  d    <- forAllWith (const "<generated diagnostic>") (genGD fuel)
  res  <- liftIO (try (evaluate (T.length (renderText d))) :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure

prop_totalityCycles :: Property
prop_totalityCycles = withTests 1 . property $ do
  res <- liftIO (try (evaluate (sum (map (T.length . renderText) [selfNothing, selfJust])))
                   :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure
