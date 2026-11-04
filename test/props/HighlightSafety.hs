{-# LANGUAGE OverloadedStrings #-}

-- | The renderer's safety rule for highlighters: a highlighted line is used
-- only if removing its SGR colour escapes gives back exactly the plain line.
-- These properties cover that rule on its own; "HighlightRender" checks it
-- end to end through the graphical renderer.
module HighlightSafety (group) where

import           Data.Text               (Text)
import qualified Data.Text               as T

import           Hedgehog
import qualified Hedgehog.Gen            as Gen
import qualified Hedgehog.Range          as Range

import           Tadka.Internal.Config   (Highlighter (..))
import           Tadka.Internal.Highlight (applyHighlighter, highlightLine, stripSgr)

group :: Group
group = Group "Highlighter safety rule"
  [ ("stripSgr removes exactly the SGR escapes",                  prop_stripInverse)
  , ("a line that is the plain line plus SGR is accepted",        prop_acceptsDecorated)
  , ("a line with any other escape sequence is rejected",         prop_rejectsInjected)
  , ("whatever a highlighter returns, the text is the plain text", prop_invariant)
  , ("highlightLine indexes lines from one and falls back outside", prop_indexing)
  , ("applyHighlighter rejects a wrong line count",               prop_wrongCount)
  , ("applyHighlighter passes a right-sized answer through",      prop_rightCount)
  ]

-- | Text with no ESC in it.
genPlain :: Gen Text
genPlain = Gen.text (Range.linear 0 12) $ Gen.frequency
  [ (6, Gen.enum ' ' '~')
  , (1, Gen.enum '\x4E00' '\x4E30')
  ]

genSgr :: Gen Text
genSgr = Gen.element
  [ "\ESC[0m", "\ESC[m", "\ESC[1;31m", "\ESC[38;5;196m", "\ESC[38;2;1;2;3m", "\ESC[49m" ]

-- | A line built from plain pieces and SGR pieces, as @(plain, shown)@ pairs:
-- the first component is what the text is, the second what it looks like.
genDecorated :: Gen [(Text, Text)]
genDecorated = Gen.list (Range.linear 0 8) $ Gen.choice
  [ (\t -> (t, t)) <$> genPlain
  , (\s -> (T.empty, s)) <$> genSgr
  ]

plainOf, shownOf :: [(Text, Text)] -> Text
plainOf = T.concat . map fst
shownOf = T.concat . map snd

genInjection :: Gen Text
genInjection = Gen.element
  [ "\ESC[2J"            -- clear screen
  , "\ESC]0;pwned\a"     -- set window title (OSC)
  , "\ESC[?1049h"        -- alternate screen
  , "\ESC(0"             -- charset switch
  , "\ESC"               -- a bare ESC
  , "\ESC[31"            -- an SGR with no terminating m
  ]

wrap :: Text -> Text
wrap l = "\ESC[1m" <> l <> "\ESC[0m"

prop_stripInverse :: Property
prop_stripInverse = property $ do
  ps <- forAll genDecorated
  stripSgr (shownOf ps) === plainOf ps

prop_acceptsDecorated :: Property
prop_acceptsDecorated = property $ do
  ps <- forAll genDecorated
  highlightLine (Just [shownOf ps]) 1 (plainOf ps) === shownOf ps

prop_rejectsInjected :: Property
prop_rejectsInjected = property $ do
  ps  <- forAll genDecorated
  bad <- forAll genInjection
  let plain = plainOf ps
  highlightLine (Just [shownOf ps <> bad]) 1 plain === plain
  highlightLine (Just [bad <> shownOf ps]) 1 plain === plain

-- | The property that matters: for /any/ answer, the text that reaches the
-- terminal is the plain text once colours are removed.
prop_invariant :: Property
prop_invariant = property $ do
  p <- forAll genPlain
  h <- forAll (Gen.choice [genHostile, (p <>) <$> genHostile, (<> p) <$> genHostile])
  stripSgr (highlightLine (Just [h]) 1 p) === p
  where
    genHostile = Gen.text (Range.linear 0 12) (Gen.element ("\ESC[;0123456789mJabc " :: String))

prop_indexing :: Property
prop_indexing = property $ do
  ls <- forAll (Gen.list (Range.linear 1 6) genPlain)
  n  <- forAll (Gen.int (Range.linear (-2) 9))
  let inRange = n >= 1 && n <= length ls
      plainAt = if inRange then ls !! (n - 1) else "fallback"
  highlightLine (Just (map wrap ls)) n plainAt
    === (if inRange then wrap plainAt else plainAt)

prop_wrongCount :: Property
prop_wrongCount = property $ do
  ls <- forAll (Gen.list (Range.linear 0 6) genPlain)
  k  <- forAll (Gen.int (Range.linear 0 8))
  let h = Highlighter "short-or-long" (\_ _ -> Just (take k (cycle ["x"])))
  (applyHighlighter h "f" ls == Nothing) === (k /= length ls)

prop_rightCount :: Property
prop_rightCount = property $ do
  ls <- forAll (Gen.list (Range.linear 0 6) genPlain)
  let h = Highlighter "wrap" (\_ xs -> Just (map wrap xs))
  applyHighlighter h "f" ls === Just (map wrap ls)
