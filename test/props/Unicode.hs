{-# LANGUAGE OverloadedStrings #-}

-- | Display width of Unicode text. Regression tests for two defects found in
-- review: treating every @Extended_Pictographic@ code point as wide (which
-- widened (c), TM, stars and dingbats that terminals draw in one cell), and
-- measuring a flag (two regional indicators) as one cell. Also pins down that
-- the ASCII\/Latin-1 fast path agrees with grapheme segmentation.
module Unicode (group) where

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Hedgehog
import qualified Hedgehog.Gen         as Gen
import qualified Hedgehog.Range       as Range

import           Tadka.Internal.Width (charWidth, clusterWidth, displayColumnAt, expandTabs,
                                       graphemeClusters, textWidth)

group :: Group
group = Group "Unicode display width"
  [ ("narrow Extended_Pictographic symbols measure one cell", prop_narrowSymbols)
  , ("emoji sequences measure two cells",                     prop_emojiSequences)
  , ("a flag (two regional indicators) measures two cells",   prop_flags)
  , ("combining marks add no width",                          prop_combining)
  , ("East Asian wide characters measure two cells",          prop_cjk)
  , ("a Hangul syllable measures two cells, however encoded", prop_hangul)
  , ("a single character measures charWidth",                 prop_singleChar)
  , ("grapheme clusters reassemble to the input",             prop_clustersConcat)
  , ("textWidth is the sum of the cluster widths",            prop_widthIsSum)
  , ("expandTabs leaves tab-free text unchanged",             prop_noTabIdentity)
  , ("expandTabs preserves display width",                    prop_tabWidthConsistent)
  ]

-- (c) (R) TM star warning check-mark left-right-arrow play-triangle heart info.
-- All are Extended_Pictographic and all are one cell wide in a terminal.
narrowSymbols :: String
narrowSymbols = "\169\174\8482\9733\9888\10004\8596\9654\10084\8505"

-- Each of these is a single cluster that terminals draw in two cells.
emojiSequences :: [Text]
emojiSequences =
  [ "\128512"                              -- grinning face
  , "\10084\65039"                         -- heart + emoji variation selector
  , "\128104\8205\128105\8205\128103"      -- family: ZWJ sequence
  , "\128104\8205\128187"                  -- man technologist: ZWJ sequence
  , "\128077\127997"                       -- thumbs up + skin-tone modifier
  , "1\65039\8419"                         -- keycap 1
  , "\127470\127475"                       -- flag: two regional indicators
  ]

prop_narrowSymbols :: Property
prop_narrowSymbols = property $ do
  c <- forAll (Gen.element narrowSymbols)
  textWidth (T.singleton c) === 1

prop_emojiSequences :: Property
prop_emojiSequences = property $ do
  s <- forAll (Gen.element emojiSequences)
  textWidth s === 2
  length (graphemeClusters s) === 1

prop_flags :: Property
prop_flags = property $ do
  n  <- forAll (Gen.int (Range.linear 1 4))
  ris <- forAll (Gen.list (Range.singleton (2 * n)) (Gen.enum '\127462' '\127487'))
  textWidth (T.pack ris) === 2 * n

prop_combining :: Property
prop_combining = property $ do
  base  <- forAll (Gen.element ("abcxyz" :: String))
  marks <- forAll (Gen.list (Range.linear 0 4) (Gen.enum '\x0300' '\x036F'))
  textWidth (T.pack (base : marks)) === 1

prop_cjk :: Property
prop_cjk = property $ do
  cs <- forAll (Gen.list (Range.linear 0 8) (Gen.enum '\x4E00' '\x4E30'))
  textWidth (T.pack cs) === 2 * length cs

prop_hangul :: Property
prop_hangul = property $ do
  s <- forAll (Gen.element ["\4370\4449\4523", "\54620" :: Text])
  textWidth s === 2

prop_singleChar :: Property
prop_singleChar = property $ do
  c <- forAll genMixedChar
  textWidth (T.singleton c) === charWidth c

prop_clustersConcat :: Property
prop_clustersConcat = property $ do
  t <- forAll genMixed
  T.concat (graphemeClusters t) === t

prop_widthIsSum :: Property
prop_widthIsSum = property $ do
  t <- forAll genMixed
  textWidth t === sum (map clusterWidth (graphemeClusters t))

prop_noTabIdentity :: Property
prop_noTabIdentity = property $ do
  t <- forAll (T.filter (/= '\t') <$> genMixed)
  w <- forAll (Gen.int (Range.linear 1 8))
  expandTabs w t === t

-- The alphabet here avoids ZWJ, regional indicators, Hangul jamo and CR: those
-- join with characters on either side, and replacing a tab by spaces could
-- legitimately change which cluster they belong to.
prop_tabWidthConsistent :: Property
prop_tabWidthConsistent = property $ do
  t <- forAll genWidthText
  w <- forAll (Gen.int (Range.linear 1 8))
  textWidth (expandTabs w t) === displayColumnAt w t (T.length t)

-- | Every kind of code point the segmenter treats specially.
genMixedChar :: Gen Char
genMixedChar = Gen.frequency
  [ (6, Gen.enum ' ' '~')
  , (1, Gen.element ("\t\r\n" :: String))
  , (1, Gen.element ("\233\169\8482" :: String)) -- Latin-1 and symbols
  , (2, Gen.enum '\x0300' '\x036F')            -- combining marks
  , (1, Gen.element ("\8205\65039" :: String)) -- ZWJ, emoji variation selector
  , (2, Gen.enum '\127462' '\127487')          -- regional indicators
  , (2, Gen.enum '\128512' '\128591')          -- emoticons
  , (2, Gen.enum '\x4E00' '\x4E30')            -- CJK
  , (1, Gen.enum '\4352' '\4607')              -- Hangul jamo
  ]

genMixed :: Gen Text
genMixed = Gen.text (Range.linear 0 30) genMixedChar

genWidthText :: Gen Text
genWidthText = Gen.text (Range.linear 0 30) $ Gen.frequency
  [ (6, Gen.enum ' ' '~')
  , (2, Gen.constant '\t')
  , (2, Gen.enum '\x0300' '\x036F')
  , (2, Gen.enum '\x4E00' '\x4E30')
  , (2, Gen.enum '\128512' '\128528')
  ]
