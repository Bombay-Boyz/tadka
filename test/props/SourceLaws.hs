{-# LANGUAGE OverloadedStrings #-}

-- | The 'SourceCode' contract, checked as a reusable set of laws and run on
-- two independent implementations: the built-in 'NamedSource' and a
-- list-of-lines source defined here. If both satisfy the laws, and agree with
-- each other on equal text, then 'SourceCode' really is an extension point
-- rather than a second name for 'NamedSource'.
module SourceLaws (group, LineSource (..), sourceLaws) where

import           Data.String                (fromString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Tadka.Internal.SourceCode  (SomeSource (..), SourceCode (..), someSource)
import           Tadka.Internal.Span        (mkSpan, resolveSpan)
import           Tadka.Internal.Types       (LineCol (..), NamedSource, mkNamedSource)

-- | A source held as a list of lines (no newline or CR inside a line). A
-- source with no lines is one empty line, as for any text.
data LineSource = LineSource Text [Text]
  deriving (Show)

linesOf :: LineSource -> [Text]
linesOf (LineSource _ ls) = if null ls then [T.empty] else ls

instance SourceCode LineSource where
  scName (LineSource n _) = n
  scLength src = T.length (T.intercalate "\n" (linesOf src))
  scLineCount src = length (linesOf src)
  scLines src (lo, hi) =
    [ (n, l) | (n, l) <- zip [1 ..] (linesOf src), n >= max 1 lo, n <= hi ]
  scLocate src off = go 1 off (linesOf src)
    where
      go n o (l : rest)
        | o <= T.length l || null rest = LineCol n (o + 1)
        | otherwise                    = go (n + 1) (o - T.length l - 1) rest
      go n o []                        = LineCol n (o + 1)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

group :: Group
group = Group "SourceCode contract"
  (  sourceLaws "NamedSource (LF, CRLF, Unicode)" genNamed
  ++ sourceLaws "LineSource" genLineSource
  ++ [ ("NamedSource: locate agrees with its own lines (LF-only text)", prop_modelNamed)
     , ("LineSource: locate agrees with its own lines",                 prop_modelLine)
     , ("LineSource and NamedSource agree on equal text",               prop_equivalent)
     , ("a CR before an LF is a column, not part of the line",          prop_crlf)
     , ("SomeSource delegates every method",                            prop_someDelegates)
     , ("SomeSource shows its name, never its text",                    prop_someShow)
     ]
  )

-- | The laws every instance must satisfy, over the generated values.
sourceLaws :: (Show s, SourceCode s) => String -> Gen s -> [(PropertyName, Property)]
sourceLaws title gen =
  [ (name "there is at least one line",              prop_countPositive gen)
  , (name "lines 1..count are enumerated in order",  prop_fullEnumeration gen)
  , (name "a window is the clipped enumeration",     prop_windowClips gen)
  , (name "scLocate is within bounds everywhere",    prop_locateBounds gen)
  , (name "scLocate never moves backwards",          prop_locateMonotone gen)
  ]
  where
    name s = fromString (title ++ ": " ++ s)

prop_countPositive :: (Show s, SourceCode s) => Gen s -> Property
prop_countPositive gen = property $ do
  s <- forAll gen
  assert (scLineCount s >= 1)

prop_fullEnumeration :: (Show s, SourceCode s) => Gen s -> Property
prop_fullEnumeration gen = property $ do
  s <- forAll gen
  map fst (scLines s (1, scLineCount s)) === [1 .. scLineCount s]

prop_windowClips :: (Show s, SourceCode s) => Gen s -> Property
prop_windowClips gen = property $ do
  s  <- forAll gen
  lo <- forAll (Gen.int (Range.linear (-3) 12))
  hi <- forAll (Gen.int (Range.linear (-3) 12))
  let full = scLines s (1, scLineCount s)
  scLines s (lo, hi) === [ p | p@(n, _) <- full, n >= lo, n <= hi ]

prop_locateBounds :: (Show s, SourceCode s) => Gen s -> Property
prop_locateBounds gen = property $ do
  s <- forAll gen
  let lcs = map (scLocate s) [0 .. scLength s]
  assert (all (\(LineCol l c) -> l >= 1 && l <= scLineCount s && c >= 1) lcs)

prop_locateMonotone :: (Show s, SourceCode s) => Gen s -> Property
prop_locateMonotone gen = property $ do
  s <- forAll gen
  let lcs = map (scLocate s) [0 .. scLength s]
  assert (and (zipWith (<=) lcs (drop 1 lcs)))

-- | A model of 'scLocate' built only from 'scLines', valid when no line
-- terminator is CR-stripped (so LF-only text): the offset of each line's
-- start is the sum of the lengths of the lines before it plus one each.
locateModel :: SourceCode s => s -> [LineCol]
locateModel s = map expected [0 .. scLength s]
  where
    ls     = map snd (scLines s (1, scLineCount s))
    starts = take (length ls) (scanl (\acc l -> acc + T.length l + 1) 0 ls)
    expected o =
      let k     = length (takeWhile (<= o) starts)
          start = starts !! (k - 1)
      in LineCol k (o - start + 1)

prop_modelNamed :: Property
prop_modelNamed = property $ do
  s <- forAll genNamedLF
  map (scLocate s) [0 .. scLength s] === locateModel s

prop_modelLine :: Property
prop_modelLine = property $ do
  s <- forAll genLineSource
  map (scLocate s) [0 .. scLength s] === locateModel s

prop_equivalent :: Property
prop_equivalent = property $ do
  ls <- forAll genLineList
  let line  = LineSource "same" ls
      named = rightOrErr (mkNamedSource "same" (T.intercalate "\n" (linesOf line)))
      offs  = [0 .. scLength line]
  scLength line    === scLength named
  scLineCount line === scLineCount named
  map (scLocate line) offs === map (scLocate named) offs
  lo <- forAll (Gen.int (Range.linear (-2) 8))
  hi <- forAll (Gen.int (Range.linear (-2) 8))
  scLines line (lo, hi) === scLines named (lo, hi)
  o <- forAll (Gen.int (Range.linear 0 (scLength line + 5)))   -- may run off the end
  l <- forAll (Gen.int (Range.linear 0 6))
  let sp = rightOrErr (mkSpan o l)
  resolveSpan line sp === resolveSpan named sp

prop_crlf :: Property
prop_crlf = withTests 1 . property $ do
  let s = rightOrErr (mkNamedSource "crlf" "ab\r\ncd")
  scLines s (1, 2)  === [(1, "ab"), (2, "cd")]
  scLocate s 2      === LineCol 1 3     -- the CR is a column of line 1
  scLocate s 4      === LineCol 2 1     -- 'c' starts line 2
  scLength s        === 6

prop_someDelegates :: Property
prop_someDelegates = property $ do
  s <- forAll genNamed
  let w = someSource s
  scName w      === scName s
  scLength w    === scLength s
  scLineCount w === scLineCount s
  scLines w (1, 3) === scLines s (1, 3)
  map (scLocate w) [0 .. scLength s] === map (scLocate s) [0 .. scLength s]

prop_someShow :: Property
prop_someShow = withTests 1 . property $ do
  show (someSource (LineSource "virtual.buf" ["secret text"])) === "SomeSource \"virtual.buf\""
  show (SomeSource (LineSource "x" [])) === "SomeSource \"x\""

-- === Generators ==============================================================

genLineChar :: Gen Char
genLineChar = Gen.frequency
  [ (6, Gen.enum ' ' '~')
  , (1, Gen.enum '\x4E00' '\x4E30')
  , (1, Gen.element ("\233\955" :: String))
  ]

genLineList :: Gen [Text]
genLineList = Gen.list (Range.linear 0 6) (Gen.text (Range.linear 0 10) genLineChar)

genLineSource :: Gen LineSource
genLineSource = LineSource "lines" <$> genLineList

-- | Arbitrary text, including CR, LF and CRLF.
genNamed :: Gen NamedSource
genNamed = do
  t <- Gen.text (Range.linear 0 30) $ Gen.frequency
         [ (6, Gen.enum ' ' '~'), (2, Gen.constant '\n'), (1, Gen.constant '\r')
         , (1, Gen.enum '\x4E00' '\x4E30') ]
  pure (rightOrErr (mkNamedSource "named" t))

-- | Text with no CR at all, so every offset belongs to a line's own text.
genNamedLF :: Gen NamedSource
genNamedLF = do
  ls <- genLineList
  pure (rightOrErr (mkNamedSource "named" (T.intercalate "\n" ls)))
