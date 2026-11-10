{-# LANGUAGE OverloadedStrings #-}

-- | How context lines behave at the end of a source. A source that ends with a
-- newline has a final empty "line" that no editor numbers; it must not be
-- shown as context, but it must still be shown when a label points at it.
module ContextLines (group) where

import           Data.Char          (isDigit)
import           Data.Text          (Text)
import qualified Data.Text          as T

import           Hedgehog
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import           Prettyprinter      (pretty)

import           GenDiag            (GD (..))
import           RenderUtil
import           Tadka
import           Tadka.Internal     (buildContext)

group :: Group
group = Group "Context lines at the end of a source"
  [ ("no blank line follows a trailing newline",                  once prop_noBlankLine)
  , ("a label at the very end still shows the final line",        once prop_labelAtEnd)
  , ("a source without a trailing newline is unaffected",         once prop_noNewline)
  , ("no line is invented after the last real one",               once prop_twoLines)
  , ("without context lines the output is unchanged",             once prop_unset)
  , ("context never reaches past the last real line",             prop_neverPast)
  ]

once :: PropertyT IO () -> Property
once = withTests 1 . property

-- | A diagnostic with one label over the given span of the given text.
pointing :: Text -> Int -> Int -> GD
pointing text offset len =
  GD "boom" Nothing (buildContext src [(sp, Just (pretty ("here" :: Text)))])
     Nothing Nothing [] Nothing Nothing
  where
    src = rightOrErr (mkNamedSource "demo.txt" text)
    sp  = rightOrErr (mkSpan offset len)

-- | The line numbers printed in a report, in order.
numbersIn :: Text -> [Text]
numbersIn out =
  [ n
  | l <- T.lines out
  , let n = T.strip (T.takeWhile (/= '│') l)
  , '│' `T.elem` l
  , not (T.null n)
  , T.all isDigit n
  ]

withContext :: Int -> Config
withContext n = withContextLines n plainConfig

prop_noBlankLine :: PropertyT IO ()
prop_noBlankLine =
  numbersIn (renderG (withContext 3) (pointing "let total = price * quantity\n" 20 8)) === ["1"]

prop_labelAtEnd :: PropertyT IO ()
prop_labelAtEnd =
  numbersIn (renderG (withContext 3) (pointing "ab\n" 3 0)) === ["1", "2"]

prop_noNewline :: PropertyT IO ()
prop_noNewline =
  numbersIn (renderG (withContext 3) (pointing "ab" 0 2)) === ["1"]

prop_twoLines :: PropertyT IO ()
prop_twoLines =
  numbersIn (renderG (withContext 3) (pointing "a\nb\n" 0 1)) === ["1", "2"]

prop_unset :: PropertyT IO ()
prop_unset =
  numbersIn (renderG plainConfig (pointing "a\nb\n" 0 1)) === ["1"]

-- | For any text ending in a newline and any one-character label on a real
-- character of it, no printed line number exceeds the number of real lines.
--
-- The label stays off the final newline itself: a span that includes that
-- newline ends at column 1 of the empty last line, so it does touch that line
-- and the line is rightly shown (see 'prop_labelAtEnd').
prop_neverPast :: Property
prop_neverPast = property $ do
  ls     <- forAll (Gen.list (Range.linear 1 5) (Gen.text (Range.linear 1 6) Gen.alphaNum))
  ctx    <- forAll (Gen.int (Range.linear 0 4))
  let text = T.unlines ls
  offset <- forAll (Gen.int (Range.linear 0 (T.length text - 2)))
  let out  = renderG (withContext ctx) (pointing text offset 1)
      nums = numbersIn out
  assert (all (\n -> read (T.unpack n) <= length ls) nums)
  assert (not (null nums))
