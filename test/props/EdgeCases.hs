{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Production edge cases mined from miette's bug-fix history (learning from
-- their scars so tadka is robust at release). Each property names the miette
-- issue it mirrors. "No crash" is enforced by forcing the rendered length
-- inside 'try'; the rest assert structural expectations.
module EdgeCases (group) where

import           Control.Exception          (SomeException, evaluate, try)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import           Data.Char                  (isControl)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty, pretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Tadka
import           Tadka.Internal             (buildContext)

group :: Group
group = Group "Production edge cases (from miette)"
  [ ("zero-length span renders a point (miette #204/#159/#32)", prop_zeroLen)
  , ("zero-length span at end of line (miette #204)",           prop_zeroLenEol)
  , ("span past end of line does not crash (miette #221)",      prop_pastEol)
  , ("span past EOF is stale, not a crash (miette #347)",       prop_pastEof)
  , ("empty source does not crash (miette #183)",               prop_emptySource)
  , ("label at offset 0 (miette 2.1.0)",                        prop_offsetZero)
  , ("CRLF line endings leave no stray CR (miette #37)",        prop_crlf)
  , ("wide chars + tabs do not crash (miette #202)",            prop_wideTab)
  , ("combining marks do not crash (miette #312/#314)",         prop_combining)
  , ("nested / overlapping spans both render (miette #316)",    prop_nested)
  , ("newline inside a label does not corrupt layout (#318)",   prop_newlineLabel)
  , ("multi-line span shows every intermediate line (#81)",     prop_noSkip)
  , ("totality over out-of-range spans (all targets)",          prop_totality)
  , ("no terminal-escape injection from source/labels (VULN #1)", prop_noInjection)
  , ("output is bounded for a huge multi-line span (VULN #2)",    prop_bounded)
  ]

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- Build + graphically render a single-label diagnostic from a source and a span.
data Ed = Ed Context
instance Diagnostic Ed where
  message _      = "edge"
  context (Ed c) = c

ctxOf :: Text -> Int -> Int -> Text -> Context
ctxOf srcTxt off len lbl =
  buildContext (rightOrErr (mkNamedSource "e.hs" srcTxt))
               [ (rightOrErr (mkSpan off len), Just (pretty lbl)) ]

gfx :: Context -> Text
gfx c = case selectRenderer (withColorMode ColorNever (withUnicodeMode UnicodeAlways (withTarget TGraphical defaultConfig))) of
  SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r (Ed c)))
  _                            -> ""

renderAll :: Context -> [Text]
renderAll c =
  [ gfx c
  , case selectRenderer (withTarget TNarratable defaultConfig) of
      SomeRenderer r@(Narratable _) -> render r (Ed c); _ -> ""
  , case selectRenderer (withTarget TJson defaultConfig) of
      SomeRenderer r@(Json _) -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r (Ed c)))); _ -> ""
  ]

noCrash :: [Text] -> PropertyT IO ()
noCrash outs = do
  res <- liftIO (try (evaluate (sum (map T.length outs))) :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure

-- === the cases ============================================================

prop_zeroLen :: Property
prop_zeroLen = withTests 1 . property $ do
  let out = gfx (ctxOf "abcdef" 2 0 "here")
  noCrash [out]
  assert ("^" `T.isInfixOf` out)              -- a point still gets a caret

prop_zeroLenEol :: Property
prop_zeroLenEol = withTests 1 . property $
  noCrash (renderAll (ctxOf "abc\ndef" 3 0 "eol"))    -- offset 3 = the newline

prop_pastEol :: Property
prop_pastEol = withTests 1 . property $
  noCrash (renderAll (ctxOf "abc\ndef" 1 10 "long"))  -- length runs past the line

prop_pastEof :: Property
prop_pastEof = withTests 1 . property $ do
  let out = gfx (ctxOf "abc" 100 3 "gone")            -- entirely out of bounds
  noCrash [out]
  assert ("unavailable" `T.isInfixOf` out)            -- stale reason surfaces

prop_emptySource :: Property
prop_emptySource = withTests 1 . property $
  noCrash (renderAll (ctxOf "" 0 0 "empty"))

prop_offsetZero :: Property
prop_offsetZero = withTests 1 . property $ do
  let out = gfx (ctxOf "abcdef" 0 3 "start")
  noCrash [out]
  assert ("e.hs:1:1" `T.isInfixOf` out)               -- points at column 1

prop_crlf :: Property
prop_crlf = withTests 1 . property $ do
  -- 'def' is on line 2; offset of 'd' = len "abc\r\n" = 5
  let out = gfx (ctxOf "abc\r\ndef\r\nghi" 5 3 "on line two")
  noCrash [out]
  assert (not ('\r' `T.elem` out))                    -- no stray carriage returns
  assert ("e.hs:2:1" `T.isInfixOf` out)               -- correct line/col

prop_wideTab :: Property
prop_wideTab = withTests 1 . property $
  noCrash [gfx (ctxOf "\t\x4E2D\x6587 x = 1" 4 1 "wide+tab")]   -- tab + CJK before span

prop_combining :: Property
prop_combining = withTests 1 . property $
  noCrash [gfx (ctxOf "e\x0301clair = 1" 0 7 "accented")]        -- combining acute

prop_nested :: Property
prop_nested = withTests 1 . property $ do
  let c = buildContext (rightOrErr (mkNamedSource "e.hs" "abcdefgh"))
            [ (rightOrErr (mkSpan 1 6), Just "outer")
            , (rightOrErr (mkSpan 2 2), Just "inner") ]
      out = gfx c
  noCrash [out]
  assert ("outer" `T.isInfixOf` out && "inner" `T.isInfixOf` out)

prop_newlineLabel :: Property
prop_newlineLabel = withTests 1 . property $ do
  let out = gfx (ctxOf "abcdef" 1 3 "line one\nline two")
  noCrash [out]
  -- the caret rail must not be broken: every non-empty output line after the
  -- header should still start with a gutter/rail column, i.e. the injected
  -- newline must not produce a bare "line two" with no rail.
  assert (not (any (== "line two") (T.lines out)))

prop_noSkip :: Property
prop_noSkip = withTests 1 . property $ do
  -- multi-line span lines 1..4 in a 5-line source; all four must appear
  let src5 = "L1xxx\nL2xxx\nL3xxx\nL4xxx\nL5xxx"
      out  = gfx (ctxOf src5 0 22 "spans four lines")   -- offset 0..21 -> line 1..4
  noCrash [out]
  assert (all (\n -> n `T.isInfixOf` out) ["L1xxx", "L2xxx", "L3xxx", "L4xxx"])

prop_totality :: Property
prop_totality = property $ do
  off <- forAll (Gen.int (Range.linear 0 40))
  len <- forAll (Gen.int (Range.linear 0 40))
  let c = buildContext (rightOrErr (mkNamedSource "e.hs" "abc\ndef\nghi"))
            [ (rightOrErr (mkSpan off len), Just "x") | off + len <= 200 ]
  noCrash (renderAll c)

-- Text mixing printable and control characters (ESC, C0, C1, DEL).
genCtrlText :: Gen Text
genCtrlText = Gen.text (Range.linear 0 24) $ Gen.frequency
  [ (6, Gen.enum ' ' '~')
  , (2, Gen.enum '\x00' '\x1F')   -- C0 controls (incl ESC, BEL, BS)
  , (1, pure '\x7F')               -- DEL
  , (1, Gen.enum '\x80' '\x9F')   -- C1 controls
  ]

-- VULN #1: no raw control character (other than the '\n' line separator) may
-- appear in the terminal-facing output, no matter what the source or label
-- contains.
prop_noInjection :: Property
prop_noInjection = property $ do
  srcTxt <- forAll genCtrlText
  lbl    <- forAll genCtrlText
  off    <- forAll (Gen.int (Range.linear 0 (max 0 (T.length srcTxt))))
  let c    = buildContext (rightOrErr (mkNamedSource "s.hs" srcTxt))
                          [ (rightOrErr (mkSpan off 1), Just (pretty lbl)) ]
      bad t = [ ch | ch <- T.unpack t, isControl ch, ch /= '\n' ]
  mapM_ (\out -> bad out === []) (renderAll c)

-- VULN #2: a span across a huge line range must not render output proportional
-- to the span (default config falls back to a bounded context window).
prop_bounded :: Property
prop_bounded = withTests 1 . property $ do
  let big = T.intercalate (T.singleton '\n') (replicate 10000 "x")
      c   = buildContext (rightOrErr (mkNamedSource "s.hs" big))
                         [ (rightOrErr (mkSpan 0 (10000 * 2 - 3)), Just "huge") ]
  assert (length (T.lines (gfx c)) < 40)
