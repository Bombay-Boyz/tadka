{-# LANGUAGE OverloadedStrings #-}

-- | 'CauseFull': every cause of the reported diagnostic appears exactly once,
-- in every renderer; the JSON cause list is flat rather than nested; and
-- rendering is bounded even for structures that loop through @cause@ and
-- @related@ with no @diagnosticId@ to cut them short.
module CauseFull (group) where

import           Control.Exception (evaluate)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text         (Text)
import qualified Data.Text         as T
import           System.Timeout    (timeout)

import           Hedgehog
import qualified Hedgehog.Gen      as Gen
import qualified Hedgehog.Range    as Range
import           Prettyprinter     (pretty)

import           GenDiag           (GD (..))
import           RenderUtil
import           Tadka
import           Tadka.Internal    (buildContext)

group :: Group
group = Group "CauseFull"
  [ ("every cause appears exactly once in every renderer",      prop_onceEach)
  , ("the same holds under CauseSummary",                       prop_onceEachSummary)
  , ("JSON lists the cause chain flat, not nested",             prop_jsonFlat)
  , ("a cause's own snippet is shown only under CauseFull",     prop_snippet)
  , ("cyclic cause/related structures render in bounded time",  prop_cyclesTerminate)
  , ("their output is bounded in size",                         prop_cyclesBounded)
  ]

-- | A diagnostic with a message and an optional cause, nothing else.
mk :: Text -> Maybe SomeDiagnostic -> GD
mk m c = GD m Nothing NoContext Nothing Nothing [] Nothing c

marker :: Int -> Text
marker k = "ZZC" <> T.pack (show k) <> "ZZ"

-- | A chain of @k@ causes, the first marked 1 and the last marked @k@.
chainOf :: Int -> Int -> Maybe SomeDiagnostic
chainOf total k
  | k > total = Nothing
  | otherwise = Just (SomeDiagnostic (mk (marker k) (chainOf total (k + 1))))

rootWith :: Int -> GD
rootWith n = mk "ZZROOTZZ" (chainOf n 1)

full, summary :: Config
full    = withCauseDetail CauseFull plainConfig
summary = withCauseDetail CauseSummary plainConfig

renderers :: [(String, Config -> GD -> Text)]
renderers = [("graphical", renderG), ("narratable", renderN), ("json", renderJ)]

prop_onceEach :: Property
prop_onceEach = property $ do
  n <- forAll (Gen.int (Range.linear 1 6))
  mapM_ (\(name, r) -> expectOnce name (r full (rootWith n)) n) renderers
  where
    expectOnce name out n = do
      annotate name
      mapM_ (\k -> T.count (marker k) out === 1) [1 .. n]

prop_onceEachSummary :: Property
prop_onceEachSummary = property $ do
  n <- forAll (Gen.int (Range.linear 1 6))
  mapM_ (\(name, r) -> expectOnce name (r summary (rootWith n)) n) renderers
  where
    expectOnce name out n = do
      annotate name
      mapM_ (\k -> T.count (marker k) out === 1) [1 .. n]

-- | Under CauseFull each entry of the root's flat cause list has an empty
-- @causes@ of its own: counting them gives exactly one per cause. When causes
-- were nested again inside each other the count came out smaller and the
-- later causes were repeated.
prop_jsonFlat :: Property
prop_jsonFlat = property $ do
  n <- forAll (Gen.int (Range.linear 1 6))
  T.count "\"causes\":[]" (renderJ full (rootWith n)) === n

-- | A cause with its own source and label.
innerCause :: GD
innerCause =
  GD "inner failure" Nothing
     (buildContext src [(sp, Just (pretty ("inner label" :: Text)))])
     Nothing Nothing [] Nothing Nothing
  where
    src = rightOrErr (mkNamedSource "cause.hs" "let inner = 1\n")
    sp  = rightOrErr (mkSpan 4 5)

outer :: GD
outer = mk "outer failure" (Just (SomeDiagnostic innerCause))

prop_snippet :: Property
prop_snippet = withTests 1 . property $ do
  let g = renderG full outer
  assert ("cause.hs" `T.isInfixOf` g)
  assert ("inner label" `T.isInfixOf` g)
  assert (not ("cause.hs" `T.isInfixOf` renderG summary outer))
  assert ("cause.hs" `T.isInfixOf` renderN full outer)
  assert (not ("cause.hs" `T.isInfixOf` renderN summary outer))
  assert ("\"file\":\"cause.hs\"" `T.isInfixOf` renderJ full outer)
  assert (not ("cause.hs" `T.isInfixOf` renderJ summary outer))

-- === Cycles ===================================================================

-- | A's cause is B; B's related diagnostic is A. No ids, so nothing cuts the
-- loop short except the depth limit.
cycA, cycB :: GD
cycA = GD "ZZAZZ" Nothing NoContext Nothing Nothing [] Nothing (Just (SomeDiagnostic cycB))
cycB = GD "ZZBZZ" Nothing NoContext Nothing Nothing [SomeDiagnostic cycA] Nothing Nothing

-- | Each is both the other's cause and the other's related diagnostic: the
-- worst case, with a cause chain and a related tree that never end.
cycC, cycD :: GD
cycC = GD "ZZCZZ" Nothing NoContext Nothing Nothing [SomeDiagnostic cycD] Nothing (Just (SomeDiagnostic cycD))
cycD = GD "ZZDZZ" Nothing NoContext Nothing Nothing [SomeDiagnostic cycC] Nothing (Just (SomeDiagnostic cycC))

cyclic :: [GD]
cyclic = [cycA, cycB, cycC, cycD]

-- | Every renderer, on every cyclic structure, at several depth limits.
cases :: [(String, Config, GD)]
cases =
  [ (name <> ", depth " <> show depth, cfg, d)
  | depth <- [1, 8, 20 :: Int]
  , d <- cyclic
  , let cfg = withRelatedDepthLimit (fromIntegral depth) full
  , (name, _) <- renderers
  ]

renderWith :: String -> Config -> GD -> Text
renderWith name cfg d = case lookup name renderers of
  Just r  -> r cfg d
  Nothing -> ""

caseName :: String -> String
caseName = takeWhile (/= ',')

prop_cyclesTerminate :: Property
prop_cyclesTerminate = withTests 1 . property $
  mapM_ (\(lbl, cfg, d) -> do
           annotate lbl
           r <- liftIO (timeout 10000000 (evaluate (T.length (renderWith (caseName lbl) cfg d))))
           assert (r /= Nothing))
        cases

prop_cyclesBounded :: Property
prop_cyclesBounded = withTests 1 . property $
  mapM_ (\(lbl, cfg, d) -> do
           annotate lbl
           assert (T.length (renderWith (caseName lbl) cfg d) < 5000000))
        cases
