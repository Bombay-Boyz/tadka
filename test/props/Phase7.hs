{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase 7 properties: the @stale@ flag is derived from 'LabelState' (not
-- inferred from absence), and a totality smoke check for the JSON handler.
module Phase7 (group) where

import           Control.Exception          (SomeException, evaluate, try)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (pretty)

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           GenDiag                    (GD (..), genGD, selfJust, selfNothing)
import           Tadka
import           Tadka.Internal             (buildContext)
import           Tadka.Internal.Related     (walkRelated)
import           Tadka.Internal.Renderer.Json (DiagnosticDTO (..), LabelDTO (..), toDTO)

group :: Group
group = Group "Phase 7 - JSON handler + DTO"
  [ ("ok label -> stale:false with position",   prop_okLabel)
  , ("stale label -> stale:true, null position", prop_staleLabel)
  , ("JSON render is total (fuelled)",           prop_totality)
  , ("JSON render is total (pathological)",      prop_totalityCycles)
  ]

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- Build a one-label DTO for a diagnostic with the given span over "abcdef".
labelOf :: Int -> Int -> LabelDTO
labelOf off len =
  case dtoLabels (toDTO (walkRelated 8 (SomeDiagnostic d))) of
    (l:_) -> l
    []    -> error "labelOf: expected exactly one label"
  where
    src = rightOrErr (mkNamedSource "f.hs" "abcdef")
    d   = GD "m" Nothing
             (buildContext src [(rightOrErr (mkSpan off len), Just (pretty ("x" :: Text)))])
             Nothing Nothing [] Nothing Nothing

prop_okLabel :: Property
prop_okLabel = property $ do
  len <- forAll (Gen.int (Range.linear 1 3))
  let l = labelOf 1 len       -- in bounds => LabelOk
  ldStale l  === False
  ldLine l   === Just 1
  ldLength l === Just len

prop_staleLabel :: Property
prop_staleLabel = withTests 1 . property $ do
  let l = labelOf 100 3       -- out of bounds => LabelStale
  ldStale l  === True
  ldLine l   === Nothing
  ldColumn l === Nothing
  ldLength l === Nothing

renderJsonText :: Diagnostic e => e -> Text
renderJsonText e = case selectRenderer (withTarget TJson defaultConfig) of
  SomeRenderer r@(Json _) -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r e)))
  _                       -> ""

prop_totality :: Property
prop_totality = property $ do
  fuel <- forAll (Gen.int (Range.linear 0 3))
  d    <- forAllWith (const "<generated diagnostic>") (genGD fuel)
  res  <- liftIO (try (evaluate (T.length (renderJsonText d))) :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure

prop_totalityCycles :: Property
prop_totalityCycles = withTests 1 . property $ do
  res <- liftIO (try (evaluate (sum (map (T.length . renderJsonText) [selfNothing, selfJust])))
                   :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure
