{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Cause chain (post-v1 hardening): the "caused by" chain is depth- and
-- cycle-safe exactly like @related@. A cause that loops back by 'diagnosticId'
-- is cut (its marker renders at most once); an id-less loop is bounded by the
-- depth budget. Generated cause chains are additionally covered by the
-- render-totality property in the Phase 11 group (genGD now emits causes).
module Cause (group) where

import           Control.Exception          (SomeException, evaluate, try)
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog
import qualified Hedgehog.Gen               as Gen

import           GenDiag                    (GD (..), selfCauseJust, selfCauseNothing)
import           Tadka

group :: Group
group = Group "Cause chain"
  [ ("cyclic cause chains render (every target)", prop_cyclicTerminates)
  , ("id-cyclic cause marker renders at most once", prop_markerAtMostOnce)
  , ("a cause chain renders a 'caused by' line",  prop_causedByAppears)
  ]

renderT :: Diagnostic e => Target -> e -> Text
renderT tgt e =
  case selectRenderer (withColorMode ColorNever (withUnicodeMode UnicodeAlways (withTarget tgt defaultConfig))) of
    SomeRenderer r@(Graphical _)  -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
    SomeRenderer r@(Narratable _) -> render r e
    SomeRenderer r@(Json _)       -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r e)))

prop_cyclicTerminates :: Property
prop_cyclicTerminates = property $ do
  tgt <- forAll (Gen.element [TGraphical, TNarratable, TJson])
  d   <- forAllWith (const "<self-causing>") (Gen.element [selfCauseJust, selfCauseNothing])
  res <- liftIO (try (evaluate (T.length (renderT tgt d))) :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure

marker :: Text
marker = "ZZCAUSEZZ"

-- A node whose cause is itself, sharing one diagnosticId: cycle detection cuts
-- the chain, so the marker (in its message) renders once.
idCyclic :: GD
idCyclic =
  let d = GD ("boom " <> marker) Nothing NoContext Nothing Nothing []
             (Just (mkDiagnosticId "self")) (Just (SomeDiagnostic d))
  in d

prop_markerAtMostOnce :: Property
prop_markerAtMostOnce = withTests 1 . property $
  mapM_ (\tgt -> assert (T.count marker (renderT tgt idCyclic) <= 1))
        [TGraphical, TNarratable, TJson]

-- Root with a distinct-id cause: a "caused by" line must appear.
leaf :: GD
leaf = GD "root cause here" Nothing NoContext Nothing Nothing [] (Just (mkDiagnosticId "leaf")) Nothing

rooted :: GD
rooted = GD "top failure" Nothing NoContext Nothing Nothing [] (Just (mkDiagnosticId "root"))
            (Just (SomeDiagnostic leaf))

prop_causedByAppears :: Property
prop_causedByAppears = withTests 1 . property $ do
  assert ("caused by" `T.isInfixOf` renderT TGraphical rooted)
  assert ("Caused by" `T.isInfixOf` renderT TNarratable rooted)
