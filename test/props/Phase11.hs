{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase 11 consolidation: the render-totality property broadened to every
-- 'Renderer' target, the cycle-detection marker property, and the vision's
-- Success Criterion exercised end-to-end (a genuinely staled span renders a
-- clear in-report reason rather than a silently shorter report).
module Phase11 (group) where

import           Control.Exception          (SomeException, evaluate, try)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty, pretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           GenDiag                    (GD (..), genGD)
import           Tadka
import           Tadka.Internal             (buildContext)

group :: Group
group = Group "Phase 11 - consolidation & release audit"
  [ ("render is total for every target (Hedgehog)", prop_totalAllTargets)
  , ("cycle marker renders at most once",           prop_cycleMarker)
  , ("staled span renders a clear reason",          prop_staleReason)
  , ("staled label is explicit in JSON",            prop_staleJson)
  ]

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- Render any diagnostic through the chosen target, collapsed to Text.
renderTarget :: Target -> SomeDiagnostic -> Text
renderTarget tgt (SomeDiagnostic e) =
  case selectRenderer cfg of
    SomeRenderer r@(Graphical _)  -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
    SomeRenderer r@(Narratable _) -> render r e
    SomeRenderer r@(Json _)       -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r e)))
  where
    cfg = withColorMode ColorNever (withUnicodeMode UnicodeAlways (withTarget tgt defaultConfig))

-- Broadens the phase-local smoke checks into one Hedgehog property over every
-- Renderer target and fuel-bounded generated diagnostics.
prop_totalAllTargets :: Property
prop_totalAllTargets = property $ do
  fuel <- forAll (Gen.int (Range.linear 0 3))
  tgt  <- forAll (Gen.element [TGraphical, TNarratable, TJson])
  d    <- forAllWith (const "<generated diagnostic>") (genGD fuel)
  res  <- liftIO (try (evaluate (T.length (renderTarget tgt (SomeDiagnostic d))))
                    :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate (show e) >> failure

-- A node that relates to itself (same diagnosticId): the repeat is cycle-omitted,
-- so a marker in its message renders at most once, on every target.
marker :: Text
marker = "ZZMARKERZZ"

cyclic :: GD
cyclic =
  let d = GD ("dup problem " <> marker) Nothing NoContext Nothing Nothing
             [SomeDiagnostic d] (Just (mkDiagnosticId "dup")) Nothing
  in d

prop_cycleMarker :: Property
prop_cycleMarker = withTests 1 . property $
  mapM_ (\tgt -> assert (T.count marker (renderTarget tgt (SomeDiagnostic cyclic)) <= 1))
        [TGraphical, TNarratable, TJson]

-- Success Criterion: a genuinely staled span (out of bounds) renders a clear
-- in-report reason, not a silently shorter report.
staleDiag :: GD
staleDiag = GD "undefined variable" (Just (rightOrErr (mkDiagnosticCode "tadka::E0001")))
               ctx Nothing Nothing [] Nothing Nothing
  where
    ctx = buildContext (rightOrErr (mkNamedSource "f.hs" "abc"))
            [ (rightOrErr (mkSpan 100 3), Just (pretty ("not in scope" :: Text))) ]

prop_staleReason :: Property
prop_staleReason = withTests 1 . property $ do
  let g = renderTarget TGraphical (SomeDiagnostic staleDiag)
      n = renderTarget TNarratable (SomeDiagnostic staleDiag)
  assert ("span unavailable" `T.isInfixOf` g)
  assert ("could not be shown" `T.isInfixOf` n)

prop_staleJson :: Property
prop_staleJson = withTests 1 . property $ do
  let j = renderTarget TJson (SomeDiagnostic staleDiag)
  assert ("\"stale\":true" `T.isInfixOf` j)
