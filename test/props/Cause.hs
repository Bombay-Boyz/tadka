{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
import           Tadka.Internal             (buildContext)

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

-- === specCause: derive == manual ==========================================

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

srcC :: NamedSource
srcC = rightOrErr (mkNamedSource "f.hs" "let a = bb")

data DErr3 = DErr3 { d3src :: NamedSource, d3prim :: Span, d3cause :: Maybe SomeDiagnostic }

deriveDiagnostic defaultSpec
  { specSourceField = Just 'd3src, specLabelFields = [('d3prim, "here")]
  , specCause = Just 'd3cause, specMessage = Just [| \_ -> "boom" |] }
  ''DErr3

data MErr3 = MErr3 { m3src :: NamedSource, m3prim :: Span, m3cause :: Maybe SomeDiagnostic }

instance Diagnostic MErr3 where
  message _ = "boom"
  context e = buildContext (m3src e) [ (m3prim e, Just "here") ]
  diagnosticCause e = m3cause e

dCauseVal, dNoCauseVal :: DErr3
dCauseVal   = DErr3 srcC (rightOrErr (mkSpan 4 1)) (Just (SomeDiagnostic leaf))
dNoCauseVal = DErr3 srcC (rightOrErr (mkSpan 4 1)) Nothing

mCauseVal, mNoCauseVal :: MErr3
mCauseVal   = MErr3 srcC (rightOrErr (mkSpan 4 1)) (Just (SomeDiagnostic leaf))
mNoCauseVal = MErr3 srcC (rightOrErr (mkSpan 4 1)) Nothing

prop_deriveCauseEqualsManual :: Property
prop_deriveCauseEqualsManual = withTests 1 . property $
  mapM_ (\tgt -> do
            renderT tgt dCauseVal   === renderT tgt mCauseVal
            renderT tgt dNoCauseVal === renderT tgt mNoCauseVal)
        [TGraphical, TNarratable, TJson]

group :: Group
group = Group "Cause chain"
  [ ("cyclic cause chains render (every target)", prop_cyclicTerminates)
  , ("id-cyclic cause marker renders at most once", prop_markerAtMostOnce)
  , ("a cause chain renders a 'caused by' line",  prop_causedByAppears)
  , ("derived specCause == manual diagnosticCause, all handlers", prop_deriveCauseEqualsManual)
  ]
