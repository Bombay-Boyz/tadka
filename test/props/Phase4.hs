{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Phase 4 properties: the one path from Config to a renderer. The exit
-- criterion is that an explicit `withTarget` is never overridden by detection.
module Phase4 (group) where

import           Data.Aeson         (Value (Object))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Prettyprinter      (pretty)

import           Hedgehog
                   (Gen, Group (..), Property, assert, failure, forAll, property,
                    success, withTests, (===))
import qualified Hedgehog.Gen       as Gen

import           Tadka

group :: Group
group = Group "Phase 4 - renderer/config scaffolding"
  [ ("withTarget override is honoured by selectRenderer", prop_targetOverride)
  , ("override survives other setters",                   prop_overrideSurvivesSetters)
  , ("no explicit target defaults to graphical",          prop_defaultTarget)
  , ("render produces output for each target",            prop_renderSmoke)
  ]

-- A trivial diagnostic: only `message`, everything else defaulted.
data TrivialDiag = TrivialDiag

instance Diagnostic TrivialDiag where
  message _ = pretty ("trivial" :: Text)

targetOf :: SomeRenderer -> Target
targetOf (SomeRenderer (Graphical _))  = TGraphical
targetOf (SomeRenderer (Narratable _)) = TNarratable
targetOf (SomeRenderer (Json _))       = TJson

genTarget :: Gen Target
genTarget = Gen.enumBounded

prop_targetOverride :: Property
prop_targetOverride = property $ do
  t <- forAll genTarget
  targetOf (selectRenderer (withTarget t defaultConfig)) === t

prop_overrideSurvivesSetters :: Property
prop_overrideSurvivesSetters = property $ do
  t <- forAll genTarget
  let cfg = withColorMode ColorNever
          . withUnicodeMode UnicodeAscii
          . withRelatedDepthLimit 3
          . withTarget t
          $ defaultConfig
  targetOf (selectRenderer cfg) === t

prop_defaultTarget :: Property
prop_defaultTarget = withTests 1 . property $
  targetOf (selectRenderer defaultConfig) === TGraphical

-- Extract each target's output through a pure helper with a concrete return
-- type; matching the GADT existential directly inside the property monad would
-- leave the result type untouchable.
narratableOutput :: Diagnostic e => Config -> e -> Maybe Text
narratableOutput cfg e = case selectRenderer cfg of
  SomeRenderer r@(Narratable _) -> Just (render r e)
  _                             -> Nothing

jsonOutput :: Diagnostic e => Config -> e -> Maybe Value
jsonOutput cfg e = case selectRenderer cfg of
  SomeRenderer r@(Json _) -> Just (render r e)
  _                       -> Nothing

-- Smoke: each target's render path runs and carries the message through.
prop_renderSmoke :: Property
prop_renderSmoke = withTests 1 . property $ do
  case narratableOutput (withTarget TNarratable defaultConfig) TrivialDiag of
    Just t  -> assert ("trivial" `T.isInfixOf` t)
    Nothing -> failure
  case jsonOutput (withTarget TJson defaultConfig) TrivialDiag of
    Just (Object _) -> success
    _               -> failure
