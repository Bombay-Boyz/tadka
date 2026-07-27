{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Phase 6 properties: prose-marker interpretation of 'Ann', and a totality
-- smoke check for the narratable handler over the shared generated set.
module Phase6 (group) where

import           Control.Exception          (SomeException, evaluate, try)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Prettyprinter               as PP

import           Hedgehog
import qualified Hedgehog.Gen                as Gen
import qualified Hedgehog.Range              as Range

import           GenDiag                     (genGD, selfJust, selfNothing)
import           Tadka

group :: Group
group = Group "Phase 6 - narratable handler"
  [ ("AnnCode renders quoted in prose",            prop_annCodeQuoted)
  , ("narratable render is total (fuelled)",       prop_totality)
  , ("narratable render is total (pathological)",  prop_totalityCycles)
  ]

-- A diagnostic whose message carries an AnnCode span, to exercise toProseMarker.
data Coded = Coded Text

instance Diagnostic Coded where
  message (Coded ident) = "undefined variable " <> PP.annotate AnnCode (PP.pretty ident)

renderText :: Diagnostic e => e -> Text
renderText e = case selectRenderer (withTarget TNarratable defaultConfig) of
  SomeRenderer r@(Narratable _) -> render r e
  _                             -> ""

-- The narratable handler wraps AnnCode content in double quotes.
prop_annCodeQuoted :: Property
prop_annCodeQuoted = property $ do
  ident <- forAll (Gen.text (Range.linear 1 8) (Gen.enum 'a' 'z'))
  let out = renderText (Coded ident)
  assert (("\"" <> ident <> "\"") `T.isInfixOf` out)

prop_totality :: Property
prop_totality = property $ do
  fuel <- forAll (Gen.int (Range.linear 0 3))
  d    <- forAllWith (const "<generated diagnostic>") (genGD fuel)
  res  <- liftIO (try (evaluate (T.length (renderText d))) :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate' e

prop_totalityCycles :: Property
prop_totalityCycles = withTests 1 . property $ do
  res <- liftIO (try (evaluate (sum (map (T.length . renderText) [selfNothing, selfJust])))
                   :: IO (Either SomeException Int))
  case res of
    Right _ -> success
    Left e  -> annotate' e

annotate' :: SomeException -> PropertyT IO ()
annotate' e = annotate (show e) >> failure
