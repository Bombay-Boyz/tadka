{-# LANGUAGE OverloadedStrings #-}

-- | Phase 2 properties: span resolution and Context construction. The crux is
-- that 'mkContextDegrading' never changes the label count or ordering.
module Phase2 (group) where

import           Data.Either        (isLeft)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Prettyprinter      (Doc, pretty)

import           Hedgehog
                   (Gen, Group (..), Property, assert, failure, forAll,
                    property, success, withTests, (===))
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

import           Tadka
import           Tadka.Internal.Span  (spanLength, spanOffset)
import           Tadka.Internal.Types (unLength, unOffset)
import           Tadka.Internal     (buildContext)

group :: Group
group = Group "Phase 2 - span resolution & context"
  [ ("resolveSpan stays within source bounds",              prop_resolveInBounds)
  , ("mkContext is Left iff any span is out of bounds",     prop_mkContextStrict)
  , ("mkContextDegrading never changes label count/order",  prop_degradingCount)
  , ("buildContext [] = NoContext",                         prop_buildContextEmpty)
  , ("buildContext dispatches to mkContextDegrading",       prop_buildContextDispatch)
  ]

genSourceText :: Gen Text
genSourceText =
  Gen.text (Range.linear 0 40)
    (Gen.frequency [(6, Gen.alphaNum), (2, Gen.constant ' '), (2, Gen.constant '\n')])

genNamedSource :: Gen NamedSource
genNamedSource = do
  name <- Gen.text (Range.linear 1 8) Gen.alpha
  txt  <- genSourceText
  either (const Gen.discard) pure (mkNamedSource name txt)

-- Offsets/lengths in a range that straddles typical source lengths, so some
-- spans resolve and some are out of bounds.
genSpan :: Gen Span
genSpan = do
  o <- Gen.int (Range.linear 0 50)
  l <- Gen.int (Range.linear 0 50)
  either (const Gen.discard) pure (mkSpan o l)

genLabelText :: Gen (Maybe (Doc Ann))
genLabelText =
  Gen.choice [pure Nothing, Just . pretty <$> Gen.text (Range.linear 1 10) Gen.alpha]

genLabelKind :: Gen LabelKind
genLabelKind = Gen.element [Primary, Secondary]

genLabeledSpan :: Gen (Labeled Span)
genLabeledSpan = Labeled <$> genSpan <*> genLabelKind <*> genLabelText

genLabeledSpans :: Gen (NonEmpty (Labeled Span))
genLabeledSpans = do
  x  <- genLabeledSpan
  xs <- Gen.list (Range.linear 0 6) genLabeledSpan
  pure (x :| xs)

prop_resolveInBounds :: Property
prop_resolveInBounds = property $ do
  src <- forAll genNamedSource
  sp  <- forAll genSpan
  case resolveSpan src sp of
    Left _   -> success
    Right rs -> do
      let o = unOffset (spanOffset rs)
          l = unLength (spanLength rs)
      assert (o >= 0)
      assert (o + l <= T.length (sourceText src))

prop_mkContextStrict :: Property
prop_mkContextStrict = property $ do
  src  <- forAll genNamedSource
  lbls <- forAll genLabeledSpans
  let anyOOB = any (\(Labeled sp _ _) -> isLeft (resolveSpan src sp)) (NE.toList lbls)
  isLeft (mkContext src lbls) === anyOOB

prop_degradingCount :: Property
prop_degradingCount = property $ do
  src  <- forAll genNamedSource
  lbls <- forAll genLabeledSpans
  let states = contextLabelStates (mkContextDegrading src lbls)
  -- 1. count preserved
  length states === NE.length lbls
  -- 2. LabelStale in exactly the positions whose span fails to resolve
  let expectedStale = map (\(Labeled sp _ _) -> isLeft (resolveSpan src sp)) (NE.toList lbls)
  map isStale states === expectedStale
  where
    isStale (LabelStale _) = True
    isStale (LabelOk _)    = False

prop_buildContextEmpty :: Property
prop_buildContextEmpty = withTests 1 . property $ do
  src <- forAll genNamedSource
  case buildContext src [] of
    NoContext -> success
    _         -> failure

prop_buildContextDispatch :: Property
prop_buildContextDispatch = property $ do
  src <- forAll genNamedSource
  neq <- forAll genLabeledSpans
  let items  = map (\(Labeled sp _ txt) -> (sp, txt)) (NE.toList neq)
      built  = buildContext src items
      viaDeg = mkContextDegrading src neq
  contextLabelStates built === contextLabelStates viaDeg
