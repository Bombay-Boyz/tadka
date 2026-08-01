{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Phase 12 properties: multi-source 'Context' construction. The crux is the
-- same as Phase 2's, extended pointwise across sources: 'mkContextMultiDegrading'
-- never changes a group's label count or order, group order itself is never
-- changed, and every single-source function is exactly the one-group special
-- case of its multi-source counterpart -- never a second, divergent
-- implementation.
module Phase12 (group) where

import           Data.Either        (isLeft)
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Text          (Text)
import           Prettyprinter      (Doc, pretty)

import           Hedgehog
                   (Gen, Group (..), Property, failure, forAll, property,
                    success, withTests, (===))
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

import           Tadka
import           Tadka.Internal     (buildContextMulti)

group :: Group
group = Group "Phase 12 - multi-source context"
  [ ("mkContextMulti is Left iff any span, in any group, is out of bounds",
      prop_multiStrict)
  , ("mkContextMultiDegrading never changes any group's label count/order",
      prop_multiDegradingCount)
  , ("mkContext/mkContextDegrading are the one-group case of their multi- counterparts",
      prop_singleGroupMatchesMulti)
  , ("buildContextMulti with every group empty = NoContext",
      prop_buildContextMultiAllEmpty)
  , ("buildContextMulti dispatches to mkContextMultiDegrading",
      prop_buildContextMultiDispatch)
  , ("buildContextMulti drops an empty group without affecting the others",
      prop_buildContextMultiDropsEmptyGroups)
  ]

-- === Generators (mirroring Phase2's, one source/label-list pair per group) ==

genSourceText :: Gen Text
genSourceText =
  Gen.text (Range.linear 0 40)
    (Gen.frequency [(6, Gen.alphaNum), (2, Gen.constant ' '), (2, Gen.constant '\n')])

genNamedSource :: Gen NamedSource
genNamedSource = do
  name <- Gen.text (Range.linear 1 8) Gen.alpha
  txt  <- genSourceText
  either (const Gen.discard) pure (mkNamedSource name txt)

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

-- One group: a source, and the non-empty label list resolved against it.
genGroup :: Gen (NamedSource, NonEmpty (Labeled Span))
genGroup = (,) <$> genNamedSource <*> genLabeledSpans

-- One to four groups, in generation order (order matters -- these properties
-- check it's preserved).
genGroups :: Gen (NonEmpty (NamedSource, NonEmpty (Labeled Span)))
genGroups = do
  g  <- genGroup
  gs <- Gen.list (Range.linear 0 3) genGroup
  pure (g :| gs)

isStale :: LabelState -> Bool
isStale (LabelStale _) = True
isStale (LabelOk _)    = False

-- Entries shape 'buildContextMulti' takes, from a group's already-resolved-
-- shaped input -- the same (span, kind, text) triples 'buildContextWith'
-- takes per label, one source per group.
asEntries :: (NamedSource, NonEmpty (Labeled Span)) -> (NamedSource, [(Span, LabelKind, Maybe (Doc Ann))])
asEntries (src, lbls) = (src, map toEntry (NE.toList lbls))
  where toEntry (Labeled sp k txt) = (sp, k, txt)

-- === Properties ==============================================================

prop_multiStrict :: Property
prop_multiStrict = property $ do
  groups <- forAll genGroups
  let anyOOB = any groupHasOOB (NE.toList groups)
      groupHasOOB (src, lbls) =
        any (\(Labeled sp _ _) -> isLeft (resolveSpan src sp)) (NE.toList lbls)
  isLeft (mkContextMulti groups) === anyOOB

prop_multiDegradingCount :: Property
prop_multiDegradingCount = property $ do
  groups <- forAll genGroups
  let states   = contextLabelStates (mkContextMultiDegrading groups)
      expected = concatMap expectGroup (NE.toList groups)
      expectGroup (src, lbls) =
        [ isLeft (resolveSpan src sp) | Labeled sp _ _ <- NE.toList lbls ]
  -- 1. total count preserved across every group
  length states === sum (fmap (NE.length . snd) groups)
  -- 2. LabelStale in exactly the positions whose span fails to resolve,
  --    per group, in group order -- never dropped, never reordered, and
  --    never attributed to the wrong group's source.
  map isStale states === expected

prop_singleGroupMatchesMulti :: Property
prop_singleGroupMatchesMulti = property $ do
  src  <- forAll genNamedSource
  lbls <- forAll genLabeledSpans
  -- The total constructor: single-source result matches the one-group
  -- multi-source result, label for label.
  contextLabelStates (mkContextDegrading src lbls)
    === contextLabelStates (mkContextMultiDegrading ((src, lbls) :| []))
  -- The strict constructor: same success/failure shape, and the same
  -- resulting labels on success.
  case (mkContext src lbls, mkContextMulti ((src, lbls) :| [])) of
    (Left _,    Left _)    -> success
    (Right c1,  Right c2)  -> contextLabelStates c1 === contextLabelStates c2
    (Left _,    Right _)   -> failure
    (Right _,   Left _)    -> failure

prop_buildContextMultiAllEmpty :: Property
prop_buildContextMultiAllEmpty = withTests 20 . property $ do
  srcs <- forAll (Gen.list (Range.linear 1 4) genNamedSource)
  case NE.nonEmpty srcs of
    Nothing -> success   -- unreachable: Range.linear 1 4 never generates []
    Just ne -> case buildContextMulti (fmap (, []) ne) of
      NoContext -> success
      _         -> failure

prop_buildContextMultiDispatch :: Property
prop_buildContextMultiDispatch = property $ do
  groups <- forAll genGroups
  let built  = buildContextMulti (fmap asEntries groups)
      viaDeg = mkContextMultiDegrading groups
  contextLabelStates built === contextLabelStates viaDeg

prop_buildContextMultiDropsEmptyGroups :: Property
prop_buildContextMultiDropsEmptyGroups = property $ do
  groups   <- forAll genGroups
  emptySrc <- forAll genNamedSource
  let withEmpty = (emptySrc, []) <| fmap asEntries groups
  contextLabelStates (buildContextMulti withEmpty)
    === contextLabelStates (buildContextMulti (fmap asEntries groups))
