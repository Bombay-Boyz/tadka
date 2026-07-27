{-# LANGUAGE OverloadedStrings #-}

-- | Phase 3 properties: the Diagnostic class and the shared related/cycle walk.
module Phase3 (group) where

import           Data.Text          (Text)
import qualified Data.Text
import           Numeric.Natural    (Natural)
import           Prettyprinter      (pretty)

import           Hedgehog
                   (Gen, Group (..), Property, assert, forAll, forAllWith, property,
                    withTests, (===))
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range

import           Tadka
                   (Diagnostic (..), DiagnosticId, SomeDiagnostic (..),
                    mkDiagnosticId)
import           Tadka.Internal.Related
                   (RelatedTree (..), TerminationReason (..), defaultRelatedDepth,
                    flattenRelated, walkRelated)

group :: Group
group = Group "Phase 3 - diagnostic class & related walk"
  [ ("default related depth is 8",                         prop_defaultDepth)
  , ("cycle: shared id visited once, not descended",       prop_cycleVisitedOnce)
  , ("Nothing-only infinite chain terminates by depth",    prop_nothingInfiniteTerminates)
  , ("Nothing-only chains are depth-limited (v4 behavior)", prop_nothingDepthLimited)
  , ("walk is total & depth-bounded for any fuelled tree", prop_walkTotal)
  ]

-- | A minimal test diagnostic: only identity and related children matter for
-- the walk. Everything else is defaulted.
data TestDiag = TestDiag
  { tdId      :: Maybe DiagnosticId
  , tdRelated :: [SomeDiagnostic]
  }

instance Diagnostic TestDiag where
  message _    = pretty ("test" :: Text)
  diagnosticId = tdId
  related      = tdRelated

-- | Random finite tree with fresh (never shared) ids, bounded by fuel.
genFuelledDiag :: (Int -> Gen (Maybe DiagnosticId)) -> Int -> Gen SomeDiagnostic
genFuelledDiag genId fuel = do
  mId  <- genId fuel
  kids <- if fuel <= 0
            then pure []
            else Gen.list (Range.linear 0 3) (genFuelledDiag genId (fuel `div` 2))
  pure (SomeDiagnostic (TestDiag mId kids))

genNoId :: Int -> Gen (Maybe DiagnosticId)
genNoId _ = pure Nothing

genFreshId :: Int -> Gen (Maybe DiagnosticId)
genFreshId fuel =
  Gen.choice
    [ pure Nothing
    , Just . mkDiagnosticId <$> Gen.text (Range.linear 1 4) Gen.alpha
        -- salt with fuel so ids across levels rarely collide (fresh-ish)
    , pure (Just (mkDiagnosticId (pretty' fuel)))
    ]
  where
    pretty' n = "n" <> tshow n
    tshow = Data.Text.pack . show

treeDepth :: RelatedTree -> Int
treeDepth (RelatedTree _ []   _) = 1
treeDepth (RelatedTree _ kids _) = 1 + maximum (map treeDepth kids)

prop_defaultDepth :: Property
prop_defaultDepth = withTests 1 . property $ defaultRelatedDepth === (8 :: Natural)

-- Two nodes sharing ids in a cycle A -> B -> A: the second A is a CycleOmitted
-- leaf, reached once, never descended.
prop_cycleVisitedOnce :: Property
prop_cycleVisitedOnce = withTests 1 . property $ do
  let idA = mkDiagnosticId "A"
      idB = mkDiagnosticId "B"
      a   = TestDiag (Just idA) [SomeDiagnostic b]
      b   = TestDiag (Just idB) [SomeDiagnostic a]
      nodes = flattenRelated (walkRelated 8 (SomeDiagnostic a))
  -- exactly: A (root), B, A(cycle marker)
  length nodes === 3
  length (filter (\(_, t) -> t == CycleOmitted) nodes) === 1

-- A structurally infinite Nothing-only chain still terminates, bounded by depth.
prop_nothingInfiniteTerminates :: Property
prop_nothingInfiniteTerminates = withTests 1 . property $ do
  let loop  = TestDiag Nothing [SomeDiagnostic loop]
      nodes = flattenRelated (walkRelated 5 (SomeDiagnostic loop))
  length nodes === 6                                   -- root + 5 descents
  assert (any (\(_, t) -> t == DepthTruncated) nodes)
  assert (all (\(_, t) -> t /= CycleOmitted) nodes)    -- no ids => no cycle path

-- v4 fallback: Nothing-only trees are depth-limited, never cycle-omitted.
prop_nothingDepthLimited :: Property
prop_nothingDepthLimited = property $ do
  limit <- fromIntegral <$> forAll (Gen.int (Range.linear 0 6))
  fuel  <- forAll (Gen.int (Range.linear 0 24))
  root  <- forAllWith (const "<diag>") (genFuelledDiag genNoId fuel)
  let tree  = walkRelated limit root
      nodes = flattenRelated tree
  assert (length nodes >= 1)
  assert (all (\(_, t) -> t /= CycleOmitted) nodes)
  assert (treeDepth tree <= fromIntegral limit + 1)    -- root level + `limit` descents

-- The walk is total (finite result) and depth-bounded for any fuelled tree,
-- with or without ids.
prop_walkTotal :: Property
prop_walkTotal = property $ do
  limit <- fromIntegral <$> forAll (Gen.int (Range.linear 0 5))
  fuel  <- forAll (Gen.int (Range.linear 0 20))
  root  <- forAllWith (const "<diag>") (genFuelledDiag genFreshId fuel)
  let tree  = walkRelated limit root
      nodes = flattenRelated tree
  assert (length nodes >= 1)                           -- forces full evaluation
  assert (treeDepth tree <= fromIntegral limit + 1)
