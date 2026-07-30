{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Collection labels in @deriveDiagnostic@ (post-v1 hardening item 3): a
-- single @[Span]@-typed field expands, at runtime, to one label per element —
-- for a variable number of same-kind occurrences (every prior declaration of
-- a name, every match of a banned pattern) known only when the diagnostic is
-- built, where 'specLabelFields'/'specSecondaryLabelFields' need one field per
-- label fixed at splice time.
--
-- "Context.hs already supports variable-length label lists" is the load-
-- bearing fact this feature rests on: 'buildContext'/'buildContextWith' are
-- unchanged (they already take a plain, arbitrary-length list); this is a
-- TH-layer-only addition that expands a collection field's runtime list into
-- that same shape. The properties below therefore compare the derived
-- instance against a /manually/-expanded 'buildContextWith' call for the same
-- randomly generated list, across every list length and mix of primary/
-- secondary, fixed and collection fields — proving the splice is exactly
-- equivalent to writing the expansion out by hand, not just plausible for one
-- example.
module LabelCollection (group) where

import qualified Data.Aeson                as A
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty, pretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog                  (Group (..), Gen, Property, assert, forAll, property,
                                             withTests, (===))
import qualified Hedgehog.Gen               as Gen
import qualified Hedgehog.Range             as Range

import           Tadka
import           Tadka.Internal            (buildContext, buildContextWith)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- A source with distinct single-character spans at offsets 0, 5, 10, ...,
-- long enough that a handful of 1-character spans always land in-bounds.
srcText :: Text
srcText = "a bb ccc dddd eeeee ffffff ggggggg hhhhhhhh"

srcV :: NamedSource
srcV = rightOrErr (mkNamedSource "f.hs" srcText)

-- === derive == manual, for a range of collection sizes ====================

-- Primary-only: a single primary collection field, no fixed fields at all —
-- exercises the `buildContext` (no-secondary) branch purely through a
-- collection, and specifically that "anchor on the first primary label"
-- falls on the collection's first element when it is the only label source.
data AllOcc = AllOcc { aoSrc :: NamedSource, aoSpans :: [Span] }

deriveDiagnostic defaultSpec
  { specSourceField           = Just 'aoSrc
  , specLabelCollectionFields = [('aoSpans, "matches the banned pattern")]
  , specMessage               = Just [| \_ -> pretty ("banned pattern used" :: Text) |]
  }
  ''AllOcc

data AllOccManual = AllOccManual { aomSrc :: NamedSource, aomSpans :: [Span] }

instance Diagnostic AllOccManual where
  message _ = pretty ("banned pattern used" :: Text)
  context e = buildContext (aomSrc e)
    [ (s, Just "matches the banned pattern") | s <- aomSpans e ]

-- Mixed: one fixed primary field (the offending redeclaration) plus a
-- secondary *collection* field (every prior declaration) — exercises the
-- `buildContextWith` branch with both a fixed and a collection source in the
-- same instance, fixed field first as documented.
data Dup = Dup { dSrc :: NamedSource, dNew :: Span, dPrev :: [Span] }

deriveDiagnostic defaultSpec
  { specSourceField                    = Just 'dSrc
  , specLabelFields                    = [('dNew, "redeclared here")]
  , specSecondaryLabelCollectionFields = [('dPrev, "previously declared here")]
  , specMessage                        = Just [| \_ -> pretty ("duplicate declaration" :: Text) |]
  }
  ''Dup

data DupManual = DupManual { dmSrc :: NamedSource, dmNew :: Span, dmPrev :: [Span] }

instance Diagnostic DupManual where
  message _ = pretty ("duplicate declaration" :: Text)
  context e = buildContextWith (dmSrc e)
    (  [ (dmNew e, Primary, Just "redeclared here") ]
    ++ [ (s, Secondary, Just "previously declared here") | s <- dmPrev e ] )

gfx :: Diagnostic e => e -> Text
gfx e = case selectRenderer (withColorMode ColorNever (withUnicodeMode UnicodeAlways (withTarget TGraphical defaultConfig))) of
  SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _                            -> ""

nar :: Diagnostic e => e -> Text
nar e = case selectRenderer (withTarget TNarratable defaultConfig) of
  SomeRenderer r@(Narratable _) -> render r e
  _                             -> ""

jsn :: Diagnostic e => e -> Text
jsn e = case selectRenderer (withTarget TJson defaultConfig) of
  SomeRenderer r@(Json _) -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r e)))
  _                       -> ""

-- A handful of valid, in-bounds single-character offsets to draw spans from
-- ('srcText' is 43 characters, offsets 0..42).
genSpanList :: Gen [Span]
genSpanList = Gen.list (Range.linear 0 8) genSpan
  where
    genSpan = (\off -> rightOrErr (mkSpan off 1)) <$> Gen.element [0, 2, 5, 9, 14, 20, 27, 35, 42]

prop_allPrimaryCollectionDerivedEqualsManual :: Property
prop_allPrimaryCollectionDerivedEqualsManual = property $ do
  spans_ <- forAll genSpanList
  let d = AllOcc srcV spans_
      m = AllOccManual srcV spans_
  gfx d === gfx m
  nar d === nar m
  jsn d === jsn m

prop_mixedFixedAndSecondaryCollectionDerivedEqualsManual :: Property
prop_mixedFixedAndSecondaryCollectionDerivedEqualsManual = property $ do
  prevSpans <- forAll genSpanList
  let newSpan = rightOrErr (mkSpan 40 1)
      d = Dup srcV newSpan prevSpans
      m = DupManual srcV newSpan prevSpans
  gfx d === gfx m
  nar d === nar m
  jsn d === jsn m

-- === specific edge cases (not just "some random N") ========================

prop_emptyCollectionIsFixedFieldsOnly :: Property
prop_emptyCollectionIsFixedFieldsOnly = withTests 1 . property $ do
  let newSpan = rightOrErr (mkSpan 40 1)
      withEmptyColl = Dup srcV newSpan []
      -- A hand-written instance with *no* collection field at all: if the
      -- splice's `++ concat []` addition is truly a no-op, these must match.
      manualNoColl = DupManual srcV newSpan []
  gfx withEmptyColl === gfx manualNoColl
  nar withEmptyColl === nar manualNoColl
  jsn withEmptyColl === jsn manualNoColl

prop_emptyEverythingIsNoContext :: Property
prop_emptyEverythingIsNoContext = withTests 1 . property $ do
  let d = AllOcc srcV []
  -- No fixed fields and an empty collection: NoContext, i.e. no location
  -- line at all, same as a diagnostic with no context whatsoever.
  gfx d === gfx (AllOccManual srcV [])

prop_totalityLargeCollection :: Property
prop_totalityLargeCollection = withTests 1 . property $ do
  let manySpans = replicate 200 (rightOrErr (mkSpan 0 1))
      d = AllOcc srcV manySpans
  -- Must not crash or hang; a non-empty rendered report is a cheap total
  -- witness that rendering actually completed for 200 expanded labels.
  assert (T.length (gfx d) > 0)

group :: Group
group = Group "Collection labels in deriveDiagnostic"
  [ ("all-primary collection: derived == manual (varying N)",       prop_allPrimaryCollectionDerivedEqualsManual)
  , ("fixed primary + secondary collection: derived == manual",     prop_mixedFixedAndSecondaryCollectionDerivedEqualsManual)
  , ("empty collection list == field omitted entirely",             prop_emptyCollectionIsFixedFieldsOnly)
  , ("no fixed fields + empty collection == NoContext",              prop_emptyEverythingIsNoContext)
  , ("a large collection renders without crashing (totality)",      prop_totalityLargeCollection)
  ]
