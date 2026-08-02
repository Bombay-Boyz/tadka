{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Primary/secondary labels (post-v1 hardening): the report location anchors
-- on the first primary label, JSON exposes an explicit @primary@ flag, and the
-- derive path (with @specSecondaryLabelFields@) renders identically to a manual
-- @buildContextWith@ instance — keeping the two doors symmetric for kinds too.
module Labels (group) where

import qualified Data.Aeson                       as A
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Encoding          as TLE
import           Prettyprinter                    (LayoutOptions (..),
                                                   PageWidth (Unbounded), layoutPretty,
                                                   pretty)
import           Prettyprinter.Render.Text        (renderStrict)

import           Hedgehog                         (Group (..), Property, property,
                                                   withTests, (===), assert)

import           Tadka
import           Tadka.Internal                   (buildContextWith)
import           Tadka.Internal.Related           (walkRelated)
import           Tadka.Internal.Renderer.Json     (LabelDTO (..), dtoLabels, toDTO)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- A source with two labels on different lines: secondary on line 1, primary on 3.
src3 :: NamedSource
src3 = rightOrErr (mkNamedSource "f.hs" "aaaa\nbbbb\ncccc\n")

kindFix :: Context
kindFix = buildContextWith src3
  [ (rightOrErr (mkSpan 0 2), Secondary, Just "context here")   -- line 1
  , (rightOrErr (mkSpan 10 2), Primary,  Just "the error")      -- line 3 (offset 10 = "aaaa\nbbbb\n" + 0)
  ]

data KindDiag = KindDiag
instance Diagnostic KindDiag where
  message _ = "boom"
  context _ = kindFix

gfx :: Diagnostic e => e -> Text
gfx e = case selectRenderer (withColorMode ColorNever (withUnicodeMode UnicodeAlways (withTarget TGraphical defaultConfig))) of
  SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _                            -> ""

prop_anchorPrimary :: Property
prop_anchorPrimary = withTests 1 . property $ do
  let out = gfx KindDiag
  -- the location line must point at the primary label's line 3, not line 1
  assert (T.isInfixOf "f.hs:3:" out)

prop_glyphs :: Property
prop_glyphs = withTests 1 . property $ do
  let out = gfx KindDiag
  assert (T.isInfixOf "-- context here" out)   -- secondary uses '-'
  assert (T.isInfixOf "^^ the error"   out)    -- primary uses '^'

-- Two Secondary labels on the same line (both on line 1, "aaaa"): must be
-- distinguishable under ColorNever even though they share a LabelKind.
kindFix3 :: Context
kindFix3 = buildContextWith src3
  [ (rightOrErr (mkSpan 0 2), Secondary, Just "first")   -- "aa"
  , (rightOrErr (mkSpan 2 2), Secondary, Just "second")  -- "aa"
  ]

data KindDiag3 = KindDiag3
instance Diagnostic KindDiag3 where
  message _ = "boom"
  context _ = kindFix3

prop_glyphsSameKindDistinct :: Property
prop_glyphsSameKindDistinct = withTests 1 . property $ do
  let out = gfx KindDiag3
  -- rank 0 keeps the Secondary anchor '-', rank 1 cycles to '~'
  assert (T.isInfixOf "-- first"  out)
  assert (T.isInfixOf "~~ second" out)

prop_jsonFlag :: Property
prop_jsonFlag = withTests 1 . property $ do
  let labels = dtoLabels (toDTO 8 (walkRelated 8 (SomeDiagnostic KindDiag)))
  map ldPrimary labels === [False, True]        -- secondary first, primary second

-- === derive (with secondary) == manual ====================================

data DErr2 = DErr2 { d2src :: NamedSource, d2prim :: Span, d2sec :: Span, d2got :: Text }
  deriving (Show)

deriveDiagnostic defaultSpec
  { specSourceField          = Just 'd2src
  , specLabelFields          = [('d2prim, "here")]
  , specSecondaryLabelFields = [('d2sec,  "context")]
  , specMessage              = Just [| \e -> pretty ("undefined " <> d2got e) |]
  }
  ''DErr2

data MErr2 = MErr2 { m2src :: NamedSource, m2prim :: Span, m2sec :: Span, m2got :: Text }

instance Diagnostic MErr2 where
  message e = pretty ("undefined " <> m2got e)
  context e = buildContextWith (m2src e)
    [ (m2prim e, Primary,   Just "here")
    , (m2sec e,  Secondary, Just "context")
    ]

srcV :: NamedSource
srcV = rightOrErr (mkNamedSource "f.hs" "let a = bb")

dVal :: DErr2
dVal = DErr2 srcV (rightOrErr (mkSpan 4 1)) (rightOrErr (mkSpan 8 2)) "x"

mVal :: MErr2
mVal = MErr2 srcV (rightOrErr (mkSpan 4 1)) (rightOrErr (mkSpan 8 2)) "x"

nar :: Diagnostic e => e -> Text
nar e = case selectRenderer (withTarget TNarratable defaultConfig) of
  SomeRenderer r@(Narratable _) -> render r e
  _                             -> ""

jsn :: Diagnostic e => e -> Text
jsn e = case selectRenderer (withTarget TJson defaultConfig) of
  SomeRenderer r@(Json _) -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r e)))
  _                       -> ""

prop_deriveEqualsManual :: Property
prop_deriveEqualsManual = withTests 1 . property $ do
  gfx dVal === gfx mVal
  nar dVal === nar mVal
  jsn dVal === jsn mVal

group :: Group
group = Group "Primary/secondary labels"
  [ ("graphical location anchors on the primary label", prop_anchorPrimary)
  , ("JSON marks primary vs secondary explicitly",      prop_jsonFlag)
  , ("graphical uses ^ for primary, - for secondary",   prop_glyphs)
  , ("same-kind labels on one line get distinct glyphs", prop_glyphsSameKindDistinct)
  , ("derived (with secondary) == manual, all handlers",prop_deriveEqualsManual)
  ]

