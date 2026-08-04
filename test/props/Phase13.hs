{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Phase 13: 'deriveDiagnosticSum' is 'deriveDiagnostic''s multi-constructor
-- counterpart (vision §6, "Sum types (v6)"). Same proof obligation as Phase 8:
-- a derived instance and a hand-written instance for a structurally identical
-- sum type render byte-for-byte identically across all three handlers. The
-- three constructors here are chosen to exercise every 'DiagnosticSpec' field
-- across the sum, not just one shape repeated three times: fixed primary
-- labels, a secondary collection field, a literal and a dynamic message, both
-- 'IdKind's ('DiagnosticId' directly and via 'Text'), a non-default severity,
-- @help@, @related@, and @diagnosticCause@ all appear on at least one
-- constructor and are absent on at least one other.
module Phase13 (group) where

import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty, pretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog                   (Group (..), Property, property, withTests, (===))

import           Tadka
import           Tadka.Internal             (buildContext, buildContextWith)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- === Derived via deriveDiagnosticSum =======================================

data CompileError
  = ParseFailure { peSrc :: NamedSource, peAt :: Span, peId :: DiagnosticId }
  | TypeMismatch { tmSrc :: NamedSource, tmAt :: Span, tmExpected :: Text, tmActual :: Text }
  | UndefinedVar { uvSrc :: NamedSource, uvAt :: Span, uvName :: Text, uvPrior :: [Span]
                 , uvRelated :: [SomeDiagnostic], uvCause :: Maybe SomeDiagnostic, uvId :: Text }

deriveDiagnosticSum
  [ ( 'ParseFailure
    , defaultSpec
        { specCode        = Just "tadka::E0101"
        , specSourceField = Just 'peSrc
        , specLabelFields = [('peAt, "here")]
        , specId          = Just 'peId
        , specMessage     = Just [| \_ -> pretty ("unexpected token" :: Text) |]
        }
    )
  , ( 'TypeMismatch
    , defaultSpec
        { specSeverity    = SevWarning
        , specHelp        = Just "check the type annotation"
        , specSourceField = Just 'tmSrc
        , specLabelFields = [('tmAt, "here")]
        , specMessage     = Just [| \e -> pretty ("type mismatch: expected " <> tmExpected e
                                                     <> ", got " <> tmActual e) |]
        }
    )
  , ( 'UndefinedVar
    , defaultSpec
        { specSourceField                    = Just 'uvSrc
        , specLabelFields                    = [('uvAt, "used here")]
        , specSecondaryLabelCollectionFields  = [('uvPrior, "shadowed here")]
        , specRelated                        = Just 'uvRelated
        , specCause                          = Just 'uvCause
        , specId                             = Just 'uvId
        , specMessage                        = Just [| \e -> pretty ("undefined variable " <> uvName e) |]
        }
    )
  ]
  ''CompileError

-- === Hand-written twin: identical fields, identical bodies =================

data CompileErrorManual
  = ParseFailureM { pemSrc :: NamedSource, pemAt :: Span, pemId :: DiagnosticId }
  | TypeMismatchM { tmmSrc :: NamedSource, tmmAt :: Span, tmmExpected :: Text, tmmActual :: Text }
  | UndefinedVarM { uvmSrc :: NamedSource, uvmAt :: Span, uvmName :: Text, uvmPrior :: [Span]
                  , uvmRelated :: [SomeDiagnostic], uvmCause :: Maybe SomeDiagnostic, uvmId :: Text }

instance Diagnostic CompileErrorManual where
  message ParseFailureM{}                        = pretty ("unexpected token" :: Text)
  message (TypeMismatchM { tmmExpected = ex, tmmActual = ac }) =
    pretty ("type mismatch: expected " <> ex <> ", got " <> ac)
  message (UndefinedVarM { uvmName = nm })       = pretty ("undefined variable " <> nm)

  code (ParseFailureM {}) = Just (rightOrErr (mkDiagnosticCode "tadka::E0101"))
  code _                  = Nothing

  severity (TypeMismatchM {}) = SevWarning
  severity _                  = SevError

  help (TypeMismatchM {}) = Just (pretty ("check the type annotation" :: Text))
  help _                  = Nothing

  context (ParseFailureM { pemSrc = s, pemAt = a }) = buildContext s [(a, Just "here")]
  context (TypeMismatchM { tmmSrc = s, tmmAt = a }) = buildContext s [(a, Just "here")]
  context (UndefinedVarM { uvmSrc = s, uvmAt = a, uvmPrior = prior }) =
    buildContextWith s
      (  [ (a, Primary, Just "used here") ]
      ++ [ (p, Secondary, Just "shadowed here") | p <- prior ] )

  related (UndefinedVarM { uvmRelated = r }) = r
  related _                                  = []

  diagnosticCause (UndefinedVarM { uvmCause = c }) = c
  diagnosticCause _                                = Nothing

  diagnosticId (ParseFailureM { pemId = i }) = Just i
  diagnosticId (UndefinedVarM { uvmId = i })  = Just (mkDiagnosticId i)
  diagnosticId _                              = Nothing

-- === Fixtures ===============================================================

srcV :: NamedSource
srcV = rightOrErr (mkNamedSource "f.hs" "let x = 1 in y")

spanX, spanY :: Span
spanX = rightOrErr (mkSpan 4 1)    -- "x"
spanY = rightOrErr (mkSpan 14 1)   -- "y"

priorD :: CompileError
priorD = ParseFailure srcV spanX (mkDiagnosticId "prior")

priorM :: CompileErrorManual
priorM = ParseFailureM srcV spanX (mkDiagnosticId "prior")

pfD :: CompileError
pfD = ParseFailure srcV spanX (mkDiagnosticId "pf-1")

pfM :: CompileErrorManual
pfM = ParseFailureM srcV spanX (mkDiagnosticId "pf-1")

tmD :: CompileError
tmD = TypeMismatch srcV spanX "Int" "Bool"

tmM :: CompileErrorManual
tmM = TypeMismatchM srcV spanX "Int" "Bool"

uvD :: CompileError
uvD = UndefinedVar srcV spanY "y" [spanX] [SomeDiagnostic priorD] (Just (SomeDiagnostic priorD)) "uv-1"

uvM :: CompileErrorManual
uvM = UndefinedVarM srcV spanY "y" [spanX] [SomeDiagnostic priorM] (Just (SomeDiagnostic priorM)) "uv-1"

-- === Renderers (identical to Phase 8's) =====================================

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

-- === Properties =============================================================

prop_parseFailure :: Property
prop_parseFailure = withTests 1 . property $ do
  gfx pfD === gfx pfM
  nar pfD === nar pfM
  jsn pfD === jsn pfM

prop_typeMismatch :: Property
prop_typeMismatch = withTests 1 . property $ do
  gfx tmD === gfx tmM
  nar tmD === nar tmM
  jsn tmD === jsn tmM

prop_undefinedVar :: Property
prop_undefinedVar = withTests 1 . property $ do
  gfx uvD === gfx uvM
  nar uvD === nar uvM
  jsn uvD === jsn uvM

group :: Group
group = Group "Phase 13 - deriveDiagnosticSum (derived == manual, per constructor)"
  [ ("ParseFailure: full property", prop_parseFailure)
  , ("TypeMismatch: full property", prop_typeMismatch)
  , ("UndefinedVar (secondary collection + related + cause): full property", prop_undefinedVar)
  ]
