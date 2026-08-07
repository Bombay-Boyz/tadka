{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Every constructor below needs its own source/span/message shape, so at
-- least one field per constructor (the payload distinguishing that variant)
-- is inherently partial: GHC's auto-generated accessor for e.g. 'tmExpected'
-- can only be total if every constructor has a 'tmExpected' field of the
-- same type, which isn't true of a genuinely heterogeneous sum type by
-- definition. Shared fields ('crSrc', 'crAt') are named and typed
-- identically across all three constructors specifically to avoid this
-- where the shape genuinely allows it; the rest ('peId', 'tmExpected',
-- 'tmActual', 'uvName', 'uvPrior', 'uvRelated', 'uvCause', 'uvId') cannot be
-- shared without losing the type-level distinction the test exists to
-- exercise, so the warning is suppressed here rather than upstream —
-- any real 'deriveDiagnosticSum' consumer with a similarly heterogeneous
-- sum type hits the same tradeoff and makes the same call locally.
{-# OPTIONS_GHC -Wno-partial-fields #-}


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
  = ParseFailure { crSrc :: NamedSource, crAt :: Span, peId :: DiagnosticId }
  | TypeMismatch { crSrc :: NamedSource, crAt :: Span, tmExpected :: Text, tmActual :: Text }
  | UndefinedVar { crSrc :: NamedSource, crAt :: Span, uvName :: Text, uvPrior :: [Span]
                 , uvRelated :: [SomeDiagnostic], uvCause :: Maybe SomeDiagnostic, uvId :: Text }

deriveDiagnosticSum
  [ ( 'ParseFailure
    , defaultSpec
        { specCode        = Just "tadka::E0101"
        , specSourceField = Just 'crSrc
        , specLabelFields = [('crAt, "here")]
        , specId          = Just 'peId
        , specMessage     = Just [| \_ -> pretty ("unexpected token" :: Text) |]
        }
    )
  , ( 'TypeMismatch
    , defaultSpec
        { specSeverity    = SevWarning
        , specHelp        = Just "check the type annotation"
        , specSourceField = Just 'crSrc
        , specLabelFields = [('crAt, "here")]
        , specMessage     = Just [| \e -> pretty ("type mismatch: expected " <> tmExpected e
                                                     <> ", got " <> tmActual e) |]
        }
    )
  , ( 'UndefinedVar
    , defaultSpec
        { specSourceField                    = Just 'crSrc
        , specLabelFields                    = [('crAt, "used here")]
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
  = ParseFailureM { crmSrc :: NamedSource, crmAt :: Span, pemId :: DiagnosticId }
  | TypeMismatchM { crmSrc :: NamedSource, crmAt :: Span, tmmExpected :: Text, tmmActual :: Text }
  | UndefinedVarM { crmSrc :: NamedSource, crmAt :: Span, uvmName :: Text, uvmPrior :: [Span]
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

  context (ParseFailureM { crmSrc = s, crmAt = a }) = buildContext s [(a, Just "here")]
  context (TypeMismatchM { crmSrc = s, crmAt = a }) = buildContext s [(a, Just "here")]
  context (UndefinedVarM { crmSrc = s, crmAt = a, uvmPrior = prior }) =
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
