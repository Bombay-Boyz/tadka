{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Phase 8: the derive path and the manual path are two doors into the same
-- room. A @deriveDiagnostic@-generated instance and a hand-written instance for
-- a structurally identical type render byte-for-byte identically across all
-- three handlers.
module Phase8 (group) where

import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty, pretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog                   (Group (..), Property, property, withTests, (===))

import           Tadka
import           Tadka.Internal             (buildContext)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- Derived via the macro (specMessage keeps the message value-dependent, matching
-- the manual twin exactly rather than dumping the whole record via `show`).
data DErr = DErr { dSrc :: NamedSource, dAt :: Span, dGot :: Text }
  deriving (Show)

deriveDiagnostic defaultSpec
  { specCode        = Just "tadka::E0007"
  , specHelp        = Just "try renaming it"
  , specUrl         = Just "https://example.org/errors/E0007"
  , specSourceField = Just 'dSrc
  , specLabelFields = [('dAt, "here")]
  , specMessage     = Just [| \e -> pretty ("undefined variable " <> dGot e) |]
  }
  ''DErr

-- Hand-written twin: identical fields, identical bodies (calling the same
-- shared functions a manual author would use).
data MErr = MErr { mSrc :: NamedSource, mAt :: Span, mGot :: Text }

instance Diagnostic MErr where
  message e = pretty ("undefined variable " <> mGot e)
  code _    = Just (rightOrErr (mkDiagnosticCode "tadka::E0007"))
  severity _ = SevError
  help _    = Just (pretty ("try renaming it" :: Text))
  url _     = Just (rightOrErr (mkUrl "https://example.org/errors/E0007"))
  context e = buildContext (mSrc e) [(mAt e, Just (pretty ("here" :: Text)))]

srcV :: NamedSource
srcV = rightOrErr (mkNamedSource "f.hs" "x = foo")

spanV :: Span
spanV = rightOrErr (mkSpan 4 3)   -- "foo"

dVal :: DErr
dVal = DErr srcV spanV "foo"

mVal :: MErr
mVal = MErr srcV spanV "foo"

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

once :: (Text, Text) -> Property
once (a, b) = withTests 1 (property (a === b))

group :: Group
group = Group "Phase 8 - deriveDiagnostic (derived == manual)"
  [ ("graphical renders identically",  once (gfx dVal, gfx mVal))
  , ("narratable renders identically", once (nar dVal, nar mVal))
  , ("json renders identically",       once (jsn dVal, jsn mVal))
  ]
