{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Phase 9: the generics path derives only 'context', identically to a
-- hand-written 'buildContext' call. @genericContext :: e -> Context@ can, by its
-- type, touch nothing else — code/severity/help/url/message/diagnosticId stay at
-- their class defaults here, exactly as in the manual twin.
module Phase9 (group) where

import qualified Data.Aeson                 as A
import           Data.Text                  (Text)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TLE
import           GHC.Generics               (Generic)
import           Prettyprinter              (LayoutOptions (..), PageWidth (Unbounded),
                                             layoutPretty, pretty)
import           Prettyprinter.Render.Text  (renderStrict)

import           Hedgehog                   (Group (..), Property, property, withTests, (===))

import           Tadka
import           Tadka.Internal             (buildContext)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id

-- Generics-wired: only message + context; context comes from genericContext,
-- which uses the Span field selector names ("gfrom", "gto") as label text.
data GErr = GErr { gsrc :: NamedSource, gfrom :: Span, gto :: Span }
  deriving (Generic)

instance Diagnostic GErr where
  message _ = "generic label wiring"
  context   = genericContext

-- Hand-written twin: same message, context via an explicit buildContext call
-- with the same label texts and order.
data MErr = MErr { msrc :: NamedSource, mfrom :: Span, mto :: Span }

instance Diagnostic MErr where
  message _ = "generic label wiring"
  context e = buildContext (msrc e)
    [ (mfrom e, Just (pretty ("gfrom" :: Text)))
    , (mto e,   Just (pretty ("gto"   :: Text)))
    ]

srcV :: NamedSource
srcV = rightOrErr (mkNamedSource "f.hs" "abcdefghij")

gVal :: GErr
gVal = GErr srcV (rightOrErr (mkSpan 0 3)) (rightOrErr (mkSpan 5 2))

mVal :: MErr
mVal = MErr srcV (rightOrErr (mkSpan 0 3)) (rightOrErr (mkSpan 5 2))

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
group = Group "Phase 9 - generics context-wiring (derived == manual)"
  [ ("graphical renders identically",  once (gfx gVal, gfx mVal))
  , ("narratable renders identically", once (nar gVal, nar mVal))
  , ("json renders identically",       once (jsn gVal, jsn mVal))
  ]
