{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers for the tests that render diagnostics: one function per
-- renderer, each taking an explicit 'Config' so no terminal detection is ever
-- involved.
module RenderUtil
  ( plainConfig
  , colourConfig
  , renderG
  , renderN
  , renderJ
  , docText
  , rightOrErr
  ) where

import qualified Data.Aeson                as A
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TLE
import           Prettyprinter             (Doc, LayoutOptions (..), PageWidth (Unbounded),
                                            layoutPretty)
import           Prettyprinter.Render.Text (renderStrict)

import           Tadka

-- | No colour, Unicode box drawing: the stable form for assertions.
plainConfig :: Config
plainConfig = withColorMode ColorNever (withUnicodeMode UnicodeAlways defaultConfig)

-- | Colour on, so a highlighter is consulted.
colourConfig :: Config
colourConfig = withColorMode ColorAlways (withUnicodeMode UnicodeAlways defaultConfig)

renderG :: Diagnostic e => Config -> e -> Text
renderG cfg e = case selectRenderer (withTarget TGraphical cfg) of
  SomeRenderer r@(Graphical _) -> renderStrict (layoutPretty (LayoutOptions Unbounded) (render r e))
  _                            -> ""

renderN :: Diagnostic e => Config -> e -> Text
renderN cfg e = case selectRenderer (withTarget TNarratable cfg) of
  SomeRenderer r@(Narratable _) -> render r e
  _                             -> ""

renderJ :: Diagnostic e => Config -> e -> Text
renderJ cfg e = case selectRenderer (withTarget TJson cfg) of
  SomeRenderer r@(Json _) -> TL.toStrict (TLE.decodeUtf8 (A.encode (render r e)))
  _                       -> ""

-- | A document as plain text, annotations dropped.
docText :: Doc ann -> Text
docText = renderStrict . layoutPretty (LayoutOptions Unbounded)

rightOrErr :: Show a => Either a b -> b
rightOrErr = either (error . show) id
