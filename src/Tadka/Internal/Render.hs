{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

-- | The single path from 'Config' to a constructible renderer.
--
-- 'selectRenderer' is the only function that builds a @*Options@ value or wraps
-- one in a 'Renderer' constructor, and the only reader of 'Config''s fields —
-- so there is one route from configuration to a renderer that does anything,
-- not two that could drift apart. 'render''s per-constructor arms dispatch to
-- the handler bodies in "Tadka.Internal.Renderer.*" (placeholders until Phases
-- 5–7).
--
-- No compatibility guarantee.
module Tadka.Internal.Render
  ( Output
  , Renderer (..)
  , SomeRenderer (..)
  , selectRenderer
  , render
  , reportDiagnostic
  ) where

import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Lazy.Char8     as BSL8
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text.IO                   as TIO
import           System.IO                       (stdout)
import           Prettyprinter                  (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text      (renderStrict)

import           Tadka.Internal.Ann             (Ann)
import           Tadka.Internal.Config
                   (Config, Target (..), configColorMode, configPalette,
                    configContextLines, configHyperlinkMode, configRelatedDepth,
                    configTabWidth, configTarget, configUnicodeMode)
import           Tadka.Internal.Diagnostic      (Diagnostic)
import           Tadka.Internal.Terminal        (detectTerminalCaps, resolveConfig)
import           Tadka.Internal.Renderer.Graphical
                   (GraphicalOptions (..), renderGraphical)
import           Tadka.Internal.Renderer.Json
                   (JsonOptions (..), renderJson)
import           Tadka.Internal.Renderer.Narratable
                   (NarratableOptions (..), renderNarratable)

-- | The output type each target renders to. Closed family.
type family Output (t :: Target) where
  Output 'TGraphical  = Doc Ann
  Output 'TNarratable = Text
  Output 'TJson       = Aeson.Value

-- | A renderer indexed by its 'Target'. The three constructors are exported so
-- call sites can pattern-match, but an @*Options@ value can only come from
-- 'selectRenderer', so this is the only route to a renderer that does anything.
data Renderer (t :: Target) where
  Graphical  :: GraphicalOptions  -> Renderer 'TGraphical
  Narratable :: NarratableOptions -> Renderer 'TNarratable
  Json       :: JsonOptions       -> Renderer 'TJson

-- | A renderer with its target hidden, as returned by 'selectRenderer'.
data SomeRenderer = forall t. SomeRenderer (Renderer t)

-- | The sole constructor of @*Options@ values and 'Renderer' wrappers, and the
-- sole reader of 'Config'. An explicit 'withTarget' override wins; absent one,
-- Phase 4 defaults to 'TGraphical' (IO-based terminal/@NO_COLOR@/@--json@
-- detection is threaded in via 'reportDiagnostic' resolving to an explicit
-- target in a later phase, keeping this function pure).
selectRenderer :: Config -> SomeRenderer
selectRenderer cfg =
  case fromMaybe TGraphical (configTarget cfg) of
    TGraphical ->
      SomeRenderer . Graphical $
        GraphicalOptions
          { goColorMode     = configColorMode cfg
          , goUnicodeMode   = configUnicodeMode cfg
          , goHyperlinkMode = configHyperlinkMode cfg
          , goPalette       = configPalette cfg
          , goRelatedDepth  = configRelatedDepth cfg
          , goTabWidth      = configTabWidth cfg
          , goContextLines  = configContextLines cfg
          }
    TNarratable ->
      SomeRenderer (Narratable (NarratableOptions (configRelatedDepth cfg)))
    TJson ->
      SomeRenderer (Json (JsonOptions (configRelatedDepth cfg)))

-- | Render a diagnostic with a specific renderer. Signature fixed here; each
-- arm dispatches to its handler body.
render :: Diagnostic e => Renderer t -> e -> Output t
render (Graphical opts)  e = renderGraphical opts e
render (Narratable opts) e = renderNarratable opts e
render (Json opts)       e = renderJson opts e

-- | Select a renderer, render, and write to the right sink: graphical to the
-- terminal, narratable to stdout, JSON to stdout as encoded JSON. Covers the
-- common case with no manual existential unwrap. (Graphical output is plain
-- text in Phase 4; ANSI styling lands with the handler in Phase 5.)
-- | Detect the sink's capabilities, resolve any @Auto@ modes to concrete ones,
-- then select, render, and write. Explicit colour/Unicode modes are unaffected.
reportDiagnostic :: Diagnostic e => Config -> e -> IO ()
reportDiagnostic cfg e = do
  caps <- detectTerminalCaps stdout
  case selectRenderer (resolveConfig caps cfg) of
    SomeRenderer r@(Graphical _) ->
      TIO.putStrLn (renderStrict (layoutPretty defaultLayoutOptions (render r e)))
    SomeRenderer r@(Narratable _) ->
      TIO.putStrLn (render r e)
    SomeRenderer r@(Json _) ->
      BSL8.putStrLn (Aeson.encode (render r e))
