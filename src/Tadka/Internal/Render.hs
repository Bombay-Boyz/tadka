{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}


-- No compatibility guarantee.
module Tadka.Internal.Render
  ( Output
  , Renderer (..)
  , SomeRenderer (..)
  , selectRenderer
  , render
  , reportDiagnostic
  , reportDiagnosticTo
  , renderDiagnosticTo
  ) where

import qualified Data.Aeson                     as Aeson
import qualified Data.ByteString.Lazy           as BSL
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Text.Encoding             (decodeUtf8)
import           System.IO                       (Handle, stdout)
import           Prettyprinter                  (Doc, defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text      (renderStrict)

import           Tadka.Internal.Ann             (Ann)
import           Tadka.Internal.Config          (Config, Target (..), configTarget)
import           Tadka.Internal.Diagnostic      (Diagnostic)
import           Tadka.Internal.Terminal        (detectTerminalCaps, handleIsUnicode, resolveConfig)
import           Tadka.Internal.Renderer.Graphical
                   (GraphicalOptions, graphicalOptions, renderGraphical)
import           Tadka.Internal.Renderer.Json
                   (JsonOptions, jsonOptions, renderJson)
import           Tadka.Internal.Renderer.Narratable
                   (NarratableOptions, narratableOptions, renderNarratable)

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


selectRenderer :: Config -> SomeRenderer
selectRenderer cfg =
  case fromMaybe TGraphical (configTarget cfg) of
    TGraphical  -> SomeRenderer (Graphical  (graphicalOptions cfg))
    TNarratable -> SomeRenderer (Narratable (narratableOptions cfg))
    TJson       -> SomeRenderer (Json       (jsonOptions cfg))

-- | Render a diagnostic with a specific renderer. Signature fixed here; each
-- arm dispatches to its handler body.
render :: Diagnostic e => Renderer t -> e -> Output t
render (Graphical opts)  e = renderGraphical opts e
render (Narratable opts) e = renderNarratable opts e
render (Json opts)       e = renderJson opts e

-- | Render a diagnostic to exactly the 'Text' 'reportDiagnosticTo' would
-- print (minus the trailing newline it adds), for a log sink, a test
-- assertion, or any destination that isn't a bare 'Handle'. Terminal-
-- capability auto-detection ('ColorAuto' etc.) needs a real handle to query
-- its TTY-ness against, so this still takes one — and resolves against it —
-- without writing anything to it itself.
renderDiagnosticTo :: Diagnostic e => Handle -> Config -> e -> IO Text
renderDiagnosticTo h cfg e = do
  caps <- detectTerminalCaps h
  pure $ case selectRenderer (resolveConfig caps cfg) of
    SomeRenderer r@(Graphical _)  -> renderStrict (layoutPretty defaultLayoutOptions (render r e))
    SomeRenderer r@(Narratable _) -> render r e
    SomeRenderer r@(Json _)       -> decodeUtf8 (BSL.toStrict (Aeson.encode (render r e)))

-- | Report a diagnostic to an arbitrary 'Handle' (e.g. 'stderr', a log
-- file), instead of 'reportDiagnostic''s hardcoded 'stdout'. Terminal
-- capabilities are detected against the SAME handle being written to, so
-- 'ColorAuto' etc. still means "does the destination I'm about to write to
-- look like a terminal", never "does stdout, regardless of where this is
-- headed".
reportDiagnosticTo :: Diagnostic e => Handle -> Config -> e -> IO ()
reportDiagnosticTo h cfg e = do
  txt <- renderDiagnosticTo h cfg e
  uni <- handleIsUnicode h
  TIO.hPutStrLn h (if uni then txt else asciiOnly txt)
  where
    -- A handle that cannot encode Unicode (a C\/POSIX locale, a Latin-1
    -- terminal, a binary handle) raises an exception on the first character
    -- it cannot encode, possibly after part of the report was already
    -- written. Non-ASCII characters (source text, messages) become @?@
    -- instead; the box-drawing glyphs were already replaced by ASCII ones,
    -- since 'detectTerminalCaps' reports such a handle as non-Unicode.
    asciiOnly = T.map (\c -> if c < '\x80' then c else '?')

-- | Report a diagnostic to 'stdout'. A thin specialisation of
-- 'reportDiagnosticTo' kept for backward compatibility and as the
-- "just reach for it" default; reach for 'reportDiagnosticTo' directly to
-- write anywhere else (e.g. 'System.IO.stderr').
reportDiagnostic :: Diagnostic e => Config -> e -> IO ()
reportDiagnostic = reportDiagnosticTo stdout
