
--
-- No compatibility guarantee (see "Tadka.Internal").
module Tadka.Internal.SourceCode
  ( SourceCode (..)
  ) where

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Tadka.Internal.Types (NamedSource, sourceName, sourceText)

-- | Read-only, windowed access to a source for rendering.
class SourceCode a where
  -- | Display name of the source (e.g. a file path).
  scName  :: a -> Text
  -- | The 1-based inclusive line range @(firstLine, lastLine)@ as
  -- @(lineNumber, lineText)@ pairs, clamped to the lines that exist and
  -- returned in ascending order. An empty or inverted range yields @[]@.
  scLines :: a -> (Int, Int) -> [(Int, Text)]
  -- | Total number of lines available (used to clamp context windows).
  scLineCount :: a -> Int

-- | The canonical, in-memory instance: the whole source is split once and the
-- requested window is filtered out of it. A trailing @\\r@ is stripped from each
-- line so @\\r\\n@ (CRLF) sources render without stray carriage returns.
instance SourceCode NamedSource where
  scName = sourceName
  scLineCount = length . T.splitOn (T.singleton '\n') . sourceText
  scLines ns (lo, hi)
    | hi < lo   = []
    | otherwise = [ p | p@(n, _) <- numbered, n >= lo, n <= hi ]
    where
      numbered = zip [1 ..] (map dropCR (T.splitOn (T.singleton '\n') (sourceText ns)))
      dropCR t = case T.stripSuffix (T.singleton '\r') t of
        Just t' -> t'
        Nothing -> t
