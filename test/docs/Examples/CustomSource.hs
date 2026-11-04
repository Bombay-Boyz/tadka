{-# LANGUAGE OverloadedStrings #-}

-- | Bringing your own source representation: here an editor buffer held as a
-- list of lines, which never exists as one big 'Text'. Implement
-- 'SourceCode' and pass the value wherever a source is expected.
module Examples.CustomSource (example) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Prettyprinter      (pretty)

import           Example
import           Tadka

-- | A named buffer: its lines, without line terminators.
data Buffer = Buffer
  { bufName  :: Text
  , bufLines :: [Text]
  }

-- A source always has at least one line, even when empty.
linesOf :: Buffer -> [Text]
linesOf b = if null (bufLines b) then [T.empty] else bufLines b

instance SourceCode Buffer where
  scName = bufName

  -- Length in code points, counting one character per line break.
  scLength b = T.length (T.intercalate "\n" (linesOf b))

  scLineCount b = length (linesOf b)

  -- A window of lines, 1-based and inclusive, clipped to what exists.
  scLines b (lo, hi) =
    [ (n, l) | (n, l) <- zip [1 ..] (linesOf b), n >= max 1 lo, n <= hi ]

  -- Line and column (both 1-based) of a code-point offset.
  scLocate b offset = go 1 offset (linesOf b)
    where
      go n o (l : rest)
        | o <= T.length l || null rest = LineCol n (o + 1)
        | otherwise                    = go (n + 1) (o - T.length l - 1) rest
      go n o []                        = LineCol n (o + 1)

-- | A diagnostic whose source is the buffer.
data NameClash = NameClash Buffer Span

instance Diagnostic NameClash where
  message _ = pretty ("name defined twice" :: Text)
  context (NameClash buf at) =
    mkContextDegrading buf (Labeled at Primary (Just (pretty ("second definition" :: Text))) :| [])

example :: Example
example = Example
  { exName        = "custom-source"
  , exRun         = renderPlain (NameClash buffer at)
  , exMustContain = [ "name defined twice", "second definition", "scratch buffer", "x = 3" ]
  }
  where
    buffer = Buffer "scratch buffer" ["x = 1", "y = 2", "x = 3"]
    at     = either (error . show) id (mkSpan 12 1)
