{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The source abstraction: everything a report needs from a piece of source
-- text, and nothing about how it is stored.
--
-- Every renderer, and span resolution itself, goes through 'SourceCode'. The
-- built-in instance is 'NamedSource' (strict 'Text' plus a name). To use
-- another representation (a rope, an editor buffer, an on-demand file
-- reader) write an instance; a 'Tadka.Internal.Context.Context' built from
-- it holds it as a 'SomeSource', so 'Diagnostic's stay one concrete type.
--
-- No compatibility guarantee for this module's path; the names are re-exported
-- from "Tadka".
module Tadka.Internal.SourceCode
  ( SourceCode (..)
  , SomeSource (..)
  , someSource
  ) where

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Tadka.Internal.Types (LineCol (..), NamedSource, sourceName, sourceText)

-- | A named body of source text that spans can point into.
--
-- Laws an instance should satisfy (the test suite checks them for
-- 'NamedSource'):
--
-- * 'scLineCount' is at least 1. An empty source has one empty line, and a
--   trailing newline starts a final, empty line.
-- * @'scLines' s (lo, hi)@ returns the lines numbered @max 1 lo@ to
--   @min hi 'scLineCount'@ in ascending order, each paired with its 1-based
--   number, with the line terminator removed (the LF, and the CR of a CRLF).
--   It returns @[]@ when that range is empty. Renderers ask for small windows,
--   so an instance over a large source should not materialise the rest.
-- * @'scLocate' s o@ is defined for @0 <= o <= 'scLength' s@ and returns the
--   1-based line, and the 1-based column counted in code points from the
--   start of that line (a CR before an LF is a column like any other
--   character, since offsets count it).
class SourceCode a where
  -- | Name shown in reports, usually a file path.
  scName      :: a -> Text
  -- | Length in code points: the unit 'Tadka.Internal.Types.Offset' and
  -- 'Tadka.Internal.Types.Length' are measured in.
  scLength    :: a -> Int
  -- | Number of lines.
  scLineCount :: a -> Int
  -- | A window of lines, inclusive on both ends, 1-based.
  scLines     :: a -> (Int, Int) -> [(Int, Text)]
  -- | The line and column of a code-point offset.
  scLocate    :: a -> Int -> LineCol

instance SourceCode NamedSource where
  scName      = sourceName
  scLength    = T.length . sourceText
  scLineCount = (+ 1) . T.count (T.singleton '\n') . sourceText
  scLines ns (lo, hi)
    | hi < lo'  = []
    | otherwise = zip [lo' ..]
                      (map dropCR (take (hi - lo' + 1)
                                        (drop (lo' - 1) (T.splitOn (T.singleton '\n') (sourceText ns)))))
    where
      lo' = max 1 lo
      dropCR t = case T.stripSuffix (T.singleton '\r') t of
        Just t' -> t'
        Nothing -> t
  scLocate ns off = LineCol lineNo col
    where
      prefix = T.take off (sourceText ns)
      lineNo = T.count (T.singleton '\n') prefix + 1
      col    = T.length (snd (T.breakOnEnd (T.singleton '\n') prefix)) + 1

-- | Any 'SourceCode', with its type forgotten. This is what a
-- 'Tadka.Internal.Context.Context' stores, and what to use to put sources of
-- different types in one multi-source context.
data SomeSource = forall s. SourceCode s => SomeSource s

instance SourceCode SomeSource where
  scName      (SomeSource s)   = scName s
  scLength    (SomeSource s)   = scLength s
  scLineCount (SomeSource s)   = scLineCount s
  scLines     (SomeSource s)   = scLines s
  scLocate    (SomeSource s)   = scLocate s

-- | Shows the source's name only: a source can be large, and 'Show' on a
-- diagnostic is for debugging.
instance Show SomeSource where
  showsPrec d (SomeSource s) = showParen (d > 10) $
    showString "SomeSource " . shows (scName s)

-- | Package any 'SourceCode' as a 'SomeSource'.
someSource :: SourceCode s => s -> SomeSource
someSource = SomeSource
