{-# LANGUAGE OverloadedStrings #-}

-- | Colouring source snippets with a 'Highlighter'. This one is a toy that
-- makes the word @let@ cyan; the @tadka-highlight@ package provides a real
-- one. Whatever a highlighter returns, Tadka only keeps it if removing the
-- colour codes gives back the original line.
module Examples.Highlight (example) where

import qualified Data.Text    as T

import           Example
import           Examples.Quickstart (demo)
import           Tadka

-- | Receives the source's name and all its lines; returns one coloured line
-- per input line, or 'Nothing' to leave the source plain.
keywords :: Highlighter
keywords = Highlighter
  { highlighterName = "keywords"
  , runHighlighter  = \_ ls -> Just (map (T.replace "let" "\ESC[36mlet\ESC[0m") ls)
  }

-- | Colour must be on for a highlighter to run.
coloured :: Config
coloured = withHighlighter keywords (withColorMode ColorAlways (withUnicodeMode UnicodeAlways defaultConfig))

example :: Example
example = Example
  { exName        = "highlight"
  , exRun         = renderWith coloured demo
  , exMustContain = [ "\ESC[36mlet\ESC[0m total = price * quantity" ]
  }
