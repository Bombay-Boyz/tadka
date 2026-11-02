{-# LANGUAGE OverloadedStrings #-}

-- | Syntax highlighting for Tadka reports, backed by @skylighting@.
--
-- > import Tadka
-- > import Tadka.Highlight.Skylighting
-- > import Skylighting (pygments)
-- >
-- > cfg = withHighlighter (skylightingAuto pygments) defaultConfig
--
-- The syntax is chosen from the source's name (its file extension), or forced
-- with 'skylightingLanguage'. A source with no matching syntax, or one the
-- tokenizer rejects, is shown as plain text.
--
-- The whole source is tokenized once per report, so multi-line constructs
-- (block comments, strings) keep their context. Tadka only accepts a
-- highlighted line if it is the plain line plus colour escapes, so nothing
-- here can change the text or the caret alignment.
--
-- /Licence:/ @skylighting@ is GPL-2; see the package description.
module Tadka.Highlight.Skylighting
  ( skylightingAuto
  , skylightingLanguage
  , skylightingWith
  ) where

import           Data.Text   (Text)
import qualified Data.Text   as T

import           Skylighting (Style, Syntax, TokenizerConfig (..), defaultFormatOpts,
                              defaultSyntaxMap, formatANSI, lookupSyntax,
                              syntaxesByFilename, tokenize)
import           Tadka       (Highlighter (..))

-- | Pick the syntax from the source's name, e.g. @src/Main.hs@ selects Haskell.
skylightingAuto :: Style -> Highlighter
skylightingAuto style = skylightingWith "skylighting:auto" style byName
  where
    byName name = case syntaxesByFilename defaultSyntaxMap (T.unpack name) of
      (syntax : _) -> Just syntax
      []           -> Nothing

-- | Always use the named syntax (a name or short name such as @"haskell"@).
skylightingLanguage :: Style -> Text -> Highlighter
skylightingLanguage style lang =
  skylightingWith ("skylighting:" <> lang) style (const (lookupSyntax lang defaultSyntaxMap))

-- | The general form: a name for the highlighter (all that 'Eq' and 'Show'
-- see of it), a style, and a function from the source's name to a syntax.
skylightingWith :: Text -> Style -> (Text -> Maybe Syntax) -> Highlighter
skylightingWith name style pick = Highlighter
  { highlighterName = name
  , runHighlighter  = run
  }
  where
    cfg = TokenizerConfig { syntaxMap = defaultSyntaxMap, traceOutput = False }

    run _ []         = Nothing
    run srcName plain = do
      syntax <- pick srcName
      toks   <- either (const Nothing) Just (tokenize cfg syntax (T.intercalate "\n" plain))
      let missing = length plain - length toks
      -- The tokenizer splits its input on newlines and drops trailing empty
      -- lines, so a source ending in blank lines comes back short; pad those
      -- (they have no tokens). More lines than we sent means something
      -- unexpected, so give up rather than misalign.
      if missing < 0
        then Nothing
        else Just (zipWith render plain (toks ++ replicate missing []))

    -- An empty line has nothing to colour; leave it exactly empty.
    render line toks
      | T.null line = line
      | otherwise   = formatANSI defaultFormatOpts style [toks]
