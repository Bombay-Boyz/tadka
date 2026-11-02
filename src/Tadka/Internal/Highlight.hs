{-# LANGUAGE OverloadedStrings #-}

-- | The renderer's side of syntax highlighting: it applies a
-- 'Tadka.Internal.Config.Highlighter' to a source's lines and checks what
-- comes back. There is no tokenizer here and no dependency on one; the
-- @tadka-highlight@ package provides a skylighting-backed 'Highlighter'.
--
-- Safety rule: a highlighted line is only used if stripping its SGR colour
-- escapes (@ESC [ params m@) yields exactly the plain line the renderer
-- would have printed anyway. A highlighter therefore cannot alter text,
-- shift caret columns, or smuggle any other terminal escape sequence into the
-- report; a line that fails the check falls back to plain text.
--
-- No compatibility guarantee.
module Tadka.Internal.Highlight
  ( applyHighlighter
  , highlightLine
  , stripSgr
  ) where

import           Data.Char                 (isDigit)
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Tadka.Internal.Config     (Highlighter (..))

-- | Run a highlighter over a whole source (so multi-line constructs keep
-- their context). 'Nothing' if the highlighter declines or returns a number
-- of lines different from the number it was given.
applyHighlighter :: Highlighter -> Text -> [Text] -> Maybe [Text]
applyHighlighter h name plain = case runHighlighter h name plain of
  Just out | length out == length plain -> Just out
  _                                     -> Nothing

-- | The highlighted form of the 1-based line @n@, or @plain@ if there is no
-- highlighted text for it or it is not exactly @plain@ plus SGR escapes.
highlightLine :: Maybe [Text] -> Int -> Text -> Text
highlightLine Nothing    _ plain = plain
highlightLine (Just lns) n plain
  | n < 1     = plain
  | otherwise = case drop (n - 1) lns of
      (hl : _) | stripSgr hl == plain -> hl
      _                               -> plain

-- | Remove every complete SGR escape (@ESC [ digits-and-semicolons m@).
-- Anything else, including a lone or malformed ESC, is left in place, so
-- the comparison in 'highlightLine' fails on it.
stripSgr :: Text -> Text
stripSgr t = case T.break (== '\ESC') t of
  (before, rest)
    | T.null rest -> before
    | otherwise   -> case sgrLength rest of
        Just n  -> before <> stripSgr (T.drop n rest)
        Nothing -> before <> T.take 1 rest <> stripSgr (T.drop 1 rest)

-- | Length of the SGR escape at the start of the text, if there is one.
sgrLength :: Text -> Maybe Int
sgrLength t = do
  rest1 <- T.stripPrefix "\ESC[" t
  let (params, rest2) = T.span (\c -> isDigit c || c == ';') rest1
  _ <- T.stripPrefix "m" rest2
  pure (2 + T.length params + 1)
