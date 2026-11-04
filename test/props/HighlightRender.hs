{-# LANGUAGE OverloadedStrings #-}

-- | Syntax highlighting through the graphical renderer, end to end: a
-- highlighter can add colour and nothing else.
module HighlightRender (group) where

import           Data.Text     (Text)
import qualified Data.Text     as T

import           Hedgehog
import           Prettyprinter (pretty)

import           GenDiag       (GD (..))
import           RenderUtil
import           Tadka
import           Tadka.Internal (buildContext)

group :: Group
group = Group "Highlighting through the renderer"
  [ ("a well-behaved highlighter's colours reach the output", once prop_applied)
  , ("escape sequences from a highlighter are dropped",       once prop_evilDropped)
  , ("a highlighter cannot change the displayed text",        once prop_rewriteIgnored)
  , ("under ColorNever the highlighter is never run",         once prop_neverUnderColorNever)
  , ("escapes in the source never reach the output",          once prop_sourceEscapes)
  ]

once :: PropertyT IO () -> Property
once = withTests 1 . property

-- | A diagnostic pointing at one character of the given source text.
diagnosing :: Text -> Text -> GD
diagnosing name text =
  GD "boom" Nothing (buildContext src [(sp, Just (pretty ("here" :: Text)))])
     Nothing Nothing [] Nothing Nothing
  where
    src = rightOrErr (mkNamedSource name text)
    sp  = rightOrErr (mkSpan 0 1)

sampleDiag :: GD
sampleDiag = diagnosing "demo.hs" "let x = 1\nlet y = 2\n"

-- | Wraps each line in a distinctive 256-colour escape; the default palette
-- never emits this one, so seeing it proves the highlighter's output was used.
colourful :: Highlighter
colourful = Highlighter "colourful" (\_ ls -> Just (map (\l -> "\ESC[38;5;201m" <> l <> "\ESC[0m") ls))

-- | Tries to clear the screen and set the window title.
hostile :: Highlighter
hostile = Highlighter "hostile" (\_ ls -> Just (map (<> "\ESC[2J\ESC]0;pwned\a") ls))

-- | Tries to replace the source text.
rewriter :: Highlighter
rewriter = Highlighter "rewriter" (\_ ls -> Just (map (const "XXXXXXXX") ls))

-- | Fails if it is ever consulted.
tripwire :: Highlighter
tripwire = Highlighter "tripwire" (\_ _ -> error "the highlighter must not run under ColorNever")

prop_applied :: PropertyT IO ()
prop_applied = do
  let with    = renderG (withHighlighter colourful colourConfig) sampleDiag
      without = renderG colourConfig sampleDiag
  assert ("\ESC[38;5;201m" `T.isInfixOf` with)
  assert ("let x = 1" `T.isInfixOf` with)
  assert (not ("\ESC[38;5;201m" `T.isInfixOf` without))

prop_evilDropped :: PropertyT IO ()
prop_evilDropped = do
  let out = renderG (withHighlighter hostile colourConfig) sampleDiag
  assert (not ("\ESC[2J" `T.isInfixOf` out))
  assert (not ("pwned" `T.isInfixOf` out))
  assert ("let x = 1" `T.isInfixOf` out)

prop_rewriteIgnored :: PropertyT IO ()
prop_rewriteIgnored = do
  let out = renderG (withHighlighter rewriter colourConfig) sampleDiag
  assert (not ("XXXXXXXX" `T.isInfixOf` out))
  assert ("let x = 1" `T.isInfixOf` out)

prop_neverUnderColorNever :: PropertyT IO ()
prop_neverUnderColorNever = do
  let out = renderG (withHighlighter tripwire plainConfig) sampleDiag
  assert (not ("\ESC" `T.isInfixOf` out))
  assert ("let x = 1" `T.isInfixOf` out)

-- | A source line that itself contains a clear-screen sequence.
prop_sourceEscapes :: PropertyT IO ()
prop_sourceEscapes = do
  let d = diagnosing "evil.txt" "a\ESC[2Jb = 1\n"
      outs = [ renderG plainConfig d
             , renderG colourConfig d
             , renderG (withHighlighter colourful colourConfig) d
             , renderG (withHighlighter hostile colourConfig) d
             ]
  assert (all (not . T.isInfixOf "\ESC[2J") outs)
