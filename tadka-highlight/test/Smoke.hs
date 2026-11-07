{-# LANGUAGE OverloadedStrings #-}

-- The README's highlighting example, built and run: a Haskell source line is
-- coloured, and the text of the line is unchanged.
import           Control.Monad                  (unless)
import           Data.List.NonEmpty             (NonEmpty (..))
import qualified Data.Text                      as T
import           Skylighting                    (pygments)
import           System.Exit                    (exitFailure)
import           System.IO                      (stdout)
import           Tadka
import           Tadka.Highlight.Skylighting    (skylightingAuto)

config :: Config
config = withHighlighter (skylightingAuto pygments) defaultConfig

newtype Demo = Demo NamedSource

instance Diagnostic Demo where
  message _ = "demo"
  context (Demo src) =
    mkContextDegrading src (Labeled (either (error . show) id (mkSpan 20 8)) Primary Nothing :| [])

main :: IO ()
main = do
  let src = either (error . show) id (mkNamedSource "demo.hs" "let total = price * quantity\n")
      cfg = withColorMode ColorAlways (withUnicodeMode UnicodeAlways config)
  out <- renderDiagnosticTo stdout cfg (Demo src)
  let coloured = "\ESC[" `T.isInfixOf` out
      intact   = "total" `T.isInfixOf` out
  unless (coloured && intact) $ do
    putStrLn "highlighting did not colour the snippet"
    exitFailure
  putStrLn "ok: the snippet is highlighted"
