{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Ad hoc diagnostics, the exception bridge, and writing a report to a
-- handle whose encoding cannot represent it.
module Bridge (group) where

import           Control.Exception      (IOException, evaluate, try)
import           Control.Monad.IO.Class (liftIO)
import           Data.Char              (isAscii)
import qualified Data.Text              as T
import           System.Directory       (getTemporaryDirectory, removeFile)
import           System.IO              (Handle, IOMode (ReadMode), hClose, hGetContents,
                                         hSetEncoding, latin1, openTempFile, utf8, withFile)

import           Hedgehog

import           GenDiag                (GD (..))
import           RenderUtil
import           Tadka

group :: Group
group = Group "Ad hoc diagnostics and the exception bridge"
  [ ("an ad hoc diagnostic has plain defaults",              once prop_defaults)
  , ("withCode, withSeverity, withHelp and withUrl take effect", once prop_setters)
  , ("the setters show up in rendered output",               once prop_settersRendered)
  , ("fromError uses displayException",                      once prop_fromError)
  , ("intoDiagnostic uses show, so a String is quoted",      once prop_intoDiagnostic)
  , ("wrapErr records the cause",                            once prop_wrapErr)
  , ("throwDiagnostic and catchDiagnostic round-trip",       once prop_roundTrip)
  , ("catchDiagnostic leaves other exceptions alone",        once prop_otherExceptions)
  , ("Show of a DiagnosticException is the narratable text", once prop_showException)
  , ("a report to a Latin-1 handle degrades instead of failing", once prop_latin1)
  , ("a report to a UTF-8 handle keeps its Unicode",         once prop_utf8)
  ]

once :: PropertyT IO () -> Property
once = withTests 1 . property

codeE1 :: DiagnosticCode
codeE1 = rightOrErr (mkDiagnosticCode "demo::E0001")

docsUrl :: Url
docsUrl = rightOrErr (mkUrl "https://example.com/errors/E1")

prop_defaults :: PropertyT IO ()
prop_defaults = do
  let d = adHoc "boom"
  severity d === SevError
  code d === Nothing
  fmap unUrl (url d) === Nothing
  fmap docText (help d) === Nothing
  docText (message d) === "boom"

prop_setters :: PropertyT IO ()
prop_setters = do
  let d = withHelp "try again"
        . withUrl docsUrl
        . withSeverity SevWarning
        . withCode codeE1
        $ adHoc "boom"
  code d === Just codeE1
  severity d === SevWarning
  fmap unUrl (url d) === Just "https://example.com/errors/E1"
  fmap docText (help d) === Just "try again"

prop_settersRendered :: PropertyT IO ()
prop_settersRendered = do
  let d = withHelp "try again" (withUrl docsUrl (withCode codeE1 (adHoc "boom")))
      n = renderN plainConfig d
      g = renderG plainConfig d
  assert ("demo::E0001" `T.isInfixOf` n)
  assert ("try again" `T.isInfixOf` n)
  assert ("https://example.com/errors/E1" `T.isInfixOf` n)
  assert ("demo::E0001" `T.isInfixOf` g)
  assert ("try again" `T.isInfixOf` g)

prop_fromError :: PropertyT IO ()
prop_fromError = do
  let out = renderN plainConfig (fromError (userError "disk full"))
  assert ("user error (disk full)" `T.isInfixOf` out)

prop_intoDiagnostic :: PropertyT IO ()
prop_intoDiagnostic = do
  let out = renderN plainConfig (intoDiagnostic ("abc" :: String))
  assert ("\"abc\"" `T.isInfixOf` out)

prop_wrapErr :: PropertyT IO ()
prop_wrapErr = do
  let inner = adHoc "inner problem"
      out   = renderN plainConfig (wrapErr "outer problem" inner)
  assert ("outer problem" `T.isInfixOf` out)
  assert ("Caused by" `T.isInfixOf` out)
  assert ("inner problem" `T.isInfixOf` out)

prop_roundTrip :: PropertyT IO ()
prop_roundTrip = do
  let d = withCode codeE1 (adHoc "boom")
  caught <- liftIO (catchDiagnostic (throwDiagnostic d) (\(SomeDiagnostic e) -> pure (renderN plainConfig e)))
  caught === renderN plainConfig d

prop_otherExceptions :: PropertyT IO ()
prop_otherExceptions = do
  r <- liftIO (try (catchDiagnostic (ioError (userError "not a diagnostic")) (\_ -> pure ())))
  case r of
    Left (_ :: IOException) -> success
    Right ()                -> failure

prop_showException :: PropertyT IO ()
prop_showException = do
  let d = withCode codeE1 (adHoc "boom")
  T.pack (show (DiagnosticException (SomeDiagnostic d))) === renderN defaultConfig d
  T.pack (diagnosticShow d) === renderN defaultConfig d

-- | A diagnostic whose message has characters outside Latin-1.
cjk :: GD
cjk = GD "日本語 ok" Nothing NoContext Nothing Nothing [] Nothing Nothing

-- | Write a report to a temporary file whose handle uses the given encoding,
-- then read the file back as raw Latin-1 characters (one per byte).
writeReport :: (Handle -> IO ()) -> IO String
writeReport setup = do
  dir <- getTemporaryDirectory
  (path, h) <- openTempFile dir "tadka-report.txt"
  setup h
  reportDiagnosticTo h (withColorMode ColorNever defaultConfig) cjk
  hClose h
  contents <- withFile path ReadMode $ \r -> do
    hSetEncoding r latin1
    s <- hGetContents r
    _ <- evaluate (length s)
    pure s
  removeFile path
  pure contents

prop_latin1 :: PropertyT IO ()
prop_latin1 = do
  s <- liftIO (writeReport (\h -> hSetEncoding h latin1))
  assert (all isAscii s)
  assert ("??? ok" `T.isInfixOf` T.pack s)

prop_utf8 :: PropertyT IO ()
prop_utf8 = do
  s <- liftIO (writeReport (\h -> hSetEncoding h utf8))
  -- Read back one byte per character, the three bytes of the UTF-8
  -- encoding of the first character (U+65E5) are E6 97 A5.
  let bytes = T.pack s
  assert ("\230\151\165" `T.isInfixOf` bytes)
  assert (not ("???" `T.isInfixOf` bytes))
