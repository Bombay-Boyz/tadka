{-# LANGUAGE OverloadedStrings #-}

-- | 'installPanicHook' replaces GHC's uncaught-exception handler, so this runs
-- the installed handler directly, with standard error redirected to a file,
-- instead of letting the test process die. It is a single property in a group
-- of its own because it changes process-wide state.
module PanicHook (group) where

import           Control.Exception      (evaluate, finally, toException)
import           Control.Monad.IO.Class (liftIO)
import           Data.List              (isInfixOf)
import           GHC.Conc               (getUncaughtExceptionHandler,
                                         setUncaughtExceptionHandler)
import           System.Directory       (getTemporaryDirectory, removeFile)
import           GHC.IO.Handle          (hDuplicate, hDuplicateTo)
import           System.IO              (IOMode (ReadMode), hClose, hFlush, hGetContents,
                                         hSetEncoding, openTempFile, stderr, utf8, withFile)

import           Hedgehog

import           Tadka

group :: Group
group = Group "installPanicHook"
  [ ("a diagnostic is reported on stderr, anything else goes to the old handler", prop_panicHook) ]

-- | Run an action with file descriptor 2 pointing at a temporary file, and
-- return what was written to it.
captureStderr :: IO () -> IO String
captureStderr act = do
  dir        <- getTemporaryDirectory
  (path, tmp) <- openTempFile dir "tadka-stderr.txt"
  hFlush stderr
  saved <- hDuplicate stderr
  hDuplicateTo tmp stderr
  act `finally` (hFlush stderr >> hDuplicateTo saved stderr >> hClose saved >> hClose tmp)
  contents <- withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    s <- hGetContents h
    _ <- evaluate (length s)
    pure s
  removeFile path
  pure contents

prop_panicHook :: Property
prop_panicHook = withTests 1 . property $ do
  previous <- liftIO getUncaughtExceptionHandler
  (reported, other) <- liftIO $
    (do installPanicHook (withColorMode ColorNever defaultConfig)
        hook <- getUncaughtExceptionHandler
        r <- captureStderr (hook (toException (DiagnosticException (SomeDiagnostic (adHoc "boom")))))
        o <- captureStderr (hook (toException (userError "plain failure")))
        pure (r, o))
      `finally` setUncaughtExceptionHandler previous
  -- A diagnostic is rendered as a report, with its severity heading.
  assert ("error" `isInfixOf` reported)
  assert ("boom" `isInfixOf` reported)
  -- Any other exception is left to the handler that was installed before.
  assert ("plain failure" `isInfixOf` other)
