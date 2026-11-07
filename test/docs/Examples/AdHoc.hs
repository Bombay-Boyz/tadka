{-# LANGUAGE OverloadedStrings #-}

-- | Diagnostics without a type of their own: build one from text, give it a
-- code and a help line, wrap an underlying error as its cause, and throw it
-- as an ordinary exception.
module Examples.AdHoc (example, report) where

import           Data.Text (Text)

import           Example
import           Tadka

loadFailed :: AdHocDiagnostic
loadFailed =
    withHelp "check that the file exists and is readable"
  . withCode (either (error . show) id (mkDiagnosticCode "demo::E0004"))
  $ wrapErr "could not load the configuration" (fromError (userError "config.toml: not found"))

-- | Throw it, catch it as a 'SomeDiagnostic', and print the report.
report :: IO ()
report =
  catchDiagnostic (throwDiagnostic loadFailed)
    (\(SomeDiagnostic e) -> reportDiagnostic defaultConfig e)

-- | The same, returning the report as text instead of printing it.
thrownAndCaught :: IO Text
thrownAndCaught =
  catchDiagnostic (throwDiagnostic loadFailed) (\(SomeDiagnostic e) -> renderPlain e)

example :: Example
example = Example
  { exName        = "ad-hoc"
  , exRun         = thrownAndCaught
  , exMustContain =
      [ "could not load the configuration", "demo::E0004"
      , "caused by", "user error (config.toml: not found)"
      , "check that the file exists and is readable" ]
  }
