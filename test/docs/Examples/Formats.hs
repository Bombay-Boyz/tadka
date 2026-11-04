{-# LANGUAGE OverloadedStrings #-}

-- | The same diagnostic in each output format: the graphical report for
-- people, the narratable one for screen readers and logs, and JSON for tools.
module Examples.Formats (example) where

import           Data.Text           (Text)

import           Example
import           Examples.Quickstart (demo)
import           Tadka

-- | The report in all three formats, one after another.
allFormats :: IO Text
allFormats = do
  graphical  <- renderWith (withTarget TGraphical plainConfig) demo
  narratable <- renderWith (withTarget TNarratable plainConfig) demo
  json       <- renderWith (withTarget TJson plainConfig) demo
  pure (graphical <> "\n\n" <> narratable <> "\n\n" <> json)

example :: Example
example = Example
  { exName        = "formats"
  , exRun         = allFormats
  , exMustContain =
      [ "unbound variable quantity"        -- graphical and narratable
      , "Location: demo.hs"                -- narratable
      , "\"code\":\"demo::E0001\""         -- JSON
      , "\"schemaVersion\":1" ]            -- JSON
  }
