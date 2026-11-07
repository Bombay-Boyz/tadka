{-# LANGUAGE OverloadedStrings #-}

-- | The same diagnostic in each output format: the graphical report for
-- people (shown in the quick start), the narratable one for screen readers
-- and logs, and JSON for tools.
module Examples.Formats (examples, printJson) where

import           Example
import           Examples.Quickstart (demo)
import           Tadka

-- | Choose the output format with 'withTarget'.
narratableConfig, jsonConfig :: Config
narratableConfig = withTarget TNarratable defaultConfig
jsonConfig       = withTarget TJson defaultConfig

-- | Print the report as JSON.
printJson :: IO ()
printJson = reportDiagnostic jsonConfig demo

examples :: [Example]
examples =
  [ Example "format-narratable" (renderWith narratableConfig demo)
      [ "unbound variable quantity", "Location: demo.hs" ]
  , Example "format-json" (renderWith jsonConfig demo)
      [ "\"code\":\"demo::E0001\"", "\"schemaVersion\":1" ]
  ]
