{-# LANGUAGE OverloadedStrings #-}

-- | One documented example: a name, an action producing its output, and
-- fragments that output must contain. The README and tutorial quote these
-- modules, and the output they show is what @--print@ produces.
module Example
  ( Example (..)
  , plainConfig
  , renderWith
  , renderPlain
  ) where

import           Data.Text (Text)
import           System.IO (stdout)

import           Tadka

data Example = Example
  { exName        :: String
  , exRun         :: IO Text
  , exMustContain :: [Text]
  }

-- | Explicit modes, so the output never depends on the terminal it runs in.
plainConfig :: Config
plainConfig = withColorMode ColorNever (withUnicodeMode UnicodeAlways defaultConfig)

renderWith :: Diagnostic e => Config -> e -> IO Text
renderWith = renderDiagnosticTo stdout

renderPlain :: Diagnostic e => e -> IO Text
renderPlain = renderWith plainConfig
