{-# LANGUAGE OverloadedStrings #-}

-- | A configuration built up from 'defaultConfig', one setting at a time.
module Examples.Configure (example) where

import           Example
import           Examples.Quickstart (demo)
import           Tadka

myConfig :: Config
myConfig =
    withColorMode ColorNever
  $ withUnicodeMode UnicodeAlways
  $ withHyperlinkMode HyperlinkAlways
  $ withContextLines 3
  $ withTabWidth 4
  $ withRelatedDepthLimit 5
  $ withTarget TGraphical
  $ defaultConfig

example :: Example
example = Example
  { exName        = "configure"
  , exRun         = renderWith myConfig demo
  , exMustContain = [ "unbound variable quantity", "demo::E0001" ]
  }
