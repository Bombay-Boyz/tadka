{-# LANGUAGE OverloadedStrings #-}

-- | One diagnostic whose labels live in two different files.
module Examples.MultiFile (example) where

import           Data.List.NonEmpty (NonEmpty (..))

import           Example
import           Tadka

data CrossFileError = CrossFileError
  { cfeSourceA :: NamedSource
  , cfeSourceB :: NamedSource
  , cfeAtA     :: Span
  , cfeAtB     :: Span
  }

instance Diagnostic CrossFileError where
  message _ = "these declarations disagree"

  context e =
    either (const NoContext) id $
      mkContextMulti
        ( (cfeSourceA e, Labeled (cfeAtA e) Primary (Just "defined here") :| [])
          :| [ (cfeSourceB e, Labeled (cfeAtB e) Primary (Just "used here") :| []) ]
        )

crossFile :: CrossFileError
crossFile = CrossFileError sourceA sourceB atA atB
  where
    sourceA = either (error . show) id (mkNamedSource "A.hs" "value = 42\n")
    sourceB = either (error . show) id (mkNamedSource "B.hs" "use = value + 1\n")
    atA     = either (error . show) id (mkSpan 0 5)
    atB     = either (error . show) id (mkSpan 6 5)

example :: Example
example = Example
  { exName        = "multi-file"
  , exRun         = renderPlain crossFile
  , exMustContain = [ "these declarations disagree", "A.hs", "B.hs", "defined here", "used here" ]
  }
