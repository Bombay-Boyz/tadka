{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Related diagnostics and cause chains. A cause is one underlying
-- diagnostic; related diagnostics are a list of further context. Both hold
-- 'SomeDiagnostic', so the diagnostics involved can be of different types.
module Examples.Chain (examples) where

import qualified Data.Text     as T
import           Prettyprinter (pretty)

import           Example
import           Tadka

-- 1. A diagnostic with a cause, derived.

data ImportError = ImportError
  { ieSource :: NamedSource
  , ieAt     :: Span
  , ieCause  :: Maybe SomeDiagnostic
  }

deriveDiagnostic defaultSpec
  { specMessage     = Just [| \_ -> "failed to resolve import" |]
  , specSourceField = Just 'ieSource
  , specLabelFields = [('ieAt, "this import")]
  , specCause       = Just 'ieCause
  }
  ''ImportError

-- | The underlying problem, which has a source of its own.
data ConfigError = ConfigError
  { ceSource :: NamedSource
  , ceAt     :: Span
  }

deriveDiagnostic defaultSpec
  { specMessage     = Just [| \_ -> "unknown module path" |]
  , specSourceField = Just 'ceSource
  , specLabelFields = [('ceAt, "no such module")]
  }
  ''ConfigError

importError :: ImportError
importError = ImportError src at (Just (SomeDiagnostic underlying))
  where
    src        = either (error . show) id (mkNamedSource "Main.hs" "import Data.Mapp\n")
    at         = either (error . show) id (mkSpan 7 9)
    underlying = ConfigError cfgSrc cfgAt
    cfgSrc     = either (error . show) id (mkNamedSource "modules.cfg" "unknown = Data.Mapp\n")
    cfgAt      = either (error . show) id (mkSpan 10 9)

-- 2. A diagnostic that gathers others as related diagnostics, by hand.

newtype BuildFailed = BuildFailed [SomeDiagnostic]

instance Diagnostic BuildFailed where
  message (BuildFailed ds) =
    pretty ("build failed with " <> T.pack (show (length ds)) <> " errors")
  related (BuildFailed ds) = ds

buildFailed :: BuildFailed
buildFailed = BuildFailed [SomeDiagnostic importError, SomeDiagnostic (adHoc "disk full")]

-- | The default shows each cause as one line; 'CauseFull' nests its report.
fullCauses :: Config
fullCauses = withCauseDetail CauseFull plainConfig

examples :: [Example]
examples =
  [ Example "cause-summary" (renderPlain importError)
      [ "failed to resolve import", "caused by", "unknown module path" ]
  , Example "cause-full" (renderWith fullCauses importError)
      [ "failed to resolve import", "modules.cfg", "no such module" ]
  , Example "related" (renderPlain buildFailed)
      [ "build failed with 2 errors", "failed to resolve import", "disk full" ]
  ]
