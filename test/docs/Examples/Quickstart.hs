{-# LANGUAGE OverloadedStrings #-}

-- | The smallest complete diagnostic: a record, an instance, a rendering.
module Examples.Quickstart
  ( UnboundVariable (..)
  , demo
  , example
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Text          (Text)
import           Prettyprinter      (pretty)

import           Example
import           Tadka

-- | What went wrong, and where.
data UnboundVariable = UnboundVariable
  { uvName   :: Text
  , uvSource :: NamedSource
  , uvAt     :: Span
  }

-- A diagnostic code is checked when it is built, so keep it in one place.
unboundCode :: DiagnosticCode
unboundCode = either (error . show) id (mkDiagnosticCode "demo::E0001")

instance Diagnostic UnboundVariable where
  message e = pretty ("unbound variable " <> uvName e)
  code _    = Just unboundCode
  help _    = Just (pretty ("check the spelling, or import the name" :: Text))
  context e = mkContextDegrading (uvSource e)
                (Labeled (uvAt e) Primary (Just (pretty ("not in scope" :: Text))) :| [])

-- | @quantity@ is the eight characters starting at offset 20.
demo :: UnboundVariable
demo = UnboundVariable "quantity" source at
  where
    source = either (error . show) id (mkNamedSource "demo.hs" "let total = price * quantity\n")
    at     = either (error . show) id (mkSpan 20 8)

example :: Example
example = Example
  { exName        = "quickstart"
  , exRun         = renderPlain demo
  , exMustContain =
      [ "unbound variable quantity", "demo::E0001", "not in scope"
      , "check the spelling", "demo.hs", "let total = price * quantity" ]
  }
