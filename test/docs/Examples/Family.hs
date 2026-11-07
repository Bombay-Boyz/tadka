{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
#if __GLASGOW_HASKELL__ >= 910
{-# OPTIONS_GHC -Wno-incomplete-record-selectors #-}
#endif

-- | A whole family of errors derived at once from one sum type.
module Examples.Family (examples) where

import           Prettyprinter (pretty)

import           Example
import           Tadka

data CompileError
  = ParseFailure
      { crSource :: NamedSource
      , crAt     :: Span
      }
  | TypeMismatch
      { tmSource   :: NamedSource
      , tmAt       :: Span
      , tmExpected :: String
      }

deriveDiagnosticSum
  [ ( 'ParseFailure
    , defaultSpec
        { specMessage     = Just [| \_ -> "unexpected token" |]
        , specSourceField = Just 'crSource
        , specLabelFields = [('crAt, "here")]
        }
    )
  , ( 'TypeMismatch
    , defaultSpec
        { specSeverity    = SevWarning
        , specMessage     = Just [| \e -> "type mismatch, expected " <> pretty (tmExpected e) |]
        , specSourceField = Just 'tmSource
        , specLabelFields = [('tmAt, "here")]
        }
    )
  ]
  ''CompileError

parseFailure :: CompileError
parseFailure = ParseFailure src at
  where
    src = either (error . show) id (mkNamedSource "Main.hs" "x = foo\n")
    at  = either (error . show) id (mkSpan 4 3)

typeMismatch :: CompileError
typeMismatch = TypeMismatch src at "Int"
  where
    src = either (error . show) id (mkNamedSource "Main.hs" "y = 'c'\n")
    at  = either (error . show) id (mkSpan 4 3)

examples :: [Example]
examples =
  [ Example "family-error" (renderPlain parseFailure)
      [ "unexpected token", "Main.hs", "here" ]
  , Example "family-warning" (renderPlain typeMismatch)
      [ "type mismatch, expected Int", "warning" ]
  ]
