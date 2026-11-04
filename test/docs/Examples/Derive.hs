{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Deriving the 'Diagnostic' instance with Template Haskell, in both
-- spellings: a record update on 'defaultSpec', and the flatter
-- 'diagnosticSpec' list.
module Examples.Derive (example) where

import           Data.Text     (Text)
import qualified Data.Text     as T
import           Prettyprinter (pretty)

import           Example
import           Tadka

-- 1. Record-update form.

data TypeMismatch = TypeMismatch
  { tmSource   :: NamedSource
  , tmAt       :: Span
  , tmExpected :: Text
  , tmActual   :: Text
  }
  deriving (Show)

deriveDiagnostic defaultSpec
  { specCode        = Just "demo::E0002"
  , specHelp        = Just "convert one side explicitly"
  , specSourceField = Just 'tmSource
  , specLabelFields = [('tmAt, "type mismatch here")]
  , specMessage     = Just [| \e -> pretty ("expected " <> tmExpected e <> ", found " <> tmActual e) |]
  }
  ''TypeMismatch

-- 2. List form.

data ArityError = ArityError
  { aeSource :: NamedSource
  , aeCall   :: Span
  , aeWanted :: Int
  , aeGiven  :: Int
  }
  deriving (Show)

deriveDiagnostic
  (diagnosticSpec
     [ diagCode "demo::E0003"
     , diagHelp "remove the extra arguments"
     , diagSource 'aeSource
     , diagLabel 'aeCall "called here"
     , diagMessage [| \e -> pretty ("expected " <> T.pack (show (aeWanted e))
                                    <> " arguments, got " <> T.pack (show (aeGiven e))) |]
     ])
  ''ArityError

mismatch :: TypeMismatch
mismatch = TypeMismatch src at "Int" "Text"
  where
    src = either (error . show) id (mkNamedSource "demo.hs" "total = count + \"1\"\n")
    at  = either (error . show) id (mkSpan 16 3)

arity :: ArityError
arity = ArityError src at 2 3
  where
    src = either (error . show) id (mkNamedSource "demo.hs" "area 3 4 5\n")
    at  = either (error . show) id (mkSpan 0 10)

example :: Example
example = Example
  { exName        = "derive"
  , exRun         = do
      a <- renderPlain mismatch
      b <- renderPlain arity
      pure (a <> "\n\n" <> b)
  , exMustContain =
      [ "expected Int, found Text", "demo::E0002", "type mismatch here"
      , "convert one side explicitly"
      , "expected 2 arguments, got 3", "demo::E0003", "called here"
      , "remove the extra arguments" ]
  }
