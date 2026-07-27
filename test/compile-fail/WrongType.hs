{-# LANGUAGE TemplateHaskell #-}
module WrongType where
import Tadka
data T = T { s :: NamedSource, a :: Span } deriving (Show)
-- 'a is Span-typed, but specSourceField requires NamedSource:
deriveDiagnostic defaultSpec { specSourceField = Just 'a } ''T
