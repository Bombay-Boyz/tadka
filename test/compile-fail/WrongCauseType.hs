{-# LANGUAGE TemplateHaskell #-}
module WrongCauseType where
import Tadka
data T = T { s :: NamedSource, c :: Span } deriving (Show)
-- 'c is Span-typed, but specCause requires Maybe SomeDiagnostic:
deriveDiagnostic defaultSpec { specCause = Just 'c } ''T
