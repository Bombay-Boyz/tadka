{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module BadCode where
import Tadka
data T = T { s :: NamedSource } deriving (Show)
-- "nope" is not a valid diagnostic code (grammar: ns::E####):
deriveDiagnostic defaultSpec { specCode = Just "nope", specSourceField = Just 's } ''T
