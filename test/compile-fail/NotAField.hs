{-# LANGUAGE TemplateHaskell #-}
module NotAField where
import Tadka
data T = T { s :: NamedSource } deriving (Show)
data U = U { us :: NamedSource }
-- 'us is a field of U, not of the target T:
deriveDiagnostic defaultSpec { specSourceField = Just 'us } ''T
