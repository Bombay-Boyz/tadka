{-# LANGUAGE TemplateHaskell #-}
module WrongCollectionType where
import Tadka
data T = T { s :: NamedSource, a :: Span } deriving (Show)
-- 'a is Span-typed, not [Span]-typed: specLabelCollectionFields needs a list:
deriveDiagnostic defaultSpec { specSourceField = Just 's, specLabelCollectionFields = [('a, "here")] } ''T
