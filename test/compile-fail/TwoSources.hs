{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module TwoSources where
import GHC.Generics (Generic)
import Tadka
data T = T { s1 :: NamedSource, s2 :: NamedSource, a :: Span } deriving (Generic)
instance Diagnostic T where
  message _ = "x"
  context   = genericContext
