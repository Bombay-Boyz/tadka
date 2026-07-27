{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module SuccessCriterion where
import Data.Text (Text)
import Tadka
data E = E { src :: NamedSource, msg :: Text, at :: Span } deriving (Show)
-- The user meant 'at, but misspelled the span field as 'msg (a Text field):
deriveDiagnostic defaultSpec { specSourceField = Just 'src, specLabelFields = [('msg, "here")] } ''E
